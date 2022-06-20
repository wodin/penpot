;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.changes
  (:require
   [app.common.data :as d]
   [app.common.logging :as log]
   [app.common.pages :as cp]
   [app.common.pages.changes-builder :as pcb]
   [app.common.pages.changes-spec :as pcs]
   [app.common.pages.helpers :as cph]
   [app.common.spec :as us]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [app.main.worker :as uw]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [potok.core :as ptk]))

;; Change this to :info :debug or :trace to debug this module
(log/set-level! :warn)

(s/def ::coll-of-uuid
  (s/every ::us/uuid))

(defonce page-change? #{:add-page :mod-page :del-page :mov-page})

(declare commit-changes)

(def commit-changes? (ptk/type? ::commit-changes))

(defn update-shapes
  ([ids update-fn] (update-shapes ids update-fn nil))
  ([ids update-fn {:keys [reg-objects? save-undo? attrs ignore-tree page-id]
                   :or {reg-objects? false save-undo? true}}]

   (us/assert ::coll-of-uuid ids)
   (us/assert fn? update-fn)

   (ptk/reify ::update-shapes
     ptk/WatchEvent
     (watch [it state _]
       (let [page-id   (or page-id (:current-page-id state))
             objects   (wsh/lookup-page-objects state page-id)
             ids       (into [] (filter some?) ids)

             changes   (reduce
                         (fn [changes id]
                           (let [opts {:attrs attrs :ignore-geometry? (get ignore-tree id)}]
                             (pcb/update-shapes changes [id] update-fn opts)))
                         (-> (pcb/empty-changes it page-id)
                             (pcb/set-save-undo? save-undo?)
                             (pcb/with-objects objects))
                         ids)]

         (when (seq (:redo-changes changes))
           (let [changes  (cond-> changes reg-objects? (pcb/resize-parents ids))]
             (rx/of (commit-changes changes)))))))))

(defn send-update-indices
  []
  (ptk/reify ::send-update-indices
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rx/of
            (fn [state]
              (-> state
                  (dissoc ::update-indices-debounce)
                  (dissoc ::update-changes))))
           (rx/observe-on :async)))

    ptk/EffectEvent
    (effect [_ state _]
      (doseq [[page-id changes] (::update-changes state)]
        (uw/ask! {:cmd :update-page-indices
                  :page-id page-id
                  :changes changes})))))

;; Update indices will debounce operations so we don't have to update
;; the index several times (which is an expensive operation)
(defn update-indices
  [page-id changes]

  (let [start (uuid/next)]
    (ptk/reify ::update-indices
      ptk/UpdateEvent
      (update [_ state]
        (if (nil? (::update-indices-debounce state))
          (assoc state ::update-indices-debounce start)
          (update-in state [::update-changes page-id] (fnil d/concat-vec []) changes)))

      ptk/WatchEvent
      (watch [_ state stream]
        (if (= (::update-indices-debounce state) start)
          (let [stopper (->> stream (rx/filter (ptk/type? :app.main.data.workspace/finalize)))]
            (rx/merge
             (->> stream
                  (rx/filter (ptk/type? ::update-indices))
                  (rx/debounce 50)
                  (rx/take 1)
                  (rx/map #(send-update-indices))
                  (rx/take-until stopper))
             (rx/of (update-indices page-id changes))))
          (rx/empty))))))

(defn changed-frames
  "Extracts the frame-ids changed in the given changes"
  [changes objects]

  (let [change->ids
        (fn [change]
          (case (:type change)
            :add-obj
            [(:parent-id change)]

            (:mod-obj :del-obj)
            [(:id change)]

            :mov-objects
            (d/concat-vec (:shapes change) [(:parent-id change)])

            []))]
    (into #{}
          (comp (mapcat change->ids)
                (keep #(cph/get-shape-id-root-frame objects %))
                (remove #(= uuid/zero %)))
          changes)))

(defn commit-changes
  [{:keys [redo-changes undo-changes
           origin save-undo? file-id]
    :or {save-undo? true}}]
  (log/debug :msg "commit-changes"
             :js/redo-changes redo-changes
             :js/undo-changes undo-changes)
  (let [error  (volatile! nil)
        page-id (:current-page-id @st/state)
        frames (changed-frames redo-changes (wsh/lookup-page-objects @st/state))]
    (ptk/reify ::commit-changes
      cljs.core/IDeref
      (-deref [_]
        {:file-id file-id
         :hint-events @st/last-events
         :hint-origin (ptk/type origin)
         :changes redo-changes
         :page-id page-id
         :frames frames})

      ptk/UpdateEvent
      (update [_ state]
        (let [current-file-id (get state :current-file-id)
              file-id         (or file-id current-file-id)
              path            (if (= file-id current-file-id)
                                [:workspace-data]
                                [:workspace-libraries file-id :data])]
          (try
            (us/assert ::pcs/changes redo-changes)
            (us/assert ::pcs/changes undo-changes)

            (update-in state path (fn [file]
                                    (-> file
                                        (cp/process-changes redo-changes false)
                                        (ctst/update-object-indices page-id))))

            (catch :default err
              (log/error :js/error err)
              (vreset! error err)
              state))))

      ptk/WatchEvent
      (watch [_ _ _]
        (when-not @error
          (let [;; adds page-id to page changes (that have the `id` field instead)
                add-page-id
                (fn [{:keys [id type page] :as change}]
                  (cond-> change
                    (page-change? type)
                    (assoc :page-id (or id (:id page)))))

                changes-by-pages
                (->> redo-changes
                     (map add-page-id)
                     (remove #(nil? (:page-id %)))
                     (group-by :page-id))

                process-page-changes
                (fn [[page-id _changes]]
                  (update-indices page-id redo-changes))]
            (rx/concat
             (rx/from (map process-page-changes changes-by-pages))

             (when (and save-undo? (seq undo-changes))
               (let [entry {:undo-changes undo-changes
                            :redo-changes redo-changes}]
                 (rx/of (dwu/append-undo entry)))))))))))
