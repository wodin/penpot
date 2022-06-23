;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.types.file
  (:require
   [app.common.data :as d]
   [app.common.pages.common :refer [file-version]]
   [app.common.spec :as us]
   [app.common.types.components-list :as ctnl]
   [app.common.types.container :as ctc]
   [app.common.types.page :as ctp]
   [app.common.types.pages-list :as ctpl]
   [app.common.types.shape.color :as ctsc]
   [app.common.uuid :as uuid]
   [clojure.spec.alpha :as s]))

;; Specs

(s/def :internal.media-object/name string?)
(s/def :internal.media-object/width ::us/safe-integer)
(s/def :internal.media-object/height ::us/safe-integer)
(s/def :internal.media-object/mtype string?)

;; NOTE: This is marked as nilable for backward compatibility, but
;; right now is just exists or not exists. We can thin in a gradual
;; migration and then mark it as not nilable.
(s/def :internal.media-object/path (s/nilable string?))

(s/def ::media-object
  (s/keys :req-un [::id
                   ::name
                   :internal.media-object/width
                   :internal.media-object/height
                   :internal.media-object/mtype]
          :opt-un [:internal.media-object/path]))

(s/def ::colors
  (s/map-of uuid? ::ctsc/color))

(s/def ::recent-colors
  (s/coll-of ::ctsc/recent-color :kind vector?))

(s/def ::typographies
  (s/map-of uuid? :ctst/typography))

(s/def ::pages
  (s/coll-of uuid? :kind vector?))

(s/def ::media
  (s/map-of uuid? ::media-object))

(s/def ::pages-index
  (s/map-of uuid? ::ctp/page))

(s/def ::components
  (s/map-of uuid? ::ctp/container))

(s/def ::data
  (s/keys :req-un [::pages-index
                   ::pages]
          :opt-un [::colors
                   ::recent-colors
                   ::typographies
                   ::media]))

;; Initialization

(def empty-file-data
  {:version file-version
   :pages []
   :pages-index {}})

(defn make-file-data
  ([file-id]
   (make-file-data file-id (uuid/next)))

  ([file-id page-id]
   (let [page (ctp/make-empty-page page-id "Page-1")]
     (-> empty-file-data
         (assoc :id file-id)
         (ctpl/add-page page)))))


;; Helpers

(defn containers-seq
  [file-data]
  (concat (map #(ctc/make-container % :page) (ctpl/pages-seq file-data))
          (map #(ctc/make-container % :component) (ctnl/components-seq file-data))))

(defn absorb-assets
  "Find all assets of a library that are used in the file, and
  move them to the file local library."
  [file-data library-data]
  (let [library-page-id (uuid/next)

        add-library-page
        (fn [file-data]
          (let [page (ctp/make-empty-page library-page-id "Library page")]
            (-> file-data
                (ctpl/add-page page))))

        find-instances-in-container
        (fn [container component]
          (let [instances (filter #(= (:component-id %) (:id component))
                                  (ctc/shapes-seq container))]
            (when (d/not-empty? instances)
              [[container instances]])))

        find-instances
        (fn [file-data component]
          (mapcat #(find-instances-in-container % component) (containers-seq file-data)))

        absorb-component
        (fn [file-data component]
          file-data)

        used-components
        (mapcat (fn [used component]
                  (let [instances (find-instances file-data component)]
                    (when instances
                      [[component instances]])))
                (ctnl/components-seq library-data))]

    (if (empty? used-components)
      file-data
      (as-> file-data $
        (add-library-page $)
        (reduce absorb-component
                $
                used-components)))))

