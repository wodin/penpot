;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.rpc.commands.binfile
  (:refer-clojure :exclude [assert])
  (:require
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.logging :as l]
   [app.common.pages.migrations :as pmg]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.media :as media]
   [app.rpc.mutations.files :refer [create-file]]
   [app.rpc.queries.files :refer [decode-row]]
   [app.storage :as sto]
   [app.storage.tmp :as tmp]
   [app.tasks.file-gc]
   [app.util.bytes :as bs]
   [app.util.fressian :as fres]
   [app.util.services :as sv]
   [app.util.time :as dt]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [cuerdas.core :as str]
   [yetti.adapter :as yt])
  (:import
   java.io.DataInputStream
   java.io.DataOutputStream
   java.io.InputStream
   java.io.OutputStream
   java.lang.AutoCloseable))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOW LEVEL STREAM IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:const buffer-size (:xnio/buffer-size yt/defaults))
(def ^:const penpot-magic-number 800099563638710213)
(def ^:const max-object-size (* 1024 1024 100)) ; Only allow 100MiB max file size.

(def ^:dynamic *position* nil)

(defn get-mark
  [id]
  (case id
    :header  1
    :stream  2
    :uuid    3
    :label   4
    :obj     5
    (ex/raise :type :validation
              :code :invalid-mark-id
              :hint (format "invalid mark id %s" id))))

;; (defn buffered-output-stream
;;   "Returns a buffered output stream that ignores flush calls. This is
;;   needed because transit-java calls flush very aggresivelly on each
;;   object write."
;;   [^java.io.OutputStream os ^long chunk-size]
;;   (proxy [java.io.BufferedOutputStream] [os (int chunk-size)]
;;     ;; Explicitly do not forward flush
;;     (flush [])
;;     (close []
;;       (proxy-super flush)
;;       (proxy-super close)))

(defmacro assert
  [expr hint]
  `(when-not ~expr
     (ex/raise :type :validation
               :code :unexpected-condition
               :hint ~hint)))

(defmacro assert-mark
  [v type]
  `(let [expected# (get-mark ~type)
         val#      (long ~v)]
    (when (not= val# expected#)
      (ex/raise :type :validation
                :code :unexpected-mark
                :hint (format "received mark %s, expected %s" val# expected#)))))

(defmacro assert-label
  [expr label]
  `(let [v# ~expr]
     (when (not= v# ~label)
       (ex/raise :type :assertion
                 :code :unexpected-label
                 :hint (format "received label %s, expected %s" v# ~label)))))

;; --- PRIMITIVE

(defn write-byte!
  [^DataOutputStream output data]
  (l/trace :fn "write-byte!" :data data :position @*position* ::l/async false)
  (.writeByte output (byte data))
  (swap! *position* inc))

(defn read-byte!
  [^DataInputStream input]
  (let [v (.readByte input)]
    (l/trace :fn "read-byte!" :val v :position @*position* ::l/async false)
    (swap! *position* inc)
    v))

(defn write-long!
  [^DataOutputStream output data]
  (l/trace :fn "write-long!" :data data :position @*position* ::l/async false)
  (.writeLong output (long data))
  (swap! *position* + 8))


(defn read-long!
  [^DataInputStream input]
  (let [v (.readLong input)]
    (l/trace :fn "read-long!" :val v :position @*position* ::l/async false)
    (swap! *position* + 8)
    v))

(defn write-bytes!
  [^DataOutputStream output ^bytes data]
  (let [size (alength data)]
    (l/trace :fn "write-bytes!" :size size :position @*position* ::l/async false)
    (.write output data 0 size)
    (swap! *position* + size)))

(defn read-bytes!
  [^InputStream input ^bytes buff]
  (let [size   (alength buff)
        readed (.readNBytes input buff 0 size)]
    (l/trace :fn "read-bytes!" :expected (alength buff) :readed readed :position @*position* ::l/async false)
    (swap! *position* + readed)
    readed))

;; --- COMPOSITE

(defn write-label!
  [^DataOutputStream output label]
  (l/trace :fn "write-label!" :label label :position @*position* ::l/async false)
  (let [^String label (if (keyword? label) (name label) label)
        ^bytes data   (.getBytes label "UTF-8")]
    (doto output
      (write-byte! (get-mark :label))
      (write-long! (alength data))
      (write-bytes! data))))

(defn read-label!
  [^DataInputStream input]
  (l/trace :fn "read-label!" :position @*position* ::l/async false)
  (let [m (read-byte! input)]
    (assert-mark m :label)
    (let [size (read-long! input)
          buff (byte-array size)]
      (read-bytes! input buff)
      (keyword (String. buff "UTF-8")))))

(defn write-uuid!
  [^DataOutputStream output id]
  (l/trace :fn "write-uuid!" :position @*position* :WRITTEN? (.size output) ::l/async false)

  (doto output
    (write-byte! (get-mark :uuid))
    (write-long! (uuid/get-word-high id))
    (write-long! (uuid/get-word-low id))))

(defn read-uuid!
  [^DataInputStream input]
  (l/trace :fn "read-uuid!" :position @*position* ::l/async false)
  (let [m (read-byte! input)]
    (assert-mark m :uuid)
    (let [a (read-long! input)
          b (read-long! input)]
      (uuid/custom a b))))

(defn write-obj!
  [^DataOutputStream output data]
  (l/trace :fn "write-obj!" :position @*position* ::l/async false)
  (let [^bytes data (fres/encode data)]
    (doto output
      (write-byte! (get-mark :obj))
      (write-long! (alength data))
      (write-bytes! data))))

(defn read-obj!
  [^DataInputStream input]
  (l/trace :fn "read-obj!" :position @*position* ::l/async false)
  (let [m (read-byte! input)]
    (assert-mark m :obj)
    (let [size (read-long! input)]
      (assert (pos? size) "incorrect header size found on reading header")
      (let [buff (byte-array size)]
        (read-bytes! input buff)
        (fres/decode buff)))))

(defn write-header!
  [^DataOutputStream output data]
  (l/trace :fn "write-header!" :data data :position @*position* ::l/async false)
  (doto output
    (write-byte! (get-mark :header))
    (write-long! penpot-magic-number)
    (write-obj! data)))

(defn read-header!
  [^DataInputStream input]
  (l/trace :fn "read-header!" :position @*position* ::l/async false)
  (let [mrk (read-byte! input)
        mnb (read-long! input)]
    (when (or (not= mrk (get-mark :header))
              (not= mnb penpot-magic-number))
      (ex/raise :type :validation
                :code :invalid-penpot-file))
    (read-obj! input)))

(defn copy-stream!
  [^OutputStream output ^InputStream input ^long size]
  (let [written (bs/copy! input output :size size)]
    (l/trace :fn "copy-stream!" :position @*position* :size size :written written ::l/async false)
    (swap! *position* + written)
    written))

(defn write-stream!
  [^DataOutputStream output stream size]
  (l/trace :fn "write-stream!" :position @*position* ::l/async false :size size)
  (doto output
    (write-byte! (get-mark :stream))
    (write-long! size))

  (copy-stream! output stream size))

(def size-2mib
  (* 1024 1024 2))

(defn read-stream!
  [^DataInputStream input]
  (l/trace :fn "read-stream!" :position @*position* ::l/async false)
  (let [m (read-byte! input)
        s (read-long! input)
        p (tmp/tempfile :prefix "penpot.binfile.")]
    (assert-mark m :stream)

    (when (> s max-object-size)
      (ex/raise :type :validation
                :code :max-file-size-reached
                :hint (str/ffmt "unable to import storage object with size % bytes" s)))

    (if (> s size-2mib)
      ;; If size is more than 2MiB, use a temporal file.
      (with-open [^OutputStream output (io/output-stream p)]
        (let [readed (bs/copy! input output :offset 0 :size s)]
          (l/trace :fn "read-stream*!" :expected s :readed readed :position @*position* ::l/async false)
          (swap! *position* + readed)
          [s p]))

      ;; If not, use an in-memory byte-array.
      [s (bs/read-as-bytes input :size s)])))

(defmacro assert-read-label!
  [input expected-label]
  `(let [readed# (read-label! ~input)
         expected# ~expected-label]
     (when (not= readed# expected#)
       (ex/raise :type :validation
                 :code :unexpected-label
                 :hint (format "unxpected label found: %s, expected: %s" readed# expected#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGH LEVEL IMPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- retrieve-file
  [pool file-id]
  (->> (db/query pool :file {:id file-id})
       (map decode-row)
       (first)))

(def ^:private sql:file-media-objects
  "SELECT * FROM file_media_object WHERE id = ANY(?)")

(defn- retrieve-file-media
  [pool {:keys [data] :as file}]
  (with-open [^AutoCloseable conn (db/open pool)]
    (let [ids (app.tasks.file-gc/collect-used-media data)
          ids (db/create-array conn "uuid" ids)]
      (db/exec! conn [sql:file-media-objects ids]))))

(def ^:private storage-object-id-xf
  (comp
   (mapcat (juxt :media-id :thumbnail-id))
   (filter uuid?)))

(def ^:private sql:file-libraries
  "WITH RECURSIVE libs AS (
     SELECT fl.id, fl.deleted_at
       FROM file AS fl
       JOIN file_library_rel AS flr ON (flr.library_file_id = fl.id)
      WHERE flr.file_id = ?::uuid
    UNION
     SELECT fl.id, fl.deleted_at
       FROM file AS fl
       JOIN file_library_rel AS flr ON (flr.library_file_id = fl.id)
       JOIN libs AS l ON (flr.file_id = l.id)
   )
   SELECT DISTINCT l.id
     FROM libs AS l
    WHERE l.deleted_at IS NULL OR l.deleted_at > now();")

(defn- retrieve-libraries
  [pool file-id]
  (map :id (db/exec! pool [sql:file-libraries file-id])))

(def ^:private sql:file-library-rels
  "SELECT * FROM file_library_rel
    WHERE file_id = ANY(?)")

(defn- retrieve-library-relations
  [pool ids]
  (with-open [^AutoCloseable conn (db/open pool)]
    (db/exec! conn [sql:file-library-rels (db/create-array conn "uuid" ids)])))

(defn write-export!
  [{:keys [pool storage ::output ::file-id ::include-libraries?]
    :or {include-libraries? false}}]

  (let [libs    (when include-libraries?
                  (retrieve-libraries pool file-id))
        rels    (when include-libraries?
                  (retrieve-library-relations pool (cons file-id libs)))
        sids    (atom #{})]

    ;; Write header with metadata
    (l/trace :hint "initialize exportation"
             :file-id file-id
             :include-libs? include-libraries?
             :libs (count libs)
             :rels (count rels)
             ::l/async false)

    (doto output
      (write-header! {:version 1
                      :sw-version (:full cf/version)}))

    ;; Write :main section
    (doto output
      (write-label! :main)
      (write-uuid! file-id)
      (write-obj! libs)
      (write-obj! rels))

    ;; Write :files section
    (write-label! output :files)
    (doseq [file-id (cons file-id libs)]
      (let [file  (retrieve-file pool file-id)
            media (retrieve-file-media pool file)]

        ;; Collect all storage ids for later write them all under
        ;; specific storage objects section.
        (swap! sids into (sequence storage-object-id-xf media))

        (l/trace :hint "write penpot file"
                 :id file-id
                 :media (count media)
                 ::l/async false)

        (doto output
          (write-uuid! file-id)
          (write-obj! file)
          (write-obj! media))))

    (l/trace :hint "write sobjects section" :items (count @sids) ::l/async false)

    (let [sids (into [] @sids)]
      ;; Write all collected storage objects
      (doto output
        (write-label! :sobjects)
        (write-obj! sids))

      (let [storage (media/configure-assets-storage storage)]
        (doseq [id sids]
          (let [{:keys [size] :as obj} @(sto/get-object storage id)]
            (l/trace :hint "write sobject" :id id ::l/async false)

            (doto output
              (write-uuid! id)
              (write-obj! (meta obj)))

            (with-open [^InputStream stream @(sto/get-object-data storage obj)]
              (let [written (write-stream! output stream size)]
                (when (not= written size)
                  (ex/raise :type :validation
                            :code :mismatch-readed-size
                            :hint (str/ffmt "found unexpected object size; size=% written=%" size written)))))))))))


(defn read-import!
  [{:keys [pool storage ::profile-id ::project-id ::input ::overwrite? ::migrate? ::ts]
    :or {overwrite? false migrate? false ts (dt/now)}
    :as cfg}]

  (letfn [(lookup-index [id index]
            (l/trace :hint "lookup-index" :id id :val (get @index id) ::l/async false)
            (or (get @index id)
                (ex/raise :type :validation
                          :code :incomplete-index
                          :hint "looks like index has missing data")))

          (process-map-form [index form]
            (cond-> form
              ;; Relink Image Shapes
              (and (map? (:metadata form))
                   (= :image (:type form)))
              (update-in [:metadata :id] lookup-index index)

              ;; This covers old shapes and the new :fills.
              (uuid? (:fill-color-ref-file form))
              (update :fill-color-ref-file lookup-index index)

              ;; This covers the old shapes and the new :strokes
              (uuid? (:storage-color-ref-file form))
              (update :stroke-color-ref-file lookup-index index)

              ;; This covers all text shapes that have typography referenced
              (uuid? (:typography-ref-file form))
              (update :typography-ref-file lookup-index index)

              ;; This covers the shadows and grids (they have directly
              ;; the :file-id prop)
              (uuid? (:file-id form))
              (update :file-id lookup-index index)))

          ;; a function responsible to analyze all file data and
          ;; replace the old :component-file reference with the new
          ;; ones, using the provided file-index
          (relink-shapes [data index]
            (walk/postwalk (fn [form]
                             (if (map? form)
                               (process-map-form index form)
                               form))
                           data))

          ;; A function responsible of process the :media attr of file
          ;; data and remap the old ids with the new ones.
          (relink-media [media index]
            (reduce-kv (fn [res k v]
                         (let [id (get index k)]
                           (if (uuid? id)
                             (-> res
                                 (assoc id (assoc v :id id))
                                 (dissoc k))
                             res)))
                       media
                       media))]

    (with-open [input (bs/zstd-input-stream input)]
      (with-open [input (bs/data-input-stream input)]

        ;; Verify that we received a proper .penpot file
        (read-header! input)

        ;; Veirify that the following sectionis :main
        (assert-read-label! input :main)

        ;; Read the main section
        (db/with-atomic [conn pool]
          (db/exec-one! conn ["SET CONSTRAINTS ALL DEFERRED"])

          (let [file-id (read-uuid! input)
                libs    (read-obj! input)
                rels    (read-obj! input)

                idx-xf  (if overwrite?
                          (map #(vector % %))
                          (map #(vector % (uuid/next))))

                index   (volatile!
                         (-> {}
                             (into idx-xf [file-id])
                             (into idx-xf libs)
                             (into idx-xf (map :file-id rels))
                             (into idx-xf (map :library-file-id rels))))

                media   (volatile! [])]

            (l/trace :hint "import file verified"
                     :id file-id
                     :libs (count libs)
                     :rels (count rels)
                     :overwrite? overwrite?
                     ::l/async false)

            ;; WARNING: reusing ids is dangerous operation, it will replace
            ;; all objects that already exists on the platform.

            (when overwrite?
              (l/trace :hint "delete previous data" ::l/async false)
              (db/exec! conn ["DELETE FROM file WHERE id = ANY(?)"
                              (db/create-array conn "uuid" (cons file-id libs))]))

            ;; Insert all file relations
            (doseq [rel rels]
              (l/trace :hint "insert file and library relations"
                       :file-id (:file-id rel)
                       :lib-id (:library-file-id rel)
                       ::l/async false)
              (db/insert! conn :file-library-rel
                          (-> rel
                              (assoc :synced-at ts)
                              (update :file-id lookup-index index)
                              (update :library-file-id lookup-index index))))

            (assert-read-label! input :files)
            (l/trace :hint "start reading penpot files" ::l/async false)

            ;; Process/Read all file
            (doseq [expected-file-id (cons file-id libs)]
              (let [file-id (read-uuid! input)
                    file    (read-obj! input)
                    media'  (read-obj! input)]

                (when (not= file-id expected-file-id)
                  (ex/raise :type :validation
                            :code :inconsistent-penpot-file
                            :hint "the penpot file seems corrupt, found unexpected uuid (file-id)"))

                (l/trace :hint "readed penpot file" :id file-id :media (count media') ::l/async false)

                ;; Update index using with media
                (vswap! index into idx-xf (map :id media'))

                ;; Store file media for later insertion
                (vswap! media into (map #(update % :id lookup-index index)) media')

                (let [file-id' (lookup-index file-id index)
                      data     (-> (:data file)
                                   (assoc :id file-id')
                                   (cond-> migrate? (pmg/migrate-data))
                                   (update :pages-index relink-shapes index)
                                   (update :components relink-shapes index)
                                   (update :media relink-media index))]

                  (create-file conn {:id file-id'
                                     :name (str "Imported: " (:name file))
                                     :project-id project-id
                                     :profile-id profile-id
                                     :modified-at ts
                                     :created-at ts
                                     :data data})

                  (l/trace :hint "penpot file persisted" :id file-id :new-id file-id' ::l/async false))))


            (assert-read-label! input :sobjects)
            (l/trace :hint "start reading storage objects" ::l/async false)

            (let [storage (media/configure-assets-storage storage)
                  sids    (read-obj! input)]

              ;; Step 1: process all storage objects
              (doseq [expected-storage-id sids]
                (l/trace :hint "expecting read storage" :expected-id expected-storage-id ::l/async false)

                (let [id    (read-uuid! input)
                      mdata (read-obj! input)]

                  (when (not= id expected-storage-id)
                    (ex/raise :type :validation
                              :code :inconsistent-penpot-file
                              :hint "the penpot file seems corrupt, found unexpected uuid (storage-object-id)"))

                  (l/trace :hint "readed storage object" :id id ::l/async false)

                  (let [[size resource] (read-stream! input)
                        hash            (sto/calculate-hash resource)
                        content         (-> (sto/content resource size)
                                            (sto/wrap-with-hash hash))
                        params          (-> mdata
                                            (assoc ::sto/deduplicate? true)
                                            (assoc ::sto/content content)
                                            (assoc ::sto/touched-at (dt/now)))
                        sobject         @(sto/put-object! storage params)]
                    (l/trace :hint "persisted storage object" :id id :new-id (:id sobject) ::l/async false)
                    (vswap! index assoc id (:id sobject)))))

              ;; Step 2: insert all file-media-object rows with correct
              ;; storage-id reference.
              (doseq [item @media]
                (l/trace :hint "inserting file media objects" :id (:id item) ::l/async false)
                (db/insert! conn :file-media-object
                            (-> item
                                (d/update-when :media-id lookup-index index)
                                (d/update-when :thumbnail-id lookup-index index)))))))))))

(defn export!
  [cfg]
  (let [path (tmp/tempfile :prefix "penpot.export.")
        id   (uuid/next)
        ts   (dt/now)
        cs   (volatile! nil)]
    (try
      (with-open [output (io/output-stream path)]
        (with-open [output (bs/zstd-output-stream output :level 6)]
          (with-open [output (bs/data-output-stream output)]
            (l/debug :hint "start exportation" :export-id id)
            (binding [*position* (atom 0)]
              (write-export! (assoc cfg ::output output))
              path))))

      (catch Throwable cause
        (vreset! cs cause)
        (throw cause))

      (finally
        (l/debug :hint "end exportation" :export-id id
                 :elapsed (str (inst-ms (dt/diff ts (dt/now))) "ms")
                 :cause @cs)))))

(defn import!
  [{:keys [::input] :as cfg}]
  (let [id (uuid/next)
        ts (dt/now)
        cs (volatile! nil)]
    (l/debug :hint "start importation" :import-id id)
    (try
      (binding [*position* (atom 0)]
        (with-open [input (io/input-stream input)]
          (read-import! (assoc cfg ::input input))))

      (catch Throwable cause
        (vreset! cs cause)
        (throw cause))

      (finally
        (l/debug :hint "end importation" :import-id id
                 :elapsed (str (inst-ms (dt/diff ts (dt/now))) "ms")
                 :error? (some? @cs)
                 :cause @cs)))))

;; --- Command: export-binfile

(s/def ::file-id ::us/uuid)
(s/def ::profile-id ::us/uuid)

(s/def ::export-binfile
  (s/keys :req-un [::profile-id ::file-id]))

#_:clj-kondo/ignore
(sv/defmethod ::export-binfile
  "Export a penpot file in a binary format."
  [{:keys [pool] :as cfg} {:keys [profile-id file-id] :as params}]
  {:hello "world"})
