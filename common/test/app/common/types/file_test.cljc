;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.types.file-test
  (:require
   [clojure.test :as t]
   [app.common.types.file :as ctf]
   ;; [clojure.pprint :refer [pprint]]
   ;; [app.common.exceptions :as ex]
   ;; [app.common.pages.init :as cpi]
   ;; [app.common.types.shape.interactions :as ctsi]
   [app.common.uuid :as uuid]
   ;; [app.common.geom.point :as gpt]
   ))

(t/deftest test-absorb-assets
  (let [file-id   (uuid/custom 2 2)
        page-id   (uuid/custom 1 1)
        file-data (ctf/make-file-data file-id page-id)]
    (prn "holaaa")
    (t/is (= file-id (:id file-data)))))

 
