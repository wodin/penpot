;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.auth
  "Auth routes provider"
  (:require
   [app.common.logging :as l]
   [app.common.spec :as us]
   [app.config :as cf]
   [clojure.spec.alpha :as s]
   [integrant.core :as ig]))

(s/def ::oidc-routes vector?)

(defmethod ig/pre-init-spec ::routes [_]
  (s/keys :req-un [::oidc-routes]))

(defmethod ig/init-key ::routes
  [_ cfg]
  [(:oidc-routes cfg)])
