;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns uxbox.main.ui.workspace.sidebar.options.rows.input-row
  (:require
   [rumext.alpha :as mf]
   [uxbox.common.data :as d]
   [uxbox.main.ui.components.select :refer [select]]
   [uxbox.main.ui.components.editable-select :refer [editable-select]]
   [uxbox.util.dom :as dom]))

(mf/defc input-row [{:keys [label options value class min max on-change type]}]
  [:div.row-flex.input-row
   [:span.element-set-subtitle label]
   [:div.input-element {:class class}

    (case type
      :select
      [:& select {:default-value value
                  :class "input-option"
                  :options options
                  :on-change on-change}]
      :editable-select
      [:& editable-select {:value value
                           :class "input-option"
                           :options options
                           :type (when (number? value) "number")
                           :on-change on-change}]

      (let [handle-change
            (fn [event]
              (let [value (-> event dom/get-target dom/get-value d/parse-integer)]
                (when (and (not (nil? on-change))
                           (or (not min) (>= value min))
                           (or (not max) (<= value max)))
                  (on-change value))))]
        [:input.input-text
         {:placeholder label
          :type "number"
          :on-change handle-change
          :value value}]))
    
    ]])