(ns uxbox.ui.workspace.rules
  (:require [sablono.core :as html :refer-macros [html]]
            [rum.core :as rum]
            [cuerdas.core :as str]
            [beicon.core :as rx]
            [uxbox.state :as s]
            [uxbox.util.dom :as dom]
            [uxbox.ui.workspace.base :as wb]
            [uxbox.ui.mixins :as mx]))

(def ^:static zoom 1)
(def ^:static step-padding 20)
(def ^:static step-size 10)
(def ^:static start-width wb/canvas-start-x)
(def ^:static start-height wb/canvas-start-y)
(def ^:static scroll-left 0)
(def ^:static scroll-top 0)

(defn big-ticks-mod [zoom] (/ 100 zoom))
(defn mid-ticks-mod [zoom] (/ 50 zoom))

(defn h-line
  [position value]
  (let [big-ticks-mod (big-ticks-mod 1)
        mid-ticks-mod (mid-ticks-mod 1)
        big-step? (< (mod value big-ticks-mod) step-size)
        mid-step? (< (mod value mid-ticks-mod) step-size)]
    (cond
      big-step?
      (html
       [:g {:key position}
        [:line {:x1 position
                :x2 position
                :y1 5
                :y2 step-padding
                :stroke "#9da2a6"}]
        [:text {:x (+ position 2)
                :y 13
                :fill "#9da2a6"
                :style {:font-size "12px"}}
         value]])

      mid-step?
      (html
       [:line {:key position
               :x1 position
               :x2 position
               :y1 10
               :y2 step-padding
               :stroke "#9da2a6"}])

      :else
      (html
       [:line {:key position
               :x1 position
               :x2 position
               :y1 15
               :y2 step-padding
               :stroke "#9da2a6"}]))))

(def ^:const +ticks+
  (concat (range (- (/ wb/viewport-width 2)) 0 step-size)
          (range 0 (/ wb/viewport-width 2) step-size)))

(def ^:const +rule-padding+ 20)


(defn h-rule-render
  [own sidebar?]
  (println "h-rule-render")
  (let [scroll (rum/react wb/scroll-a)
        scroll-x (:x scroll)
        scroll-y (:y scroll)
        translate-x (- (- wb/canvas-scroll-padding) (:x scroll))]
    (println (count +ticks+))
    (html
     [:svg.horizontal-rule
      {:width wb/viewport-width
       :height 20}
      [:g {:transform (str "translate(" translate-x ", 0)")}
       (for [tick +ticks+
             :let [pos (* (+ tick wb/canvas-start-x wb/canvas-scroll-padding +rule-padding+) zoom)]]
         (h-line pos tick))]])))

(def h-rule
  (mx/component
   {:render h-rule-render
    :name "h-rule"
    :mixins [mx/static rum/reactive]}))


(defn v-line
  [position value]
  (cond
    (< (mod value big-ticks-mod) step-size)
    (html
     [:g {:key position}
      [:line {:y1 position
              :y2 position
              :x1 5
              :x2 step-padding
              :stroke "#9da2a6"}]
      [:text {:y position
              :x 5
              :transform (str/format "rotate(90 0 %s)" position)
              :fill "#9da2a6"
              :style {:font-size "12px"}}
       value]])

    (< (mod value mid-ticks-mod) step-size)
    (html
     [:line {:key position
             :y1 position
             :y2 position
             :x1 10
             :x2 step-padding
             :stroke "#9da2a6"}])

    :else
    (html
     [:line {:key position
             :y1 position
             :y2 position
             :x1 15
             :x2 step-padding
             :stroke "#9da2a6"}])))

(defn v-rule-render
  [own sidebar?]
  (let [height wb/viewport-height
        ticks (concat (range (- step-padding start-height) 0 step-size)
                      (range 0 (- height start-height step-padding) step-size))]
    (html
     [:svg.vertical-rule
      {:width 20
       :height wb/viewport-height}
      [:g
       [:rect {:x 0
               :y step-padding
               :height height
               :width step-padding
               :fill "rgb(233, 234, 235)"}]
       (for [tick ticks
             :let [pos (* (+ tick start-height) zoom)]]
         (v-line pos tick))]])))

(def v-rule
  (mx/component
   {:render v-rule-render
    :name "v-rule"
    :mixins [mx/static rum/reactive]}))
