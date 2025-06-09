(ns formform-render
  (:require [nextjournal.clerk.render :as r]
            [nextjournal.clerk.viewer :as v]))

(defn- const->col [c]
  (case c
    :N "black"
    :U "red-500"
    :I "green-500"
    :M "blue-500"))

(defn render-vcell-svg [c {:keys [formform/cellsize
                                  formform/grid-px
                                  formform/comp-svg?]}]
  (let [size (or cellsize 10)
        grid-px (or grid-px 1)
        rect-el [:rect {:class (str "fill-" (const->col c) " "
                                    "stroke-gray-700 "
                                    "stroke-[" grid-px "]")
                        :x 0
                        :y 0
                        :width size
                        :height size}]]
    (v/html
     (if comp-svg?
       rect-el
       [:svg {:width size
              :height size}
        rect-el]))))

(defn render-vrow-svg [xs {:keys [formform/cellsize
                                  formform/comp-svg?]
                           :as opts}]
  (let [n (count xs)
        size (or cellsize 10)
        g-els (map-indexed
               (fn [i c]
                 [:g
                  {:transform (str "translate(" (* i size) ",0)")}
                  [r/inspect-presented
                   (assoc opts :formform/comp-svg? true) c]])
               xs)]
    (v/html
     (if comp-svg?
       g-els
       (into [:svg {:class "inline-block"
                    :width (* size n)
                    :height size}]
             g-els)))))

(defn render-vgrid-svg [xs {:keys [formform/cellsize]
                            :as opts}]
  (v/html
   (let [nrow (count xs)
         ncol (count (:nextjournal/value (first xs)))
         size (or cellsize 10)]
     (into [:svg {:width  (* size ncol)
                  :height (* size nrow)}]
           (map-indexed
            (fn [i x]
              [:g
               {:transform (str "translate(0," (* i size) ")")}
               [r/inspect-presented (assoc opts :formform/comp-svg? true) x]])
            xs)))))

(defn render-rule-svg [[ptn v] {:keys [formform/cellsize]
                                :as opts}]
  (v/html
   (let [size (or cellsize 10)]
     [:svg {:class "inline-block"
            :width  (* size (count ptn))
            :height (* size 2)}
      [r/inspect-presented (assoc opts :formform/comp-svg? true) ptn]
      [:g
       {:transform (str "translate(" (* size (quot (count ptn) 2)) "," size ")")}
       [r/inspect-presented (assoc opts :formform/comp-svg? true) v]]])))
