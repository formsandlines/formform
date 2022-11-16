(ns formform.vmap-notes2
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc :refer :all]
            [formform.expr :as expr :refer :all]
            [formform.io :refer [parse]]
            [formform.utils :as utils]))


(defn gen-margins
  [{:keys [gap-bounds gap-growth]
    :or {gap-bounds [0.1 ##Inf] gap-growth 0.3}}
   dim]
  (let [growth-fn (fn [part-dim]
                    (min (max (* part-dim gap-growth)
                              (first gap-bounds))
                         (second gap-bounds)))]
    (mapv growth-fn (range 0 dim))))

(def KRGB {:N "#000000"
           :U "#FF0000"
           :I "#00FF00"
           :M "#0000FF"})

(def const->coords {:N [0 0]
                    :U [0 1]
                    :I [1 0]
                    :M [1 1]})

(defn vmap-viz
  ([vmap] (vmap-viz {} vmap))
  ([{:keys [scale-to-fit? cellsize padding margins stroke-width
            colors bg-color stroke-color label]
     :or {scale-to-fit? false padding 0 stroke-width 0.5
          colors KRGB bg-color nil stroke-color nil label nil}}
    vmap]
   (let [dim       (:dim (meta vmap))
         cellsize  (if (nil? cellsize)
                     12
                     cellsize)
         margins   (reverse (take dim (if (nil? margins)
                                        (utils/geom-seq 0.1 2.0)
                                        margins)))
         sum-gaps  (fn [margins] (apply + (map (fn [n i]
                                                 (* n (utils/pow-nat 2 i)))
                                               margins (range))))
         vmap-size (+ (* (utils/pow-nat 2 dim) cellsize)
                      (* (sum-gaps margins) cellsize))
         vmap-diag (clojure.math/sqrt (* (* vmap-size vmap-size) 2))
         full-size (+ vmap-diag padding)]

     [:svg {:width  (if scale-to-fit? "100%" full-size)
            :height (if scale-to-fit? "100%" full-size)
            :view-box (let [shift (+ (/ padding 2))]
                        (str "-" shift " -" shift
                             " " full-size " " full-size))}
      (when (some? bg-color)
        [:rect {:x (str "-" (/ padding 2))
                :y (str "-" (/ padding 2))
                :width  full-size
                :height full-size
                :fill   bg-color}])
      (into [:g {:transform
                 (str "translate(" 0 "," (/ vmap-diag 2) ")"
                      "scale(" cellsize ") "
                      "rotate(" -45 ") ")}]
            ((fn f [vmap dim margins]
               (map
                (fn [[k v]]
                  [:g {:transform
                       (let [coords (const->coords k)
                             gaps   (if (> (count margins) 1)
                                      (sum-gaps (rest margins))
                                      0)
                             shift  (+ (utils/pow-nat 2 dim)
                                       (first margins)
                                       gaps)
                             x (* (coords 0) shift)
                             y (* (coords 1) shift)]
                         (str "translate(" x "," y ")"))}
                   (if (> dim 0)
                     (f v (dec dim) (rest margins))
                     [:rect (merge {:width  1
                                    :height 1
                                    :fill (colors v)}
                                   (if (nil? stroke-color)
                                     {}
                                     {:stroke-width (/ stroke-width cellsize)
                                      :stroke stroke-color}))])])
                vmap))
             vmap (dec dim) margins))

      ;; ! TODO
      (when (some? label)
        [:g {:transform (str "translate(" 0 "," vmap-diag ")")}
         [:text {}
          label]])])))




(def input "{@ a,b,c}{@ b,c,a}{@ a,c,b}")
; (def input "{a,b,c,d}{c,a,e,b}{b,f,c,a}(e f)")

(def expr (parse input))

(def dna (FDNA->dna (first (=>* expr))))

(def vdct (dna->vdict dna {}))

(def vmap (vdict->vmap vdct))

(clerk/html
 (vmap-viz vmap))

(clerk/html
 (vmap-viz {:cellsize 16
            :padding 20
            :margins (cons 0.0 (utils/geom-seq 0.2 2.0))
            :bg-color "white"
            :stroke-color "white"
            :stroke-width 0.75}
           vmap))

