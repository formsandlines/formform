(ns formform.vmap-notes
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc :refer :all]
            [formform.expr :as expr :refer :all]
            [formform.layout :refer [vmap-geometry]]
            [formform.io :refer [parse]]))

(def KRGB {:N "#000000"
           :U "#FF0000"
           :I "#00FF00"
           :M "#0000FF"})

(def KRGY {:N "#000050"
           :U "#ff0088"
           :I "#00ed80"
           :M "#fff08c"})

(defn vmap [{:keys [colors] :or {colors KRGB}}
            vmap-geom]
  (let [size (:size vmap-geom)
        diag (clojure.math/sqrt (* (* size size) 2))]
    [:svg {:width  diag
           :height diag}
     (conj [:g {:transform
                (str "translate(0," (/ diag 2) ")
                               rotate(-45)")}]
           ((fn f [{:keys [nodes pos size value]}]
              (if (some? value)
                [:rect {:x (first pos) :y (second pos)
                        :width  size
                        :height size
                        ; :stroke-width 0.5
                        ; :stroke "black"
                        :fill (colors value)}]
                (into [:g {:transform
                           (str "translate(" (first pos) ","
                                (second pos) ")")}]
                      (map f nodes))))
            vmap-geom))]))

(defn ->vmap-geom
  ([input] (->vmap-geom input {}))
  ([input opts]
   (vmap-geometry (merge {:gap-bounds [0.5 ##Inf]
                          :cellsize 12
                          :gap-growth 2.5} opts)
                  (dna->vdict
                   (FDNA->dna (first (=>* (parse input) opts)))
                   {}))))


(clerk/html (vmap {:colors KRGY}
                  (->vmap-geom "((a)(b))")))

(clerk/html (vmap {:colors KRGY}
                  (->vmap-geom "(a) b")))

(clerk/html (vmap {:colors KRGY}
                  (->vmap-geom "(:U)")))

(clerk/html (vmap {:colors KRGY}
                  (->vmap-geom "{@ L,E,R}{@ E,R,L}{@ L,R,E}"
                               {:vars ["L" "E" "R"]})))

(clerk/html (vmap {:colors KRGY}
                  (->vmap-geom "((c) d) ((d) c) (a b (a b))")))

(clerk/html (vmap {:colors KRGY}
                  (->vmap-geom "{..@. x, y, z}")))


(def input "{@ L,E,R}{@ E,R,L}{@ L,R,E}")

(def expr (parse input))

(def dna (FDNA->dna (first (=>* expr {:vars ["L" "E" "R"]}))))

(vdict->vmap (dna->vdict dna {}))

(def vmap-geom (vmap-geometry {:gap-bounds [1.0 ##Inf]
                               :cellsize 16
                               :gap-growth 3.0}
                              (dna->vdict dna {})))
