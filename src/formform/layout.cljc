(ns formform.layout
  (:require [formform.calc :as calc]))

;; ========================================================================
;;     formform layout module
;; EXPERIMENTAL, MIGHT BE DELETED!
;;     -- created 11/2022, (c) Peter Hofmann
;; ========================================================================


(defn vmap-geometry
  [{:keys [cellsize gap-bounds gap-growth]
    :or {gap-bounds [0.5 ##Inf] gap-growth 1.5}}
   vdict]
  (let [dim      (count (ffirst vdict))
        cellsize (if (nil? cellsize)
                   ;; proprtional cellsize
                   ;; -> reduction by 2px for each additional var if dim > 3
                   ;; (based on observation, should be refined)
                   (let [n (- 17 (if (> dim 3)
                                   (* 2 (- dim 3))
                                   0))]
                     (max 2 n)) ;; min size of 2px
                   cellsize)
        margins  (let [growth-fn (fn [part-dim]
                                   (min (max (* part-dim gap-growth)
                                             (first gap-bounds))
                                        (second gap-bounds)))]
                   ;; margins for dim 0 … n
                   (mapv growth-fn (range 0 (inc dim))))

        add-geometry
        (fn [vmap _ depth dim]
          (let [part-dim    (- dim depth 1)
                part-size   (if (== part-dim 0)
                              cellsize
                              (+ (* (get-in vmap [:N :nodes 0 :size]) 2)
                                 (get-in vmap [:N :nodes 0 :margin])))
                part-margin (margins part-dim)
                part-shift  (+ part-size part-margin)]
            {:nodes (mapv
                     (fn [[k vmap-quadrant]]
                       (let [shift-x? (case k (:M :I) true false) ;; N I
                             shift-y? (case k (:M :U) true false) ;; U M
                             geometry {:key    k
                                       :pos    [(if shift-x? part-shift 0)
                                                (if shift-y? part-shift 0)]
                                       :size   part-size
                                       :margin part-margin}]
                         (if (== part-dim 0)
                           (into {:value vmap-quadrant} geometry)
                           (merge vmap-quadrant geometry))))
                     vmap)}))]

    (if (== dim 0)
      {:value  (vdict '())
       :key    nil
       :pos    [0 0]
       :size   cellsize
       :margin (margins 0)}
      (let [vmap-geom (calc/vdict->vmap add-geometry vdict)]
        (assoc vmap-geom
               :key    nil
               :pos    [0 0]
               :size   (+ (* (get-in vmap-geom [:nodes 0 :size]) 2)
                          (get-in vmap-geom [:nodes 0 :margin]))
               :margin (margins dim))))))


(comment

  (vmap-geometry {} (calc/dna->vdict (calc/rand-dna 3) {}))
  (vmap-geometry {} (calc/dna->vdict (calc/rand-dna 2) {}))
  (vmap-geometry {} (calc/dna->vdict (calc/rand-dna 1) {}))
  (vmap-geometry {} (calc/dna->vdict (calc/rand-dna 0) {}))

  (calc/vdict->vmap (calc/dna->vdict (calc/rand-dna 1) {}))
  (calc/vdict->vmap (calc/dna->vdict (calc/rand-dna 0) {}))


  )
