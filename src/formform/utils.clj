(ns formform.utils)

(defn has-decimal? [n] (< (int n) n))

(defn pow-nat [x n] (apply * (repeat n x)))



(comment

  (has-decimal? 1.01)
  (pow-nat 4 3)

  )
