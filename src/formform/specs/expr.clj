(ns formform.specs.expr
  (:require [formform.expr :as expr]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs



(comment

  (s/conform ::valid-operator
             [:seq-re :<r 'b])

  (s/conform ::valid-operator
             [:- 'x 'y])


  )

(comment
  
  (defmulti mspec :tag)

  (defmethod mspec :int [_] (s/keys :req-un [::tag ::i]))

  (s/conform (s/multi-spec mspec :tag)
             {:tag :int
              :i 12})



  (defmulti mspec2 first)

  (defmethod mspec2 :x [_] #(int? (second %)))
  (defmethod mspec2 :y [_] #(string? (second %)))

  (s/conform (s/multi-spec mspec2 first)
             [:y "12"])

  (mspec2 [:x 12])
  
  )

