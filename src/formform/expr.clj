(ns formform.expr
  (:require [formform.calc :as calc]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive expressions

(def n '[ nil ]) ; use '_ as an alternative?
(def m '[ () ])
(def u '[ :mn ])
(def i '[ (:mn) ])

(def const->expr {:N n :U u :I i :M m})
(def expr->const {n :N u :U i :I m :M})




(comment

  (const->expr :M)
  (expr->const '[ () ])

  (let [expr `[ (~@m ~@n) ~@m ]]
    expr)

  )
