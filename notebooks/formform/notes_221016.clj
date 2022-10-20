(ns formform.notes-221016
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr
             :refer [MARK UFORM IFORM FORM
                     cnt> ctx> => =>*
                     <· ·> <·· ··> <·> <··> 
                     ·n ·m ·u ·i ·var ·uncl ·dna · ··]]))

;; Nesting expressions can save a lot of manual labour:

(apply <· (map #(·var (str "x" %)) (range 10)))
(apply ·> (map #(·var (str "x" %)) (range 10)))

;; Vectors are not normally merged in expressions, since they are technically
;; not expressions:

(·· (·· 'a 'b) ['c 'd] (· (·· 'e 'f) ['g 'h]))

;; But they are in nesting and chaining expressions, because they distinguish
;; groups there:

(def ngroups [['a 'b] ['c 'd] ['e 'f]])

(apply <· ngroups)
(apply ·> ngroups)
(apply <·> ngroups)
(apply <·· ngroups)
(apply ··> ngroups)
(apply <··> ngroups)
