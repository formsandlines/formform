(ns formform.usage-notes
  (:require [nextjournal.clerk :as clerk] 
            [formform.calc :as calc]
            [formform.expr :as expr
             :refer [MARK UFORM IFORM FORM
                     cnt> ctx> => =>*
                     ·n ·m ·u ·i ·var ·uncl ·dna · ··]]))

;; # Usage notes

;; ## Calculate a FORM

;; To construct FORMs, use `form-expr` or the shortcut `·`:

(def form-a (· (· 'a) 'b))

(def form-b (· 'a (· 'b)))

;; Multiple FORMs can be combined/related in an expression using `make-expr`
;; or just `··`:

(def ex (·· form-a form-b))

(expr/vars ex {})

;; Now use `=>*` to fully interpret and calculate the expression:

(def result (=>* ex))

;; This result is called a formDNA expression, it can be embedded
;; in other expressions like this:

(·· (· 'a result) 'b)

;; Or processed as a value structure using the `calc` module:

(calc/dna->vdict (expr/dna-expr->dna-seq result) {})

;; If you just want to obtain a value table like this,
;; use `eval-all` instead, which may be more performant:

(expr/eval-all ex {:sorted? true})

