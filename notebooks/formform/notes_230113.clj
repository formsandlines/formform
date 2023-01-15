(ns formform.notes-230113
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]))


; # formform

;; `make` constructs an expression of any given type

(make)

;; Keywords represent “symbolic expressions”, which are expressions that have a special, predefined *interpretation* that is part of the global environment of *formform*:

;; with more than one expression, `make` constructs an arrangement, designated with the operator `:-` as a symbolic expression of `((…))`

(make 'a 'b)
(make '(a) '(b (c)))

;; optionally, it can be constructed as data – any sequential will do:
(= (make '(:-))
   (make [:-])
   (make (make)))

;; by default, it will splice nested arrangements (root level) together and remove empty space, for ease of composition:
(= (make)
   (make nil)
   (make (make) (make (make))))

(make 'a (make 'b 'c) 'd)

;; setting the option `:splice` to false, prevents this behaviour:
(make {:splice? false} 'a (make 'b 'c) 'd)

;; the splicing behaviour is the same with all predefined expression constructors

;; ---

;; `form` constructs a marked arrangement, which is the primitive expression of `()`
(form)

(= ['a 'b]
   (make ['a 'b])
   (make {:mark? true} 'a 'b))

(form (form) (form (form)))

;; ---

;; Other predefined expressions can be constructed by specifying their operator as a first argument

;; `make` will omit the outer `[:-]` (by application of the law of crossing) to reduce redundancy:
(= [:uncl "cool"]
   (make :uncl "cool")
   (make (make :uncl "cool")))

;; however, as nested data it will not be omitted, to enable other args to step into the arrangement:
(make [:uncl "cool"]) ;=> [:- [:uncl "cool"]]

;; `form` will just mark the expression, either way:
(= [[:uncl "cool"]]
   (form :uncl "cool")
   (form [:uncl "cool"]))

;; They can, of course, be part of other expressions:
(= [:- 'a [:uncl "cool"] 'b]
   (make 'a (make :uncl "cool") 'b)
   (make 'a [:uncl "cool"] 'b))

;; In symexprs that expand to nested expressions, each argument is interpreted as a nested space
;; multiple elements can enter the same space by using `(make)` or just `[:-]`:

; !!! fixme

; (interpret-op (make :seq-re :<r [:- 'a 'b] [:- 'c 'd] 'e))

(make (make :seq-re :<r 'l 'e 'r)
      (make :seq-re :<r 'l 'r 'e))

;; ---

;; interpretation is 1:1 substitution
(= [[]]
   (interpret (make :-))
   (interpret [:-])
   (interpret* [:- :N]))

(= [[[]]]
   (interpret (make :- '()))
   (interpret* (make :- :M)))

(interpret :N)
(interpret :M)
(interpret :U)
(interpret :I)

;; through an env, variables can be interpreted
(interpret (make 'a 'b))
; (interpret (make 'a 'b) {'a :N, 'b :U}) ;=> [[:N :U]]
(take 3 (iterate interpret [:- :N :U]))

;; simplification is recursive substitution
(= []
   (simplify (make []))
   (simplify (make (form)))
   (simplify (form [[]])))

(= nil
   (simplify (make))
   (simplify (make [[]]))
   (simplify (form (form))))

;; `:I => [:U]`
(= :U
   (simplify (make :seq-re :<r nil nil))
   (simplify (make :U))) ;=> :U

(= [:U]
   (simplify (form :seq-re :<r nil nil))
   (simplify (make :I))) ;=> [:U]

;; env values will also be simplified
(simplify (make 'a 'b)) ;=> [[a b]]

(= (simplify (make 'a 'b) {'a :N, 'b :U})
   (simplify [[nil [:seq-re :<r nil nil]]])) ;=> :U



