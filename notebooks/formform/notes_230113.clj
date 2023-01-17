(ns formform.notes-230113
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]))


; # formform

;; ## Creating expressions

;; ### The `make` constructor

;; `make` constructs an expression of any given type

(clerk/row
  (make) (make []) (make :U) (make 'x) (make "hello world"))

;; with more than one argument, `make` constructs an arrangement, which is a symbolic expression of `((…))`, designated with the operator `:-` 

(clerk/row
  (make 'a 'b) 
  (make '[a] '[b [c]]) 
  (make [(make "x" "y")] [(make :M)]))

;; an arrangement can also be constructed directly as data – any sequential will do:

(= '[:- a b]
   (make '(:- a b))
   (make [:- 'a 'b])
   (make (make 'a) 'b))

;; by default, `make` will splice nested arrangements (at root level) together and remove empty space, for ease of composition:

(= nil
   (make '(:-))
   (make [:- nil])
   (make (make nil nil) nil)
   (make)
   (make nil)
   (make (make) (make (make))))

(make 'a (make 'b 'c) 'd)

;; setting the option `:splice` to false, prevents this behaviour:
(make {:splice? false} 'a (make 'b 'c) nil nil 'd)


;; ### The `form` constructor

;; `form` constructs a FORM – a marked arrangement, which is the primitive expression of `()`

(form)

;; use `form` for any *marked* expression and `make` for any *unmarked* expression

(form (form 'a) 'b)

;; as with all structural expressions, FORMs can be constructed directly with any Clojure sequential:

(= (form) [] '() (list) (range 0))

(= ['a 'b]
   (form 'a 'b)
   (make ['a 'b])
   (make {:mark? true} 'a 'b))

;; the splicing behaviour for nested expressions is the same as with `make`, although, of course, nested FORMs will not be spliced:

(= '[[] [[]]]
   (form [] [[]])
   (form (form) (form (form)))
   (form [:- (form) (form nil (make (form)))]))


;; ## Symbolic expressions

;; A symbolic expression is an expression that is identified by a *symbol* (represented as a keyword in Clojure) which has a predefined interpretation.

;; There are two kinds of symbolic expressions:
;; - *expression symbols*, which are just symbols interpreted as expressions
;; - *operators*, which are special FORMs that start with a symbol and are interpreted based on the shape and position of their other contents

;; ### Constructing symbols

;; Predefined expression symbols in *formform* are:

(clerk/row
 (make :N) (make :U) (make :I) (make :M))

;; ### Constructing operators

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



;; Keywords represent “symbolic expressions”, which are expressions that have a special, predefined *interpretation* that is part of the global environment of *formform*:


