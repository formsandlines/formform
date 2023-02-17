(ns formform.symbolic-expressions
  (:require [nextjournal.clerk :as clerk]
            [formform.expr :as expr :refer :all]))


;; # A strange loop

;; Re-entry FORM definitions depend on symbolization so that the FORM is able to refer to itself. $f = ((f a) b)$ involves a relation between $f$ and its interpretation that an evaluator must internalize. Although the relation is representable in the FORM calculus, its internalization is not. And yet, without it, no algebraic treatment of FORMs would ever be possible.

;; The environment can be thought of as an internal memory of the observer:

(take 5
      (iterate (partial interpret-walk {'f '((f a) b)}) 
               'f))

;; But it is not part of the expression itself. To express the intent of internalization, I came up with what I call the *memory FORM*:

(def mem-ex1 [:mem [['f '((f a) b)]] 'f])

;; It is itself a symbolic expression and as such product of the same internalization process that it represents. Therefore it depends on an evaluation context, where it acts as an injunction for the observer to memorize the specified sequence of associations.

;; Outside of such a context, it is merely a conjunction of equivalences, but all the information for the observer is lost:

(interpret mem-ex1)

;; A non-recursive association can be easily evaluated as if we had defined it in an environment:

(=> [:mem [['a :M]] 'a])

;; However, most recursive/re-entry definitions cannot be evaluated, since we cannot know its final value/FORM in infinite substitution:

(try (=> [:mem [[:f '(:f)]] :f])
     (catch Exception e
       "Infinite recursion!"))

;; Although algebraically the evaluator knows that `:f` will remain `:f` forever in the self-equivalent case, it does not know what value `:f` is supposed to be:

(simplify [:mem [[:f '((:f nil) nil)]] :f])

;; But fortunately, Ralf Peyn has shown us that we can just mark the whole expression and let it be another primitive value in itself:

(simplify :U)

;; Interpreting the undetermined expression symbol (aka the *uFORM*) gives us an operator that is itself a symbolization of a self-equivalent re-entry FORM:

(interpret :U)

;; Interpreting that brings us back to our recursive definition as a memory FORM, but we lose all the information about what to do with `:f*`:

(interpret (interpret :U))
