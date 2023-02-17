(ns formform.introduction
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]
            [formform.io :as io]))


;; # Introduction to formform

;; ## Overview

;; formform (in its current state) has two core modules:
;; - `formform.calc` to calculate with 4 values
;; - `formform.expr` to work with FORM expressions

;; Most users may want to work with `expr`, which covers representation, interpretation, simplification and evaluation of all expressions from the FORM calculus and much more.

;; It is built on top of the `calc` module, which by itself will only be useful for you if you want to work with constant values directly or create and transform a variety of value structures.

;; Therefore I will focus on the `expr` module in this introduction, giving you an overview of how things that you might want to do with the library can be accomplished.

;; ## Representation

;; ### FORM as data

;; In formform, FORMs are represented as nested sequences (_lists_ or _vectors_ in Clojure).

^{::clerk/visibility {:code :hide :result :show}}
(clerk/row [] [[]] [[] [[]]] '…)

^{::clerk/visibility {:code :hide :result :show}}
(clerk/row '() '(()) '(() (())) '…)

;; We will use the square-bracket representation throughout this introduction, since it is easier and more idiomatic (for our purposes) in Clojure to work with vectors.

;; > Side note: Although this is a very direct and familiar looking representation, the inherent order property of sequences is unfortunately not an attribute of FORMs. It is important to keep this difference in mind, even if formform will try to make you forget about it. Order preservation has its own merits and is necessary for some applications like visualization.

;; Sometimes it is necessary to list multiple forms in one expression without changing their value by a mark. Of course, we can double-mark them:

[[[] [] []]]

;; But this does not reflect what Spencer-Brown calls the “unmarked form”. In formform, there is a special construction for this, which is called an *arrangement* (to again borrow a term Spencer-Brown used here):

(def arr-ex1 [:- [] [] []])

;; To nest/compose expressions which contain arrangements cleanly, we can use the expression constructor, which is just called `make`:

[:- [] arr-ex1 []] ;; this leaves an ugly redundant arrangement

(make [] arr-ex1 []) ;; use `make` to splice inner arrangements

;; There is also the `form` constructor, which does the same things as `make`, but for marked expressions:

(form [] arr-ex1 [])

;; > `make` does not only create and splice arrangements, but is the general constructor for all kinds of expressions in formform. If you want to make sure your expressions are well-formed, you should use it instead of bare data, since it will also validate your input.


;; ### Constant values

;; All four values of the FORM calculus introduced in _uFORM iFORM_ are available as constants in formform and each have their corresponding interpretation as a FORM expression:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/table {:nextjournal.clerk/width :prose}
             (clerk/use-headers
              (into [["" "Constant" "FORM interpretation"]]
                    (let [vals [:N :M :U :I]]
                      (apply map vector
                             [["Unmarked" "Marked" "Undetermined" "Imaginary"]
                              vals
                              (map interpret vals)])))))

;; Don’t get confused by the interpretation of `:U`, we will get to that very soon. `nil` is Clojures expression for “nothing” and it serves to represent the empty space in formform as well (which, of course, cannot really be represented).

;; You might use constants in an expression like this:

(def form-ex1 [[:M] :N])

;; Looking at the interpretation …

(interpret-walk form-ex1)

;; … it is easy to see, that it is equivalent to the mark:

(simplify form-ex1)

;; And evaluates back to the constant `:M`:

(=> form-ex1)

;; Notice the circularity between simplification and evaluation:
;; > FORM → value → FORM → …


;; ### Variable values

;; Variables can be represented as strings or (Clojure) symbols. You can use one or the other, but a string is more flexible in terms of the characters that can be used:

["black cat" "🐈‍⬛" 'black-cat]

;; Of course, a variable has no fixed interpretation (hence the name):

(interpret 'x)

;; But it can be interpreted by a given environment:

(interpret {'x :U} 'x)

;; An uninterpretable variable or symbolic expression evaluates to a “hole” (represented by an underscore `:_`), which is a placeholder for any constant value:

(=> 'x)


;; ### Symbolic expressions

;; While the primitives I introduced above can already cover a wide variety of simple expressions, they are not capable of representing _undetermined_ and _unclear_ FORMs, since they lack a means of abstraction.

;; This is where symbolic expressions become useful. A symbolic expression is an expression that is identified by a *symbol* (represented as a keyword in Clojure) which has a predefined interpretation.

;; There are two kinds of symbolic expressions:
;; - *expression symbols*, that are just symbols interpreted as expressions
;; - *operators*, that are special FORMs that start with a symbol and are interpreted based on the shape and position of their other contents

;; We have already encountered some specimen of both kinds!

;; Expressions of constant values are actually expression symbols, which is why they have an interpretation:

(interpret :M)

;; The _arrangement_ expression which captures the idea of the unmarked FORM is an example of an operator and gets interpreted as (you may have guessed it) a double-marked FORM:

(interpret [:- 'a 'b])

;; Notice the `:-` keyword, which is the symbol at the start of this operator that identifies it as an arrangement operator. You can think about it as the ‘$+$’ in ‘$x + y$’.

;; #### Self-equivalent re-entry FORMs

;; You may at some point want to represent some of the *self-equivalent re-entry FORMs* that Ralf Peyn introduced in _uFORM iFORM_.

;; Now we will come back to that strange interpretation of `:U`, which you can now identify as an operator with the keyword `:seq-re` (short for “self-equivalent re-entry FORM”):

(interpret :U)

;; This is our *uFORM*. The two `nil` at the end are the empty nested spaces in $f = ((f .) .)$ (marked with a dot here).

;; Here is how the explicit nesting looks like:

(nest-exprs {} '… 'a 'b)

;; Right after the identifier comes what I call a “re-entry signature”. This FORM has the signature `:<r`, which I will explain shortly.

;; Self-equivalent re-entry FORMs have 4 parameters:
;; - *resolution parity* → even or odd number of nested subexpressions
;; - *re-entry parity* → even or odd number of re-entries
;; - *open/closed* → first re-entry step is unmarked (open) or marked (closed)
;; - *interpretation* → how the observer should interpret the re-entry

;; While resolution parity is already determined by the number of items after the re-entry signature, the three remaining parameters are set by the signature itself, which consists of the following elements (in order):

;; - `<..r` for _even_, `<..r.` for _odd_ re-entry numbers or `<r` alone for _any_ re-entry number (will be determined logically)
;; - `'` for the _alternative_ interpretation (“recursive identity”) instead of the standard interpretation (“recursive instruction”)
;; - `_` for an _open_ (unmarked) re-entry FORM instead of a closed one

;; > By the way, the `<` designates the nesting direction which would otherwise only be known by convention.

;; So `:<r` tells us that we have any re-entry number, use the standard interpretation and the FORM is closed. We can also get this information, by converting the signature into an options map:

(seq-reentry-sign->opts :<r)

;; Here are all available options with their default values:

;; - `:parity` (of re-entry number) → `:even`, `:odd` or `:any` (default)
;; - `:open?` (unmarked FORM) → `true` or `false` (default)
;; - `interpr` (interpretation of `mn`) → `:rec-instr` (default) or `:rec-ident`

;; If you do not feel comfortable with the signature notation, you can also let the constructor generate it by providing an options map like this:

(make :seq-re {:parity :odd, :open? true, :interpr :rec-instr} 'a 'b)


;; #### Unclear FORMs


;; ### Parsing formula notation

;; Even though formform has a pretty lightweight data model to represent expressions, it is hard to understand for non-programmers and not very elegant to write.

;; To be more accessible to user interfaces, formform provides a *formula notation*, that I specified based on the popular parentheses notation for simple binary FORMs, but extended to work with all expressions in formform.

;; To parse the notation, require the `formform.io` module and use `read-expr`:

(io/read-expr "(((a) b) 'black cat 🐈‍⬛')")

(io/read-expr ":N (:U) (:I (:M))")

;; Expression data can be converted back to formula strings using `print-expr`:

(io/print-expr [[["a"] "b"] "black cat 🐈‍⬛"])

(io/print-expr [:- :N [:U] [:I [:M]]])


;; ### Simplification of expressions

;; ### Evaluation of expressions

