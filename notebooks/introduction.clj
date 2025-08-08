(ns introduction
  {:nextjournal.clerk/toc true}
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]
            [formform.io :as io]))


;; # Introduction to formform

;; ## Overview

;; formform (in its current state) has three core modules:
;; - `formform.calc` to calculate with 4 values
;; - `formform.expr` to represent and evaluate FORM expressions
;; - `formform.emul` to emulate FORMal systems as cellular automata

;; Most users may want to start with `expr`, which covers representation, interpretation, simplification and evaluation of all expressions from the FORM calculus and much more.

;; It is built on top of the `calc` module, which by itself will only be useful for you if you want to work with constant values directly or create and transform a variety of value structures.

;; Therefore I will focus on the `expr` module in this introduction, giving you an overview of how things that you might want to do with the library can be accomplished.

;; > If you are interested in the `emul` module, check out the [emul introduction](./introduction_emul). However, It is recommended (although not required), to read this introduction first.

;; ## Representation

;; ### FORM as data

;; In formform, FORMs are represented as nested sequences (_lists_ or _vectors_ in Clojure).

^{::clerk/visibility {:code :hide :result :show}}
(clerk/row [] [[]] [[] [[]]] '…)

^{::clerk/visibility {:code :hide :result :show}}
(clerk/row '() '(()) '(() (())) '…)

;; We will use the square-bracket representation throughout this introduction, since it is easier and more idiomatic (for our purposes) in Clojure to work with vectors.

;; > Side note: Although this is a very direct and familiar looking representation, the inherent order property of sequences is not an attribute of the syntax of FORM logic. It is important to keep this difference in mind, even if formform will try to make you forget about it. Order preservation has its own merits and is necessary for some applications like visualization.

;; Sometimes it is necessary to list multiple forms in one expression without changing their value by a mark. Of course, we can _double-mark_ them:

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
                    (let [vals [:n :m :u :i]]
                      (apply map vector
                             [["Unmarked" "Marked" "Undetermined" "Imaginary"]
                              vals
                              (map interpret vals)])))))

;; Don’t get confused by the interpretation of `:u`, we will get to that very soon. `nil` is Clojures expression for “nothing” and it serves to represent the empty space in formform as well (which, of course, cannot really be represented).

;; You might use constants in an expression like this:

(def form-ex1 [[:m] :n])

;; Looking at the interpretation …

(interpret-walk form-ex1)

;; … it is easy to see, that it is equivalent to the mark:

(simplify form-ex1)

;; And evaluates back to the constant `:m`:

(=> form-ex1)

;; Notice the circularity between simplification and evaluation:
;; > FORM → value → FORM → …


;; ### Variable values

;; Variables can be represented as strings or (Clojure) symbols. You can use one or the other, but a string is more flexible in terms of the characters that can be used:

["black cat" "🐈‍⬛" 'black-cat]

;; Of course, a variable has no fixed interpretation (hence the name):

(interpret 'x)

;; But it can be interpreted by a given environment:

(interpret {'x :u} 'x)

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

(interpret :m)

;; The _arrangement_ expression which captures the idea of the unmarked FORM is an example of an operator and gets interpreted as (you may have guessed it) a double-marked FORM:

(interpret [:- 'a 'b])

;; Notice the `:-` keyword, which is the symbol at the start of this operator that identifies it as an arrangement operator. You can think about it as the ‘$+$’ in ‘$x + y$’.

;; #### Self-equivalent re-entry FORMs

;; You may at some point want to represent some of the *self-equivalent re-entry FORMs* that Ralf Peyn introduced in _uFORM iFORM_.

;; Now we will come back to that strange interpretation of `:u`, which you can now identify as an operator with the keyword `:seq-re` (short for “self-equivalent re-entry FORM”):

(interpret :u)

;; This is our *uFORM*. The two `nil` at the end are the empty nested spaces in $f = ((f .) .)$ (marked with a dot here).

;; Here is how the explicit nesting looks like:

(form-nested-l '… 'a 'b)

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

(make :seq-re {:parity :odd, :open? true, :interpr :rec-ident} 'a 'b)


;; #### Unclear FORMs

;; The *unclear FORM* is also represented as an operator in formform:

(def uncl (make :uncl "foo" "bar"))

;; Its interpretation is aligned with the definition in uFORM iFORM:

(interpret uncl)


;; #### Other operators

;; There are two more predefined operators in formform (the *memory FORM* and the *formDNA expression*) and you can even define your own, but this is outside of the scope of this introduction, which should merely give you an idea on how to work with familiar concepts from uFORM iFORM in formform.


;; ## Simplification

;; Simplifying expressions will (try to) reduce them to their most primitive FORM. All deductions in the algorithm are justified by the axioms of FORM logic:

(simplify [:- [] []])

(simplify [[]])

;; Of course, variables cannot be simplified on their own:

(simplify 'a)

(simplify [:- 'a ['a ['a 'a]]])

;; But they can still be dominated by the mark (via deduction rules):

(simplify ['a ['a] 'b])

;; `simplify` can take an _environment_ to interpret variables directly:

(simplify ['a ['b]] {'a :u 'b :i})

;; As you can see, `:u` cannot be further simplified, since it is a primitive value, just like _mn_ (‘m’ on top of ‘n’) in uFORM iFORM:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/table {:nextjournal.clerk/width :prose}
             (clerk/use-headers
              (into [["" "Constant" "Simplification"]]
                    (let [vals [:n :m :u :i]]
                      (apply map vector
                             [["Unmarked" "Marked" "Undetermined" "Imaginary"]
                              vals
                              (map simplify vals)])))))

(simplify [:seq-re :<r nil nil])

;; > To keep things simple, I have chosen `:u` to represent the _uFORM_ aka ‘mn’ and `:i` for the _iFORM_ aka ‘(mn)’, but there is an alias `:mn` for `:u`, if you want to use this symbol instead.

;; Arrangements using the `:-` operator will be simplified to either merge into the parent expression (if possible) or become double marked FORMs:

(simplify [:- 'a [:- 'b 'c] 'd])

;; Simplifying the _unclear_ operator results in a *formDNA expression*, which is essentially a value structure (which I call *formDNA*) wrapped inside an expression:

(simplify [:uncl "love"])

;; Looking at the value table in evaluation (which we will get to in the next section) reveals this correspondence:

^{::clerk/visibility {:code :hide :result :show}}
(let [m (eval-all [:uncl "love"])]
  (clerk/table
   {::clerk/width :prose}
   (cons [(first (:varorder m)) "result"]
         (map (fn [[i r]] [(first i) r]) (:results m)))))

;; > Remember that this is the result expected from interpreting the unclear FORM as `[:seq-re :<r "love" "love"]`.


;; ## Evaluation

;; Evaluation of expressions in formform depends on a prior simplification step, which is ultimately needed to evaluate self-equivalent re-entry FORMs (which cannot be determined arithmetically, as we know from uFORM iFORM).

;; This means that it uses the same algorithm as `simplify`, but interprets the simpilfied expression as a value constant instead:

(evaluate [:- [] []])

(evaluate [[]])

;; Notice that `evaluate` also gives us the simplified expression. This is especially useful when it cannot be determined to a single value:

(evaluate [:uncl "love"])

;; However, just like with `simplify`, you can provide an environment to interpret variables and determine the result:

(evaluate [:uncl "love"] {"love" :m})

;; Or you can use `eval-all` to evaluate the expression with all possible interpretations for all variables. The `:results` are provided as tuples, where the first item is a list of interpretations for each variable in the order specified by `:varorder`.

^{::clerk/auto-expand-results? true}
(eval-all [:uncl "love"])

^{::clerk/auto-expand-results? true}
(eval-all [['a] 'b])


;; ## *formula notation*

;; Even though formform has a pretty lightweight data model to represent and easily manipulate expressions, it is hard to understand for non-programmers and not very elegant to write.

;; To be more accessible to user interfaces, formform provides the *formula notation*, that I have designed based on the common parentheses notation for simple binary FORMs, but extended to work with all expressions in formform.


;; ### Reading formulas

;; To parse _formulas_, require the `formform.io` module and call `read-expr`:

(io/read-expr "(((a) b) cat)")

;; As you can see, any word beginning with an alphabetic letter (upper- or lowercase) is a variable. But if you sorround them with (single or double) quotes, they may contain spaces and unicode characters (but no brackets of any kind):

(io/read-expr "'black cat 🐈‍⬛' \"e^π👁️ + 1 = 0\"")

;; Constants and other expression symbols are written just like in Clojure:

(io/read-expr ":m :u :mn")

;; Note that without the colon prefix, constants are interpreted as variables instead. This is by design, to be able to distinguish constants (and expression symbols in general) from variables more easily.

;; You can also use digits to represent constants, but keep in mind that the default interpretation is in “nuim-code” (`n=0`, `u=1`, `i=2`, `m=3`), which you can change by specifying a different `:sort-code`:

calc/nuim-code
calc/nmui-code

(=> (io/read-expr ":1"))
(=> (io/read-expr {:sort-code calc/nmui-code} ":1"))

;; > I encourage you to stick with letters instead of digits – they always have an unambiguous interpretation in the calculus that can be easily understood and remembered by anyone without the need for decoding.

;; There is no explicit notation for “emptyness”/`nil`, but the empty formula is equivalent to it:

(io/read-expr "")

;; Self-equivalent re-entry FORMs have a short notation with curly brackets, where each nested space is separated by a comma:

(io/read-expr "{}")
(io/read-expr "{a, (b {,}) {:m,}, c}")

;; As you can see, the default signature is `:<r`, but of course, you can provide your own:

(io/read-expr "{..@~._ a,b}")

;; > Note: the signature syntax is very similar, the only differences are `r` ↔ `@`, `r'` ↔ `@~` and there is no `<` arrow in the FORMula signature.

;; Or use option parameters (separated by pipes):

(io/read-expr "{2r+1|open|alt| a,b}")

;; Unclear FORMs also have a special syntax and can wrap any text between two slashes, just like quoted variable names:

(io/read-expr "/👁️ 👄 👁️/")


;; ### Printing as formula

;; Expression data can be converted (back) to formula strings using `print-expr`:

(io/print-expr [[["a"] "b"] "black cat 🐈‍⬛"])

(io/print-expr [:- :n [:u] [:i [:m]]])

;; > Note that a formula that is being read and then printed from this data may not result in the same formula string (e.g. some whitespace may be added or removed), but structure and semantics should still be equal.


;; ## More information

;; * [introduction to formform.emul](./introduction_emul)
;; * [source repository](https://github.com/formsandlines/formform)
;; * [API documentation](https://formform.dev/docs)

