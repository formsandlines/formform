(ns formform.usage-notes
  (:require [nextjournal.clerk :as clerk] 
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]))

;; # Usage notes

;; ## Basic usage

;; ### Construction

;; To construct an expression of a single FORM, use the function `<>`:

(<>)

(def <form-a> (<> (<> 'a) 'b))

(def <form-b> (<> 'a (<> 'b)))

;; Multiple FORMs can be combined/related in an expression using `<->`:

(<->)

(<-> (<>) (<>) (<>))

(def <rel> (<-> <form-a> <form-b>))

;; ### Evaluation

;; Simple expressions can be evaluated using `eval-expr`:

(eval-expr (<> (<>) (<>)))

;; However, variables are not simple:

(eval-expr (<> 'x))

;; The `:_` here is a value “hole”, which means we are missing information
;; to interpret the expression. We can provide an interpretation of the 
;; variables in the expression as a map:

(eval-expr (<> 'x) {'x :U})

;; We can also use `eval-all` to apply each possible interpretation to an 
;; expression and evaluate all interpreted expressions to get a value table:

(eval-all <rel> {:sorted? true})

;; ### Reduction

;; To just simplify an expression without evaluation, use `reduce-expr`:

(reduce-expr (<-> (<> (<> 'a)) (<> (<>)) 'b))

;; ### Special FORMs

;; There are a number of expressions for simple values and FORMs:

(<-> <N> <M> <U> <I>)

(<-> <none> <mark> <uform> <iform>)

;; Unclear FORMs can be expressed like this:

(def uncl (<uncl> "foo"))

(expand-unclear (first uncl))

;; ### Self-equivalent re-entry FORMs

;; Self-equivalent re-entry FORMs can be expressed in two different ways:
;; - using an options map
;; - using a re-entry “signature”

(def seq-re (<seq-re> {:parity :odd} ['a] ['b]))

(<2r+1 ['a] ['b])

(expand-seq-reentry (first seq-re))

;; ### Nested FORMs

;; It can get tedious to write nested FORM expressions, but these shortcuts 
;; can help:

(clerk/table
 (clerk/use-headers
  (let [ctxs [['a] ['b] ['c] ['d] ['e]]]
    [["Nest to left"    "Nest undirected"   "Nest to right"]
     [(apply << ctxs)   (apply <+> ctxs)    (apply >> ctxs)]
     [(apply <-< ctxs)  (apply <|> ctxs)    (apply >-> ctxs)]])))

;; To put multiple FORMs into the same nesting space, just use `<->`:

(>> (<-> 'a 'b) ['c] (<-> 'd 'e 'f))

(<|> (<-> 'a 'b) ['c] (<-> 'd 'e 'f))

;; You can also compose nestings like any other expression:

(<-< (<< ['a] ['b]) (>> ['a] ['b]))

(<< (<-< ['x1] ['x2] ['x3]) ['x1] ['x2] ['x3])

;; Or write your own construction shortcuts:

(defn <n-< [n & xs]
  (if (> n 1)
    (apply <-< (apply <n-< (dec n) xs) xs)
    (apply <-< xs)))

(defn <n< [n & xs] (<> (apply <n-< n xs)))

(<n< 4 ['x1] ['x2] ['x3])


;; ## Defining expressions

;; Expressions can be defined using ordinary Clojure functions that return
;; new expressions.

;; The convention is to surround “expression constructors” with angular brackets
;; `<name>`, as we have already seen from built-in expression constructors:

(defn <and> [a b] (<> (<> a) (<> b)))

(<and> <I> <U>)

;; However, these definitions are internal to your programming environment
;; and will disappear in the resulting expression data.

;; To “remember” a definition, you can create them as part of an expression,
;; by using so called “memory FORMs” (like expression-local environments):

(<mem> [['a <M>]] (<> 'a))

;; Here, we defined `a` as being equivalent to `:M` in the context `(a)`.

;; Recreating a parameterized function definition like the above `<and>` is a 
;; bit trickier and requires each “calling” expression to also have a local
;; environment with the arguments bound to their values:

(<mem> [[:and (<> (<> 'a) (<> 'b))]]
       (<mem> [['a <I>] ['b <U>]] :and))

;; This can be annoying, but is necessary since FORM logic expressions have no
;; order assumption for their contents and therefore there is no telling which
;; value gets assigned to which variable in the “call” and there is no list 
;; of all parameters in the definition either.

;; However, this also means that there are no requirements for the “arity” of
;; a definition and that it can be interpreted partially or not at all or even
;; inherit interpretations from parent expressions.

;; That being said, in most cases it is not recommended to use this kind of
;; abstraction unless you are dealing with very complex expressions where a lot
;; of structure is repeated which makes the output hard to read and reason 
;; about. But if your reasoning is confined to you programmatic environment, 
;; normal function definitions will be much more convenient.

;; Another exception would be if you need to work with symbolic abstractions
;; to encapsulate logic in a meaningful way.

;; You can define and re-define anything in an environment, but be aware that
;; overwriting pre-defined symbols may not be allowed.


;; ### Recursive definitions

;; Expression-local definitions can be recursive, but will not be evaluated
;; since the evaluator cannot assume them to be self-equivalent.

(<-> {:f* (<> :f*)} :f*)

;; However, they serve as a low-level representation of self-equivalent 
;; re-entry FORMs and will be used more generally in the `comp` module to
;; model time in computations.

(clerk/table (clerk/use-headers
               [["uFORM" "iFORM"]
                [UFORM IFORM]
                [<expanded-uform> <expanded-iform>]
                [(expand-seq-reentry (first <expanded-uform>))
                 (apply <-> (map (comp FORM expand-seq-reentry)
                             (first <expanded-iform>)))]]))


;; If you want to evaluate self-equivalent re-entry FORMs, use the dedicated
;; expression constructors instead.

(<r ['a] ['b])

(expand-seq-reentry (first (<r ['a] ['b])))

