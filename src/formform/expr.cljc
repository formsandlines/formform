(ns formform.expr
  "API for the `expr` module of `formform`.

  ## Concepts

  ### Basic Expressions
  
  **expression**  
  → interpret the _representation_ as an indicator of a _value_
  
  * _representation_ → _form_ {syntax} and intentionality {semantics}
  * _form_ → relate structure and code
  * _value_ → _calc/value_
  * _value_ → _calc/constant_ {determined} or _formDNA_ {contingent}
  
  **variable**  
  → the interpretation of the _expression symbol_ is undetermined
  
  **form expression** {FORM}  
  → _expression_ of _form_  
  → invert the _value_ of the _relation_
  
  * _relation_ → relate the _values_ of the content of the _expression_


  ### Symbolic Expressions

  **symbolic expression**  
  → _expression symbol_ or _operator_
  
  **expression symbol**  
  → interpret the symbol as a specific _expression_
  
  **operator**  
  → interpret the structure by its symbol as an _expression_ pattern

  #### Types
  
  **arrangement** {`:-`}  
  → _operator_ to construct _relations_

  **unclear FORM** {`:uncl`}  
  → _operator_ to construct unclear _FORMs_

  **seq-reentry FORM** {`:seq-re`}  
  → _operator_ to construct self-equivalent re-entry _FORMs_

  **memory FORM** {`:mem`}  
  → _operator_ to construct memory _FORMs_
  
  * _rem pair_ → observe and remember equality between the two expressions

  **formDNA** {`:fdna`}  
  → _operator_ to construct _calc/formDNA_ _expressions_
  "
  (:require
   [formform.calc.specs :as calc-sp]
   [formform.expr.specs :as sp]
   [formform.expr.symexpr :as symx]
   [formform.expr.core :as core]
   [formform.expr.operators :as ops]
   [formform.utils :as utils]
   [clojure.spec.alpha :as s]
   #_[clojure.spec.gen.alpha :as gen])
  #?(:cljs (:require-macros
            [formform.expr :refer [defoperator defsymbol]])))


(s/def :opts/ordered? boolean?)
(s/def :opts/unmarked? boolean?)
(s/def :opts/+meta? boolean?)
(s/def :opts/rtl? boolean?)
(s/def :opts/ltr? boolean?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structures

;;=========================================================================
;; Expressions

;;-------------------------------------------------------------------------
;; expression

(def expression? (partial s/valid? ::sp/expression))
(def variable? (partial s/valid? ::sp/variable))
(def form? (partial s/valid? ::sp/form-expr))

(s/fdef make
  :args (s/* any?)
  :ret  ::sp/expression)
(defn make
  "Constructor for expressions of any kind. Validates its input.

  If the first argument (or the first after the options map) is a keyword of a registered operator, will call the constructor for that operator

  Can be given an options map as first argument:

  * `mark?` (default: false) marks the whole expression, creating a FORM
  * `splice?` (default: true) dissolves all top-level arrangements"
  [& args]
  (apply core/make args))

(s/fdef form
  :args (s/* any?)
  :ret  ::sp/struct-expr)
(defn form
  "Constructor for FORM expressions `[ … ]`. Calls [[make]] on `args`."
  [& args]
  (apply core/form args))


(defn mark-exprs
  "Obsolete → use [[make-marked]] or [[form-marked]] instead."
  [opts & exprs]
  (apply core/mark-exprs opts exprs))

(s/fdef make-marked
  :args (s/* ::sp/expression)
  :ret  ::sp/expression)
(defn make-marked
  "Returns an arrangement with each `exprs` argument marked, e.g. `(a) (b) …`.

  * group expressions with arrangements: `(make x y …)`"
  [& exprs]
  (apply core/mark-exprs {:unmarked? true} exprs))

(s/fdef form-marked
  :args (s/* ::sp/expression)
  :ret  ::sp/form-expr)
(defn form-marked
  "Returns a FORM with each `exprs` argument marked, e.g. `((a) (b) …)`.

  * group expressions with arrangements: `(make x y …)`"
  [& exprs]
  (apply core/mark-exprs {:unmarked? false} exprs))


(defn nest-exprs
  "Obsolete → use [[make-nested-l]]/[[make-nested-r]] or [[form-nested-l]]/[[form-nested-r]] instead."
  [opts & exprs]
  (apply core/nest-exprs opts exprs))

(s/fdef make-nested-l
  :args (s/* ::sp/expression)
  :ret  ::sp/expression)
(defn make-nested-l
  "Nests `exprs` leftwards in an arrangement, e.g. `((…) y) z`.

  * use `nil` for empty expressions
  * use an arrangement `(make x y …)` to add multiple exprs. to the same level"
  [& exprs]
  (apply core/nest-exprs {:unmarked? true :ltr? false} exprs))

(s/fdef make-nested-r
  :args (s/* ::sp/expression)
  :ret  ::sp/expression)
(defn make-nested-r
  "Nests `exprs` rightwards in an arrangement, e.g. `a (b (…))`.

  * use `nil` for empty expressions
  * use an arrangement `(make x y …)` to add multiple exprs. to the same level"
  [& exprs]
  (apply core/nest-exprs {:unmarked? true :ltr? true} exprs))

(s/fdef form-nested-l
  :args (s/* ::sp/expression)
  :ret  ::sp/form-expr)
(defn form-nested-l
  "Nests `exprs` leftwards in a FORM, e.g. `((((…) y) z)`.

  * use `nil` for empty expressions
  * use an arrangement `(make x y …)` to add multiple exprs. to the same level"
  [& exprs]
  (apply core/nest-exprs {:unmarked? false :ltr? false} exprs))

(s/fdef form-nested-r
  :args (s/* ::sp/expression)
  :ret  ::sp/form-expr)
(defn form-nested-r
  "Nests `exprs` rightwards in a FORM, e.g. `(a (b (…)))`.

  * use `nil` for empty expressions
  * use an arrangement `(make x y …)` to add multiple exprs. to the same level"
  [& exprs]
  (apply core/nest-exprs {:unmarked? false :ltr? true} exprs))



;; Utils

;; ? check if :ret is subset of :subexprs
(s/fdef find-subexprs
  :args (s/cat :expr ::sp/expression
               :subexprs (s/coll-of ::sp/expression :kind set?))
  :ret  (s/every ::sp/expression
                 :kind sequential?))
(defn find-subexprs
  "Finds all subexpressions in `expr` that match any element of the given set `subexprs`."
  [expr subexprs]
  (core/find-subexprs expr subexprs))

;; ? opts first
(s/fdef find-vars
  :args (s/cat :expr ::sp/expression
               :opts (s/keys :opt-un [:opts/ordered? ::sp/vars]))
  :ret  ::sp/varseq)
(defn find-vars
  "Finds all variables in an expresson or returns the expression itself if it is a variable.

  Options:

  * `{:ordered true}` to return variables in: type order → alphanumeric order
  * `{:vars #{…}}` can be given a set of specific variables to find"
  [expr opts]
  (core/find-vars expr opts))

(s/fdef gen-vars
  :args (s/cat :n nat-int?)
  :ret  ::sp/varseq)
(defn gen-vars
  "Generates a number of variables with random names."
  [n]
  (core/gen-vars n))

;; ? do we need this function
(defn expr->const
  "Returns the constant value/expression that corresponds to its simplest (as per [[simplify]]) input `expr` (including itself):  

  ```
  nil                     → :n
  []                      → :m
  [:seq-re :<r nil nil]   → :u
  [[:seq-re :<r nil nil]] → :i
  [:u]                    → :i
  ```
  "
  [expr]
  (symx/expr->const expr))

(s/fdef permute-vars
  :args (s/cat :varorder ::sp/varorder)
  :ret  (s/every ::sp/varorder))
(defn permute-vars
  "Generates all permutations of a variable order (a sequence of variables)."
  [varorder]
  (core/permute-vars varorder))


;;=========================================================================
;; Symbolic Expressions

;;-------------------------------------------------------------------------
;; symbolic expression

(def expr-symbol? (partial s/valid? ::sp/expr-symbol))
(def op-symbol? (partial s/valid? ::sp/op-symbol))
(def operator? (partial s/valid? ::sp/operator))

(def op-symbol
  "Returns the symbol of a given operator."
  symx/op-symbol)


(defmacro defoperator
  "Defines a new operator by its symbol (a keyword), a vector of arguments and an interpretation function. Takes additional key-value pairs for options.
  
  Registers various methods for the operator: 

  * `interpret-op` (use [[interpret]]) to access the interpretation function
  * `make-op` (use [[make]]) → constructor (either uses the provided `args` or a custom constructor function via the option `:constructor`)
  * `simplify-op` (use [[simplify]]) → simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)
  * [[valid-op?]] → validator (provided by the `:predicate` option)
  * [[op-data]] → returns a key-value map of the operator arguments
  * [[op-get]] → returns a specific value by a given argument-key"
  [k args interpretation & params]
  (apply symx/defoperator-impl k args interpretation params))

(defmacro defsymbol
  "Defines a new expression symbol by its symbol (a keyword) and an interpretation function. Takes additional key-value pairs for options.

  Registers various methods for the expression symbol:

  * `interpret-sym` (use [[interpret]]) → to access the interpretation function
  * `simplify-sym` (use [[simplify]]) → simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)"
  [k interpretation & params]
  (apply symx/defsymbol-impl k interpretation params))

;; Multimethod specs - just for documentation
;; -> do not work with instrumentation

(s/fdef make-op
  :args (s/cat :tag  ::sp/op-symbol
               :args (s/* any?))
  :ret  ::sp/operator)
(def make-op
  "Constructs a symbolic expression given a registered operator and parameters.
  
  Note: default to use [[make]] instead of `make-op`"
  symx/make-op)

(s/fdef valid-op?
  :args (s/cat :operator (s/spec ::sp/generic-operator))
  :ret  boolean?)
(def valid-op?
  "Validates the shape of a symbolic expression with a registered operator."
  symx/valid-op?)

(s/fdef op-get
  :args (s/cat :operator (s/spec ::sp/operator)
               :param    keyword?)
  :ret  any?)
(def op-get
  "Gets a specified part from a symbolic expression with a registered operator."
  symx/op-get)

(s/fdef op-data
  :args (s/cat :operator (s/spec ::sp/operator))
  :ret  (s/map-of keyword? any?))
(def op-data
  "Gets all parameters from a symbolic expression with a registered operator as a map."
  symx/op-data)


;;=========================================================================
;; Predefined Operators

;;-------------------------------------------------------------------------
;; arrangement {`:-`}

(def arrangement? (partial s/valid? ::sp/arrangement))


;;-------------------------------------------------------------------------
;; unclear FORM {`:uncl`}

(def unclear? (partial s/valid? ::sp/unclear))


;;-------------------------------------------------------------------------
;; seq-reentry FORM {`:seq-re`}

(def seq-reentry? (partial s/valid? ::sp/seq-reentry))

(s/fdef seq-re
  :args (s/cat :specs        (s/or :sign ::sp/seq-reentry-signature
                                   :opts
                                   (s/keys :opt-un [:opts.seq-reentry/parity
                                                    :opts.seq-reentry/open?
                                                    :opts.seq-reentry/interpr]))
               :nested-exprs (s/* ::sp/expression))
  :ret  ::sp/seq-reentry)
(defn seq-re
  "Constructs a self-equivalent re-entry FORM given the arguments:

  * `specs`: either a `seq-reentry-signature` or an options map
  * `nested-exprs`: zero or more expressions intended as a nested sequence"
  [specs & nested-exprs]
  (apply ops/seq-re specs nested-exprs))

(s/fdef seq-reentry-sign->opts
  :args (s/cat :sign ::sp/seq-reentry-signature)
  :ret  ::sp/seq-reentry-opts)
(defn seq-reentry-sign->opts
  "Maps signatures for self-equivalent re-entry FORMs to their corresponding option-maps."
  [sign]
  (ops/seq-reentry-sign->opts sign))

(s/fdef seq-reentry-opts->sign
  :args (s/cat :opt-map (s/keys :opt-un [:opts.seq-reentry/parity
                                         :opts.seq-reentry/open?
                                         :opts.seq-reentry/interpr]))
  :ret  ::sp/seq-reentry-signature)
(defn seq-reentry-opts->sign
  "Inverse map of [[seq-reentry-sign->opts]] with default args."
  [opt-map]
  (ops/seq-reentry-opts->sign opt-map))

(def seq-reentry-signature? (partial s/valid? ::sp/seq-reentry-signature))
(def seq-reentry-opts? (partial s/valid? ::sp/seq-reentry-opts))


;;-------------------------------------------------------------------------
;; memory FORM {`:mem`}

(def memory? (partial s/valid? ::sp/memory))
(def rem-pair? (partial s/valid? ::sp/rem-pair))

(s/fdef memory
  :args (s/cat :rem-pairs ::sp/rem-pairs
               :exprs     (s/* ::sp/expression))
  :ret  ::sp/memory)
(defn memory
  "Constructs a memory FORM from a given list of `rem-pair`s (key-value pairs, where both the key and the value is an expression) and one or more expressions which are in their scope."
  [rem-pairs & exprs]
  (apply ops/memory rem-pairs exprs))

(s/fdef memory-replace
  :args (s/cat :mem        (s/spec ::sp/memory)
               :repl-pairs (s/* ::sp/rem-pair))
  :ret  ::sp/memory)
(defn memory-replace
  "Takes a memory FORM and replaces its `rem-pair`s by one or more given replacement pairs."
  [mem & repl-pairs]
  (apply ops/memory-replace mem repl-pairs))

(s/fdef memory-extend
  :args (s/cat :mem       (s/spec ::sp/memory)
               :ext-pairs (s/* ::sp/rem-pair))
  :ret  ::sp/memory)
(defn memory-extend
  "Takes a memory FORM and extends its `rem-pair`s by one or more given extension pairs."
  [mem & ext-pairs]
  (apply ops/memory-extend mem ext-pairs))


;;-------------------------------------------------------------------------
;; formDNA {`:fdna`}

(def formDNA? (partial s/valid? ::sp/formDNA))

(s/fdef formDNA-perspectives
  :args (s/cat :fdna ::sp/formDNA)
  :ret  ::sp/arrangement) ;; ? be more specific
(defn formDNA-perspectives
  "Takes a formDNA expression and returns its formDNA perspective group."
  [fdna]
  (ops/formDNA-perspectives fdna))

#_
(defn dna->expr
  [dna]
  (make :fdna (gen-vars (calc/dna-dimension dna)) dna))


;;=========================================================================
;; Compound Expressions

;;-------------------------------------------------------------------------
;; isolator FORM class

(s/fdef isolator
  :args ::calc-sp/const
  :ret  ::sp/expression)
(defn isolator
  "Given a constant, returns the corresponding FORM from the isolator class."
  [c]
  (ops/const->isolator c))

;;-------------------------------------------------------------------------
;; selector FORM class

(s/fdef selector
  :args (s/alt :ar1 (s/cat :vars->consts (s/map-of ::sp/variable
                                                   ::calc-sp/const))
               :ar2 (s/cat :vars->consts (s/map-of ::sp/variable
                                                   ::calc-sp/const)
                           :simplify? boolean?))
  :ret  ::sp/expression)
(defn selector
  "Given a map variable->constant, returns a FORM from the selector class."
  ([vars->consts] (ops/selector vars->consts))
  ([vars->consts simplify?]
   (ops/selector vars->consts simplify?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

;;-------------------------------------------------------------------------
;; Compare expressions

;; ! check if correct
(s/fdef equal?
  :args (s/* ::sp/expression)
  :ret  boolean?)
(defn equal?
  "Equality check for expressions. Two expressions are considered equal, if their formDNAs are equal. Compares formDNAs from evaluation results of each expression by calling [[calc/equal-dna?]].

  WARNING: Procedure and assumptions are being reassessed. Use with caution!

  * ordering of variable names in formDNA matters, see [[find-vars]]
  * stricter than [[equiv]], which compares by [[calc/equiv-dna?]]"
  [& exprs]
  (apply core/equal? exprs))

;; ! check if correct
(s/fdef equiv?
  :args (s/* ::sp/expression)
  :ret  boolean?)
(defn equiv?
  "Equivalence check for expressions. Two expressions are considered equivalent, if their formDNAs are equivalent. Compares formDNAs from evaluation results of each expression by calling [[calc/equiv-dna?]].

  WARNING: Procedure and assumptions are being reassessed. Use with caution!

  * ordering of variable names in formDNA is irrelevant
  * looser than [[equal]], which compares by [[calc/equal-dna?]]
  * can be slow on expressions with 6+ variables"
  [& exprs]
  (apply core/equiv? exprs))


;;-------------------------------------------------------------------------
;; Interpret expressions

(s/fdef interpret
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment))
  :ret  ::sp/expression)
(defn interpret
  "Interprets an expression of any kind. Returns the original expression if it cannot be interpreted.

  Can be given an `env` map to interpret variables (as keys). This map can have an optional `--defocus` entry whose value is a set of items that should not be interpreted and a complementary `--focus` entry to only interpret the items specified in its set and nothing else.

  * the keywords `:ops` / `:syms` / `:vars` designate _all_ operations / expression symbols / variables
  * an operator symbol can provided to designate a specific operator
  * any other expression (like a variable) can be designated as itself
  * `--focus` and `--defocus` can cancel each other out if they contain the same item so you usually pick one or the other"
  ([expr] (core/interpret expr))
  ([expr env]
   (core/interpret env expr)))

(s/fdef interpret*
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment))
  :ret  ::sp/expression)
(defn interpret*
  "Like [[interpret]], but repeats substitution on interpreted expressions until they cannot be interpreted any further."
  ([expr] (core/interpret* expr))
  ([expr env]
   (core/interpret* env expr)))

(s/fdef interpret-walk
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment))
  :ret  ::sp/expression)
(defn interpret-walk
  "Recursively calls [[interpret]] on given expression and all its subexpressions with a depth-first walk."
  ([expr] (core/interpret-walk expr))
  ([expr env]
   (core/interpret-walk env expr)))

(s/fdef interpret-walk*
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment))
  :ret  ::sp/expression)
(defn interpret-walk*
  "Like [[interpret-walk]], but repeats substitution on interpreted (sub-)expressions until they cannot be interpreted any further."
  ([expr] (core/interpret-walk* expr))
  ([expr env]
   (core/interpret-walk* env expr)))

;; ? spec observe-[…]

;; ? needed
(s/fdef interpret-op
  :args (s/cat :operator (s/spec ::sp/operator))
  :ret  ::sp/expression)
(def interpret-op
  "Interprets a symbolic expression with a registered operator.
  
  Note: default to use [[interpret]] instead"
  symx/interpret-op)

;; ? needed
(s/fdef interpret-sym
  :args (s/cat :expr-symbol ::sp/expr-symbol)
  :ret  ::sp/expression)
(def interpret-sym
  "Interprets a registered symbol.
  
  Note: default to use [[interpret]] instead"
  symx/interpret-sym)


;;-------------------------------------------------------------------------
;; Simplify expressions

(s/def :opts/allow-hole-results? boolean?)
(s/def :opts/allow-hole-exprs? boolean?)

;; ? remove in impl
(s/fdef simplify
  :args (s/alt :ar1 (s/cat :x   ::sp/expression)
               :ar2 (s/cat :x   ::sp/expression
                           :env ::sp/environment)
               :ar3 (s/cat :x   ::sp/expression
                           :env ::sp/environment
                           :opts (s/keys :opt-un [:opts/allow-hole-exprs?])))
  :ret  ::sp/expression)
(defn simplify
  "Simplifies a FORM recursively until it cannot be further simplified. All deductions are justified by the axioms of FORM logic.

  * if no simplification applies, tries to retrieve the value from given `env`
  * if retrieval was unsuccessful, returns `x` as is

  An `opts` map can be provided with the following keys:

  * `:allow-hole-exprs?` → (default: `false`) allows “value holes” (`:_`) in the input expression (each instance will be treated like a different variable)"
  ([x] (core/cnt> x))
  ([x env] (core/cnt> x env))
  ([x env opts] (core/cnt> x (update env :--opts merge opts))))
;; alias
(def >> "Alias for [[simplify]]."
  simplify)

;; ? remove in impl
(s/fdef simplify-in
  :args (s/alt :ar1 (s/cat :ctx ::sp/context)
               :ar2 (s/cat :ctx ::sp/context
                           :env ::sp/environment)
               :ar3 (s/cat :ctx ::sp/context
                           :env ::sp/environment
                           :opts (s/keys :opt-un [:opts/allow-hole-exprs?])))
  :ret  ::sp/context)
(defn simplify-in
  "Simplifies a context/sequence of FORMs recursively until it cannot be further simplified. All deductions are justified by the axioms of FORM logic.

  An `opts` map can be provided with the following keys:

  * `:allow-hole-exprs?` → (default: `false`) allows “value holes” (`:_`) in the input expression (each instance will be treated like a different variable)"
  ([ctx] (core/ctx> ctx))
  ([ctx env] (core/ctx> ctx env))
  ([ctx env opts] (core/ctx> ctx (update env :--opts merge opts))))
;; alias
(def in>> "Alias for [[simplify-in]]."
  simplify-in)

(defn simplify-expr-chain
  "Obsolete → use [[simplify-nested-l]] or [[simplify-nested-r]] instead!"
  ([chain env] (core/simplify-nesting-chain chain env))
  ([opts chain env] (core/simplify-nesting-chain opts chain env)))
(def chain>> "Obsolete → use [[nested-l>>]] or [[nested-r>>]] instead!"
  simplify-expr-chain)

(s/fdef simplify-nested-l
  :args (s/alt :ar1 (s/cat :nesting-chain ::sp/nesting-chain-l)
               :ar2 (s/cat :nesting-chain ::sp/nesting-chain-l
                           :env  ::sp/environment)
               :ar3 (s/cat :nesting-chain ::sp/nesting-chain-l
                           :env  ::sp/environment
                           :opts (s/keys :opt-un [:opts/allow-hole-exprs?])))
  :ret ::sp/nesting-chain-l)
(defn simplify-nested-l
  "Reduces a leftward `nesting-chain`, a sequence of expressions `( … x y z )` whose interpretation is `( [[[…] x] y] z )`, to a simplified nesting chain, possibly spliced or shortened via inference.

  * takes an optional `env` that gets applied to the nested expansion

  An `opts` map can be provided with the following keys:

  * `:allow-hole-exprs?` → (default: `false`) allows “value holes” (`:_`) in the input expression (each instance will be treated like a different variable)"
  ([nesting-chain]
   (core/simplify-nesting-chain {:rtl? true} nesting-chain {}))
  ([nesting-chain env]
   (core/simplify-nesting-chain {:rtl? true} nesting-chain env))
  ([nesting-chain env opts]
   (core/simplify-nesting-chain {:rtl? true} nesting-chain
                                (update env :--opts merge opts))))

;; alias
(def nested-l>> "Alias for [[simplify-nested-l]]."
  simplify-nested-l)

(s/fdef simplify-nested-r
  :args (s/alt :ar1 (s/cat :nesting-chain ::sp/nesting-chain-r)
               :ar2 (s/cat :nesting-chain ::sp/nesting-chain-r
                           :env  ::sp/environment)
               :ar3 (s/cat :nesting-chain ::sp/nesting-chain-r
                           :env  ::sp/environment
                           :opts (s/keys :opt-un [:opts/allow-hole-exprs?])))
  :ret  ::sp/nesting-chain-r)
(defn simplify-nested-r
  "Reduces a rightward `nesting-chain`, a sequence of expressions `( a b c … )` whose interpretation is `( a [b [c […]]] )`, to a simplified nesting chain, possibly spliced or shortened via inference.
  
  * takes an optional `env` that gets applied to the nested expansion

  An `opts` map can be provided with the following keys:

  * `:allow-hole-exprs?` → (default: `false`) allows “value holes” (`:_`) in the input expression (each instance will be treated like a different variable)"
  ([nesting-chain]
   (core/simplify-nesting-chain {:rtl? false} nesting-chain {}))
  ([nesting-chain env]
   (core/simplify-nesting-chain {:rtl? false} nesting-chain env))
  ([nesting-chain env opts]
   (core/simplify-nesting-chain {:rtl? false} nesting-chain
                                (update env :--opts merge opts))))
;; alias
(def nested-r>> "Alias for [[simplify-nested-r]]."
  simplify-nested-r)


;;-------------------------------------------------------------------------
;; Evaluate expressions

(s/fdef eval->expr
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment))
  :ret  ::sp/expression)
(defn eval->expr
  "Evaluates a FORM `expr` with an optional `env` and returns an expression: either a constant or the simplified input expression, if it could not be determined to a value.  

  * `env` must be a map from variables to expressions"
  ([expr] (core/=> expr))
  ([expr env] (core/=> expr env)))
(def => "Alias for [[eval->expr]]."
  eval->expr)


(s/def :opts/reduce-dna? boolean?)
(s/def :opts/pre-simplify? boolean?)
(s/def :opts/rich-results? boolean?)

(s/fdef eval->expr-all
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment)
               :ar3 (s/cat :expr ::sp/expression
                           :env  ::sp/environment
                           :opts (s/keys :opt-un [::sp/varorder
                                                  :opts/allow-hole-results?
                                                  :opts/allow-hole-exprs?
                                                  :opts/reduce-dna?
                                                  :opts/pre-simplify?])))
  :ret  ::sp/expression)
(defn eval->expr-all
  "Like [[eval->expr]], but evaluates all possible interpretations of any occurring variable in the `expr`. Returns an expression: either a constant, a formDNA expression (which collects all interpretation results) or the simplified input expression, if it could not be determined to a value in any of its interpretations.  
  * `env` must be a map from variables to expressions

  An `opts` map can be provided with the following keys:

  * `:varorder` → sets the variable interpretation order for the resulting formDNA
  * `:pre-simplify?` → (default: `true`) simplifies the expression before interpretation, which might reduce terms and therefore evaluation time
  * `:reduce-dna?` → (default: `true`) if result is a formDNA expression which can be reduced to fewer terms, reduces it to further simplify the output
  * `:allow-hole-results?` → (default: `false`) sets a “value hole” (`:_`) in place of an uninterpretable result
  * `:allow-hole-exprs?` → (default: `false`) allows “value holes” (`:_`) in the input expression (each instance will be treated like a different variable)"
  ([expr] (core/=>* expr {} {}))
  ([expr env] (core/=>* expr env {}))
  ([expr env opts] (core/=>* expr env opts)))
(def =>* "Alias for [[eval->expr-all]]."
  eval->expr-all)


(s/fdef eval->val
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment))
  :ret  ::calc-sp/const_)
(defn eval->val
  "Evaluates a FORM `expr` with an optional `env` and returns a value: either a constant or a “value hole” `:_` (in case a value cannot be determined, which usually happens when variables remain uninterpreted).  

  * `env` must be a map from variables to expressions"
  ([expr] (core/==> expr))
  ([expr env] (core/==> expr env)))
(def ==> "Alias for [[eval->val]]."
  eval->val)

(s/fdef eval->val-all
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment)
               :ar3 (s/cat :expr ::sp/expression
                           :env  ::sp/environment
                           :opts (s/keys :opt-un [::sp/varorder
                                                  :opts/allow-hole-exprs?
                                                  :opts/reduce-dna?
                                                  :opts/pre-simplify?])))
  :ret  ::calc-sp/dna)
(defn eval->val-all
  "Like [[eval->val]], but evaluates all possible interpretations of any occurring variable in the `expr`. Returns a complex value called formDNA, which collects all interpretation results.

  * `env` must be a map from variables to expressions

  Note: interpretation results might (in very rare cases) include “value holes” `:_`, if a value cannot be determined. This applies to unregistered symbols (like `:pie`) or uninterpretable results from custom, user-defined operators. To avoid holes, wrap them in unclear FORMs, e.g. `(make :uncl :pie)`.

  An `opts` map can be provided with the following keys:

  * `:varorder` → sets the variable interpretation order for the resulting formDNA
  * `:pre-simplify?` → (default: `false`) simplifies the expression before interpretation, which might reduce terms and therefore evaluation time
  * `:reduce-dna?` → (default: `false`) if result is a formDNA expression which can be reduced to fewer terms, reduces it to further simplify the output
  * `:allow-hole-exprs?` → (default: `false`) allows “value holes” (`:_`) in the input expression (each instance will be treated like a different variable)"
  ([expr] (core/==>* expr {} {}))
  ([expr env] (core/==>* expr env {}))
  ([expr env opts] (core/==>* expr env opts)))
(def ==>* "Alias for [[eval->val-all]]."
  eval->val-all)


(s/fdef eval-tsds->val-all
  :args (s/& (s/* #{0 1}) #(== (count %) 6))
  :ret  ::calc-sp/dna)
(defn eval-tsds->val-all
  "Convenience function that takes a 6-digit binary `selection` (as a vector) for a triple-selective decision system (TsDS) and returns the formDNA for the (as by [[eval->val-all]]) evaluated expression.

  Note: contrary to [[eval->val-all]], the resulting formDNA does not get reduced."
  [& selection]
  (core/==>* (make :tsds (vec selection) 'a 'b 'c)
             {} {:opts/reduce-dna? false :opts/pre-simplify? false}))
(def ts==>* "Alias for [[eval-tsds->val-all]]."
  eval-tsds->val-all)


(s/def :evaluate/result (s/or :const ::calc-sp/const
                              :expr  ::sp/expression))

(s/fdef evaluate
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment))
  :ret  (s/keys :req-un [:evaluate/result]))
(defn evaluate
  "Evaluates a FORM `expr` with an optional `env` and returns a map with a `:result` entry that is either a constant or `nil`, if it could not be determined to a value. It also contains a `:simplified` entry with the simplified expression.  
  * `env` must be a map from variables to expressions"
  ([expr] (core/evaluate expr {}))
  ([expr env] (core/evaluate expr env)))


(s/def :eval-all/results
  (let [interpr-spec ::calc-sp/vpoint
        result-spec  (s/or :const ::calc-sp/const
                           :hole  ::calc-sp/val-hole
                           :none  nil?
                           :map   (s/keys :req-un [:evaluate/result]))]
    (s/and (s/coll-of (s/cat :interpretation interpr-spec
                             :result         result-spec)
                      :kind sequential?)
           ::calc-sp/dna-count)))

(s/fdef eval-all
  :args (s/alt :ar1 (s/cat :expr ::sp/expression)
               :ar2 (s/cat :expr ::sp/expression
                           :env  ::sp/environment)
               :ar3 (s/cat :expr ::sp/expression
                           :env  ::sp/environment
                           :opts (s/keys :opt-un [::sp/varorder
                                                  :opts/allow-hole-results?
                                                  :opts/allow-hole-exprs?
                                                  :opts/pre-simplify?
                                                  :opts/rich-results?])))
  :ret  (s/keys :req-un [::sp/varorder :eval-all/results]))
(defn eval-all
  "Like [[evaluate]], but evaluates all possible interpretations of any occurring variable in the `expr`, with an optional `env`.  
  * `env` must be a map from variables to expressions

  Returns a map with the following entries:  

  * `:results` → vector of `[<interpretation> <result>]` tuples (with `result` like in `evaluate`), think of it as a kind of value table
  * `:varorder` → the reading order of variable interpretations

  An `opts` map can be provided with the following keys:

  * `:varorder` → sets the variable interpretation order for the results
  * `:rich-results?` → each results value will be a map as if returned by [[evaluate]]
  * `:pre-simplify?` → (default: `false`) simplifies the expression before interpretation, which might reduce terms and therefore evaluation time
  * `:allow-hole-results?` → (default: `false`) sets a “value hole” (`:_`) in place of `nil` for an uninterpretable result
  * `:allow-hole-exprs?` → (default: `false`) allows “value holes” (`:_`) in the input expression (each instance will be treated like a different variable)"
  ([expr] (core/eval-all expr {} {}))
  ([expr env] (core/eval-all expr env {}))
  ([expr env opts] (core/eval-all expr env opts)))


;; Exclude all multimethods
;; -> instrumentation of multimethods causes issues with defmethod 
;;    and other problems (e.g. ClassCastException) due to wrapping
(def ^:private exclude-from-instrumentation
  #{`make-op
    `valid-op?
    `interpret-op
    `simplify-op
    `op-get
    `op-data
    `interpret-sym
    `simplify-sym})

(def ^:no-doc fns-with-specs
  (remove exclude-from-instrumentation
          (utils/list-fn-specs "formform.expr")))



(comment
  (=> ['a])
  (=>* [])

  (==> ['a])
  (==>* [])

  (eval-all [:mem [['a ['a]]] 'a])
  ;; => [:fdna [a] [:m :i :u :n]]
  (simplify [:mem [['a ['a]]] 'a])

  (meta (evaluate ['a [['a]]]))

  (require '[formform.calc :as calc])

  (map (fn [vars vv]
         (->> (apply hash-map (interleave vars vv))
              (simplify '[a [b]])))
       (repeat ['a 'b])
       (calc/vspace 2))

  ,)

(comment

  (make :uncl "hey")
  (==>* ['a (make :uncl :g)])

  (require '[formform.calc :as calc])
  (-> (eval-all [['a] 'b]) :results (calc/filter-dna [:u :i]))

  ,)

(comment
  ;; ? should `::sp/expression` spec `::sp/operator` instead of `::sp/generic-operator`
  (s/conform ::sp/expression [:uncl "ä"]) ;=> [:operator {:tag :uncl, :label "ä"}]

  ;; ? should `::sp/struct-expr` and/or `::sp/context` designate their matches
  (s/conform ::sp/expression [:foo [] []]) ;=> [:form [:foo [] []]]
  (s/conform ::sp/expression [[:m] [:- 'a ['b]]]) ;=> [:form [[:m] [:- a [b]]]]

  (s/conform ::sp/context [[:m] [:- 'a ['b]] 'x :u nil])
  ;;=> [[:form [:m]] [:operator {:tag :-, :args-unchecked [a [b]]}] [:variable [:sym x]] [:expr-symbol :u] [:empty nil]]


  (s/valid? ::sp/operator (seq-re {} nil))
  (seq-reentry-opts->sign {})
  (seq-reentry-sign->opts :<r))

(comment

  (s/conform :eval-all/results
             (:results
              (eval-all [:- 'a [:x ['b]]] {} {:allow-hole-results? true})))

  (s/conform :eval-all/results
             (:results
              (eval-all [:- 'a [:x ['b]]] {} {:allow-hole-results? true
                                              :rich-results? true})))

  (s/conform :eval-all/results
             (:results
              (eval-all [:- 'a [:x ['b]]] {} {:allow-hole-results? true
                                              :ordered-results? true})))

  (s/conform :eval-all/results
             (:results (eval-all [:- 'a [['b]]])))

  (s/conform :eval-all/results
             (:results (eval-all [:- 'a [['b]]]
                                 {} {:ordered-results? true})))

  (s/conform :eval-all/results
             (:results
              (eval-all [:- 'a [:x ['b]]] {} {:allow-hole-results? true})))

  (s/conform :eval-all/results
             (:results (eval-all [:- 'a [:x ['b]]])))

  (s/conform :eval-all/results
             (:results (eval-all [:- 'a [['b]]])))

  )

