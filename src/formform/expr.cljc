(ns formform.expr
  "API for the `expr` module of `formform`."
  (:require
   [formform.calc :as calc]
   [formform.expr.common :as common]
   [formform.expr.symexpr :as symx]
   [formform.expr.core :as core]
   [formform.expr.operators :as ops]
   [formform.utils :as utils]
   [clojure.spec.alpha :as s]
   #_[clojure.spec.gen.alpha :as gen]))

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
;; → interpret the _representation_ as an indicator of a _value_
;; 
;; - representation → _form_ {syntax} and intentionality {semantics}
;; - form → relate structure and code
;; - value → _calc/value_
;; - value > _calc/constant_ {determined} or _formDNA_ {contingent}
;;
;; variable
;; → the interpretation of the _expression symbol_ is undetermined
;; 
;; form expression {FORM}
;; → _expression_ of _form_
;; > invert the _value_ of the _relation_
;; 
;; - relation > relate the _values_ of the content of the _expression_
;;

;; ! check for predicates in input validation -> performance
(s/def ::expression
  (s/or :empty          nil?
        :operator       ::generic-operator
        :form           ::struct-expr
        :expr-symbol    ::expr-symbol
        :variable       ::variable
        :unknown-symbol keyword?))

(s/def ::struct-expr
  (s/every ::expression
           :kind sequential?))

(s/def ::form-expr
  (s/and ::struct-expr
         (partial s/invalid? ::generic-operator)))

(s/def ::variable (s/or :str string? :sym symbol?))

(s/def ::vars (s/coll-of ::variable
                         :kind (complement map?)))

(s/def ::varorder (s/coll-of ::variable
                             :kind sequential?))

(s/def ::varseq ::varorder)

(s/def ::expr-chain
  (s/coll-of ::expression
             :kind sequential?
             :min-count 1))

(def expression? (partial s/valid? ::expression))
(def variable? (partial s/valid? ::variable))
(def form? (partial s/valid? ::form-expr))


(s/fdef make
  :args (s/* any?)
  :ret  ::expression)
(defn make
  "Constructor for expressions of any kind. Validates its input. If the first argument (or the first after the options map) is a keyword of a registered operator, will call the constructor for that operator

  Can be given an options map as first argument:

  * `mark?` (default: false) marks the whole expression, creating a FORM
  * `splice?` (default: true) dissolves all top-level arrangements"
  [& args]
  (apply core/make args))

(s/fdef form
  :args (s/* any?)
  :ret  ::struct-expr)
(defn form
  "Constructor for FORM expressions. Calls `make` on arguments."
  [& args]
  (apply core/form args))

;; ? be more specific for :ret
(s/fdef mark-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked?])
               :exprs (s/* ::expression))
  :ret  ::expression)
(defn mark-exprs
  "Chains expressions like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`

  * group expressions with arrangements: `[:- x y …]`"
  [opts & exprs]
  (apply core/mark-exprs opts exprs))

(s/fdef nest-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked? :opts/ltr?])
               :exprs (s/* ::expression))
  :ret  ::expression)
(defn nest-exprs
  "Nests expressions leftwards `(((…)a)b)` or rightwards `(a(b(…)))` if `{:ltr? true}`

  * use `nil` for empty expressions
  * use an arrangement `(make x y …)` to add multiple exprs. to the same level"
  [opts & exprs]
  (apply core/nest-exprs opts exprs))


;; Utils

;; ? check if :ret is subset of :subexprs
(s/fdef find-subexprs
  :args (s/cat :expr ::expression
               :subexprs (s/coll-of ::expression :kind set?))
  :ret  (s/every ::expression
                 :kind sequential?))
(defn find-subexprs
  "Finds all subexpressions in `expr` that match any element of the given set `subexprs`."
  [expr subexprs]
  (core/find-subexprs expr subexprs))

;; ? opts first
(s/fdef find-vars
  :args (s/cat :expr ::expression
               :opts (s/keys :opt-un [:opts/ordered? ::vars]))
  :ret  ::varseq)
(defn find-vars
  "Finds all variables in an expresson or returns the expression itself if it is a variable.

  Options:

  * {:ordered true} to return variables in: type order -> alphanumeric order
  * {:vars #{…}} can be given a set of specific variables to find"
  [expr opts]
  (core/find-vars expr opts))

(s/fdef gen-vars
  :args (s/cat :n nat-int?)
  :ret  ::varseq)
(defn gen-vars
  "Generates a number of variables with random names."
  [n]
  (core/gen-vars n))

(defn expr->const
  "Given an expression, returns the corresponding constant value."
  [expr]
  (symx/expr->const expr))


;;=========================================================================
;; Symbolic Expressions

;;-------------------------------------------------------------------------
;; symbolic expression
;; → _expression symbol_ or _operator_
;;
;; expression symbol
;; → interpret the symbol as a specific _expression_
;;
;; operator
;; → interpret the structure by its symbol as an _expression_ pattern
;;

(s/def ::expr-symbol
  (s/and keyword? #(% (methods symx/interpret-sym))))

(s/def ::op-symbol
  (s/and keyword? #(% (methods symx/interpret-op))))

(s/def ::generic-operator
  (s/cat :tag            ::op-symbol
         :args-unchecked (s/* any?)))

(defmulti op-spec
  "Given an operator symbol, returns the spec for the operator (if defined)."
  first)

(s/def ::operator (s/multi-spec op-spec first))

(def expr-symbol? (partial s/valid? ::expr-symbol))
(def op-symbol? (partial s/valid? ::op-symbol))
(def operator? (partial s/valid? ::operator))

(def op-symbol
  "Returns the symbol of a given operator."
  symx/op-symbol)


(defmacro defoperator
  "Defines a new operator by its symbol (a keyword), a vector of arguments and an interpretation function. Takes additional key-value pairs for options.
  
  Registers various methods for the operator: 

  * `interpret-op` to access the interpretation function
  * `make-op` -> constructor (either uses the provided `args` or a custom constructor function via the option `:constructor`)
  * `simplify-op` -> simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)
  * `valid-op?` -> validator (provided by the `:predicate` option)
  * `op-data` -> returns a key-value map of the operator arguments
  * `op-get` -> returns a specific value by a given argument-key"
  [k args interpretation & {:as params}]
  (apply symx/defoperator-impl k args interpretation params))

(defmacro defsymbol
  "Defines a new expression symbol by its symbol (a keyword) and an interpretation function. Takes additional key-value pairs for options.

  Registers various methods for the expression symbol:

  * `interpret-sym` -> to access the interpretation function
  * `simplify-sym` -> simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)"
  [k interpretation & {:as params}]
  (apply symx/defsymbol-impl k interpretation params))

;; Multimethod specs - just for documentation
;; -> do not work with instrumentation

(s/fdef make-op
  :args (s/cat :tag  ::op-symbol
               :args (s/* any?))
  :ret  ::operator)
(def make-op
  "Constructs a symbolic expression given a registered operator and parameters.
  
  Note: default to use `make` instead of `make-op`"
  symx/make-op)

(s/fdef valid-op?
  :args (s/cat :operator (s/spec ::generic-operator))
  :ret  boolean?)
(def valid-op?
  "Validates the shape of a symbolic expression with a registered operator."
  symx/valid-op?)

(s/fdef op-get
  :args (s/cat :operator (s/spec ::operator)
               :param    keyword?)
  :ret  any?)
(def op-get
  "Gets a specified part from a symbolic expression with a registered operator."
  symx/op-get)

(s/fdef op-data
  :args (s/cat :operator (s/spec ::operator))
  :ret  (s/map-of keyword? any?))
(def op-data
  "Gets all parameters from a symbolic expression with a registered operator as a map."
  symx/op-data)


;;=========================================================================
;; Predefined Operators

(defmethod op-spec :default [_] ::operator)
(defmethod op-spec common/tag_arrangement [_] ::arrangement)
(defmethod op-spec common/tag_unclear [_] ::unclear)
(defmethod op-spec common/tag_seq-reentry [_] ::seq-reentry)
(defmethod op-spec common/tag_memory [_] ::memory)
(defmethod op-spec common/tag_formDNA [_] ::formDNA)


;;-------------------------------------------------------------------------
;; arrangement {`:-`}
;; → _operator_ to construct _relations_

(s/def ::arrangement
  (s/cat :tag   (partial = common/tag_arrangement)
         :exprs (s/* #(s/valid? ::expression %))))

(def arrangement? (partial s/valid? ::arrangement))


;;-------------------------------------------------------------------------
;; unclear FORM {`:uncl`}
;; → _operator_ to construct unclear _FORMs_

(s/def ::unclear
  (s/cat :tag   (partial = common/tag_unclear)
         :label #(and (string? %) ((complement empty?) %))))

(def unclear? (partial s/valid? ::unclear))


;;-------------------------------------------------------------------------
;; seq-reentry FORM {`:seq-re`}
;; → _operator_ to construct self-equivalent re-entry _FORMs_

(s/def ::seq-reentry-signature ops/seq-reentry-signature?)

(s/def :opts.seq-reentry/parity #{:odd :even :any})
(s/def :opts.seq-reentry/open? boolean?)
(s/def :opts.seq-reentry/interpr #{:rec-ident :rec-instr})

(s/def ::seq-reentry-opts
  (s/keys :req-un [:opts.seq-reentry/parity
                   :opts.seq-reentry/open?
                   :opts.seq-reentry/interpr]))

(s/def ::seq-reentry
  (s/cat :tag (partial = common/tag_seq-reentry)
         :sign ::seq-reentry-signature
         :nested-exprs (s/+ ::expression)))

(def seq-reentry? (partial s/valid? ::seq-reentry))

(s/fdef seq-re
  :args (s/cat :specs        (s/or :sign ::seq-reentry-signature
                                   :opts
                                   (s/keys :opt-un [:opts.seq-reentry/parity
                                                    :opts.seq-reentry/open?
                                                    :opts.seq-reentry/interpr]))
               :nested-exprs (s/* ::expression))
  :ret  ::seq-reentry)
(defn seq-re
  "Constructs a self-equivalent re-entry FORM given the arguments:

  * `specs`: either a `seq-reentry-signature` or an options map
  * `nested-exprs`: zero or more expressions intended as a nested sequence"
  [specs & nested-exprs]
  (apply ops/seq-re specs nested-exprs))

(s/fdef seq-reentry-sign->opts
  :args (s/cat :sign ::seq-reentry-signature)
  :ret  ::seq-reentry-opts)
(defn seq-reentry-sign->opts
  "Maps signatures for self-equivalent re-entry FORMs to their corresponding option-maps."
  [sign]
  (ops/seq-reentry-sign->opts sign))

(s/fdef seq-reentry-opts->sign
  :args (s/cat :opt-map (s/keys :opt-un [:opts.seq-reentry/parity
                                         :opts.seq-reentry/open?
                                         :opts.seq-reentry/interpr]))
  :ret  ::seq-reentry-signature)
(defn seq-reentry-opts->sign
  "Inverse map of seq-reentry-sign->opts with default args."
  [opt-map]
  (ops/seq-reentry-opts->sign opt-map))

(def seq-reentry-signature? (partial s/valid? ::seq-reentry-signature))
(def seq-reentry-opts? (partial s/valid? ::seq-reentry-opts))


;;-------------------------------------------------------------------------
;; memory FORM {`:mem`}
;; → _operator_ to construct memory _FORMs_
;; 
;; rem pair
;; → observe and remember equality between the two expressions

(s/def ::rem-pair
  (s/tuple #(s/valid? ::expression %)
           #(s/valid? ::expression %)))
(s/def ::rem-pairs
  (s/coll-of ::rem-pair
             :kind sequential?
             :into []))

(s/def ::memory
  (s/cat :tag       (partial = common/tag_memory)
         :rem-pairs ::rem-pairs
         :ctx       (s/* #(s/valid? ::expression %))))

(def memory? (partial s/valid? ::memory))
(def rem-pair? (partial s/valid? ::rem-pair))

(s/fdef memory
  :args (s/cat :rem-pairs ::rem-pairs
               :exprs     (s/* ::expression))
  :ret  ::memory)
(defn memory
  "Constructs a memory FORM from a given list of `rem-pair`s (key-value pairs, where both the key and the value is an expression) and one or more expressions which are in their scope."
  [rem-pairs & exprs]
  (apply ops/memory rem-pairs exprs))

(s/fdef memory-replace
  :args (s/cat :mem        (s/spec ::memory)
               :repl-pairs (s/* ::rem-pair))
  :ret  ::memory)
(defn memory-replace
  "Takes a memory FORM and replaces its `rem-pair`s by one or more given replacement pairs."
  [mem & repl-pairs]
  (apply ops/memory-replace mem repl-pairs))

(s/fdef memory-extend
  :args (s/cat :mem       (s/spec ::memory)
               :ext-pairs (s/* ::rem-pair))
  :ret  ::memory)
(defn memory-extend
  "Takes a memory FORM and extends its `rem-pair`s by one or more given extension pairs."
  [mem & ext-pairs]
  (apply ops/memory-extend mem ext-pairs))


;;-------------------------------------------------------------------------
;; formDNA {`:fdna`}
;; → _operator_ to construct _calc/formDNA_ _expressions_

(s/def ::formDNA
  (s/and (s/nonconforming
          (s/cat :tag      (partial = common/tag_formDNA)
                 :varorder ::varorder
                 :dna      :formform.calc/dna))
         #(== (count (second %)) (calc/dna-dimension (nth % 2)))))

(def formDNA? (partial s/valid? ::formDNA))


;;=========================================================================
;; Compound Expressions

;;-------------------------------------------------------------------------
;; isolator FORM class

(s/fdef isolator
  :args :formform.calc/const
  :ret  ::expression)
(defn isolator
  "Given a constant, returns the corresponding FORM from the isolator class."
  [c]
  (ops/const->isolator c))

;;-------------------------------------------------------------------------
;; selector FORM class

(s/fdef selector
  :args (s/alt :ar1 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.calc/const))
               :ar2 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.calc/const)
                           :simplify? boolean?))
  :ret  ::expression)
(defn selector
  "Given a map variable->constant, returns a FORM from the selector class."
  ([vars->consts] (ops/selector vars->consts))
  ([vars->consts simplify?]
   (ops/selector vars->consts simplify?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

(s/def ::environment map?)

(s/def ::context
  (s/coll-of ::expression
             :kind sequential?))

;;-------------------------------------------------------------------------
;; Compare expressions

(s/fdef equal
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)
(defn equal
  "Equality check for expressions. Two expressions are considered equal, if their formDNAs are equal. Compares formDNAs from evaluation results of each expression by calling `calc/equal-dna`.

  * ordering of variable names in formDNA matters, see `find-vars`
  * stricter than `equiv`, which compares by `calc/equiv-dna`"
  [& exprs]
  (apply core/equal exprs))

(s/fdef equiv
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)
(defn equiv
  "Equivalence check for expressions. Two expressions are considered equivalent, if their formDNAs are equivalent. Compares formDNAs from evaluation results of each expression by calling `calc/equiv-dna`.

  * ordering of variable names in formDNA is irrelevant
  * looser than `equal`, which compares by `calc/equal-dna`
  * can be slow on expressions with 6+ variables"
  [& exprs]
  (apply core/equiv exprs))


;;-------------------------------------------------------------------------
;; Interpret expressions

(s/fdef interpret
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defn interpret
  "Interprets an expression of any kind. Returns the original expression if it cannot be interpreted.

  Can be given an `env` map to interpret variables (as keys). This map can have an optional `--defocus` entry whose value is a set of items that should not be interpreted and a complementary `--focus` entry to only interpret the items specified in its set and nothing else.

  * the keywords `:ops` / `:syms` / `:vars` designate _all_ operations / expression symbols / variables
  * an operator symbol can provided to designate a specific operator
  * any other expression (like a variable) can be designated as itself
  * `--focus` and `--defocus` can cancel each other out if they contain the same item so you usually pick one or the other"
  ([expr] (core/interpret expr))
  ([env expr]
   (core/interpret env expr)))

(s/fdef interpret*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defn interpret*
  "Like `interpret`, but repeats substitution on interpreted expressions until they cannot be interpreted any further."
  ([expr] (core/interpret* expr))
  ([env expr]
   (core/interpret* env expr)))

(s/fdef interpret-walk
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defn interpret-walk
  "Recursively calls `interpret` on given expression and all its subexpressions with a depth-first walk."
  ([expr] (core/interpret-walk expr))
  ([env expr]
   (core/interpret-walk env expr)))

(s/fdef interpret-walk*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defn interpret-walk*
  "Like `interpret-walk`, but repeats substitution on interpreted (sub-)expressions until they cannot be interpreted any further."
  ([expr] (core/interpret-walk* expr))
  ([env expr]
   (core/interpret-walk* env expr)))

;; ? spec observe-[…]

;; ? needed
(s/fdef interpret-op
  :args (s/cat :operator (s/spec ::operator))
  :ret  ::expression)
(def interpret-op
  "Interprets a symbolic expression with a registered operator.
  
  Note: default to use `interpret` instead"
  symx/interpret-op)

;; ? needed
(s/fdef interpret-sym
  :args (s/cat :expr-symbol ::expr-symbol)
  :ret  ::expression)
(def interpret-sym
  "Interprets a registered symbol.
  
  Note: default to use `interpret` instead"
  symx/interpret-sym)


;;-------------------------------------------------------------------------
;; Simplify expressions

;; ? remove in impl
(s/fdef simplify
  :args (s/alt :ar1 (s/cat :x   ::expression)
               :ar2 (s/cat :x   ::expression
                           :env ::environment))
  :ret  ::expression)
(defn simplify
  "Simplifies a FORM recursively until it cannot be further simplified. All deductions are justified by the axioms of FORM logic.

  * if `x` is a complex FORM, calls `simplify-context` on `x`
  * if no simplification applies, tries to retrieve the value from given `env`
  * if retrieval was unsuccessful, returns `x` as is"
  ([x] (core/cnt> x))
  ([x env]
   (core/cnt> x env)))
;; alias
(def >> simplify)

;; ? remove in impl
(s/fdef simplify-in
  :args (s/alt :ar1 (s/cat :ctx ::context)
               :ar2 (s/cat :ctx ::context
                           :env ::environment))
  :ret  ::context)
(defn simplify-in
  "Simplifies a context/sequence of FORMs recursively until it cannot be further simplified. All deductions are justified by the axioms of FORM logic.

  * for complex expressions, calls `expr.core/simplify-content` on every unique element"
  ([ctx] (core/ctx> ctx))
  ([ctx env]
   (core/ctx> ctx env)))
;; alias
(def in>> simplify-in)

;; ? needed
(s/fdef simplify-op
  :args (s/cat :operator (s/spec ::operator)
               :env      ::environment)
  :ret  ::expression)
(def simplify-op
  "Simplifies a symbolic expression with a registered operator given an optional environment.
  
  Note: default to use `simplify` instead"
  symx/simplify-op)

;; ? needed
(s/fdef simplify-sym
  :args (s/cat :expr-symbol ::expr-symbol
               :env         ::environment)
  :ret  ::expression)
(def simplify-sym
  "Simplifies a registered symbol given an optional environment.
  
  Note: default to use `simplify` instead"
  symx/simplify-sym)

(s/fdef simplify-expr-chain
  :args (s/alt :ar2 (s/cat :chain ::expr-chain
                           :env   ::environment)
               :ar3 (s/cat :opts  (s/keys :opt-un [:opts/rtl?])
                           :chain ::expr-chain
                           :env   ::environment))
  :ret  ::expr-chain)
(defn simplify-expr-chain
  "Reduces a sequence of expressions, intended to be linked in a `chain`, to a sequence of simplified expressions, possibly spliced or shortened via inference.

  * assumes rightward-nesting, e.g. `(…(…(…)))`
  * for leftward-nesting, e.g. `(((…)…)…)`, pass `{:rtl? true}`"
  ([chain env] (core/simplify-expr-chain chain env))
  ([opts chain env] (core/simplify-expr-chain opts chain env)))
(def chain>> simplify-expr-chain)


;;-------------------------------------------------------------------------
;; Evaluate expressions

;; ? remove in impl
(s/fdef eval->expr
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  :formform.calc/const?)
(defn eval->expr
  ;; ! verify/correct docstring
  "Evaluates a FORM expression with an optional `env` and returns a constant expression with attached metadata including the maximally reduced expression in `:expr` and the environment in `:env`.

  * `env` must be a map with a content/variable in `expr` as a key"
  ([expr] (core/=> expr))
  ([expr env] (core/=> expr env)))
(def => eval->expr)

;; ? remove in impl
(s/fdef eval->expr-all
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment)
               :ar3 (s/cat :opts (s/keys :opt-un [::varorder])
                           :expr ::expression
                           :env  ::environment))
  :ret  ::formDNA)
(defn eval->expr-all
  ;; ! verify/correct docstring
  "Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.

  * if `to-fdna?` is false, returns a seq of results as returned by `=>` in the order of the corresponding `vspace` ordering"
  ([expr] (core/=>* expr))
  ([expr env] (core/=>* expr env))
  ([opts expr env] (core/=>* opts expr env)))
(def =>* eval->expr-all)

(s/def :evaluate/result (s/or :const :formform.calc/const
                              :expr  ::expression))

(s/fdef evaluate
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  (s/keys :req-un [:evaluate/result]))
(defn evaluate
  "Evaluates a FORM expresson with an optional `env` and returns either a constant or the simplified expression if it could not be determined to a value."
  ([expr] (evaluate expr {}))
  ([expr env]
   (let [res (core/eval-simplified expr env)
         v   (let [v (:val res)]
               (if (= :_ v)
                 (:expr res)
                 v))]
     (with-meta {:result v} res))))

(s/def :eval-all/results
  (s/and (s/coll-of
          (s/cat :interpretation (s/coll-of :formform.calc/const
                                            :kind sequential?)
                 :result         :formform.calc/const)
          :kind sequential?)
         :formform.calc/dna-count))

(s/fdef eval-all
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment)
               :ar3 (s/cat :opts (s/keys :opt-un [::varorder])
                           :expr ::expression
                           :env  ::environment))
  :ret  (s/keys :req-un [::varorder :eval-all/results]))
(defn eval-all
  "Evaluates a FORM expresson for all possible interpretations of any occurring variable in the expresson. Returns a map with a `:results` key whose value is a sequence of `[<interpretation> <result>]` tuples and with a `:varorder` key whose value is the reading order for variable results in the interpretations. This output is particularly suited for value tables."
  ([expr] (eval-all expr {}))
  ([expr env] (eval-all {} expr env))
  ([opts expr env]
   (let [{:keys [varorder results] :as res}
         (core/eval-simplified* (merge opts {:only-vals? false}) expr env)
         vdict (map #(vector (mapv (:env %) varorder)
                             (:val %))
                    results)]
     (with-meta {:varorder varorder :results vdict} res))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation specs for testing

(s/fdef core/splice-ctx
  :args (s/cat :ctx ::context)
  :ret  ::context)
; (defn splice-ctx
;   "Dissolves arrangements in given context such that their elements become direct children of the context itself."
;   [ctx]
;   (core/splice-ctx ctx))

(s/fdef core/substitute-expr
  :args (s/cat :env  ::environment
               :expr ::expression)
  :ret  ::expression)
; (defn substitute-expr
;   "Substitutes an expression by a matching expression in given environment. Returns the original expression if match failed.")

(s/fdef core/ctx->cnt
  :args (s/cat :opts (s/keys :opt-un [:opts/+meta?])
               :ctx  ::context)
  :ret  (s/or :expr+meta (s/tuple ::expression
                                  ::environment)
              :expr ::expression))

(s/fdef core/cnt->ctx
  :args (s/cat :cnt ::expression)
  :ret  ::context)

(s/fdef core/simplify-matching-content
  :args (s/alt :ar1 (s/cat :x ::expression)
               :ar2 (s/cat :x ::expression
                           :default any?))
  :ret  ::expression)

(s/fdef core/simplify-form
  :args (s/cat :form ::struct-expr
               :env  ::environment)
  :ret  ::expression)

;; ? is content = expression
(s/fdef core/simplify-content
  :args (s/cat :x   ::expression
               :env ::environment)
  :ret  ::expression)

(s/fdef core/simplify-by-calling
  :args (s/cat :ctx ::context
               :env ::environment)
  :ret  (s/tuple ::context
                 ::environment))

(s/fdef core/simplify-by-crossing
  :args (s/cat :ctx ::context)
  :ret  ::context)

(s/fdef core/substitute-in-context
  :args (s/cat :env ::environment
               :ctx ::context)
  :ret  ::context)

(s/fdef core/simplify-context
  :args (s/cat :ctx ::context
               :env ::environment)
  :ret  ::context)

(s/fdef core/simplify-env
  :args (s/cat :env ::environment)
  :ret  ::environment)



(s/fdef core/mark
  :args (s/cat :exprs (s/coll-of ::expression :kind sequential?))
  :ret  ::expression)

(s/fdef formform.utils/nest-left
  :args (s/cat :items sequential?)
  :ret  (s/or :nil nil?
              :seq sequential?))

(s/fdef formform.utils/nest-right
  :args (s/cat :items sequential?)
  :ret  (s/or :nil nil?
              :seq sequential?))


(s/fdef ops/construct-unclear
  :args (s/cat :op-k ::op-symbol
               :args (s/* any?))
  :ret  ::unclear)


(s/fdef ops/simplify-rems
  :args (s/cat :rem-pairs ::rem-pairs
               :env       ::environment)
  :ret  (s/tuple ::rem-pairs
                 ::environment))

(s/fdef ops/filter-rems
  :args (s/cat :rem-pairs ::rem-pairs
               :ctx       ::context)
  :ret  ::rem-pairs)

(s/fdef ops/simplify-memory
  :args (s/cat :mem (s/spec ::memory)
               :env ::environment)
  :ret  ::expression)


(s/fdef ops/simplify-seq-reentry
  :args (s/cat :seq-re (s/spec ::seq-reentry)
               :env    ::environment)
  :ret  ::expression)

(s/fdef ops/construct-seq-reentry
  :args (s/cat :op-k         ::op-symbol
               :specs        (s/or :sign ::seq-reentry-signature
                                   :opts
                                   (s/keys :opt-un [:opts.seq-reentry/parity
                                                    :opts.seq-reentry/open?
                                                    :opts.seq-reentry/interpr]))
               :nested-exprs (s/* ::expression))
  :ret  ::seq-reentry)


(s/fdef ops/filter-formDNA
  :args (s/cat :fdna ::formDNA
               :env  ::environment)
  :ret  ::formDNA)

(s/fdef ops/simplify-formDNA
  :args (s/cat :operator (s/spec ::formDNA)
               :env      ::environment)
  :ret  (s/or :fdna  ::formDNA
              :const :formform.calc/const))

(s/fdef ops/construct-formDNA
  :args (s/alt :ar1 (s/cat :op-k ::op-symbol)
               :ar2 (s/cat :op-k ::op-symbol
                           :dna  :formform.calc/dna)
               :ar3 (s/cat :op-k     ::op-symbol
                           :varorder ::varseq
                           :dna      :formform.calc/dna))
  :ret  ::formDNA)


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

  (s/form (s/get-spec `symx/make-op))

  (s/form
   (s/get-spec
    (ffirst (for [[k v] (s/registry)
                  :when (and (symbol? k))]
                            
             [k v]))))

  (s/conform ::expression []) ;=> [:form []]
  (s/conform ::expression :a) ;=> [:unknown-symbol :a]
  (s/conform ::expression :M) ;=> [:expr-symbol :M]
  (s/conform ::expression "x") ;=> [:variable [:str "x"]]
  (s/conform ::expression 'x) ;=> [:variable [:sym x]]
  (s/conform ::expression nil) ;=> [:empty nil]

  ;; ? should `::expression` spec `::operator` instead of `::generic-operator`
  (s/conform ::expression [:uncl "ä"]) ;=> [:operator {:tag :uncl, :label "ä"}]

  ;; ? should `::struct-expr` and/or `::context` designate their matches
  (s/conform ::expression [:foo [] []]) ;=> [:form [:foo [] []]]
  (s/conform ::expression [[:M] [:- 'a ['b]]]) ;=> [:form [[:M] [:- a [b]]]]

  (s/conform ::context [[:M] [:- 'a ['b]] 'x :U nil])
  ;=> [[:form [:M]] [:operator {:tag :-, :args-unchecked [a [b]]}] [:variable [:sym x]] [:expr-symbol :U] [:empty nil]]


 (s/valid? ::operator (seq-re {} nil))
 (seq-reentry-opts->sign {})
 (seq-reentry-sign->opts :<r))

