(ns formform.expr
  "API for the `expr` module of `formform`."
  (:require
   [formform.calc :as calc]
   [formform.expr.common :as common]
   [formform.expr.symexpr :as symx]
   [formform.expr.core :as core]
   [formform.expr.operators :as ops]
   #?(:clj  [formform.utils :as utils :refer [defapi]]
      :cljs [formform.utils :as utils :refer-macros [defapi]])
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data specs

(defmulti op-spec first)

(s/def ::expr-symbol
  (s/and keyword? #(% (methods symx/interpret-sym))))

(s/def ::op-symbol
  (s/and keyword? #(% (methods symx/interpret-op))))

(s/def ::generic-operator
  (s/cat :tag            ::op-symbol
         :args-unchecked (s/* any?)))

(s/def ::operator (s/multi-spec op-spec first))

(s/def ::arrangement
  (s/cat :tag   (partial = common/tag_arrangement)
         :exprs (s/* #(s/valid? ::expression %))))


(s/def ::pure-form
  (s/and ::form
         (complement
          (partial s/valid? ::generic-operator))))

(s/def ::variable (s/or :str string? :sym symbol?))

(s/def ::varorder (s/coll-of ::variable
                             :kind sequential?))

;; ! check for predicates in input validation -> performance
(s/def ::expression
  (s/or :empty nil?
        :operator ::generic-operator
        :form ::form
        :expr-symbol ::expr-symbol
        :variable ::variable
        :unknown-symbol keyword?))

;; ? is this general notion confusing
; (s/def ::form sequential?)
(s/def ::form
  (s/every ::expression
           :kind sequential?))

(s/def ::context
  (s/coll-of ::expression
             :kind sequential?))

(s/def ::environment map?)

(s/def ::expr-chain
  (s/coll-of ::expression
             :kind sequential?
             :min-count 1))


(s/def ::unclear
  (s/cat :tag   (partial = common/tag_unclear)
         :label #(and (string? %) ((complement empty?) %))))


(s/def ::rem-pair
  (s/tuple #(s/valid? ::expression %)
           #(s/valid? ::expression %)))

(s/def ::rems
  (s/coll-of ::rem-pair
             :kind sequential?
             :into []))

(s/def ::memory
  (s/cat :tag  (partial = common/tag_memory)
         :rems ::rems
         :ctx  (s/* #(s/valid? ::expression %))))


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


(s/def ::formDNA
  (s/and (s/nonconforming
          (s/cat :tag      (partial = common/tag_formDNA)
                 :varorder ::varorder
                 :dna      :formform.calc/dna))
         #(== (count (second %)) (calc/dna-dimension (nth % 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator specs

(defmethod op-spec :default [_] ::operator)
(defmethod op-spec common/tag_arrangement [_] ::arrangement)
(defmethod op-spec common/tag_unclear [_] ::unclear)
(defmethod op-spec common/tag_memory [_] ::memory)
(defmethod op-spec common/tag_seq-reentry [_] ::seq-reentry)
(defmethod op-spec common/tag_formDNA [_] ::formDNA)

(defapi common/tag_arrangement)
(defapi common/tag_unclear)
(defapi common/tag_memory)
(defapi common/tag_seq-reentry)
(defapi common/tag_formDNA)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs & API

(s/def :opts/ordered? boolean?)
(s/def :opts/unmarked? boolean?)
(s/def :opts/+meta? boolean?)
(s/def :opts/rtl? boolean?)
(s/def :opts/ltr? boolean?)

(s/def ::vars (s/coll-of ::variable
                         :kind (complement map?)))

(s/def ::varseq ::varorder)


;;-------------------------------------------------------------------------
;; `symexpr` functions

;; Multimethod specs - just for documentation
;; -> do not work with instrumentation

(s/fdef symx/make-op
  :args (s/cat :tag  ::op-symbol
               :args (s/* any?))
  :ret  ::operator)
(defapi symx/make-op
  "Constructs a symbolic expression given a registered operator and parameters.")

(s/fdef symx/valid-op?
  :args (s/cat :operator (s/spec ::generic-operator))
  :ret  boolean?)
(defapi symx/valid-op?
  "Validates the shape of a symbolic expression with a registered operator.")

(s/fdef symx/interpret-op
  :args (s/cat :operator (s/spec ::operator))
  :ret  ::expression)
(defapi symx/interpret-op
  "Interprets a symbolic expression with a registered operator.")

(s/fdef symx/simplify-op
  :args (s/cat :operator (s/spec ::operator)
               :env      ::environment)
  :ret  ::expression)
(defapi symx/simplify-op
  "Simplifies a symbolic expression with a registered operator given an optional environment.")

(s/fdef symx/op-get
  :args (s/cat :operator (s/spec ::operator)
               :param    keyword?)
  :ret  any?)
(defapi symx/op-get
  "Gets a specified part from a symbolic expression with a registered operator.")

(s/fdef symx/op-data
  :args (s/cat :operator (s/spec ::operator))
  :ret  (s/map-of keyword? any?))
(defapi symx/op-data
  "Gets all parameters from a symbolic expression with a registered operator as a map.")

;;

(defapi symx/op-symbol
  "Returns the symbol of an operator.")

(defapi symx/defoperator ;; macro
  "Defines a new operator by its symbol (a keyword), a vector of arguments and an interpretation function. Takes additional key-value pairs for options.
  
  Registers various methods for the operator: 

  * `interpret-op` to access the interpretation function
  * `make-op` -> constructor (either uses the provided `args` or a custom constructor function via the option `:constructor`)
  * `simplify-op` -> simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)
  * `valid-op?` -> validator (provided by the `:predicate` option)
  * `op-data` -> returns a key-value map of the operator arguments
  * `op-get` -> returns a specific value by a given argument-key")

(s/fdef symx/interpret-sym
  :args (s/cat :expr-symbol ::expr-symbol)
  :ret  ::expression)
(defapi symx/interpret-sym
  "Interprets a registered symbol.")

(s/fdef symx/simplify-sym
  :args (s/cat :expr-symbol ::expr-symbol
               :env         ::environment)
  :ret  ::expression)
(defapi symx/simplify-sym
  "Simplifies a registered symbol given an optional environment.")

(defapi symx/defsymbol ;; macro
  "Defines a new expression symbol by its symbol (a keyword) and an interpretation function. Takes additional key-value pairs for options.

  Registers various methods for the expression symbol:

  * `interpret-sym` -> to access the interpretation function
  * `simplify-sym` -> simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)")

(defapi symx/expr-symbol?)
(defapi symx/op-symbol?)
(defapi symx/operator?)
(defapi symx/arrangement?)

(defapi expr->const
  "Given an expression, returns the corresponding constant value.")


;;-------------------------------------------------------------------------
;; `core` functions

(defapi core/form?)
(defapi core/pure-form?)
(defapi core/variable?)
(defapi core/expression?)

(defapi core/struct-expr?)
(defapi core/literal-expr?)

(s/fdef core/splice-ctx
  :args (s/cat :ctx ::context)
  :ret  ::context)
(defapi core/splice-ctx
  "Dissolves arrangements in given context such that their elements become direct children of the context itself.")

(s/fdef core/make
  :args (s/* any?)
  :ret  ::expression)
(defapi core/make
  "Constructor for expressions of any kind. Validates its input. If the first argument (or the first after the options map) is a keyword of a registered operator, will call the constructor for that operator

  Can be given an options map as first argument:

  * `mark?` (default: false) marks the whole expression, creating a FORM
  * `splice?` (default: true) dissolves all top-level arrangements")

(s/fdef core/form
  :args (s/* any?)
  :ret  ::form)
(defapi core/form
  "Constructor for FORM expressions. Calls `make` on arguments.")

(s/fdef core/substitute-expr
  :args (s/cat :env  ::environment
               :expr ::expression)
  :ret  ::expression)
(defapi core/substitute-expr
  "Substitutes an expression by a matching expression in given environment. Returns the original expression if match failed.")

(s/fdef core/interpret
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defapi core/interpret
  "Interprets an expression of any kind. Returns the original expression if it cannot be interpreted.

  Can be given an `env` map to interpret variables (as keys). This map can have an optional `--defocus` entry whose value is a set of items that should not be interpreted and a complementary `--focus` entry to only interpret the items specified in its set and nothing else.

  * the keywords `:ops` / `:syms` / `:vars` designate _all_ operations / expression symbols / variables
  * an operator symbol can provided to designate a specific operator
  * any other expression (like a variable) can be designated as itself
  * `--focus` and `--defocus` can cancel each other out if they contain the same item so you usually pick one or the other")

(s/fdef core/interpret*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defapi core/interpret*
  "Like `interpret`, but repeats substitution on interpreted expressions until they cannot be interpreted any further.")

(s/fdef core/interpret-walk
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defapi core/interpret-walk
  "Recursively calls `interpret` on given expression and all its subexpressions with a depth-first walk.")

(s/fdef core/interpret-walk*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)
(defapi core/interpret-walk*
  "Like `interpret-walk`, but repeats substitution on interpreted (sub-)expressions until they cannot be interpreted any further.")

;; ? check if :ret is subset of :subexprs
(s/fdef core/find-subexprs
  :args (s/cat :expr ::expression
               :subexprs (s/coll-of ::expression :kind set?))
  :ret  (s/every ::expression
                 :kind sequential?))
(defapi core/find-subexprs
  "Finds all subexpressions in `expr` that match any element of the given set `subexprs`.")

(s/fdef core/find-vars
  :args (s/cat :expr ::expression
               :opts (s/keys :opt-un [:opts/ordered? ::vars]))
  :ret  ::varseq)
(defapi core/find-vars
  "Finds all variables in an expresson or returns the expression itself if it is a variable.

  Options:

  * {:ordered true} to return variables in: type order -> alphanumeric order
  * {:vars #{…}} can be given a set of specific variables to find")

(s/fdef core/gen-vars
  :args (s/cat :n nat-int?)
  :ret  ::varseq)
(defapi core/gen-vars
  "Generates a number of variables with random names.")

;; ? spec observe-[…]

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
  :args (s/cat :form ::form
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

(s/fdef core/cnt>
  :args (s/alt :ar1 (s/cat :x   ::expression)
               :ar2 (s/cat :x   ::expression
                           :env ::environment))
  :ret  ::expression)
(defapi core/cnt>
  "Simplifies a FORM content recursively until it cannot be further simplified.
  All deductions are justified by the axioms of FORM logic.

  * if `x` is a complex FORM, calls `simplify-context` on `x`
  * if no simplification applies, tries to retrieve the value from given `env`
  * if retrieval was unsuccessful, returns `x` as is")

(s/fdef core/ctx>
  :args (s/alt :ar1 (s/cat :ctx ::context)
               :ar2 (s/cat :ctx ::context
                           :env ::environment))
  :ret  ::context)
(defapi core/ctx>
  "Simplifies a FORM context recursively until it cannot be further simplified.
  All deductions are justified by the axioms of FORM logic.

  * for complex expressions, calls `simplify-content` on every unique element")


(s/fdef core/=>
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  :formform.calc/const?)
(defapi core/=>
  "Evaluates a FORM expression with an optional `env` and returns a Constant expression with attached metadata including the maximally reduced expression in `:expr` and the environment in `:env`.

  * `env` must be a map with a content/variable in `expr` as a key")

(s/fdef core/=>*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment)
               :ar3 (s/cat :opts (s/keys :opt-un [::varorder])
                           :expr ::expression
                           :env  ::environment))
  :ret  ::formDNA)
(defapi core/=>*
  "Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.

  * if `to-fdna?` is false, returns a seq of results as returned by `=>` in the order of the corresponding `vspace` ordering")

(s/def :evaluate/result (s/or :const :formform.calc/const
                              :expr  ::expression))

(s/fdef core/evaluate
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  (s/keys :req-un [:evaluate/result]))
(defapi core/evaluate)

(s/def :eval-all/results
  (s/and (s/coll-of
          (s/cat :interpretation (s/coll-of :formform.calc/const
                                            :kind sequential?)
                 :result         :formform.calc/const)
          :kind sequential?)
         :formform.calc/dna-count))

(s/fdef core/eval-all
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment)
               :ar3 (s/cat :opts (s/keys :opt-un [::varorder])
                           :expr ::expression
                           :env  ::environment))
  :ret  (s/keys :req-un [::varorder :eval-all/results]))
(defapi core/eval-all)

(s/fdef core/equal
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)
(defapi core/equal
  "Equality check for expressions. Two expressions are considered equal, if their formDNAs are equal. Compares formDNAs from evaluation results of each expression by calling `calc/equal-dna`.

  * ordering of variable names in formDNA matters, see `find-vars`
  * stricter than `equiv`, which compares by `calc/equiv-dna`")

(s/fdef core/equiv
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)
(defapi core/equiv
  "Equivalence check for expressions. Two expressions are considered equivalent, if their formDNAs are equivalent. Compares formDNAs from evaluation results of each expression by calling `calc/equiv-dna`.

  * ordering of variable names in formDNA is irrelevant
  * looser than `equal`, which compares by `calc/equal-dna`
  * can be slow on expressions with 6+ variables")


(s/fdef core/mark
  :args (s/cat :exprs (s/coll-of ::expression :kind sequential?))
  :ret  ::expression)

;; ? be more specific for :ret
(s/fdef core/mark-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked?])
               :exprs (s/* ::expression))
  :ret  ::expression)
(defapi core/mark-exprs
  "Chains expressions like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`

  * group expressions with arrangements: `[:- x y …]`")

(s/fdef formform.utils/nest-left
  :args (s/cat :items sequential?)
  :ret  (s/or :nil nil?
              :seq sequential?))

(s/fdef formform.utils/nest-right
  :args (s/cat :items sequential?)
  :ret  (s/or :nil nil?
              :seq sequential?))

(s/fdef core/nest-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked? :opts/ltr?])
               :exprs (s/* ::expression))
  :ret  ::expression)
(defapi core/nest-exprs
  "Nests expressions leftwards `(((…)a)b)` or rightwards `(a(b(…)))` if `{:ltr? true}`

  * use `nil` for empty expressions
  * use an arrangement `(make x y …)` to add multiple exprs. to the same level")

(s/fdef core/simplify-expr-chain
  :args (s/alt :ar2 (s/cat :chain ::expr-chain
                           :env   ::environment)
               :ar3 (s/cat :opts  (s/keys :opt-un [:opts/rtl?])
                           :chain ::expr-chain
                           :env   ::environment))
  :ret  ::expr-chain)
(defapi core/simplify-expr-chain
  "Reduces a sequence of expressions, intended to be linked in a `chain`, to a sequence of simplified expressions, possibly spliced or shortened via inference.

  * assumes rightward-nesting, e.g. `(…(…(…)))`
  * for leftward-nesting, e.g. `(((…)…)…)`, pass `{:rtl? true}`")


;;-------------------------------------------------------------------------
;; `operators` functions

(defapi ops/unclear?)

(s/fdef ops/construct-unclear
  :args (s/cat :op-k ::op-symbol
               :args (s/* any?))
  :ret  ::unclear)


(defapi ops/rem-pairs?)

(s/fdef ops/memory-replace
  :args (s/cat :mem        (s/spec ::memory)
               :repl-pairs (s/* ::rem-pair))
  :ret  ::memory)
(defapi ops/memory-replace)

(s/fdef ops/memory-extend
  :args (s/cat :mem       (s/spec ::memory)
               :ext-pairs (s/* ::rem-pair))
  :ret  ::memory)
(defapi ops/memory-extend)

(s/fdef ops/simplify-rems
  :args (s/cat :rems ::rems
               :env  ::environment)
  :ret  (s/tuple ::rems
                 ::environment))

(s/fdef ops/filter-rems
  :args (s/cat :rems ::rems
               :ctx  ::context)
  :ret  ::rems)

(s/fdef ops/simplify-memory
  :args (s/cat :mem (s/spec ::memory)
               :env ::environment)
  :ret  ::expression)

(s/fdef ops/memory
  :args (s/cat :rems  ::rems
               :exprs (s/* ::expression))
  :ret  ::memory)
(defapi ops/memory
  "Constructs a memory FORM.")

(defapi ops/seq-reentry-defaults)

;; should not be spec’d since its a map, not a function!
; (s/fdef ops/seq-reentry-sign->opts
;   :args (s/cat :sign ::seq-reentry-signature)
;   :ret  ::seq-reentry-opts)
(defapi ops/seq-reentry-sign->opts
  "Maps signatures for self-equivalent re-entry FORMs to their corresponding option-maps.")

(s/fdef ops/seq-reentry-opts->sign
  :args (s/cat :opt-map (s/keys :opt-un [:opts.seq-reentry/parity
                                         :opts.seq-reentry/open?
                                         :opts.seq-reentry/interpr]))
  :ret  ::seq-reentry-signature)
(defapi ops/seq-reentry-opts->sign
  "Inverse map of seq-reentry-sign->opts with default args.")

(defapi ops/seq-reentry-signature?)
(defapi ops/seq-reentry-opts?)

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

(s/fdef ops/seq-re
  :args (s/cat :specs        (s/or :sign ::seq-reentry-signature
                                   :opts
                                   (s/keys :opt-un [:opts.seq-reentry/parity
                                                    :opts.seq-reentry/open?
                                                    :opts.seq-reentry/interpr]))
               :nested-exprs (s/* ::expression))
  :ret  ::seq-reentry)
(defapi ops/seq-re
  "Constructs a self-equivalent re-entry FORM given the arguments:

  * `specs`: either a `seq-reentry-signature` or an options map
  * `nested-exprs`: zero or more expressions intended as a nested sequence")


(defapi ops/const->isolator)

(s/fdef ops/sel
  :args (s/alt :ar1 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.calc/const))
               :ar2 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.calc/const)
                           :simplify? boolean?))
  :ret  ::expression)
(defapi ops/sel)


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
  #{`symx/make-op
    `symx/valid-op?
    `symx/interpret-op
    `symx/simplify-op
    `symx/op-get
    `symx/op-data
    `symx/interpret-sym
    `symx/simplify-sym})

(def ^:no-doc fns-with-specs
  (remove exclude-from-instrumentation
          (utils/list-fn-specs "formform.expr")))


(comment

  (s/form (s/get-spec `symx/make-op))

  (s/form
   (s/get-spec
    (ffirst (for [[k v] (s/registry)
                 :when (and (symbol? k)
                            )]
             [k v]))))

  (s/conform ::expression []) ;=> [:form []]
  (s/conform ::expression :a) ;=> [:unknown-symbol :a]
  (s/conform ::expression :M) ;=> [:expr-symbol :M]
  (s/conform ::expression "x") ;=> [:variable [:str "x"]]
  (s/conform ::expression 'x) ;=> [:variable [:sym x]]
  (s/conform ::expression nil) ;=> [:empty nil]

  ;; ? should `::expression` spec `::operator` instead of `::generic-operator`
  (s/conform ::expression [:uncl "ä"]) ;=> [:operator {:tag :uncl, :label "ä"}]

  ;; ? should `::form` and/or `::context` designate their matches
  (s/conform ::expression [:foo [] []]) ;=> [:form [:foo [] []]]
  (s/conform ::expression [[:M] [:- 'a ['b]]]) ;=> [:form [[:M] [:- a [b]]]]

  (s/conform ::context [[:M] [:- 'a ['b]] 'x :U nil])
  ;=> [[:form [:M]] [:operator {:tag :-, :args-unchecked [a [b]]}] [:variable [:sym x]] [:expr-symbol :U] [:empty nil]]


(s/valid? ::operator (seq-re {} nil))
(seq-reentry-opts->sign {})
(seq-reentry-sign->opts :<r)
  )


