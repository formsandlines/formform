(ns formform.expr
  "API for the `expr` module of `formform`."
  (:require
   [formform.expr.common
    :refer [tag_memory tag_formDNA tag_unclear tag_arrangement
            tag_seq-reentry]]
   [formform.expr.symexpr :as symx]
   [formform.expr.core :as core]
   [formform.expr.operators :as ops]
   #?(:clj  [formform.utils :as utils :refer [defapi]]
      :cljs [formform.utils :as utils :refer-macros [defapi]])
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [orchestra.spec.test :as stest]))


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
  (s/cat :tag   (partial = tag_arrangement)
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

(def form? (partial s/valid? ::form))
(def pure-form? (partial s/valid? ::pure-form))
(def variable? (partial s/valid? ::variable))
(def expression? (partial s/valid? ::expression))

(def struct-expr? #(or (form? %) (symx/operator? %)))
(def literal-expr? #(or (symx/expr-symbol? %) (variable? %)))

(s/def ::expr-chain
  (s/coll-of ::expression
             :kind sequential?
             :min-count 1))


(s/def ::unclear
  (s/cat :tag   (partial = tag_unclear)
         :label #(and (string? %) ((complement empty?) %))))


(s/def ::rem-pair
  (s/tuple #(s/valid? ::expression %)
           #(s/valid? ::expression %)))

(s/def ::rems
  (s/coll-of ::rem-pair
             :kind sequential?
             :into []))

(s/def ::memory
  (s/cat :tag  (partial = tag_memory)
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
  (s/cat :tag (partial = tag_seq-reentry)
         :sign ::seq-reentry-signature
         :nested-exprs (s/+ ::expression)))


(s/def ::formDNA
  (s/and (s/nonconforming
          (s/cat :tag      (partial = tag_formDNA)
                 :varorder ::varorder
                 :dna      :formform.calc/dna))
         #(== (count (second %)) (calc/dna-dimension (nth % 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator specs

(defmethod op-spec :default [_] ::operator)
(defmethod op-spec tag_arrangement [_] ::arrangement)
(defmethod op-spec tag_unclear [_] ::unclear)
(defmethod op-spec tag_memory [_] ::memory)
(defmethod op-spec tag_seq-reentry [_] ::seq-reentry)
(defmethod op-spec tag_formDNA [_] ::formDNA)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs & API

(s/def :opts/ordered? boolean?)
(s/def :opts/unmarked? boolean?)
(s/def :opts/+meta? boolean?)
(s/def :opts/rtl? boolean?)
(s/def :opts/ltr? boolean?)

(s/def ::vars (s/coll-of :formform.specs.expr/variable
                         :kind (complement map?)))

(s/def ::varseq ::varorder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multimethod specs - just for documentation
;; -> do not work with instrumentation

(s/fdef symx/make-op
  :args (s/cat :tag  ::op-symbol
               :args (s/* any?))
  :ret  ::operator)

(s/fdef symx/valid-op?
  :args (s/cat :operator (s/spec ::generic-operator))
  :ret  boolean?)

(s/fdef symx/interpret-op
  :args (s/cat :operator (s/spec ::operator))
  :ret  ::expression)

(s/fdef symx/simplify-op
  :args (s/cat :operator (s/spec ::operator)
               :env      ::environment)
  :ret  ::expression)

(s/fdef symx/op-get
  :args (s/cat :operator (s/spec ::operator)
               :param    keyword?)
  :ret  any?)

(s/fdef symx/op-data
  :args (s/cat :operator (s/spec ::operator))
  :ret  (s/map-of keyword? any?))

(s/fdef symx/interpret-sym
  :args (s/cat :expr-symbol ::expr-symbol)
  :ret  ::expression)

(s/fdef symx/simplify-sym
  :args (s/cat :expr-symbol ::expr-symbol
               :env         ::environment)
  :ret  ::expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs

(s/fdef core/splice-ctx
  :args (s/cat :ctx ::context)
  :ret  ::context)

(s/fdef core/make
  :args (s/* any?)
  :ret  ::expression)

(s/fdef core/form
  :args (s/* any?)
  :ret  ::form)

(s/fdef core/substitute-expr
  :args (s/cat :env  ::environment
               :expr ::expression)
  :ret  ::expression)

(s/fdef core/interpret
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

(s/fdef core/interpret*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

(s/fdef core/interpret-walk
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

(s/fdef core/interpret-walk*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

;; ? check if :ret is subset of :subexprs
(s/fdef core/find-subexprs
  :args (s/cat :expr ::expression
               :subexprs (s/coll-of ::expression :kind set?))
  :ret  (s/every ::expression
                 :kind sequential?))

(s/fdef core/find-vars
  :args (s/cat :expr ::expression
               :opts (s/keys :opt-un [:opts/ordered? ::vars]))
  :ret  ::varseq)

(s/fdef core/gen-vars
  :args (s/cat :n nat-int?)
  :ret  ::varseq)

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

(s/fdef core/ctx>
  :args (s/alt :ar1 (s/cat :ctx ::context)
               :ar2 (s/cat :ctx ::context
                           :env ::environment))
  :ret  ::context)


(s/fdef core/=>
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  :formform.calc/const?)

(s/fdef core/=>*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment)
               :ar3 (s/cat :opts (s/keys :opt-un [::varorder])
                           :expr ::expression
                           :env  ::environment))
  :ret  ::formDNA)

(s/def :evaluate/result (s/or :const :formform.calc/const
                              :expr  ::expression))

(s/fdef core/evaluate
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  (s/keys :req-un [:evaluate/result]))

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

(s/fdef core/equal
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)

(s/fdef core/equiv
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)


(s/fdef core/mark
  :args (s/cat :exprs (s/coll-of ::expression :kind sequential?))
  :ret  ::expression)

;; ? be more specific for :ret
(s/fdef core/mark-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked?])
               :exprs (s/* ::expression))
  :ret  ::expression)

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

(s/fdef core/simplify-expr-chain
  :args (s/alt :ar2 (s/cat :chain ::expr-chain
                           :env   ::environment)
               :ar3 (s/cat :opts  (s/keys :opt-un [:opts/rtl?])
                           :chain ::expr-chain
                           :env   ::environment))
  :ret  ::expr-chain)


(s/fdef ops/construct-unclear
  :args (s/cat :op-k ::op-symbol
               :args (s/* any?))
  :ret  ::unclear)


(s/fdef ops/memory-replace
  :args (s/cat :mem        (s/spec ::memory)
               :repl-pairs (s/* ::rem-pair))
  :ret  ::memory)

(s/fdef ops/memory-extend
  :args (s/cat :mem       (s/spec ::memory)
               :ext-pairs (s/* ::rem-pair))
  :ret  ::memory)

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


(s/fdef ops/seq-reentry-opts->sign
  :args (s/cat :opt-map (s/keys :opt-un [:opts.seq-reentry/parity
                                         :opts.seq-reentry/open?
                                         :opts.seq-reentry/interpr]))
  :ret  ::seq-reentry-signature)

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


(s/fdef ops/sel
  :args (s/alt :ar1 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.calc/const))
               :ar2 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.calc/const)
                           :simplify? boolean?))
  :ret  ::expression)


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



(comment
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

  )


