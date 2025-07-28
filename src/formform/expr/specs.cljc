(ns formform.expr.specs
  (:require [formform.calc.specs :as calc-sp]
            [formform.calc :as calc]
            [formform.expr.common :as common]
            [formform.expr.core :as core]
            [formform.expr.symexpr :as symx]
            [formform.expr.operators :as ops]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]))


;;-------------------------------------------------------------------------
;; expression

;; ! check for predicates in input validation -> performance
(s/def ::expression
  (s/or :empty          nil?
        :operator       ::generic-operator
        :form           ::struct-expr
        :expr-symbol    ::expr-symbol
        :variable       ::variable
        :unknown-symbol keyword?))

;; structural expression → form the expression as a sequence of expressions
(s/def ::struct-expr
  (s/every ::expression
           :kind sequential?))

;; FORM expression → structural expression that is not an operator
(s/def ::form-expr
  (s/and ::struct-expr
         #((complement s/valid?) ::generic-operator %)))

(s/def ::variable (s/or :str string? :sym symbol?))

(s/def ::vars (s/coll-of ::variable
                         :kind (complement map?)))

(s/def ::varorder (s/coll-of ::variable
                             :kind sequential?))

(s/def ::varseq ::varorder)

;; expression chain → ordered sequence of expressions
(s/def ::expr-chain
  (s/coll-of ::expression
             ;; :min-count 1  ; ? verify if empty seq can be equiv to ( nil )
             :kind sequential?))

;; nesting chain → interpret the expression chain as nested expressions
;; * rightwards: `( a b c … )` → `( a [b [c […]]] )`
;; * leftwards:  `( … x y z )` → `( [[[…] x] y] z )`
(s/def ::nesting-chain-l ::expr-chain)
(s/def ::nesting-chain-r ::expr-chain)

;; context → unordered sequence of expressions
(s/def ::context
  (s/coll-of ::expression
             :kind sequential?))

(s/def ::environment map?)


;;-------------------------------------------------------------------------
;; symbolic expression

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

(s/def ::operator (s/multi-spec op-spec symx/op-symbol))

(defmethod op-spec common/tag_arrangement [_] ::arrangement)
(defmethod op-spec common/tag_unclear [_] ::unclear)
(defmethod op-spec common/tag_seq-reentry [_] ::seq-reentry)
(defmethod op-spec common/tag_memory [_] ::memory)
(defmethod op-spec common/tag_formDNA [_] ::formDNA)

;;-------------------------------------------------------------------------
;; arrangement

(s/def ::arrangement
  (s/cat :tag   (partial = common/tag_arrangement)
         :exprs (s/* #(s/valid? ::expression %))))

;;-------------------------------------------------------------------------
;; unclear FORM

(s/def ::unclear
  (s/cat :tag   (partial = common/tag_unclear)
         :label #(and (string? %) ((complement empty?) %))))

;;-------------------------------------------------------------------------
;; seq-reentry FORM

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

;;-------------------------------------------------------------------------
;; memory FORM

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

;;-------------------------------------------------------------------------
;; formDNA

(s/def ::formDNA
  (s/and (s/nonconforming
          (s/cat :tag      (partial = common/tag_formDNA)
                 :varorder ::varorder
                 :dna      ::calc-sp/dna))
         #(== (count (second %)) (calc/dna-dimension (nth % 2)))))


;;-------------------------------------------------------------------------
;; function specs (impl)


(s/fdef core/splice-ctx
  :args (s/cat :ctx ::context)
  :ret  ::context)
;; (defn splice-ctx
;;   "Dissolves arrangements in given context such that their elements become direct children of the context itself."
;;   [ctx]
;;   (core/splice-ctx ctx))

(s/fdef core/substitute-expr
  :args (s/cat :env  ::environment
               :expr ::expression)
  :ret  ::expression)
;; (defn substitute-expr
;;   "Substitutes an expression by a matching expression in given environment. Returns the original expression if match failed.")

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
              :const ::calc-sp/const))

(s/fdef ops/construct-formDNA
  :args (s/alt :ar1 (s/cat :op-k ::op-symbol)
               :ar2 (s/cat :op-k ::op-symbol
                           :dna  ::calc-sp/dna)
               :ar3 (s/cat :op-k     ::op-symbol
                           :varorder ::varseq
                           :dna      ::calc-sp/dna))
  :ret  ::formDNA)


(def ^:no-doc fns-with-specs (utils/list-fn-specs "formform.expr"))

