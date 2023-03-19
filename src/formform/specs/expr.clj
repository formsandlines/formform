(ns formform.specs.expr
  (:require [formform.expr :as expr]
            [formform.specs.calc]
            [clojure.spec.alpha :as s]
            ; [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))


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

(s/fdef formform.expr/make-op
  :args (s/cat :tag  ::op-symbol
               :args (s/* any?))
  :ret  ::operator)

(s/fdef formform.expr/valid-op?
  :args (s/cat :operator (s/spec ::generic-operator))
  :ret  boolean?)

(s/fdef formform.expr/interpret-op
  :args (s/cat :operator (s/spec ::operator))
  :ret  ::expression)

(s/fdef formform.expr/simplify-op
  :args (s/cat :operator (s/spec ::operator)
               :env      ::environment)
  :ret  ::expression)

(s/fdef formform.expr/op-get
  :args (s/cat :operator (s/spec ::operator)
               :param    keyword?)
  :ret  any?)

(s/fdef formform.expr/op-data
  :args (s/cat :operator (s/spec ::operator))
  :ret  (s/map-of keyword? any?))

(s/fdef formform.expr/interpret-sym
  :args (s/cat :expr-symbol ::expr-symbol)
  :ret  ::expression)

(s/fdef formform.expr/simplify-sym
  :args (s/cat :expr-symbol ::expr-symbol
               :env         ::environment)
  :ret  ::expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs

(s/fdef formform.expr/splice-ctx
  :args (s/cat :ctx ::context)
  :ret  ::context)

(s/fdef formform.expr/make
  :args (s/* any?)
  :ret  ::expression)

(s/fdef formform.expr/form
  :args (s/* any?)
  :ret  ::form)

(s/fdef formform.expr/substitute-expr
  :args (s/cat :env  ::environment
               :expr ::expression)
  :ret  ::expression)

(s/fdef formform.expr/interpret
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

(s/fdef formform.expr/interpret*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

(s/fdef formform.expr/interpret-walk
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

(s/fdef formform.expr/interpret-walk*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :env  ::environment
                           :expr ::expression))
  :ret  ::expression)

;; ? check if :ret is subset of :subexprs
(s/fdef formform.expr/find-subexprs
  :args (s/cat :expr ::expression
               :subexprs (s/coll-of ::expression :kind set?))
  :ret  (s/every ::expression
                 :kind sequential?))

(s/fdef formform.expr/find-vars
  :args (s/cat :expr ::expression
               :opts (s/keys :opt-un [:opts/ordered? ::vars]))
  :ret  ::varseq)

(s/fdef formform.expr/gen-vars
  :args (s/cat :n nat-int?)
  :ret  ::varseq)

;; ? spec observe-[…]

(s/fdef formform.expr/ctx->cnt
  :args (s/cat :opts (s/keys :opt-un [:opts/+meta?])
               :ctx  ::context)
  :ret  (s/or :expr+meta (s/tuple ::expression
                                  ::environment)
              :expr ::expression))

(s/fdef formform.expr/cnt->ctx
  :args (s/cat :cnt ::expression)
  :ret  ::context)

(s/fdef formform.expr/simplify-matching-content
  :args (s/alt :ar1 (s/cat :x ::expression)
               :ar2 (s/cat :x ::expression
                           :default any?))
  :ret  ::expression)

(s/fdef formform.expr/simplify-form
  :args (s/cat :form ::form
               :env  ::environment)
  :ret  ::expression)

;; ? is content = expression
(s/fdef formform.expr/simplify-content
  :args (s/cat :x   ::expression
               :env ::environment)
  :ret  ::expression)

(s/fdef formform.expr/simplify-by-calling
  :args (s/cat :ctx ::context
               :env ::environment)
  :ret  (s/tuple ::context
                 ::environment))

(s/fdef formform.expr/simplify-by-crossing
  :args (s/cat :ctx ::context)
  :ret  ::context)

(s/fdef formform.expr/substitute-in-context
  :args (s/cat :env ::environment
               :ctx ::context)
  :ret  ::context)

(s/fdef formform.expr/simplify-context
  :args (s/cat :ctx ::context
               :env ::environment)
  :ret  ::context)

(s/fdef formform.expr/simplify-env
  :args (s/cat :env ::environment)
  :ret  ::environment)

(s/fdef formform.expr/cnt>
  :args (s/alt :ar1 (s/cat :x   ::expression)
               :ar2 (s/cat :x   ::expression
                           :env ::environment))
  :ret  ::expression)

(s/fdef formform.expr/ctx>
  :args (s/alt :ar1 (s/cat :ctx ::context)
               :ar2 (s/cat :ctx ::context
                           :env ::environment))
  :ret  ::context)


(s/fdef formform.expr/=>
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  :formform.specs.calc/const?)

(s/fdef formform.expr/=>*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment)
               :ar3 (s/cat :opts (s/keys :opt-un [::varorder])
                           :expr ::expression
                           :env  ::environment))
  :ret  ::formDNA)

(s/def :evaluate/result (s/or :const :formform.specs.calc/const
                              :expr  ::expression))

(s/fdef formform.expr/evaluate
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  (s/keys :req-un [:evaluate/result]))

(s/def :eval-all/results
  (s/and (s/coll-of
          (s/cat :interpretation (s/coll-of :formform.specs.calc/const
                                            :kind sequential?)
                 :result         :formform.specs.calc/const)
          :kind sequential?)
         :formform.specs.calc/dna-count))

(s/fdef formform.expr/eval-all
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment)
               :ar3 (s/cat :opts (s/keys :opt-un [::varorder])
                           :expr ::expression
                           :env  ::environment))
  :ret  (s/keys :req-un [::varorder :eval-all/results]))

(s/fdef formform.expr/equal
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)

(s/fdef formform.expr/equiv
  :args (s/cat :exprs (s/* ::expression))
  :ret  boolean?)


(s/fdef formform.expr/mark
  :args (s/cat :exprs (s/coll-of ::expression :kind sequential?))
  :ret  ::expression)

;; ? be more specific for :ret
(s/fdef formform.expr/mark-exprs
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

(s/fdef formform.expr/nest-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked? :opts/ltr?])
               :exprs (s/* ::expression))
  :ret  ::expression)

(s/fdef formform.expr/simplify-expr-chain
  :args (s/alt :ar2 (s/cat :chain ::expr-chain
                           :env   ::environment)
               :ar3 (s/cat :opts  (s/keys :opt-un [:opts/rtl?])
                           :chain ::expr-chain
                           :env   ::environment))
  :ret  ::expr-chain)


(s/fdef formform.expr/construct-unclear
  :args (s/cat :op-k ::op-symbol
               :args (s/* any?))
  :ret  ::unclear)


(s/fdef formform.expr/memory-replace
  :args (s/cat :mem        (s/spec ::memory)
               :repl-pairs (s/* ::rem-pair))
  :ret  ::memory)

(s/fdef formform.expr/memory-extend
  :args (s/cat :mem       (s/spec ::memory)
               :ext-pairs (s/* ::rem-pair))
  :ret  ::memory)

(s/fdef formform.expr/simplify-rems
  :args (s/cat :rems ::rems
               :env  ::environment)
  :ret  (s/tuple ::rems
                 ::environment))

(s/fdef formform.expr/filter-rems
  :args (s/cat :rems ::rems
               :ctx  ::context)
  :ret  ::rems)

(s/fdef formform.expr/simplify-memory
  :args (s/cat :mem (s/spec ::memory)
               :env ::environment)
  :ret  ::expression)

(s/fdef formform.expr/memory
  :args (s/cat :rems  ::rems
               :exprs (s/* ::expression))
  :ret  ::memory)


(s/fdef formform.expr/seq-reentry-opts->sign
  :args (s/cat :opt-map (s/keys :opt-un [:opts.seq-reentry/parity
                                         :opts.seq-reentry/open?
                                         :opts.seq-reentry/interpr]))
  :ret  ::seq-reentry-signature)

(s/fdef formform.expr/simplify-seq-reentry
  :args (s/cat :seq-re (s/spec ::seq-reentry)
               :env    ::environment)
  :ret  ::expression)

(s/fdef formform.expr/construct-seq-reentry
  :args (s/cat :op-k         ::op-symbol
               :specs        (s/or :sign ::seq-reentry-signature
                                   :opts
                                   (s/keys :opt-un [:opts.seq-reentry/parity
                                                    :opts.seq-reentry/open?
                                                    :opts.seq-reentry/interpr]))
               :nested-exprs (s/* ::expression))
  :ret  ::seq-reentry)

(s/fdef formform.expr/seq-re
  :args (s/cat :specs        (s/or :sign ::seq-reentry-signature
                                   :opts
                                   (s/keys :opt-un [:opts.seq-reentry/parity
                                                    :opts.seq-reentry/open?
                                                    :opts.seq-reentry/interpr]))
               :nested-exprs (s/* ::expression))
  :ret  ::seq-reentry)


(s/fdef formform.expr/sel
  :args (s/alt :ar1 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.specs.calc/const))
               :ar2 (s/cat :vars->consts (s/map-of ::variable
                                                   :formform.specs.calc/const)
                           :simplify? boolean?))
  :ret  ::expression)


(s/fdef formform.expr/filter-formDNA
  :args (s/cat :fdna ::formDNA
               :env  ::environment)
  :ret  ::formDNA)

(s/fdef formform.expr/simplify-formDNA
  :args (s/cat :operator (s/spec ::formDNA)
               :env      ::environment)
  :ret  (s/or :fdna  ::formDNA
              :const :formform.specs.calc/const))

(s/fdef formform.expr/construct-formDNA
  :args (s/alt :ar1 (s/cat :op-k ::op-symbol)
               :ar2 (s/cat :op-k ::op-symbol
                           :dna  :formform.specs.calc/dna)
               :ar3 (s/cat :op-k     ::op-symbol
                           :varorder ::varseq
                           :dna      :formform.specs.calc/dna))
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



