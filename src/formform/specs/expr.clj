(ns formform.specs.expr
  (:require [formform.expr :as expr]
            [formform.specs.calc]
            [clojure.spec.alpha :as s]
            ; [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))


(s/def ::vars (s/coll-of ::variable))

(s/def :opts/ordered? boolean?)
(s/def :opts/unmarked? boolean?)
(s/def :opts/+meta? boolean?)
(s/def :opts/rtl? boolean?)
(s/def :opts/ltr? boolean?)

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
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  ::expression)

(s/fdef formform.expr/interpret*
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  ::expression)

;; ? check if :ret is subset of :subexprs
(s/fdef formform.expr/find-subexprs
  :args (s/cat :expr ::expression
               :subexprs (s/coll-of ::expression :kind set?))
  :ret  (s/every ::expression))

(s/fdef formform.expr/find-vars
  :args (s/cat :expr ::expression
               :opts (s/keys :opt-un [:opts/ordered? ::vars]))
  :ret  ::vars)

(s/fdef formform.expr/gen-vars
  :args (s/cat :n nat-int?)
  :ret  ::vars)

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


(s/fdef formform.expr/eval-expr
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :env  ::environment))
  :ret  (s/or :dna     :formform.specs.calc/dna
              :var-dna (s/tuple :formform.specs.calc/var-const)))

(s/def :opts.eval-all/to-fdna? boolean?)

(s/fdef formform.expr/eval-all
  :args (s/alt :ar1 (s/cat :expr ::expression)
               :ar2 (s/cat :expr ::expression
                           :opts (s/keys :opt-un [:opts.eval-all/to-fdna?
                                                  ::vars])))
  :ret  (s/or :formDNA ::formDNA
              :results (s/and (s/coll-of (s/tuple :formform.specs.calc/const))
                              :formform.specs.calc/dna-count)))

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

(s/fdef formform.expr/nest-left
  :args (s/cat :exprs (s/coll-of ::expression :kind sequential?))
  :ret  (s/or :nil  nil? ;; if :exprs = []
              :expr ::expression))

(s/fdef formform.expr/nest-right
  :args (s/cat :exprs (s/coll-of ::expression :kind sequential?))
  :ret  (s/or :nil  nil? ;; if :exprs = []
              :expr ::expression))

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
  :args (s/cat :rems (s/coll-of ::rem-pair)
               :env  ::environment)
  :ret  (s/tuple (s/coll-of ::rem-pair)
                 ::environment))

(s/fdef formform.expr/filter-rems
  :args (s/cat :rems (s/coll-of ::rem-pair)
               :ctx  ::context)
  :ret  (s/coll-of ::rem-pair))

(s/fdef formform.expr/simplify-memory
  :args (s/cat :mem (s/spec ::memory)
               :env ::environment)
  :ret  ::expression)

(s/fdef formform.expr/memory
  :args (s/cat :rems  (s/coll-of ::rem-pair)
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
               :ar3 (s/cat :op-k ::op-symbol
                           :vars ::vars
                           :dna  :formform.specs.calc/dna))
  :ret  ::formDNA)



(comment

  (s/conform ::seq-reentry
             [:seq-re :<r [nil]])

  (expr/simplify-expr-chain {} [nil] {})
  (s/conform ::expr-chain
             [])

  (s/conform (s/cat :op-k         ::op-symbol
                    :specs        (s/or :sign ::seq-reentry-signature
                                        :opts ::seq-reentry-opts)
                    :nested-exprs (s/* ::expression))
             [:seq-re :<r nil])

  (s/conform ::seq-reentry-signature
             :<..r)

  (s/conform (s/keys :req-un [:opts.seq-reentry/parity
                              :opts.seq-reentry/open?
                              :opts.seq-reentry/interpr])
             {:parity :even :open? false :interpr :rec-ident})

  (expr/memory [] {})
  (s/conform ::expression '())
  (s/conform (s/coll-of nil?)
             {})



  (s/conform (s/cat :mem        (s/spec ::memory)
                    :repl-pairs (s/* ::rem-pair))
             [[:mem [] nil] ['a :b] [:x []] ])

  (s/conform (s/cat :n (s/coll-of int?)
                    :s (s/* ::rem-pair))
             [[12] ['a 'b]])

  (s/conform ::memory [:mem [] nil])
  (s/conform (s/* ::rem-pair) [[:a :b]])

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



  (s/conform :formform.specs.calc/dna [:N :U :I :M])
  (s/conform (s/and (s/coll-of (s/tuple :formform.specs.calc/const))
                    :formform.specs.calc/dna-count)
             [[:N] [:U] [:I] [:M]])

  (set [1 2 3])

  (s/conform ::expression '(:x))
  (s/conform :opts/ordered? false)
  (s/conform ::vars #{:x})
  (s/conform ::variable :x)
  (s/conform (s/coll-of keyword?) #{:x})


  (defn foo-disp [[k & _]] k)
  (defmulti foo foo-disp)
  (defmethod foo :add [[k a b]] (+ a b))
  (defmethod foo :mul [[k a b]] (* a b))
  (defmethod foo :join [[k a b]] (str a b))

  (defmethod foo :div [[k a b]] (/ a b))

  (foo [:add 2 5])
  (foo [:mul 2 1])
  (foo [:div 10 2])
  (foo [:join 2 4])

  (s/fdef foo-disp
    :args (s/cat :op (s/spec
                      (s/cat :tag keyword?
                             :a int?
                             :b int?)))
    :ret  int?)

  (stest/instrument `foo-disp)


  )

(comment

  (s/conform (s/cat :op (s/spec ::generic-operator))
             [[:- 'a 'b]])

  (s/explain (s/cat :op ::generic-operator)
             [[:uncl "hello"]])

  (s/explain ::op-symbol [:uncl "hello"])

  (s/conform ::valid-operator
             [:seq-re :<r 'b])

  (s/conform ::valid-operator
             [:- 'x 'y])


  )

(comment
  
  (defmulti mspec :tag)

  (defmethod mspec :int [_] (s/keys :req-un [::tag ::i]))

  (s/conform (s/multi-spec mspec :tag)
             {:tag :int
              :i 12})



  (defmulti mspec2 first)

  (defmethod mspec2 :x [_] #(int? (second %)))
  (defmethod mspec2 :y [_] #(string? (second %)))

  (s/conform (s/multi-spec mspec2 first)
             [:y "12"])

  (mspec2 [:x 12])
  
  )

