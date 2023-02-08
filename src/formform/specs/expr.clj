(ns formform.specs.expr
  (:require [formform.expr :as expr]
            [formform.specs.calc]
            [clojure.spec.alpha :as s]
            ; [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))


(s/def ::vars (s/coll-of ::variable))

(s/def :opts/ordered? boolean?)
(s/def :opts/unmarked? boolean?)
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
;; ? spec ctx->cnt
;; ? spec cnt->ctx

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
  :args (s/cat :x    ::expression
               :env  ::environment)
  :ret  ::expression)

;; ? spec simplify-by-calling
;; ? spec simplify-by-crossing
;; ? spec substitute-in-context

(s/fdef formform.expr/simplify-context
  :args (s/cat :ctx  ::context
               :env  ::environment)
  :ret  ::context)

;; ? spec simplify-env

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

;; ? spec chain

;; ? be more specific for :ret
(s/fdef formform.expr/mark-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked?])
               :exprs (s/* ::expression))
  :ret  ::expression)

;; ? spec nest-left
;; ? spec nest-right

(s/fdef formform.expr/nest-exprs
  :args (s/cat :opts  (s/keys :opt-un [:opts/unmarked? :opts/ltr?])
               :exprs (s/* ::expression))
  :ret  ::expression)

(s/fdef formform.expr/simplify-expr-chain
  :args (s/alt :ar2 (s/cat :chain ::expr-chain
                           :env   ::environment)
               :ar3 (s/cat :opts  (s/keys :opt-un [:opts/rtl?])
                           :exprs ::expr-chain
                           :env   ::environment))
  :ret  ::expr-chain)




(comment
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

