;; ========================================================================
;;     formform symbolic expression module
;;     -- created 03/2023, (c) Peter Hofmann
;; ========================================================================

(ns formform.expr.symexpr
  (:require
   [clojure.spec.alpha :as s]
   [formform.expr.common :refer [tag_arrangement]])
  #?(:cljs (:require-macros
            [formform.expr.symexpr :refer [defoperator defsymbol]])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defn methodname [k common-name] (symbol (str common-name "->" (name k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator methods

(defmulti make-op
  "Constructs a symbolic expression given a registered operator and parameters."
  (fn [op-k & _] {:pre [(keyword? op-k)]} op-k))

(defmulti valid-op?
  "Validates the shape of a symbolic expression with a registered operator."
  (fn [[op-k & _]] {:pre [(keyword? op-k)]} op-k))

(defmulti interpret-op
  "Interprets a symbolic expression with a registered operator."
  (fn [[op-k & _]] {:pre [(keyword? op-k)]} op-k))

(defmulti simplify-op
  "Simplifies a symbolic expression with a registered operator given an optional environment."
  (fn [[op-k & _] _] {:pre [(keyword? op-k)]} op-k))

(defmulti op-get
  "Gets a specified part from a symbolic expression with a registered operator."
  (fn [[op-k & _] param] {:pre [(keyword? op-k)
                                (keyword? param)]} [op-k param]))

(defmulti op-data
  "Gets all parameters from a symbolic expression with a registered operator as a map."
  (fn [[op-k & _]] {:pre [(keyword? op-k)]} op-k))


;; default methods

(declare op-symbol?)
(declare simplify-content)

;; default constructor
(defmethod make-op :default make-op->unknown
  [op-k & args]
  (if (op-symbol? op-k)
    (let [op (apply vector op-k args)]
      (if (op-k (methods valid-op?))
        (if (valid-op? op)
          op
          (throw (ex-info (str "Invalid operator arguments" op-k)
                          {:op op-k :args args})))
        op))
    (throw (ex-info (str "Unknown operator " op-k)
                    {:op op-k :args args}))))

(defmethod valid-op? :default valid-op?->unknown
  [[op-k & _ :as expr]]
  (throw (ex-info (str "Don’t know how to validate " op-k)
                  {:op op-k :expr expr})))

(defmethod interpret-op :default interpret-op->unknown
  [[op-k & _ :as expr]]
  (throw (ex-info (str "Don’t know how to interpret " op-k)
                  {:op op-k :expr expr})))

(defmethod simplify-op :default simplify-op->unknown
  [[op-k & _ :as expr] env]
  (if (op-symbol? op-k)
    ;; ? applicative order (eval args first) would be more efficient
    ;;   but how to know if all args are expressions?
    ;;   -> should leave that to dedicated simplifier
    (simplify-content (interpret-op expr) env)
    (throw (ex-info (str "Don’t know how to simplify " op-k)
                    {:op op-k :expr expr :env env}))))

(defmethod op-get :default op-get->unknown
  [[op-k & _ :as expr] param]
  (throw (ex-info (str "Unknown operator " op-k)
                  {:op op-k :expr expr :param param})))

(defmethod op-data :default op-data->unknown
  [[op-k & _ :as expr]]
  (throw (ex-info (str "Unknown operator " op-k)
                  {:op op-k :expr expr})))

(def op-symbol first)


(defmacro defoperator
  [k args interpretation & {:keys [constructor predicate reducer]}]
  (let [[params r] (if (= (get args (- (count args) 2)) '&)
                     [(take (- (count args) 2) args) (last args)]
                     [args nil])
        varargs?   (some? r)
        all-params (if varargs? (concat params [r]) params)
        methodname (partial methodname k)
        op-sym     (gensym "op")]
    `(do (defmethod interpret-op ~k ~(methodname "interpret-op")
           [[~'_ ~@args]] ~interpretation)
         (defmethod valid-op? ~k ~(methodname "valid-op?")
           [~op-sym] ~(if (some? predicate)
                        (list predicate op-sym)
                        ;; default predicate just checks arg count
                        `(and (sequential? ~op-sym)
                              (~(if varargs? '>= '==)
                               (count ~op-sym)
                               ~(inc (count params))))))
         ;; if no constructor specified, default `make-op` case applies
         ~(when (some? constructor)
            `(defmethod make-op ~k ~(methodname "make-op")
               [~op-sym & args#] (apply ~constructor ~op-sym args#)))
         ;; if no reducer specified, defaults to interpretation
         ~(when (some? reducer)
            `(defmethod simplify-op ~k ~(methodname "simplify-op")
               [~op-sym env#] (~reducer ~op-sym env#)))
         ;; define method to get all parameters with names
         (defmethod op-data ~k ~(methodname "op-data")
           [~op-sym] (zipmap ~(mapv (comp keyword str) all-params)
                             (if (> ~(count all-params) ~(count params))
                               (conj (subvec ~op-sym 1 ~(inc (count params)))
                                     (subvec ~op-sym ~(inc (count params))))
                               (subvec ~op-sym 1))))
         ;; define getter for each param by name
         ;; ? necessary
         (doseq [[i# param#] ~(vec (map-indexed
                                    (fn [i x] [i ((comp keyword str) x)])
                                    all-params))]
           (defmethod op-get [~k param#] ~(methodname "op-get")
             [~op-sym ~'_] (if (== i# ~(count params))
                             (into [] (subvec ~op-sym (inc i#)))
                             (~op-sym (inc i#))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol methods

(defmulti interpret-sym
  "Interprets a registered symbol."
  (fn [sym] {:pre [(keyword? sym)]} sym))

(defmulti simplify-sym
  "Simplifies a registered symbol given an optional environment."
  (fn [sym _] {:pre [(keyword? sym)]} sym))

(defmacro defsymbol
  [k interpretation & {:keys [reducer]}]
  (let [methodname (partial methodname k)]
    `(do (defmethod interpret-sym ~k ~(methodname "interpret-sym")
           [~'_] ~interpretation)
         ;; if no reducer specified, defaults to interpretation
         ~(when (some? reducer)
            `(defmethod simplify-sym ~k ~(methodname "simplify-sym")
               [sym# env#] (~reducer sym# env#))))))

;; default methods

(declare expr-symbol?)

(defmethod interpret-sym :default interpret-sym->unknown
  [sym-k]
  (throw (ex-info (str "Don’t know how to interpret " sym-k)
                  {:sym sym-k})))

(defmethod simplify-sym :default simplify-sym->unknown
  [sym-k env]
  (if (expr-symbol? sym-k)
    (interpret-sym sym-k) ;; defaults to interpretation
    (throw (ex-info (str "Don’t know how to simplify " sym-k)
                    {:sym sym-k :env env}))))


(defmulti op-spec first)
(defmethod op-spec :default [_] :formform.expr/operator)

;; !! unchecked
(def expr-symbol? #(and (keyword? %)
                        (% (methods interpret-sym))))
(def op-symbol? #(and (keyword? %)
                      (% (methods interpret-op))))
(def operator? #(and (sequential? %)
                     (op-symbol? (first %))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Operators

;; !! shallow predicate
(def arrangement? (partial s/valid? :formform.expr/arrangement))

(defmethod op-spec tag_arrangement [_] :formform.expr/arrangement)

(defoperator tag_arrangement [& exprs] (vector (into [] exprs))
  :predicate arrangement?
  ; :reducer (fn [[_ & exprs] env] (simplify-content [exprs] env))
  )

(defn arr-prepend [x rel-expr]
  (apply vector tag_arrangement x (rest rel-expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Symbols

(defsymbol :N nil)
(defsymbol :M [])
(defsymbol :U [:seq-re :<r nil nil] :reducer (fn [u _] u)) ;; ! not DRY
(defsymbol :I [:U])

(defsymbol :0 :N)
(defsymbol :1 :U)
(defsymbol :2 :I)
(defsymbol :3 :M)

(defsymbol :mn :U)

(def expr->const {nil :N
                  [] :M
                  [:seq-re :<r nil nil] :U
                  [[:seq-re :<r nil nil]] :I
                  [:U] :I})

(comment

  )
