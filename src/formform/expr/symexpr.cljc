;; ========================================================================
;;     formform symbolic expression module
;;     -- created 03/2023, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.expr.symexpr
  (:require
   [formform.expr.common :refer [tag_arrangement]])
  #?(:cljs (:require-macros
            [formform.expr.symexpr :refer [defoperator defsymbol]])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defn methodname [k common-name] (symbol (str common-name "->" (name k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator methods

(defmulti make-op
  (fn [op-k & _] {:pre [(keyword? op-k)]} op-k))

(defmulti valid-op?
  (fn [[op-k & _]] {:pre [(keyword? op-k)]} op-k))

(defmulti interpret-op
  (fn [[op-k & _]] {:pre [(keyword? op-k)]} op-k))

(defmulti simplify-op
  (fn [[op-k & _] _] {:pre [(keyword? op-k)]} op-k))

(defmulti op-get
  (fn [[op-k & _] param] {:pre [(keyword? op-k)
                               (keyword? param)]} [op-k param]))

(defmulti op-data
  (fn [[op-k & _]] {:pre [(keyword? op-k)]} op-k))


;; default methods

(declare op-symbol?)

;; default constructor
(defmethod make-op :default make-op->unknown
  [op-k & args]
  (if (op-symbol? op-k)
    (let [op (apply vector op-k args)]
      (if (op-k (methods valid-op?))
        (if (valid-op? op)
          op
          (throw (ex-info (str "Invalid operator arguments: " op-k)
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

;; default method for `simplify-op` is in expr.core
;; this is due to its dependency on `core/simplify-content`

(defmethod op-get :default op-get->unknown
  [[op-k & _ :as expr] param]
  (throw (ex-info (str "Unknown operator " op-k)
                  {:op op-k :expr expr :param param})))

(defmethod op-data :default op-data->unknown
  [[op-k & _ :as expr]]
  (throw (ex-info (str "Unknown operator " op-k)
                  {:op op-k :expr expr})))

(def op-symbol first)


(defn defoperator-impl
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

(defmacro defoperator
  [k args interpretation & params]
  (apply defoperator-impl k args interpretation params))


(comment
  ;; redesign wip
  #_
  (defoperator :mem [rems ctx]
    "MemoryFORM."
    (let [eqs (apply make
                     (map (fn [[k v]] (form (form k v)
                                            (form (form k) (form v))))
                          rems))]
      (form eqs (apply form ctx)))

    (valid-op?
     [_]
     (and (symx/operator? %)
          (= tag_memory (op-symbol %))
          (rem-pairs? (second %))))

    (simplify-op
     [_ env]
     (let [[rems env] (simplify-rems (op-get mem :rems) env)
           ctx  (core/simplify-context (op-get mem :ctx) env)
           rems (filter-rems rems ctx)]
       (if (empty? rems)
         (apply make ctx)
         (apply make-op tag_memory rems ctx))))
    )
  ,)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol methods

(defmulti interpret-sym
  (fn [sym] {:pre [(keyword? sym)]} sym))

(defmulti simplify-sym
  (fn [sym _] {:pre [(keyword? sym)]} sym))

(defn defsymbol-impl
  [k interpretation & {:keys [reducer]}]
  (let [methodname (partial methodname k)]
    `(do (defmethod interpret-sym ~k ~(methodname "interpret-sym")
           [~'_] ~interpretation)
         ;; if no reducer specified, defaults to interpretation
         ~(when (some? reducer)
            `(defmethod simplify-sym ~k ~(methodname "simplify-sym")
               [sym# env#] (~reducer sym# env#))))))

(defmacro defsymbol
  [k interpretation & params]
  (apply defsymbol-impl k interpretation params))

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


;; !! unchecked
(def expr-symbol? #(and (keyword? %)
                        (% (methods interpret-sym))))
(def op-symbol? #(and (keyword? %)
                      (% (methods interpret-op))))
(def operator? #(and (sequential? %)
                     (op-symbol? (first %))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Operators

;; !! unchecked & shallow predicate
(def arrangement? #(and (operator? %)
                        (= tag_arrangement (op-symbol %))))

(defoperator tag_arrangement [& exprs] (vector (into [] exprs))
  :predicate arrangement?)
;; :reducer (fn [[_ & exprs] env] (simplify-content [exprs] env))


(defn arr-prepend [x rel-expr]
  (apply vector tag_arrangement x (rest rel-expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Symbols

(def void  nil)
(def mark  [])
(def uform [:seq-re :<r nil nil])

(defsymbol :n void)
(defsymbol :m mark)
(defsymbol :u uform :reducer (fn [u _] u))
(defsymbol :i [:u])

(defsymbol :0 :n)
(defsymbol :1 :u)
(defsymbol :2 :i)
(defsymbol :3 :m)

(defsymbol :mn :u)

;; simplest possible expression -> constant
(def expr->const {void    :n
                  mark    :m
                  uform   :u
                  [uform] :i
                  [:u]    :i

                  :n :n
                  :m :m
                  :u :u
                  :i :i})


