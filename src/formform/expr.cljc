(ns formform.expr
  (:require
   [clojure.set :as set]
   [formform.calc :as calc]
   [formform.utils :as utils]
   #?(:clj  [clojure.core.match :refer [match]]
      :cljs [cljs.core.match :refer-macros [match]])
   [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros
            [formform.expr :refer [defoperator defsymbol]])))

;; ========================================================================
;;     formform expression module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================


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
(declare interpret)

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
    (interpret expr env) ;; defaults to interpretation
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression types

;; ? is this general notion confusing
(s/def :formform.specs.expr/form sequential?)

(s/def :formform.specs.expr/expr-symbol
  (s/and keyword? #(% (methods interpret-sym))))

(s/def :formform.specs.expr/op-symbol
  (s/and keyword? #(% (methods interpret-op))))

(s/def :formform.specs.expr/operator
  (s/and sequential?
         #(s/valid? :formform.specs.expr/op-symbol (op-symbol %))))

(s/def :formform.specs.expr/pure-form
  (s/and (partial s/valid? :formform.specs.expr/form)
         (complement (partial s/valid? :formform.specs.expr/operator))))

(s/def :formform.specs.expr/variable (s/or :str string? :sym symbol?))

(s/def :formform.specs.expr/expression
  (s/or :form (partial s/valid? :formform.specs.expr/form)
        :empty nil?
        :symbolic-expr keyword?
        ; :expr-symbol (partial s/valid? :formform.specs.expr/expr-symbol)
        ; :operator (partial s/valid? :formform.specs.expr/operator)
        :variable (partial s/valid? :formform.specs.expr/variable)))

(def form? (partial s/valid? :formform.specs.expr/form))
(def expr-symbol? (partial s/valid? :formform.specs.expr/expr-symbol))
(def op-symbol? (partial s/valid? :formform.specs.expr/op-symbol))
(def operator? (partial s/valid? :formform.specs.expr/operator))
(def pure-form? (partial s/valid? :formform.specs.expr/pure-form))
(def variable? (partial s/valid? :formform.specs.expr/variable))
(def expression? (partial s/valid? :formform.specs.expr/expression))

(def struct-expr? #(or (form? %) (operator? %)))
(def literal-expr? #(or (expr-symbol? %) (variable? %)))

(def expr->const {nil :N, [] :M, [:U] :I})

(declare arrangement?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Operations

(defn- splice-ctx
  "Dissolves arrangements in given context such that their elements become direct children of the context itself."
  [ctx]
  (reduce (fn [acc x] (cond
                        (nil? x) acc
                        (arrangement? x) (into acc (rest x))
                        :else (conj acc x)))
          [] ctx))

(defn make
  "Constructor for expressions of any kind. Validates its input.
  - if the first argument (or the first after the options map) is a keyword of a registered operator, will call the constructor for that operator
  - can be given an options map as first argument:
    - `mark?` (default: false) marks the whole expression, creating a FORM
    - `splice?` (default: true) dissolves all top-level arrangements"
  [& args]
  (let [[{:keys [mark? splice?] :or {mark? false splice? true}}
         [x & r]] (if (map? (first args))
                    [(first args) (rest args)]
                    [{} args])]
    (if (operator? [x])
      (let [op (apply make-op x r)]
        (if mark? [op] op))
      (let [exprs (if splice?
                    (splice-ctx (cons x r))
                    (cons x r))]
        (if mark?
          (if (every? expression? exprs)
            (vec exprs)
            (throw (ex-info "Contains invalid expressions." {:exprs exprs})))
          (condp = (count exprs)
            0 nil
            1 (let [expr (first exprs)]
                (if (expression? expr)
                  expr
                  (throw (ex-info "Invalid expression." {:expr expr}))))
            (apply vector :- exprs)))))))

(defn form
  "Constructor for FORM expressions. Calls `make` on arguments."
  [& args]
  (let [[opts ctx] (if (map? (first args))
                     [(first args) (rest args)]
                     [{} args])]
    (apply make (merge opts {:mark? true}) ctx)))

(defn- substitute-expr
  "Substitutes an expression by a matching expression in given environment. Returns the original expression if match failed."
  [env expr]
  (let [x (get env expr :not-found)]
    (if (= x :not-found) expr x)))

;; ? should registered keywords be substitutable by env
(defn interpret
  "Interprets expressions of any kind. Unless defined otherwise, contents are only interpreted for simple FORMs."
  ([expr] (interpret expr {}))
  ([expr env]
   (let [expr (substitute-expr env expr)]
     (cond
       (operator? expr)    (interpret-op expr)
       (form? expr)        (into (empty expr)
                                 (remove nil? (map #(interpret % env) expr)))
       (expr-symbol? expr) (interpret-sym expr)
       :else expr))))

(defn interpret*
  "Recursively calls `interpret` on given expression and its subexpressions down to the deepest nesting, until there is nothing left to interpret."
  ([expr] (interpret* expr {}))
  ([expr env]
   (loop [expr expr
          i    0]
     (let [expr' (interpret expr env)]
       (cond
         (= expr' expr) expr'
         (> i 400)      (throw (ex-info "Too many interpretation attempts! Possibly caused by co-dependent interpretations or mutually recursive associations in the env."
                                        {:type :infinite-substitution}))
         :else (recur expr' (inc i)))))))

(defn find-vars
  "Finds all variables in an expresson or returns the expression itself if it is a variable.
  
  Options:
  - {:ordered true} to return variables in type order > alphanumeric order
  - {:vars #{…}} can be given a set of specific variables to find"
  [expr {:keys [ordered? vars] :or {ordered? false}}]
  (if (sequential? expr)
    (let [children (fn [x] (filter #(or (struct-expr? %) (if (nil? vars)
                                                           (variable? %)
                                                           (vars %)))
                                   x))
          vs (->> (seq expr)
                  (tree-seq sequential? children)
                  rest
                  (filter (complement sequential?))
                  distinct)]
      (if ordered?
        (sort utils/compare-names vs)
        vs))
    (if (variable? expr)
      (list expr)
      '())))

(defn gen-vars
  [n]
  (map #(str "v__" %) (range n)))

;; Observations are accumulated contents in reduction

(defn- observed-put [env x]
  (update env :observed #(if (nil? %) #{x} (conj % x))))

(defn- observed-disj [env x]
  (update env :observed #(if (nil? %) % (disj % x))))

(defn- observed-merge [env xs]
  (update env :observed #(if (nil? %) (set xs) (set/union % (set xs)))))

(defn- observed-get [env]
  (get env :observed))

(defn- observed-removeall [env]
  (dissoc env :observed))

(defn- observed-has? [env x]
  (not= (get-in env [:observed x] :not-found) :not-found))

(defn- ctx->cnt [{:keys [+meta?] :or {+meta? false}} ctx]
  (let [expr (condp == (count ctx)
               0 nil
               1 (first ctx)
               (apply make ctx))]
    (if +meta? [expr (meta ctx)] expr)))

(defn- cnt->ctx [cnt]
  (cond
    (arrangement? cnt) (rest cnt)
    (and (form? cnt)
         (== 1 (count cnt))
         (form? (first cnt))) (vec (first cnt))
    :else [cnt]))

(defn simplify-matching-content
  "Tries to reduce given content `x` to its simplest FORM if it matches a simple equivalent representation."
  ([x] (simplify-matching-content x x))
  ([x default]
   (case x ;; [] = '() in cases! (cljs doesn’t like ((…)) in cases)
     ([] :M [nil] [:N] [[[]]] [[:M]] [[[nil]]] [[[:N]]]
         [[:U :I]] [[:I :U]])
     []
     (nil :N [[]] [:M] [[nil]] [[:N]]
          [:U :I] [:I :U])
     nil
     (:U [:I] [[:U]] [[[:I]]])
     :U
     (:I [:U] [[:I]] [[[:U]]])
     [:U]
     default)))

(declare simplify-context)

;; ? metadata consistent after simplify-matching-content
(defn simplify-form
  [form env]
  (let [form (simplify-context form env)
        expr (simplify-matching-content
              (if (and (== 1 (count form))
                       (pure-form? (first form)) )
                (case (count (first form))
                  0 nil           ;; (()) = nil
                  1 (ffirst form) ;; ((x)) = x
                  form)
                form))]
    (if (struct-expr? expr)
      (vary-meta expr #(merge % (meta form)))
      expr)))

(defn simplify-content
  [x env]
  (let [res (simplify-matching-content x :failed)]
    (if (not= :failed res)
      res
      (cond
        (operator? x)    (if ((op-symbol x) (methods simplify-op))
                           (simplify-op x env)
                           (simplify-content (interpret x env) env))
        (form? x)        (simplify-form x env)
        (expr-symbol? x) (interpret-sym x)
        :else (substitute-expr env x)))))

;; ? kinda messy - how to make this more systematic
(defn- simplify-by-calling
  "Tries to reduce given context to `()` if it contains the equivalent of a `MARK` or both `:U` and `:I` are present in the context and/or given `env`.
  - remembers any occurrence of `:U` or `:I` in the `env`."
  [ctx env]
  ;; find mark in the expression
  (if (some #{[] :M [nil] [:N] [[[]]] [[:M]] [[[nil]]] [[[:N]]]}
            ctx)
    [[[]] env]
    ;; else find and remember :U in the expression
    ;; match with :I if seen
    (let [[seenU? seenI?] [(observed-has? env :U) (observed-has? env :I)]
          [seenU? env-next]
          (if (and (not seenU?)
                   (some #{:U [:I] [[:U]] [[[:I]]]} ctx))
            [true (observed-put env :U)]
            [seenU? env])]
      (if (and seenU? seenI?)
        [[[]] env-next]
        ;; else find and remember :I in the expression
        ;; match with :U if seen
        (let [[seenI? env-next]
              (if (and (not seenI?)
                       (some #{:I [:U] [[:I]] [[[:U]]]} ctx))
                [true (observed-put env-next :I)]
                [seenI? env-next])]
          (if (and seenU? seenI?)
            [[[]] env-next]
            ;; else return the input expression with distinct elements
            [(let [ctx (distinct ctx)
                   obs (observed-get env)]
               (if (nil? obs)
                 ctx
                 (remove #(and (obs %) (not (env %))) ctx)))
             env-next]))))))

(defn- simplify-by-crossing
  "Tries to reduce some `((x …))` to `x …` for each FORM in given context."
  [ctx]
  (let [f (fn [ctx x]
            (if (pure-form? x)
              (let [[x' & r] x]
                (if (and (pure-form? x') (empty? r))
                  (into [] (concat ctx x'))
                  (conj ctx x)))
              (conj ctx x)))]
    (reduce f [] ctx)))

(defn- substitute-in-context
  [env ctx]
  (loop [ctx  ctx
         i    0]
    (let [ctx' (map (partial substitute-expr env) ctx)]
      (cond
        (= ctx' ctx) (splice-ctx ctx')
        (> i 400)    (throw (ex-info "Too many substitutions! Possibly caused by two mutually recursive associations in the env."
                                     {:type :infinite-substitution}))
        :else (recur ctx' (inc i))))))

;; ? add optim option to not observe values for (de)generation
;; ? add option for assumption mn ≠ mn to prevent relation of U/I
(defn simplify-context
  [ctx env]
  (let [env (update env :depth #(if (nil? %) 0 (inc %)))]
    (if (< (:depth env) 400)
      ;; substitute matches from env upfront
      (loop [ctx (substitute-in-context env ctx)
             i   0]
        (let [[ctx env-next] (simplify-by-calling ctx env)]
          (if (or (= ctx [[]]) (= ctx []))
            (vary-meta ctx #(assoc % :env-next env-next))
            (let [env-next (observed-merge env-next ctx)
                  ctx' (->> (map #(simplify-content
                                   % (observed-disj env-next %))
                                 ctx)
                            simplify-by-crossing
                            (remove nil?))]
              (cond
                (= ctx' ctx) (vary-meta ctx' #(assoc % :env-next env-next))
                (> i 3)      (throw (ex-info "Too many reduction attempts!"
                                             {:type :infinite-reduction}))
                :else (recur ctx' (inc i)))))))
      (throw (ex-info "Context too deeply nested, possibly caused by a self-contradicting re-entry definition." {:type :stack-overflow})))))

(defn- simplify-env
  [env]
  (update-vals env #(simplify-content % {})))

;; ? should env be reduced completely before substitution?
(defn cnt>
  "Simplifies a FORM content recursively until it cannot be further simplified.
  All deductions are justified by the axioms of FORM logic.
  - if `x` is a complex FORM, calls `simplify-context` on `x`
  - if no simplification applies, tries to retrieve the value from given `env`
  - if retrieval was unsuccessful, returns `x` as is"
  ([x]     (cnt> x {}))
  ([x env] (simplify-matching-content
             (simplify-content x (simplify-env env))))) ;; interpret ?

(defn ctx>
  "Simplifies a FORM context recursively until it cannot be further simplified.
  All deductions are justified by the axioms of FORM logic.
  - for complex expressions, calls `simplify-content` on every unique element"
  ([ctx]     (ctx> ctx {}))
  ([ctx env] (vec (simplify-context ctx (simplify-env env)))))

(def simplify cnt>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation

(defn eval-expr
  "Evaluates a FORM expression with an optional `env` and returns a Constant expression with attached metadata including the maximally reduced expression in `:expr` and the environment in `:env`.
  - `env` must be a map with a content/variable in `expr` as a key"
  ([expr] (eval-expr expr {}))
  ([expr env]
   (let [res (cnt> expr env)
         v (case res
             []   calc/M
             nil  calc/N
             :U   calc/U
             [:U] calc/I
             nil)
         m {:expr res :env env}]
     (if (some? v)
       ;; ? maybe a map instead
       (with-meta [ v ] m)
       (with-meta [ :_ ] m)))))
;; alias
(def => eval-expr)

(defn eval-all
  "Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.
  - if `to-fdna?` is false, returns a seq of results as returned by `=>` in the order of the corresponding `vspace` ordering"
  ([expr] (eval-all expr {}))
  ([expr {:keys [to-fdna? vars] :or {to-fdna? true}}]
   (let [vars (if (nil? vars) (find-vars expr {:ordered? true}) vars)
         vspc (calc/vspace (count vars))
         all-envs (fn [vars]
                    (map (comp (partial apply hash-map)
                               (partial interleave vars)) vspc))
         envs (all-envs vars)]
     (if to-fdna?
       (let [consts (mapv (comp first (partial eval-expr expr)) envs)]
         (make :fdna (vec vars) (rseq consts)))
       (let [results (map (partial eval-expr expr) envs)]
         (with-meta results {:vars vars :vspc vspc}))))))
;; alias
(def =>* eval-all)

#_(defn eval-expr
      "Calls `=>` but instead of an expression returns the `const` value or nil."
      ([expr]     (eval-expr expr {}))
      ([expr env] (let [[x] (=> expr env)]
                    (if (calc/const? x)
                      x
                      :_)))) ; nil?

;; ? return a map of map-keys var->interpr. instead
#_(defn eval-all
      "Calls `=>*` but instead of an expression returns a `vdict`"
      [expr {:keys [sorted?] :or {sorted? true}}]
      (let [rs (=>* expr {:to-fdna? false})
            {:keys [vars vspc]} (meta rs)]
        (with-meta
          (->> rs
               (map first)
               (map vector vspc)
               (into (if sorted? (sorted-map-by calc/compare-dna) (hash-map))))
          {:vars vars})))

(defn equal
  "Equality check for expressions. Two expressions are considered equal, if their formDNAs are equal. Compares formDNAs from evaluation results of each expression by calling `calc/equal-dna`.
  - ordering of variable names in formDNA matters, see `find-vars`
  - stricter than `equiv`, which compares by `calc/equiv-dna`"
  [& exprs]
  (let [data     (map (comp op-data eval-all) exprs)
        dnas     (map :dna data)
        varlists (map :vars data)]
    (and (apply = varlists)
         (apply calc/equal-dna dnas))))

(defn equiv
  "Equivalence check for expressions. Two expressions are considered equivalent, if their formDNAs are equivalent. Compares formDNAs from evaluation results of each expression by calling `calc/equiv-dna`.
  - ordering of variable names in formDNA is irrelevant
  - looser than `equal`, which compares by `calc/equal-dna`
  - can be slow on expressions with 6+ variables"
  [& exprs]
  (apply calc/equiv-dna (map (comp #(op-get % :dna) eval-all) exprs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chained expressions

(defn- chain [exprs] (map form exprs))

;; ? choose different term than “chain”
(defn chained-expr
  "Chains expressions like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`
  - group expressions with arrangements: `[:- x y …]`"
  [{:keys [unmarked?] :or {unmarked? false}} & exprs]
  (let [f-chained (chain exprs)]
    (if unmarked?
      (apply make f-chained)
      (apply form f-chained))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nested expressions

;; ? get rid of `nil`
(defn- nest-left [exprs]
  (if (empty? exprs)
    nil
    (loop [r      (rest exprs)
           nested (splice-ctx [(first exprs)])]
      (if (empty? r)
        nested
        (let [[expr & r] r
              nested     (splice-ctx (concat [nested] [expr]))]
          (if (empty? r)
            nested
            (recur r nested)))))))

;; ? get rid of `nil`
(defn- nest-right [exprs]
  (if (empty? exprs)
    nil
    (let [[expr & r] exprs]
      (if (empty? r)
        (splice-ctx [expr])
        (splice-ctx (concat [expr] [(nest-right r)]))))))

(defn nested-expr
  "Nests expressions leftwards `(((…)a)b)` or rightwards `(a(b(…)))` if `{:rightwards? true}`
  - use `nil` for empty expressions
  - use an arrangement `(make x y …)` to add multiple exprs. to the same level"
  [{:keys [unmarked? rightwards?]
    :or {unmarked? false rightwards? false} :as opts} expr & exprs]
  {:pre [(map? opts)]}
  (let [exprs  (cons expr exprs)
        nested (if rightwards? (nest-right exprs) (nest-left exprs))]
    (apply (if unmarked? make form) nested)))

;; ! check assumptions about env while merging nesting contexts via crossing
;; ! needs MASSIVE refactoring
(defn simplify-expr-chain
  "Reduces a sequence of expressions, intended to be linked in a chain, to a sequence of simplified expressions, possibly spliced or shortened via inference.
  - assumes rightward-nesting, e.g. `(…(…(…)))`
  - for leftward-nesting, e.g. `(((…)…)…)`, pass `{:rtl? true}`"
  ([exprs env] (simplify-expr-chain {} exprs env))
  ([{:keys [rtl?] :or {rtl? false}} exprs env]
   (vec
    (loop [[expr & r]  (if rtl? (reverse exprs) exprs)
           env         env
           simpl-exprs (if rtl? '() [])]
      (let [[expr m] (ctx->cnt {:+meta? true}
                               (simplify-context (cnt->ctx expr) env))
            ;; needs upstream env to reduce with observed values
            ;; ? is there a cleaner approach than using meta
            ;; ? :depth will be incremented in simplify -> maybe not
            env (dissoc (:env-next m) :depth)]
        (if (and (empty? r) (= expr nil))
          (condp == (count simpl-exprs)
            0 (conj simpl-exprs nil)
            ;; by law of calling
            ;; > (a ()) = (())
            1 (conj (empty simpl-exprs) (form))
            ;; > (a (() (… (z)))) = (a (())) = (a)
            (if rtl?
              (rest simpl-exprs)
              (butlast simpl-exprs)))
          (let [simpl-exprs (if (and (> (count simpl-exprs) 1)
                                     (nil? ((if rtl? first last)
                                            simpl-exprs)))
                              ;; by law of crossing
                              ;; > (a (b ( (x …)))) = (a (b x …))
                              ;; > (a (b ( (() …)))) = (a (()))
                              (if rtl?
                                (let [[xs before] (split-at 2 simpl-exprs)
                                      ctx  [expr (second xs)]
                                      expr (ctx->cnt
                                            {} ;; ? correct env
                                            (simplify-context ctx {}))]
                                  (conj before expr))
                                (let [[before xs] (split-at
                                                   (- (count simpl-exprs) 2)
                                                   simpl-exprs)
                                      ctx  [(first xs) expr]
                                      expr (ctx->cnt
                                            {} ;; ? correct env
                                            (simplify-context ctx {}))]
                                  (conj (vec before) expr)))
                              (conj simpl-exprs expr))]
            (cond (= expr []) (cond
                                ;; by law of calling/crossing
                                ;; > (()) = (())
                                ;; > (a (b (() …))) = (a (b))
                                ;; > (a (b ( (() …)))) = (a (())) = (a)
                                (== 1 (count simpl-exprs)) simpl-exprs
                                rtl? (rest simpl-exprs)
                                :else (butlast simpl-exprs))
                  (empty? r) simpl-exprs
                  :else (recur r env simpl-exprs)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-defined Operators

;;-------------------------------------------------------------------------
;; syntactic operators

(s/def :formform.specs.expr/arrangement
  (s/cat :tag   (partial = :-)
         :exprs (s/* #(s/valid? :formform.specs.expr/expression %))))

(def arrangement? (partial s/valid? :formform.specs.expr/arrangement))

(defoperator :- [& exprs] (vector (into [] exprs))
  :predicate arrangement?
  ; :reducer (fn [[_ & exprs] env] (simplify-content [exprs] env))
  )

(defn arr-prepend [x rel-expr]
  (apply vector :- x (rest rel-expr)))

(defoperator :* [& exprs] (apply vector :- (map form exprs)))
(defoperator :| [& exprs] (mapv form exprs))

;; Nested expressions
(defoperator :<- [& exprs]
  (apply nested-expr {:unmarked? false :rightwards? false} exprs))
(defoperator :-> [& exprs]
  (apply nested-expr {:unmarked? false :rightwards? true} exprs))
(defoperator :< [& exprs]
  (apply nested-expr {:unmarked? true :rightwards? false} exprs))
(defoperator :> [& exprs]
  (apply nested-expr {:unmarked? true :rightwards? true} exprs))

;; Chained expressions
;; ? redundant because of :* and :|
(defoperator :<-> [& exprs]
  (apply chained-expr {:unmarked? false} exprs))
(defoperator :<> [& exprs]
  (apply chained-expr {:unmarked? true} exprs))


;;-------------------------------------------------------------------------
;; unclear FORMs

(s/def :formform.specs.expr/unclear
  (s/cat :tag   (partial = :uncl)
         :label #(and (string? %) ((complement empty?) %))))

(def unclear? (partial s/valid? :formform.specs.expr/unclear))

(defn construct-unclear [op-k & args]
  (let [label (->> args
                   (remove nil?)
                   (interpose " ")
                   (apply str))
        op    [op-k label]]
    (if (valid-op? op)
      op
      (throw (ex-info "Invalid label for unclear FORM."
                      {:label label})))))

(defoperator :uncl [label] [:seq-re :<r label label]
  :constructor construct-unclear
  :predicate unclear?
  :reducer
  (fn [[_ label] env]
    (let [fdna [:fdna [label] [:N :U :U :U]]]
      (simplify-op fdna env))))

;;-------------------------------------------------------------------------
;; memory FORMs

(s/def :formform.specs.expr/rem-pairs
  ;; ? maybe allow all expressions as keys
  (s/coll-of (s/tuple
              ; #(or (keyword? %) (symbol? %))
              #(s/valid? :formform.specs.expr/expression %)
              #(s/valid? :formform.specs.expr/expression %)) :into []))

(s/def :formform.specs.expr/memory
  (s/cat :tag  (partial = :mem)
         :rems :formform.specs.expr/rem-pairs
         :ctx  (s/* #(s/valid? :formform.specs.expr/expression %))))

(def rem-pairs? (partial s/valid? :formform.specs.expr/rem-pairs))

(def memory? (partial s/valid? :formform.specs.expr/memory))

(defn memory-replace [[_ _ & ctx] & repl-pairs]
  {:pre [(rem-pairs? repl-pairs)]}
  (apply make-op :mem (vec repl-pairs) ctx))

(defn memory-extend [[_ rems & ctx] & ext-pairs]
  {:pre [(rem-pairs? ext-pairs)]}
  (apply make-op :mem (vec (concat rems ext-pairs)) ctx))

(defn- simplify-rems
  [rems env]
  (if (empty? rems)
    [[] env]
    (loop [[[k v] & r] rems
           rems-reduced []
           env env]
      (let [v (simplify-content v env)
            rems-reduced (conj rems-reduced [k v])
            env (assoc env k v)]
        (if (empty? r)
          [rems-reduced env]
          (recur r rems-reduced env))))))

(defn- filter-rems
  [rems ctx]
  (let [rem-vars (set (find-vars ctx {:vars (set (map first rems))}))
        rev-rems (reverse rems)]
    (first (reduce
            (fn [[acc rem-vars varlist-next] [v x :as rem]]
              (if (some? (rem-vars v))
                (if (= v x) ;; remove self-reference [x x]
                  [acc rem-vars (rest varlist-next)]
                  (let [opts {:vars (set varlist-next)}
                        vars (if (struct-expr? x)
                               (find-vars x opts)
                               '())]
                    [(conj acc rem)
                     (disj (into rem-vars vars) v)
                     (rest varlist-next)]))
                [acc rem-vars (rest varlist-next)]))
            [[] rem-vars (map first (rest rev-rems))]
            rev-rems))))

(defn- simplify-memory
  [mem env]
  (let [[rems env] (simplify-rems (op-get mem :rems) env)
        ctx  (simplify-context (op-get mem :ctx) env)
        rems (filter-rems rems ctx)]
    (if (empty? rems)
      (apply make ctx)
      (apply make-op :mem rems ctx))))

(defoperator :mem [rems & ctx]
  (let [eqs (apply make
                   (map (fn [[k v]] (form (form k v)
                                          (form (form k) (form v))))
                        rems))]
    (form eqs (apply form ctx)))
  :predicate memory?
  :reducer simplify-memory)

(defn memory
  "Constructs a memory FORM."
  [rems & exprs]
  (make-op :mem rems exprs))

;;-------------------------------------------------------------------------
;; self-equivalent re-entry FORMs

(def seq-reentry-defaults {:parity :any, :open? false, :interpr :rec-instr})

(def seq-reentry-sign->opts
  "Maps signatures for self-equivalent re-entry FORMs to their corresponding option-maps."
  (let [ds seq-reentry-defaults]
    {:<r      (merge ds {})
     :<..r    (merge ds {:parity :even})
     :<..r.   (merge ds {:parity :odd})
     :<r_     (merge ds {:open? true})
     :<..r_   (merge ds {:open? true :parity :even})
     :<..r._  (merge ds {:open? true :parity :odd})

     :<r'     (merge ds {:interpr :rec-ident})
     :<..r'   (merge ds {:interpr :rec-ident :parity :even})
     :<..r'.  (merge ds {:interpr :rec-ident :parity :odd})
     :<r'_    (merge ds {:interpr :rec-ident :open? true})
     :<..r'_  (merge ds {:interpr :rec-ident :open? true :parity :even})
     :<..r'._ (merge ds {:interpr :rec-ident :open? true :parity :odd})}))

(defn seq-reentry-opts->sign
  "Inverse map of seq-reentry-sign->opts with default args."
  [m]
  (let [opts (merge seq-reentry-defaults
                    (select-keys m [:parity :open? :interpr]))
        opts->sign (set/map-invert seq-reentry-sign->opts)]
    (opts->sign opts)))

(s/def :formform.specs.expr/seq-reentry-signature
  (comp some? seq-reentry-sign->opts))

(s/def :formform.specs.expr/seq-reentry-opts
  (comp some? (set (vals seq-reentry-sign->opts))))

(s/def :formform.specs.expr/seq-reentry
  (s/cat :tag (partial = :seq-re)
         :sign #(s/valid? :formform.specs.expr/seq-reentry-signature %)
         :nested-exprs (s/+ #(s/valid? :formform.specs.expr/expression %))))

(def seq-reentry-signature?
  (partial s/valid? :formform.specs.expr/seq-reentry-signature))

(def seq-reentry-opts?
  (partial s/valid? :formform.specs.expr/seq-reentry-opts))

(def seq-reentry? (partial s/valid? :formform.specs.expr/seq-reentry))

(defn simplify-seq-reentry
  [seq-re env]
  (let [env   (observed-removeall env) ;; no de/generation across seq-re bounds
        sign  (op-get seq-re :sign)
        exprs (op-get seq-re :nested-exprs)
        {:keys [parity open? interpr] :as specs} (seq-reentry-sign->opts sign)
        ;; ! check if :U from environment can be equivalent to :U in exprs
        [re-entry? exprs]
        (let [re-expr     (if (arrangement? (first exprs))
                            (arr-prepend sign (first exprs))
                            [:- sign (first exprs)]) ;; apply?
              [e & exprs] (simplify-expr-chain {:rtl? true}
                                               (cons re-expr (rest exprs))
                                               env)
              [x & r]     (if (arrangement? e)
                            (op-get e :exprs)
                            [e])]
          (if (= x sign)
            [true  (cons (cond (empty? r) nil
                               (== 1 (count r)) (first r)
                               :else (apply make :- r))
                         exprs)]
            [false (cons e exprs)]))
        >< (fn [expr] (simplify-content expr env))] ; interpret ?
    (if re-entry?
      ;; try all possible cases for re-entry reduction:
      (if (<= (count exprs) 3)
        (let [exprs (vec exprs)
              [x y z] (map #(get exprs % :∅) (range 3))]
          (match [interpr open? parity  x y z]
            ;; primitive cases:
            [_ _     :even nil :∅  :∅] :U ;; ((f))
            [_ _     _     nil :∅  :∅] [:U] ;; (f)
            [_ false _     nil nil :∅] :U ;; ((f))
            [_ true  _     nil nil :∅] [:U] ;; (((f))) => (f)

            ;; if interpretation of `mn` is “recursive identity”
            ;; and `f = ((f))` can be separated from the rest:
            ;; f x      |  (f) x 
            [:rec-ident true  :even xs :∅ :∅] (>< (make :U xs))
            [:rec-ident true  _     xs :∅ :∅] (>< (make :I xs))
            ;; (f) x    |  ((f x))
            [:rec-ident true  _     nil ys :∅] (>< (make :I ys))
            [:rec-ident false _     xs nil :∅] (>< (make :U xs))
            ;; ((f x))  |  (((f) x))
            [:rec-ident false :even nil ys nil] (>< (make :U ys))
            [:rec-ident false _     nil ys nil] (>< (make :I ys))

            ;; by case distinction:
            [_ _     _     (:or :U ([:U] :seq)) nil :∅] [:U] ;; ((f U/I))
            [_ _     _     nil (:or :U ([:U] :seq)) :∅] :U ;; ((f) U/I)

            [_ false :even (:or :U ([:U] :seq)) :∅ :∅] :U ;; (f U/I)
            [_ true  :even (:or :U ([:U] :seq)) :∅ :∅] [:U] ;; (f U/I)
            [_ true  _     (:or :U ([:U] :seq)) :∅ :∅] :U ;; (f U/I)
            [_ false _     (:or :U ([:U] :seq)) :∅ :∅] [:U] ;; (f U/I)

            [_ false :even nil (:or :U ([:U] :seq)) nil] [:U] ;; (((f) U/I))
            [_ true  :even nil (:or :U ([:U] :seq)) nil] :U ;; (((f) U/I))
            [_ true  _     nil (:or :U ([:U] :seq)) nil] [:U] ;; (((f) U/I))
            [_ false _     nil (:or :U ([:U] :seq)) nil] :U ;; (((f) U/I))

             ;; if nothing applies, return the reduced seq-reentry FORM:
            :else (apply make :seq-re sign exprs)))
        (apply make :seq-re sign exprs))
      ;; re-entry vanished due to dominance of the mark
      ;; this is not a re-entry FORM anymore
      (>< (apply nested-expr {:unmarked? open? :rightwards? false}
                 exprs)))))

(defn- construct-seq-reentry
  [op-k specs & nested-exprs]
  (let [signature
        (cond
          (keyword? specs) (if (seq-reentry-signature? specs)
                             specs
                             (throw (ex-info "Invalid re-entry signature."
                                             {:arg specs})))
          (map? specs) (seq-reentry-opts->sign specs)
          :else (throw (ex-info "Invalid re-entry specifications."
                                {:arg specs})))
        nested-exprs (if (empty? nested-exprs) [nil] nested-exprs)
        op (apply vector :seq-re signature nested-exprs)]
    (if (seq-reentry? op)
      op
      ;; ! redundant checks
      (throw (ex-info (str "Invalid operator arguments" op-k)
                      {:op op-k :args (cons specs nested-exprs)})))))

;; ? should even/odd construct redundant re-entries in interpretation?
(defoperator :seq-re [sign & nested-exprs]
  (let [[x & ys :as exprs] nested-exprs
        {:keys [parity open?]} (seq-reentry-sign->opts sign)
        res-odd?  (odd? (count exprs))
        pre-step? (and res-odd? (not (= parity :even)))
        re   [:f* (apply nested-expr {:unmarked? false :rightwards? false}
                         (make :f* x) (if res-odd?
                                        (concat ys (cons x ys))
                                        ys))]
        pre  (when pre-step?
               [(if open? :f2 :f1)
                (apply nested-expr {:unmarked? false :rightwards? false}
                       (make :f* x) ys)])
        open (when open?
               [:f1 (apply nested-expr {:unmarked? true :rightwards? false}
                           (make (if pre-step? :f2 :f*) x) ys)])
        init (if (or pre-step? open?) :f1 :f*)]
    (make-op :mem (vec (remove nil? [re pre open])) init))
  :constructor construct-seq-reentry
  :predicate seq-reentry?
  :reducer simplify-seq-reentry)

(defn seq-re
  "Constructs a self-equivalent re-entry FORM given the arguments:
  - `specs`: either a `seq-reentry-signature` or an options map
  - `nested-exprs`: zero or more expressions intended as a nested sequence"
  [specs & nested-exprs]
  (apply construct-seq-reentry :seq-re specs nested-exprs))

;;-------------------------------------------------------------------------
;; Compound expressions

;; Isolator FORMs/class
(defoperator :N->M [x] (form (seq-re :<r (form x))
                             (seq-re :<..r (form x))))
(defoperator :M->M [x] (form (seq-re :<r x)
                             (seq-re :<..r x)))
(defoperator :U->M [x] (form (form (seq-re :<r (form x)) x)
                             (form (seq-re :<..r x) (form x))))
(defoperator :I->M [x] (form (form (seq-re :<r x) (form x))
                             (form (seq-re :<..r (form x)) x)))

(def const->isolator {:N (partial make :N->M)
                      :M (partial make :M->M)
                      :U (partial make :U->M)
                      :I (partial make :I->M)})

;; ! refactor
;; Selector FORMs/class
;; ? extend with flipped U/I for vcross (maybe a wrapper)
(defn sel
  ([vars->consts] (sel vars->consts true))
  ([vars->consts simplify?]
   (if (and simplify? (every? #{calc/M calc/N} (vals vars->consts)))
     (apply form (map (fn [[v c]]
                        (if (= c calc/M) (form v) v)) vars->consts))
     (let [select-UI (fn [[v c]] (case c
                                   :N [(form v) (form v)]
                                   :M [v v]
                                   :U [v (form v)]
                                   :I [(form v) v]))
           all-selections (map select-UI vars->consts)]
       (make (form (apply make (map first all-selections)) (form :seq-re :<..r))
             (form (apply make (map second all-selections)) (form :seq-re :<r)))))))


;; Logical spaces

(defoperator :<n> [& exprs]
  (let [n-exprs (apply form exprs)]
    (make (form n-exprs :U) (form n-exprs :I))))

(defoperator :<m> [& exprs]
  (let [m-exprs (apply make (map form exprs))]
    (make (form m-exprs :U) (form m-exprs :I))))

(defoperator :<u> [& exprs]
  (let [n-exprs (apply form exprs)
        m-exprs (apply make (map form exprs))]
    (make (form n-exprs :U) (form m-exprs :I))))

(defoperator :<i> [& exprs]
  (let [n-exprs (apply form exprs)
        m-exprs (apply make (map form exprs))]
    (make (form m-exprs :U) (form n-exprs :I))))


;;-------------------------------------------------------------------------
;; formDNA

(s/def :formform.specs.expr/formDNA
  (s/and (s/nonconforming
          (s/cat :tag  (partial = :fdna)
                 :vars (s/coll-of :formform.specs.expr/variable)
                 :dna  :formform.specs.calc/dna))
         #(== (count (second %)) (calc/dna-dimension (nth % 2)))))

(def formDNA? (partial s/valid? :formform.specs.expr/formDNA))

(defn- filter-formDNA
  "Filters the `dna` by values from given `env` whose keys match variables in the `varlist` of the formDNA."
  [fdna env]
  (let [;; assumes only constant values
        ;; ? what about unevaluated FORMs and formDNA?
        {:keys [vars dna]} (op-data fdna)
        matches (map #(let [v (get env % :_)
                            v (if (or (= :_ v) (calc/const? v))
                                v
                                (expr->const v))]
                        (vector % v))
                     vars)
        vpoint (map second matches)]
    (make :fdna (mapv first (filter #(= (second %) :_) matches))
          (calc/filter-dna dna vpoint))))

(defn- simplify-formDNA
  [operator env]
  (let [filtered-fdna (filter-formDNA operator env)]
    (if (empty? (op-get filtered-fdna :vars))
      (first (op-get filtered-fdna :dna))
      filtered-fdna)))

(defn- construct-formDNA
  ([op-k] (construct-formDNA op-k [] [calc/N]))
  ([op-k dna] (let [vars (vec (gen-vars (calc/dna-dimension dna)))]
                (construct-formDNA op-k vars dna)))
  ([op-k vars dna]
   (let [op (vector op-k vars dna)]
     (if (formDNA? op)
       op
       (throw (ex-info (str "Invalid operator arguments" op-k)
                       {:op op-k :vars vars :dna dna}))))))

(defoperator :fdna [vars dna]
  (if (empty? vars)
    (make (first dna))
    (let [vdict (calc/dna->vdict dna {})]
      (apply make
             (map (fn [[vpoint res]]
                    (apply form
                           (form res)
                           (map #(form ((const->isolator %1) %2))
                                vpoint vars)))
                  vdict))))
  :constructor construct-formDNA
  :predicate formDNA?
  :reducer simplify-formDNA)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-defined Symbols

(defsymbol :N nil)
(defsymbol :M (form))
(defsymbol :U (seq-re :<r nil nil) :reducer (fn [u _] u))
(defsymbol :I (form :U))

(defsymbol :0 :N)
(defsymbol :1 :U)
(defsymbol :2 :I)
(defsymbol :3 :M)

(defsymbol :mn :U)


(comment
  (simplify-content :N {})
  (simplify-matching-content :X :default)

  ;; ! this shouldnt work:
  (simplify-seq-reentry [:seq-re nil] {})
  (op-get [:seq-re 'a] :sign)
  (op-data [:seq-re 'a])

  ;; ? should formDNA result from evaluations be contracted:
  (calc/reduce-dna-seq [:N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N])

  (seq-re {} 'l 'e 'r)

  (make :* 'a 'b)

  (seq-re :<r 'x)

  (=>* (seq-re :<r 'a 'b))

  (eval-expr [:M :N] {})
  (cnt> [:M :N] {})
  )

  


