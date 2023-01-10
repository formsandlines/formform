(ns formform.expr
  (:require
   [clojure.set :as set]
   [formform.calc :as calc]
   [formform.expr-old-221119 :as expr-old]
   [formform.utils :as utils]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk]
   [clojure.spec.alpha :as s]))

;; ========================================================================
;;     formform expression module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbolic expressions

(defmulti make-op
  "Constructs a symbolic expression given a registered operator and parameters."
  (fn [op-k & _] {:pre [(keyword? op-k)]} op-k))

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

(defmethod make-op :default make-op->unknown
  [op-k & args]
  (throw (ex-info (str "Unknown operator " op-k)
                  {:op op-k :args args})))

(defmethod interpret-op :default interpret-op->unknown
  [[op-k & _ :as expr]]
  (throw (ex-info (str "Don’t know how to interpret " op-k)
                  {:op op-k :expr expr})))

(defmethod simplify-op :default simplify-op->unknown
  [[op-k & _ :as expr] env]
  (throw (ex-info (str "Don’t know how to simplify " op-k)
                  {:op op-k :expr expr :env env})))

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
  [k args interpretation & {:keys [constructor reducer]}]
  (let [[params r] (if (= (get args (- (count args) 2)) '&)
                     [(take (- (count args) 2) args) (last args)]
                     [args '()])
        all-params (if (and (seq? r) (empty? r))
                     params
                     (concat params [r]))
        methodname (fn [common-name]
                     (symbol (str common-name "->" (name k))))]
    `(do ~(let [[_ args body]
                (if (some? constructor)
                  constructor
                  [nil `[~'_ ~@args] `(apply vector ~k ~@params ~r)])]
            `(defmethod make-op ~k ~(methodname "make-op")
               ~args ~body))
         (defmethod interpret-op ~k ~(methodname "interpret-op")
           [[~'_ ~@args]] ~interpretation)
         ~(when (some? reducer)
            (let [[_ args body] reducer]
              `(defmethod simplify-op ~k ~(methodname "simplify-op")
                 ~args ~body)))
         ;; define method to get all parameters with names
         (defmethod op-data ~k ~(methodname "op-data")
           [op#] (zipmap ~(mapv (comp keyword str) all-params)
                         (if (> ~(count all-params) ~(count params))
                           (conj (subvec op# 1 ~(inc (count params)))
                                 (subvec op# ~(inc (count params))))
                           (subvec op# 1))))
         ;; define getter for each param by name
         ;; ? necessary
         (doseq [[i# param#] ~(vec (map-indexed
                                    (fn [i x] [i ((comp keyword str) x)])
                                    all-params))]
           (defmethod op-get [~k param#] ~(methodname "op-get")
             [op# ~'_] (if (== i# ~(count params))
                         (subvec op# (inc i#))
                         (op# (inc i#))))))))

(defmulti interpret-sym
  "Interprets a registered symbol."
  (fn [sym] {:pre [(keyword? sym)]} sym))

(defmulti simplify-sym
  "Simplifies a registered symbol given an optional environment."
  (fn [sym _] {:pre [(keyword? sym)]} sym))

(defmacro defsymbol
  [k interpretation & {:keys [reducer]}]
  `(do (defmethod interpret-sym ~k [~'_] ~interpretation)
       ~(when (some? reducer)
          (let [[_ args body] reducer]
            `(defmethod simplify-sym ~k ~args ~body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

(s/def :formform.specs.expr/form sequential?)

(s/def :formform.specs.expr/expr-symbol
  (s/and keyword? #(% (methods interpret-sym))))

(s/def :formform.specs.expr/op-symbol
  (s/and keyword? #(% (methods interpret-op))))

(s/def :formform.specs.expr/operator
  (s/and sequential?
         #(s/valid? :formform.specs.expr/op-symbol (op-symbol %))))

(s/def :formform.specs.expr/variable (s/or :str string? :sym symbol?))

(def form? (partial s/valid? :formform.specs.expr/form))
(def expr-symbol? (partial s/valid? :formform.specs.expr/expr-symbol))
(def op-symbol? (partial s/valid? :formform.specs.expr/op-symbol))
(def operator? (partial s/valid? :formform.specs.expr/operator))
(def variable? (partial s/valid? :formform.specs.expr/variable))

(def struct-expr? #(or (form? %) (operator? %)))
(def literal-expr? #(or (expr-symbol? %) (variable? %)))

(def expr->const {nil :N, [] :M, [:U] :I})

(def arrangement? #(and (operator? %) (= (op-symbol %) :-)))

(defn- splice-ctx
  [ctx]
  (reduce (fn [acc x] (cond
                        (nil? x) acc
                        (arrangement? x) (into acc (rest x))
                        :else (conj acc x)))
          [] ctx))

(defn make [& args]
  (let [[{:keys [mark? splice?] :or {mark? false splice? true}}
         [x & ctx]] (if (map? (first args))
                      [(first args) (rest args)]
                      [{} args])]
    (if (operator? [x])
      (let [op (apply make-op x ctx)]
        (if mark? [op] op))
      (let [ctx (if splice?
                  (splice-ctx (cons x ctx))
                  (cons x ctx))]
        (if mark?
          (vec ctx)
          (apply make-op :- ctx))))))

(defn form [& args]
  (let [[opts ctx] (if (map? (first args))
                     [(first args) (rest args)]
                     [{} args])]
    (apply make (merge opts {:mark? true}) ctx)))

(defn seq-re [sign & nested-exprs]
  (apply make :seq-re sign nested-exprs))

;; ! interpret expr itself, not only contents
(defn interpret
  [expr]
  (if (struct-expr? expr)
    (let [ctx (map #(cond (operator? %)    (interpret-op %)
                          (expr-symbol? %) (interpret-sym %)
                          :else %)
                   expr)]
      (apply make ctx))
    (if (expr-symbol? expr)
      (interpret-sym expr)
      expr)))

;; ! does not find nested re-entry FORMs
(defn find-vars
  "Finds all variables in a structural expresson.
  
  Options:
  - {:ordered true} to return variables in type order > alphanumeric order
  - {:vars #{…}} can be given a set of specific variables to find"
  [expr {:keys [ordered? vars] :or {ordered? false}}]
  (let [children (fn [x] (filter #(or (struct-expr? %) (if (nil? vars)
                                                         (variable? %)
                                                         (vars %))) x))
        vs (->> (seq expr)
                (tree-seq sequential? children)
                rest
                (filter (complement sequential?))
                distinct)]
    (if ordered?
      (sort utils/compare-names vs)
      vs)))

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

(defn- simplify-matching-content
  "Tries to reduce given content `x` to its simplest FORM if it matches a simple equivalent representation."
  ([x] (simplify-matching-content x x))
  ([x default]
   (case x ;; [] = '() in cases! (cljs doesn’t like ((…)) in cases)
     ([] :M [nil] [:N] [[[]]] [[:M]] [[[nil]]] [[[:N]]]
         ((:U :I)) ((:I :U)))
     []
     (nil :N [[]] [:M] [[nil]] [[:N]]
          (:U :I) (:I :U))
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
  (let [ctx  (simplify-context form env)
        expr (simplify-matching-content ctx)]
    (if (struct-expr? expr)
      (vary-meta expr #(merge % (meta ctx)))
      expr)))

(defn- simplify-content
  [x env]
  (let [res (simplify-matching-content x :failed)]
    (if (not= :failed res)
      res
      (cond
        (operator? x)    (if ((op-symbol x) (methods simplify-op))
                           (simplify-op x env)
                           (interpret-op x))
        (form? x)        (simplify-form x env)
        (expr-symbol? x) (interpret-sym x)
        :else x))))

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
  "Tries to reduce some `((x))` to `x` for each FORM in given context."
  [ctx]
  (let [f (fn [ctx x]
            (if (form? x)
              ;; ? test for [:- …]
              (let [[x' & r] x]
                (if (and (form? x') (empty? r))
                  (into [] (concat ctx x'))
                  (conj ctx x)))
              (conj ctx x)))]
    (reduce f [] ctx)))

(defn- substitute-from-env
  [ctx env]
  (loop [ctx  ctx
         i    0]
    (let [ctx' (map #(let [x (get env % :not-found)]
                       (if (= x :not-found) % x)) ctx)]
      (cond
        (= ctx' ctx) (splice-ctx ctx')
        (> i 400)    (throw (ex-info "Too many substitutions! Possibly caused by two mutually recursive associations in the env."
                                     {:type :infinite-substitution}))
        :else (recur ctx' (inc i))))))

;; ? add optim option to not observe values for (de)generation
;; ? add option for assumption mn ≠ mn to prevent relation of U/I
(defn- simplify-context
  [ctx env]
  (let [env (update env :depth #(if (nil? %) 0 (inc %)))]
    (if (< (:depth env) 400)
      ;; substitute matches from env upfront
      (loop [ctx (substitute-from-env ctx env)
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

;; ? how does this work
(defn simplify
  [expr env]
  (loop [[x & r] expr
         expr    []]
    (let [x (cond
              (and (keyword? x)
                   (x (methods simplify-op))) (simplify-op expr env)
              :else x)
          expr (conj expr x)]
      (if (empty? r)
        (apply make expr)
        (recur r expr)))))


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

(defoperator :- [& ctx] (form (apply form ctx))
  :reducer
  (fn [operator env] (simplify-form (interpret-op operator) env)))

(defn arr-prepend [x rel-expr]
  (apply vector :- x (rest rel-expr)))

(defoperator :* [& ctx] (apply make :- (map form ctx))
  :reducer
  (fn [operator env] (simplify-form (interpret-op operator) env)))

(defoperator :| [& ctx] (apply form (map form ctx))
  :reducer
  (fn [operator env] (simplify-form (interpret-op operator) env)))

;;-------------------------------------------------------------------------
;; unclear FORMs

(defoperator :uncl [label] (make :<r label label)
  :reducer
  (fn [[_ label] env]
    (let [fdna [:fdna [label] (calc/make-dna :N :U :U :U)]]
      (#'expr-old/reduce-expr fdna env))))

;;-------------------------------------------------------------------------
;; memory FORMs

(defn rem-pairs? [xs]
  (and (seqable? xs) (every? seqable? xs)))

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

(defoperator :mem [rems & ctx]
  (let [eqs (apply make
                   (map (fn [[k v]] (form (form k v)
                                          (form (form k) (form v))))
                        rems))]
    (form eqs (apply form ctx)))
  :reducer
  (fn [mem env]
    (let [[rems env] (simplify-rems (op-get mem :rems) env)
          ctx  (simplify-context (op-get mem :ctx) env)
          rems (filter-rems rems ctx)]
      (if (empty? rems)
        (apply make ctx)
        (apply make-op :mem rems ctx)))))

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

(defn simplify-seq-reentry
  [seq-re env]
  (let [sign  (op-get seq-re :sign)
        exprs (op-get seq-re :nested-exprs)
        {:keys [parity open? interpr] :as specs} (seq-reentry-sign->opts sign)
        ;; ! check if mn from environment can be equivalent to mn in exprs
        [re-entry? exprs]
        (let [re-expr     (if (arrangement? (first exprs))
                            (arr-prepend sign (first exprs))
                            (make :- sign (first exprs))) ;; apply?
              [e & exprs] (simplify-expr-chain {:rtl? true}
                                               (cons re-expr (rest exprs))
                                               env)
              [x & r]     (if (arrangement? e)
                            (op-get e :ctx)
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

(defoperator :seq-re [sign & nested-exprs]
  (let [[x & ys :as exprs] nested-exprs
        {:keys [parity open?]} (seq-reentry-sign->opts sign)
        res-odd?  (odd? (count exprs))
        pre-step? (and res-odd? (not (= parity :even)))
        re   [:f* (apply nested-expr {:unmarked? false :rightwards? false}
                         (into [:f*] x) (if res-odd?
                                          (concat ys (cons x ys))
                                          ys))]
        pre  (when pre-step?
               [(if open? :f2 :f1)
                (apply nested-expr {:unmarked? false :rightwards? false}
                       (into [:f*] x) ys)])
        open (when open?
               [:f1 (apply nested-expr {:unmarked? true :rightwards? false}
                           (into [(if pre-step? :f2 :f*)] x) ys)])
        init (if (or pre-step? open?) :f1 :f*)]
    (make-op :mem (vec (remove nil? [re pre open])) init))
  :reducer
  (fn [seq-re env]
    (simplify-seq-reentry seq-re env)))

;;-------------------------------------------------------------------------
;; Compound expressions

;; Isolator FORMs/class
(defoperator :N->M [x] (form (make :<r (form x))
                             (make :<2r (form x))))
(defoperator :M->M [x] (form (make :<r x)
                             (make :<2r x)))
(defoperator :U->M [x] (form (form (make :<r (form x)) x)
                             (form (make :<2r x) (form x))))
(defoperator :I->M [x] (form (form (make :<r x) (form x))
                             (form (make :<2r (form x)) x)))

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
       (make (form (apply make (map first all-selections)) (form :<2r))
             (form (apply make (map second all-selections)) (form :<r)))))))

;;-------------------------------------------------------------------------
;; formDNA

(defn- filter-fdna
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

(defoperator :fdna [vars dna]
  (if (empty? vars)
    (make dna)
    (let [vdict (calc/dna->vdict dna {})]
      (apply make
             (map (fn [[vpoint res]]
                    (apply form
                           (form res)
                           (map #(form ((const->isolator %1) %2)) vpoint vars)))
                  vdict))))
  :constructor
  (fn [op-k & args]
    (case (count args)
      0 [op-k [] [calc/N]]
      1 (let [[dna] args
              vars (vec (gen-vars (calc/dna-dim dna)))]
          [op-k vars dna])
      (apply vector op-k args)))
  :reducer
  (fn [operator env]
    (let [filtered-fdna (filter-fdna operator env)]
      (if (empty? (op-get filtered-fdna :vars))
        (op-get filtered-fdna :dna)
        filtered-fdna))))



(comment

  (make 'a (make 'b) 'c)
  (form 'a (form 'b) (make 'c (form 'd)))

  (interpret-op (make :* 'a 'b 'c))
  (interpret-op (make :| 'a 'b 'c))

  (form :| 'a 'b 'c)
  (form :* 'a 'b 'c)

  (simplify-expr-chain ['a [:- []] 'x] {})
  (simplify-expr-chain ['a [[]] 'x] {})

  (simplify-form (interpret-op [:- []]) {})
  (simplify-content [:- []] {})
  (simplify-content [[:- 'a 'b]] {})
  (simplify-op [:- []] {})
  (operator? [:o])
  (when ((op-symbol (make [])) (methods simplify-op))
    true)

  ;; ! fix that
  (= (simplify-expr-chain {:rtl? true} [[:- []] 'a 'x] {})
     '[a x])
  (nested-expr {} [:- []] 'a 'x)

  (= (simplify-expr-chain [[:- []] 'a 'x] {})
     '[[]])
  (nested-expr {:rightwards? true} [:- []] 'a 'x)


  (make :fdna ['a 'b] [:N :U :I :M
                       :U :I :M :N
                       :N :N :U :N
                       :M :U :N :U])

  (op-data [:seq-re :<r 'a 'b])

  (make :fdna)
  (make :fdna [:U :I])


  (defoperator :and [a b] [[a] [b]])
  (interpret (make :and 'x 'y))

  (interpret [:- 'a 'b [:- 'x]])


  (defoperator :foo [oh my] [[oh] [my]]
    :constructor
    (fn [_ & args]
      (condp = (count args)
        1 [:foo (first args)]
        3 (let [[oh my god] args]
            [:foo oh (str my god)]))))

  (make-op :- 'a 'b)
  (op-data (make-op :foo 'a 'b 'c))
  (op-data (make-op :foo 'a))

  (op-data (make :foo 'a "hallo " "welt"))

  (make-op :? 'a 'b)
  (interpret-op [:? 'a 'b])
  (make-op :uncl 'a 'b)

  (defmulti some-disp (fn [args] (first args)))
  (defmethod some-disp :wah
    [[_ & args]]
    (condp = (count args)
      1 (let [[a] args] (str "single " a))
      2 (let [[a b] args] (keyword (str a b)))))

  (some-disp [:wah 12 42])
  (some-disp [:wah 12])


  (defmulti mydisp (fn [expr] (:op expr)))
  (defmethod mydisp nil [[& xs]] (apply vector xs))
  (defmethod mydisp :- [{:keys [ctx]}] (apply vector :- ctx))
  (defmethod mydisp :uncl [{:keys [label]}] [:<r label label])

  (mydisp [12 53])
  (mydisp {:op :- :ctx ['a 'b 'c]})
  (mydisp {:op :uncl :label "cool"})

  (defmulti mydisp2 (fn
                      ([] nil)
                      ([k & _] k)))
  (defmethod mydisp2 nil [] "whatever")
  (defmethod mydisp2 :- [{:keys [ctx]}] (apply vector :- ctx))
  (defmethod mydisp2 :uncl [{:keys [label]}] [:<r label label])

  (mydisp2)

  ; (defmulti)
  ; (s/multi-spec)
  ; (make-hierarchy)
  ; (derive)
  ; (prefers)
  ; (prefer-method)


  (simplify-seq-reentry (make :seq-re :<r 'a 'b) {})

  (simplify-seq-reentry (make :seq-re :<r nil) {})
  (simplify-seq-reentry (make :seq-re :<..r nil) {})
  (simplify-seq-reentry (make :seq-re :<..r. nil) {})

  (op-data (make :seq-re :<r 'a 'b 'c))

  (seq-reentry-sign->opts :<r')

  (arr-prepend :x [:- 'a 'b])

  )


