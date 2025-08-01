;; ========================================================================
;;     formform expression core module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.expr.core
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [clojure.math.combinatorics :as combo]
   [formform.calc :as calc]
   [formform.expr.common :refer [tag_arrangement tag_formDNA]]
   [formform.expr.symexpr :as symx]
   [formform.utils :as utils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates

(declare expression?)

;; !! unchecked
(def form? #(and (sequential? %)
                 (every? expression? %)))
(def pure-form? #(and (form? %)
                      (not (symx/operator? %))))
(def variable? #(or (string? %)
                    (symbol? %)))
(def expression? #(or (nil? %)
                      (symx/operator? %)
                      (form? %)
                      (symx/expr-symbol? %)
                      (variable? %)
                      (keyword? %)))

(def struct-expr? #(or (form? %) (symx/operator? %)))
(def literal-expr? #(or (symx/expr-symbol? %) (variable? %)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Operations

(defn splice-ctx
  [ctx]
  (reduce (fn [acc x] (cond
                       (nil? x) acc
                       (symx/arrangement? x) (into acc (rest x))
                       :else (conj acc x)))
          [] ctx))

(defn make
  [& args]
  (let [[{:keys [mark? splice?] :or {mark? false splice? true}}
         [x & r]] (if (map? (first args))
                    [(first args) (rest args)]
                    [{} args])]
    (if (symx/operator? [x])
      (let [op (apply symx/make-op x r)]
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
            (apply vector tag_arrangement exprs)))))))

(defn form
  [& args]
  (let [[opts ctx] (if (map? (first args))
                     [(first args) (rest args)]
                     [{} args])]
    (apply make (merge opts {:mark? true}) ctx)))


(defn- substitute-expr
  [env expr]
  (let [x (get env expr :not-found)]
    (if (= x :not-found) expr x)))

;; ! check if defaults from `env` destructuring must be propagated in 
;;   `substitute-expr`
(defn interpret
  ([expr] (interpret {} expr))
  ([{:keys [--defocus --focus] :or {--defocus #{}
                                    --focus #{}} :as env} expr]
   (cond
     (--defocus expr)               expr
     (and (seq --focus)
          (if (symx/operator? expr)
            (nil? (--focus (symx/op-symbol expr)))
            (nil? (--focus expr)))) expr
     (and (nil? (--defocus :ops))
          (symx/operator? expr)
          (nil? (--defocus (symx/op-symbol expr)))) (symx/interpret-op expr)
     (and (nil? (--defocus :syms))
          (symx/expr-symbol? expr)) (symx/interpret-sym expr)
     (and (nil? (--defocus :vars))
          (variable? expr))         (substitute-expr env expr)
     :else expr)))

(defn interpret-walk
  ([expr] (interpret-walk {} expr))
  ([env expr]
   (walk/postwalk (partial interpret env) expr)))

;; ? return a lazy seq of iterations instead
(defn- prod=iterate-unless-eq [interpret-fn]
  (fn f*
    ([expr] (f* {} expr))
    ([env expr]
     (loop [expr expr
            i    0]
       (let [expr' (interpret-fn env expr)]
         (cond
           (= expr' expr) expr'
           (> i 400)      (throw (ex-info "Too many interpretation attempts! Possibly caused by co-dependent interpretations or mutually recursive associations in the env."
                                          {:type :infinite-substitution}))
           :else (recur expr' (inc i))))))))

(def interpret*
  (prod=iterate-unless-eq interpret))

(def interpret-walk*
  (prod=iterate-unless-eq interpret-walk))


;; ? be more specific than `sequential?`
(defn find-subexprs
  [expr subexprs]
  (if (some? (subexprs expr))
    (list expr)
    (if (sequential? expr)
      (filter subexprs
              (tree-seq sequential? seq (seq expr)))
      '())))

;; ! type order not recognized
(defn find-vars
  [expr {:keys [ordered? vars] :or {ordered? false}}]
  (if (sequential? expr)
    (let [children (fn [x] (filter #(or (struct-expr? %) (if (nil? vars)
                                                           (variable? %)
                                                           ((set vars) %)))
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

(defn observed-put [env x]
  (update env :--observed #(if (nil? %) #{x} (conj % x))))

(defn observed-disj [env x]
  (update env :--observed #(if (nil? %) % (disj % x))))

(defn observed-merge [env xs]
  (update env :--observed #(if (nil? %) (set xs) (set/union % (set xs)))))

(defn observed-get [env]
  (get env :--observed))

(defn observed-removeall [env]
  (dissoc env :--observed))

(defn observed-has? [env x]
  (not= (get-in env [:--observed x] :not-found) :not-found))

(defn ctx->cnt [{:keys [+meta?] :or {+meta? false}} ctx]
  (let [expr (condp == (count ctx)
               0 nil
               1 (first ctx)
               (apply make ctx))]
    (if +meta? [expr (meta ctx)] expr)))

;; ! implicitly simplifies ((…)) -> […]
;; ? should this be ((…)) -> [((…))] instead
(defn cnt->ctx [cnt]
  (cond
    (symx/arrangement? cnt) (rest cnt)
    (and (form? cnt)
         (== 1 (count cnt))
         (form? (first cnt))) (vec (first cnt))
    :else [cnt]))

(defn simplify-matching-content
  "Tries to reduce given content `x` to its simplest FORM if it matches a simple equivalent representation."
  ([x] (simplify-matching-content x x))
  ([x default]
   (case x ;; [] = '() in cases! (cljs doesn’t like ((…)) in cases)
     ([] :m [nil] [:n] [[[]]] [[:m]] [[[nil]]] [[[:n]]]
      [[:u :i]] [[:i :u]])
     []
     (nil :n [[]] [:m] [[nil]] [[:n]]
          [:u :i] [:i :u])
     nil
     (:u [:i] [[:u]] [[[:i]]])
     :u
     (:i [:u] [[:i]] [[[:u]]])
     [:u]
     default)))

(declare simplify-context)

;; ? metadata consistent after simplify-matching-content
(defn simplify-form
  [form env]
  (let [form (simplify-context form env)
        expr (simplify-matching-content
              (if (and (== 1 (count form))
                       (pure-form? (first form)))
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
        (symx/operator? x)    (symx/simplify-op x env)
        (form? x)             (simplify-form x env)
        (symx/expr-symbol? x) (symx/interpret-sym x)
        :else (substitute-expr env x)))))

(defmethod symx/simplify-op :default simplify-op->unknown
  [[op-k & _ :as expr] env]
  (if (symx/op-symbol? op-k)
    ;; ? applicative order (eval args first) would be more efficient
    ;;   but how to know if all args are expressions?
    ;;   -> should leave that to dedicated simplifier
    (simplify-content (symx/interpret-op expr) env)
    (throw (ex-info (str "Don’t know how to simplify " op-k)
                    {:op op-k :expr expr :env env}))))

;; ? kinda messy - how to make this more systematic
(defn- simplify-by-calling
  "Tries to reduce given context to `()` if it contains the equivalent of a `MARK` or both `:u` and `:i` are present in the context and/or given `env`.
  - remembers any occurrence of `:u` or `:i` in the `env`."
  [ctx env]
  ;; find mark in the expression
  (if (some #{[] :m [nil] [:n] [[[]]] [[:m]] [[[nil]]] [[[:n]]]}
            ctx)
    [[[]] env]
    ;; else find and remember :u in the expression
    ;; match with :i if seen
    (let [[seenU? seenI?] [(observed-has? env :u) (observed-has? env :i)]
          [seenU? env-next]
          (if (and (not seenU?)
                   (some #{:u [:i] [[:u]] [[[:i]]]} ctx))
            [true (observed-put env :u)]
            [seenU? env])]
      (if (and seenU? seenI?)
        [[[]] env-next]
        ;; else find and remember :i in the expression
        ;; match with :u if seen
        (let [[seenI? env-next]
              (if (and (not seenI?)
                       (some #{:i [:u] [[:i]] [[[:u]]]} ctx))
                [true (observed-put env-next :i)]
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
  ([x]     (cnt> x {}))
  ([x env] (simplify-matching-content
            (simplify-content x (simplify-env env))))) ;; interpret ?

(defn ctx>
  ([ctx]     (ctx> ctx {}))
  ([ctx env] (vec (simplify-context ctx (simplify-env env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation

(defn eval-simplified
  [expr env]
  (let [simpl-expr (cnt> expr env)
        result     (case simpl-expr
                     []   :m
                     nil  :n
                     :u   :u
                     [:u] :i
                     calc/var-const)]
    {:expr simpl-expr
     :env env
     :val result}))

;; ? remove
(def nuim-code-reversed ((comp vec reverse) calc/nuim-code))

;; ? check for validity of varorder
;; ? remove `reverse-results?` bc formDNA no longer reversed
;;   -> maybe a more general sort-code option
(defn eval-simplified*
  [{:keys [varorder only-vals? reverse-results?]} expr global-env]
  (let [vars (if (nil? varorder) (find-vars expr {:ordered? true}) varorder)
        vspc (calc/vspace (if reverse-results?
                            nuim-code-reversed
                            calc/nuim-code)
                          (count vars))
        envs (mapv (comp (partial merge global-env)
                         (partial apply hash-map)
                         (partial interleave vars)) vspc)
        results (mapv (if only-vals?
                        (comp :val (partial eval-simplified expr))
                        (partial eval-simplified expr)) envs)]
    {:varorder (vec vars)
     :vspace vspc
     :results results}))

;; (defn calc*
;;   [expr])

(defn =>
  ([expr] (=> expr {}))
  ([expr env]
   (:val (eval-simplified expr env))))

(defn =>*
  ([expr] (=>* expr {}))
  ([expr env] (=>* {} expr env))
  ([opts expr env]
   (let [{:keys [varorder results]}
         (eval-simplified*
          (merge opts {:only-vals? true}) expr env)]
     [tag_formDNA varorder results])))

(defn equal
  [& exprs]
  (let [data     (map (comp symx/op-data =>*) exprs)
        dnas     (map :dna data)
        varlists (map :varorder data)]
    (and (apply = varlists)
         (apply calc/equal-dna dnas))))

(defn equiv
  [& exprs]
  (apply calc/equiv-dna (map (comp #(symx/op-get % :dna) =>*) exprs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chained expressions

(defn- mark [exprs] (map form exprs))

(defn mark-exprs
  [{:keys [unmarked?] :or {unmarked? false}} & exprs]
  (let [f-chained (mark exprs)]
    (if unmarked?
      (apply make f-chained)
      (apply form f-chained))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nested expressions

(defn nest-exprs
  [{:keys [unmarked? ltr?]
    :or {unmarked? false ltr? false} :as opts} expr & exprs]
  {:pre [(map? opts)]}
  (let [exprs  (cons expr exprs)
        nested (if ltr?
                 (utils/nest-right splice-ctx exprs)
                 (utils/nest-left splice-ctx exprs))]
    (apply (if unmarked? make form) nested)))

;; ! check assumptions about env while merging nesting contexts via crossing
;; ! needs MASSIVE refactoring
(defn simplify-nesting-chain
  ([chain env] (simplify-nesting-chain {} chain env))
  ([{:keys [rtl?] :or {rtl? false}} chain env]
   (vec
    (loop [[expr & r]  (if rtl? (reverse chain) chain)
           env         env
           simpl-chain (if rtl? '() [])]
      (let [[expr m] (ctx->cnt {:+meta? true}
                               (simplify-context (cnt->ctx expr) env))
            ;; needs upstream env to reduce with observed values
            ;; ? is there a cleaner approach than using meta
            ;; ? :depth will be incremented in simplify -> maybe not
            env (dissoc (:env-next m) :depth)]
        (if (and (empty? r) (= expr nil))
          (condp == (count simpl-chain)
            0 (conj simpl-chain nil)
            ;; by law of calling
            ;; > (a ()) = (())
            1 (conj (empty simpl-chain) (form))
            ;; > (a (() (… (z)))) = (a (())) = (a)
            (if rtl?
              (rest simpl-chain)
              (butlast simpl-chain)))
          (let [simpl-chain (if (and (> (count simpl-chain) 1)
                                     (nil? ((if rtl? first last)
                                            simpl-chain)))
                              ;; by law of crossing
                              ;; > (a (b ( (x …)))) = (a (b x …))
                              ;; > (a (b ( (() …)))) = (a (()))
                              (if rtl?
                                (let [[xs before] (split-at 2 simpl-chain)
                                      ctx  [expr (second xs)]
                                      expr (ctx->cnt
                                            {} ;; ? correct env
                                            (simplify-context ctx {}))]
                                  (conj before expr))
                                (let [[before xs] (split-at
                                                   (- (count simpl-chain) 2)
                                                   simpl-chain)
                                      ctx  [(first xs) expr]
                                      expr (ctx->cnt
                                            {} ;; ? correct env
                                            (simplify-context ctx {}))]
                                  (conj (vec before) expr)))
                              (conj simpl-chain expr))]
            (cond (= expr []) (cond
                                ;; by law of calling/crossing
                                ;; > (()) = (())
                                ;; > (a (b (() …))) = (a (b))
                                ;; > (a (b ( (() …)))) = (a (())) = (a)
                                (== 1 (count simpl-chain)) simpl-chain
                                rtl? (rest simpl-chain)
                                :else (butlast simpl-chain))
                  (empty? r) simpl-chain
                  :else (recur r env simpl-chain)))))))))

(defn permute-vars
  [varorder]
  (combo/permutations varorder))


(comment
  ;; ? should env be always first arg
  ;; ! test more interpret function
  ;; ? should registered keywords be substitutable by env

  ;; simplify-op -> interpret-op or interpret or something else?
  ;; simplify-content

  (interpret-walk {:--focus #{[:- 'x]}} [:- 'a ['b [ [:- 'x] ]] :m])
  (interpret-walk {:--defocus #{[:- 'x]}} [:- 'a ['b [ [:- 'x] ]] :m])

  ;; form </> data in this case
  (interpret [:if #(= % []) :x :m :u])

  (find-vars [['x] 'z 'a] {})
  (find-vars [['x] 'z 'a] {:ordered? true})
  (find-vars [['x] "a" 'z "x" 'a] {})
  (find-vars [['x] "a" 'z "x" 'a] {:ordered? true}))

