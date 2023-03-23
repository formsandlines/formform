;; ========================================================================
;;     formform expression module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================

(ns formform.expr
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [clojure.spec.alpha :as s]
   [formform.calc :as calc]
   [formform.symexpr.common :refer [tag_arrangement tag_formDNA]]
   [formform.symexpr.core :as symx]
   [formform.utils :as utils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

(s/def :formform.specs.expr/pure-form
  (s/and :formform.specs.expr/form
         (complement
          (partial s/valid? :formform.specs.symexpr.core/generic-operator))))

(s/def :formform.specs.expr/variable (s/or :str string? :sym symbol?))

(s/def :formform.specs.expr/varorder (s/coll-of :formform.specs.expr/variable
                                                :kind sequential?))

;; ! check for predicates in input validation -> performance
(s/def :formform.specs.expr/expression
  (s/or :empty nil?
        :operator :formform.specs.symexpr.core/generic-operator
        :form :formform.specs.expr/form
        :expr-symbol :formform.specs.symexpr.core/expr-symbol
        :variable :formform.specs.expr/variable
        :unknown-symbol keyword?))

;; ? is this general notion confusing
; (s/def :formform.specs.expr/form sequential?)
(s/def :formform.specs.expr/form 
  (s/every :formform.specs.expr/expression
           :kind sequential?))

(s/def :formform.specs.expr/context
  (s/coll-of :formform.specs.expr/expression
             :kind sequential?))

(s/def :formform.specs.expr/environment map?)

(def form? (partial s/valid? :formform.specs.expr/form))
(def pure-form? (partial s/valid? :formform.specs.expr/pure-form))
(def variable? (partial s/valid? :formform.specs.expr/variable))
(def expression? (partial s/valid? :formform.specs.expr/expression))

(def struct-expr? #(or (form? %) (symx/operator? %)))
(def literal-expr? #(or (symx/expr-symbol? %) (variable? %)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Operations

(defn splice-ctx
  "Dissolves arrangements in given context such that their elements become direct children of the context itself."
  [ctx]
  (reduce (fn [acc x] (cond
                        (nil? x) acc
                        (symx/arrangement? x) (into acc (rest x))
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

;; ! check if defaults from `env` destructuring must be propagated in 
;;   `substitute-expr`
(defn interpret
  "Interprets an expression of any kind. Returns the original expression if it cannot be interpreted.

  Can be given an `env` map to interpret variables (as keys).
  This map can have an optional `--defocus` entry whose value is a set of items that should not be interpreted and a complementary `--focus` entry to only interpret the items specified in its set and nothing else.
  - the keywords `:ops` / `:syms` / `:vars` designate _all_ operations / expression symbols / variables
  - an operator symbol can provided to designate a specific operator
  - any other expression (like a variable) can be designated as itself
  - `--focus` and `--defocus` can cancel each other out if they contain the same item so you usually pick one or the other"
  ([expr] (interpret {} expr))
  ([{:keys [--defocus --focus] :or {--defocus #{}
                                    --focus #{}} :as env} expr]
   (cond
     (--defocus expr)                          expr
     (and (seq --focus)
          (if (symx/operator? expr)
            (nil? (--focus (symx/op-symbol expr)))
            (nil? (--focus expr))))            expr
     (and (nil? (--defocus :ops))
          (symx/operator? expr)
          (nil? (--defocus (symx/op-symbol expr)))) (symx/interpret-op expr)
     (and (nil? (--defocus :syms))
          (symx/expr-symbol? expr))                 (symx/interpret-sym expr)
     (and (nil? (--defocus :vars))
          (variable? expr))                    (substitute-expr env expr)
     :else expr)))

(defn interpret-walk
  "Recursively calls `interpret` on given expression and all its subexpressions with a depth-first walk."
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
  "Like `interpret`, but repeats substitution on interpreted expressions until they cannot be interpreted any further."
  (prod=iterate-unless-eq interpret))

(def interpret-walk*
  "Like `interpret-walk`, but repeats substitution on interpreted (sub-)expressions until they cannot be interpreted any further."
  (prod=iterate-unless-eq interpret-walk))


;; ? be more specific than `sequential?`
(defn find-subexprs
  "Finds all subexpressions in `expr` that match any element of the given set `subexprs`."
  [expr subexprs]
  (if (some? (subexprs expr))
    (list expr)
    (if (sequential? expr)
      (filter subexprs
              (tree-seq sequential? seq (seq expr)))
      '())))

;; ! type order not recognized
(defn find-vars
  "Finds all variables in an expresson or returns the expression itself if it is a variable.
  
  Options:
  - {:ordered true} to return variables in: type order -> alphanumeric order
  - {:vars #{…}} can be given a set of specific variables to find"
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
        (symx/operator? x)    (symx/simplify-op x env)
        (form? x)             (simplify-form x env)
        (symx/expr-symbol? x) (symx/interpret-sym x)
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

(defn- eval-simplified
  [expr env]
  (let [simpl-expr (cnt> expr env)
        result     (case simpl-expr
                     []   :M
                     nil  :N
                     :U   :U
                     [:U] :I
                     calc/var-const)]
    {:expr simpl-expr
     :env env
     :val result}))

;; ? check for validity of varorder
(defn- eval-simplified*
  [{:keys [varorder only-vals?]} expr global-env]
  (let [vars (if (nil? varorder) (find-vars expr {:ordered? true}) varorder)
        vspc (calc/vspace (count vars))
        envs (mapv (comp (partial merge global-env)
                         (partial apply hash-map)
                         (partial interleave vars)) vspc)
        results (mapv (if only-vals?
                        (comp :val (partial eval-simplified expr))
                        (partial eval-simplified expr)) envs)]
    {:varorder (vec vars)
     :vspace vspc
     :results results}))

; (defn calc*
;   [expr])

(defn =>
  "Evaluates a FORM expression with an optional `env` and returns a Constant expression with attached metadata including the maximally reduced expression in `:expr` and the environment in `:env`.
  - `env` must be a map with a content/variable in `expr` as a key"
  ([expr] (=> expr {}))
  ([expr env]
   (:val (eval-simplified expr env))))

(defn =>*
  "Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.
  - if `to-fdna?` is false, returns a seq of results as returned by `=>` in the order of the corresponding `vspace` ordering"
  ([expr] (=>* expr {}))
  ([expr env] (=>* {} expr {}))
  ([opts expr env]
   (let [{:keys [varorder results]}
         (eval-simplified* (merge opts {:only-vals? true}) expr env)]
     [tag_formDNA varorder (rseq results)])))

(defn evaluate
  ([expr] (evaluate expr {}))
  ([expr env]
   (let [res (eval-simplified expr env)
         v   (let [v (:val res)]
               (if (= :_ v)
                 (:expr res)
                 v))]
     (with-meta {:result v} res))))

(defn eval-all
  ([expr] (eval-all expr {}))
  ([expr env] (eval-all {} expr {}))
  ([opts expr env]
   (let [{:keys [varorder results] :as res}
         (eval-simplified* (merge opts {:only-vals? false}) expr env)
         vdict (map #(vector (mapv (:env %) varorder)
                             (:val %))
                    results)]
     (with-meta {:varorder varorder :results vdict} res))))


(defn equal
  "Equality check for expressions. Two expressions are considered equal, if their formDNAs are equal. Compares formDNAs from evaluation results of each expression by calling `calc/equal-dna`.
  - ordering of variable names in formDNA matters, see `find-vars`
  - stricter than `equiv`, which compares by `calc/equiv-dna`"
  [& exprs]
  (let [data     (map (comp symx/op-data =>*) exprs)
        dnas     (map :dna data)
        varlists (map :varorder data)]
    (and (apply = varlists)
         (apply calc/equal-dna dnas))))

(defn equiv
  "Equivalence check for expressions. Two expressions are considered equivalent, if their formDNAs are equivalent. Compares formDNAs from evaluation results of each expression by calling `calc/equiv-dna`.
  - ordering of variable names in formDNA is irrelevant
  - looser than `equal`, which compares by `calc/equal-dna`
  - can be slow on expressions with 6+ variables"
  [& exprs]
  (apply calc/equiv-dna (map (comp #(symx/op-get % :dna) =>*) exprs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chained expressions

(defn- mark [exprs] (map form exprs))

(defn mark-exprs
  "Chains expressions like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`
  - group expressions with arrangements: `[:- x y …]`"
  [{:keys [unmarked?] :or {unmarked? false}} & exprs]
  (let [f-chained (mark exprs)]
    (if unmarked?
      (apply make f-chained)
      (apply form f-chained))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nested expressions

(defn nest-exprs
  "Nests expressions leftwards `(((…)a)b)` or rightwards `(a(b(…)))` if `{:ltr? true}`
  - use `nil` for empty expressions
  - use an arrangement `(make x y …)` to add multiple exprs. to the same level"
  [{:keys [unmarked? ltr?]
    :or {unmarked? false ltr? false} :as opts} expr & exprs]
  {:pre [(map? opts)]}
  (let [exprs  (cons expr exprs)
        nested (if ltr?
                 (utils/nest-right splice-ctx exprs)
                 (utils/nest-left splice-ctx exprs))]
    (apply (if unmarked? make form) nested)))

(s/def :formform.specs.expr/expr-chain
  (s/coll-of :formform.specs.expr/expression
             :kind sequential?
             :min-count 1))

;; ! check assumptions about env while merging nesting contexts via crossing
;; ! needs MASSIVE refactoring
(defn simplify-expr-chain
  "Reduces a sequence of expressions, intended to be linked in a `chain`, to a sequence of simplified expressions, possibly spliced or shortened via inference.
  - assumes rightward-nesting, e.g. `(…(…(…)))`
  - for leftward-nesting, e.g. `(((…)…)…)`, pass `{:rtl? true}`"
  ([chain env] (simplify-expr-chain {} chain env))
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


(comment
  ;; ? should env be always first arg
  ;; ! test more interpret function
  ;; ? should registered keywords be substitutable by env

  ;; simplify-op -> interpret-op or interpret or something else?
  ;; simplify-content

  (interpret-walk {:--focus #{[:- 'x]}} [:- 'a ['b [ [:- 'x] ]] :M])
  (interpret-walk {:--defocus #{[:- 'x]}} [:- 'a ['b [ [:- 'x] ]] :M])

  ;; form </> data in this case
  (interpret [:if #(= % []) :x :M :U])

  (find-vars [['x] 'z 'a] {})
  (find-vars [['x] 'z 'a] {:ordered? true})
  (find-vars [['x] "a" 'z "x" 'a] {})
  (find-vars [['x] "a" 'z "x" 'a] {:ordered? true})


  )
