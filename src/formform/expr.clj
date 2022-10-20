(ns formform.expr
  (:require #_[clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [formform.calc :as calc]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen]))


;; Predicates

(s/def :formform.specs.expr/expr-tag #(:expr (meta %)))

;; ? underspecified
(s/def :formform.specs.expr/expr
  (s/and vector? :formform.specs.expr/expr-tag))

(def expr? (partial s/valid? :formform.specs.expr/expr))
(def expr-tag? (partial s/valid? :formform.specs.expr/expr-tag))
(def form? seq?)
(def mem-form? map?)
(defn variable? [x] (or (string? x) (symbol? x)))

(defn dna-expr?
  [x]
  (if (expr? x)
    (if-let [[vars dna] x]
      (and
        (vector? vars)
        (calc/dna? dna)
        (== (count vars) (calc/dna-dim dna)))
      false)
    false))

;; Helper

(defn- merge-ctx
  ([ctx] (merge-ctx identity ctx))
  ([pred ctx]
   (reduce (fn [acc x] (if (and (vector? x) (pred x))
                         (into acc x)
                         (conj acc x))) [] ctx)))

(defn- nest-left [ctx]
  (if (empty? ctx)
    '()
    (loop [xs     (rest ctx)
           nested (seq (merge-ctx [(first ctx)]))]
      (if (empty? xs)
        nested
        (let [[x & r] xs
              nested' (seq (merge-ctx [nested x]))]
          (if (empty? r)
            nested'
            (recur r nested')))))))

(defn- nest-right [ctx]
  (if (empty? ctx)
    '()
    (let [[x & r] ctx]
      (if (empty? r)
        (seq (merge-ctx (list x)))
        (seq (merge-ctx (list x (nest-right r))))))))

(defn- chain [ctx]
  (map #(seq (merge-ctx (list %))) ctx))

(defn gen-vars
  [n]
  (map #(str "v__" %) (range n)))

(defn seq-reentry-opts->sign
  "Constructs a signature for self-equivalent re-entry FORMs given an options map."
  [{:keys [parity open? interpr]
    :or {parity :any open? false interpr :rec-instr}}]
  (let [[re-entry pre-entry] (case parity
                               :any  [nil  nil]
                               :even [".." nil]
                               :odd  [".." "."])
        pre-open (if open? "_" nil)
        alt-interpr (if (= :rec-instr interpr) nil "'")]
    (keyword (str "<" re-entry "re" alt-interpr pre-entry pre-open))))

(defn seq-reentry-sign->opts
  "Returns a corresponding map of options given a signature for self-equivalent re-entry FORMs."
  [sign]
  (let [defaults {:parity :any, :open? false, :interpr :rec-instr}]
    (merge defaults
      (case sign
        :<re      {}
        :<..re    {:parity :even}
        :<..re.   {:parity :odd}
        :<re_     {:open? true}
        :<..re_   {:open? true :parity :even}
        :<..re._  {:open? true :parity :odd}

        :<re'     {:interpr :rec-ident}
        :<..re'   {:interpr :rec-ident :parity :even}
        :<..re'.  {:interpr :rec-ident :parity :odd}
        :<re'_    {:interpr :rec-ident :open? true}
        :<..re'_  {:interpr :rec-ident :open? true :parity :even}
        :<..re'._ {:interpr :rec-ident :open? true :parity :odd}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORM

(def MARK '())

(defn FORM
  ([] MARK)
  ([x & ys] (seq (merge-ctx #(and (expr? %) (not (map? (first %))))
                   (cons x ys)))))

(def UFORM :mn)
(def IFORM (FORM UFORM))

(defn UNCLEAR
  [x & ys] [:unclear (apply str x ys)])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(defn make-expr
  ([] (with-meta [] {:expr true}))
  ([x & ctx]
   (let [[env ctx] (if (map? x) [x ctx] [nil (cons x ctx)])
         merged-ctx (let [ctx (merge-ctx #(and (expr? %) (not (map? (first %))))
                                ctx)]
                      (if (seq env) (into [env] ctx) ctx))]
     (with-meta merged-ctx {:expr true}))))

(defn seq->expr [xs]
  (let [v (if (vector? xs)
            xs
            (vec xs))]
    (with-meta v {:expr true})))

(defn vars
  [expr {:keys [ordered?] :or {ordered? false}}]
  (let [children (fn [x] (filter #(or (form? %) (variable? %)) x))]
    (let [vs (->> (seq expr)
                  (tree-seq sequential? children)
                  rest
                  (filter (complement sequential?))
                  distinct)]
      (if ordered?
        (sort utils/compare-names vs)
        vs))))


;; Basic expression types

(def none-expr  (make-expr nil))
(def mark-expr  (make-expr MARK))
(def uform-expr (make-expr UFORM))
(def iform-expr (make-expr IFORM))

(defn form-expr [& ctx] (make-expr (apply FORM ctx)))

(defn unclear-expr [x & ys] (make-expr (apply UNCLEAR x ys)))

(defn unclear-expr->label [uncl-expr]
  (second (first uncl-expr)))


;; Special expression types

(def n-expr (make-expr calc/N))
(def m-expr (make-expr calc/M))
(def u-expr (make-expr calc/U))
(def i-expr (make-expr calc/I))

(def const->expr {:N n-expr :U u-expr :I i-expr :M m-expr})
(def expr->const {n-expr :N u-expr :U i-expr :I m-expr :M})

(defn var-expr [x]
  (when (or string? symbol? keyword?)
    (make-expr x)))

(defn dna-expr
  ([] (dna-expr [] :N))
  ([dna]
   (let [vars (gen-vars (calc/dna-dim dna))]
     (dna-expr vars dna)))
  ([vars dna]
   {:pre [(== (count vars) (calc/dna-dim dna))]}
   (make-expr [vars dna]))
  ([vars c & cs]
   (make-expr [vars (apply calc/make-dna c cs)])))

(defn dna-expr->dna-seq [dna-expr]
  (second (first dna-expr)))

(defn dna-expr->varlist [dna-expr]
  (first (first dna-expr)))

;; TODO
(defn filter-dna-seq-by-env
  [dna-seq env]
  (let [vpoint (mapv second (sort-by first (seq env)))]
    (calc/filter-dna-seq dna-seq vpoint)))


(defn nested-expr
  "Nests contents leftwards `(((…)a)b)` or rightwards `(a(b(…)))`
  if `{:rightwards? true}`
  - use brackets `[x y …]` to group expressions on the same level"
  ([env] (make-expr env))
  ([{:keys [unmarked? rightwards?]
     :or {unmarked? false rightwards? false}
     :as env} & ctx]
   (let [f-nested (if rightwards? (nest-right ctx) (nest-left ctx))]
     (apply make-expr (dissoc env :unmarked? :rightwards?)
       (if unmarked? f-nested (list f-nested))))))

(defn chained-expr
  "Chains content like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`
  - use brackets `[x y …]` to group expressions on the same link"
  ([env] (make-expr env))
  ([{:keys [unmarked?] :or {unmarked? false} :as env} & ctx]
   (let [f-chained (chain ctx)
         env (dissoc env :unmarked?)]
     (if unmarked?
       (apply make-expr env f-chained)
       (make-expr env (apply form-expr f-chained))))))


(defn seq-reentry-expr
  "Constructor for self-equivalent re-entry expressions"
  [env & ctx]
  (let [sign (seq-reentry-opts->sign env)
         ; ctx-merged (map #(if (vector? %) (merge-ctx %) %) ctx)
        ]
    (make-expr (apply vector sign ctx))))


;; Aliases (maybe temporary)

(def ·· make-expr)
(def ·  form-expr)

(def ·none  none-expr)
(def ·mark  mark-expr)
(def ·uform uform-expr)
(def ·iform iform-expr)
(def ·uncl  unclear-expr)

(def ·n n-expr)
(def ·m m-expr)
(def ·u u-expr)
(def ·i i-expr)
(def ·var var-expr)
(def ·dna dna-expr)

(def ·< (partial nested-expr {:unmarked? false :rightwards? false}))
(def ·> (partial nested-expr {:unmarked? false :rightwards? true}))
(def ··<  (partial nested-expr {:unmarked? true :rightwards? false}))
(def ··>  (partial nested-expr {:unmarked? true :rightwards? true}))
(def ·*  (partial chained-expr {:unmarked? false}))
(def ··*  (partial chained-expr {:unmarked? true}))

(def ·seq-re seq-reentry-expr)

(def ·r (partial seq-reentry-expr {:parity :any}))
(def ·2r (partial seq-reentry-expr {:parity :even}))
(def ·2r+1 (partial seq-reentry-expr {:parity :odd}))
(def ··r (partial seq-reentry-expr {:parity :any :open? true}))
(def ··2r (partial seq-reentry-expr {:parity :even :open? true}))
(def ··2r+1 (partial seq-reentry-expr {:parity :odd :open? true}))

(def ·r' (partial seq-reentry-expr {:parity :any :interpr :rec-ident}))
(def ·2r' (partial seq-reentry-expr {:parity :even :interpr :rec-ident}))
(def ·2r'+1 (partial seq-reentry-expr {:parity :odd :interpr :rec-ident}))
(def ··r' (partial seq-reentry-expr 
                   {:parity :any :open? true :interpr :rec-ident}))
(def ··2r' (partial seq-reentry-expr 
                    {:parity :even :open? true :interpr :rec-ident}))
(def ··2r'+1 (partial seq-reentry-expr 
                      {:parity :odd :open? true :interpr :rec-ident}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduction

(declare reduce-context)

; (defn- reduce-dna
;   [dna env]
;   (let [vars (keys env)]
;     nil))

(defn reduce-seq-reentry
  [[sign & ctx]]
  (let [[x & ys] ctx
        {:keys [parity open? interpr]} (seq-reentry-sign->opts sign)
        ctx       (if (zero? (count ctx)) (conj ctx nil) ctx)
        res-odd?  (odd? (count ctx))
        pre-step? (and res-odd? (not (= parity :even)))
        re [:f* (apply nested-expr {:unmarked? false :rightwards? false}
                  (make-expr :f* x) (if res-odd?
                                      (concat ys (cons x ys))
                                      ys))]
        pre  (when pre-step?
               [(if open? :f2 :f1)
                (apply nested-expr {:unmarked? false :rightwards? false}
                  (make-expr :f* x) ys)])
        open (when open?
               [:f1 (apply nested-expr {:unmarked? true :rightwards? false}
                      (make-expr (if pre-step? :f2 :f*) x) ys)])
        init (if (or pre-step? open?) :f1 :f*)]
    (make-expr (into {} [re pre open]) init)))


(defn- reduce-matching-content
  ([x] (reduce-matching-content x x))
  ([x default]
   (case x
     (() :M (nil) (:N) ((())) ((:M)) (((nil))) (((:N)))
         ((:U :I)) ((:I :U)))
     '()
     (nil :N (()) (:M) ((nil)) ((:N))
          (:U :I) (:I :U))
     nil
     (:mn :U (:I) ((:mn)) ((:U)) (((:I))))
     :mn
     ((:mn) :I (:U) (((:mn))) ((:I)) (((:U))))
     '(:mn)
     default)))

(defn- reduce-content
  [x env]
  (let [r (reduce-matching-content x :failed)]
    (if (not= :failed r)
      r
      (cond
       ; (dna-expr? x) (let [[vars dna] x]
       ;                 )
       ; (calc/dna? x)
        (form? x)     (let [r (reduce-context x env)]
                        (reduce-matching-content r))
        (mem-form? x) nil  ;; TODO
        :else (if-let [interpr (env x)]
                (cond
                  (keyword? interpr) interpr
                  :else (let [r (reduce-context interpr env)]
                          (if (<= (count r) 1)
                            (first r)
                            (list r))))
                x)))))


(defn- reduce-by-calling
  ;; kinda messy - how to make this more systematic?
  [ctx env]
  (if (some #{'() :M '(nil) '(:N) '((())) '((:M)) '(((nil))) '(((:N)))}
        ctx)
    ['( () ) env]
    (let [[seen-U seen-I] [(:U env) (:I env)]
          [seen-U env]
          (if (and (not seen-U)
                (some #{:mn :U '(:I) '((:mn)) '((:U)) '(((:I)))}
                  ctx))
            [true (assoc env :U true)]
            [seen-U env])]
      (if (and seen-U seen-I)
        ['( () ) env]
        (let [[seen-I env]
              (if (and (not seen-I)
                    (some #{'(:mn) :I '(:U) '(((:mn))) '((:I)) '(((:U)))}
                      ctx))
                [true (assoc env :I true)]
                [seen-I env])]
          (if (and seen-U seen-I)
            ['( () ) env]
            [ctx env]))))))

(defn- reduce-by-crossing
  [ctx]
  (let [f (fn [ctx' x]
            (if (form? x)
              (let [[x' & r] x]
                (if (and (form? x') (empty? r))
                  (into [] (concat ctx' x'))
                  (conj ctx' x)))
              (conj ctx' x)))]
    (reduce f [] ctx)))

(defn- reduce-context
  [ctx env]
  (loop [ctx ctx
         env env]
    (let [[ctx env] (reduce-by-calling ctx env)]
      (if (= ctx '( () ))
        ctx
        (let [ctx' (->> ctx
                        distinct
                        (map #(reduce-content % env))
                        reduce-by-crossing
                        (remove nil?))]
          (if (= ctx' ctx)
            ctx'
            (recur ctx' env)))))))


(defn cnt>
  "Reduces a FORM content recursively until it cannot be further reduced.
  All reductions are justified by the axioms of FORM logic.
  - if `x` is a complex FORM, calls `reduce-context` on `x`
  - if no reduction applies, tries to retrieve the value from given `env`
  - if retrieval was unsuccessful, returns `x` as is"
  ([x]     (cnt> x {}))
  ([x env] (reduce-content x env)))

(defn ctx>
  "Reduces a FORM context recursively until it cannot be further reduced.
  All reductions are justified by the axioms of FORM logic.
  - for complex expressions, calls `reduce-content` on every unique element"
  ([ctx]     (ctx> ctx {}))
  ([ctx env] (vec (reduce-context ctx env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation

(defn =>
  "Evaluates a FORM expression with an optional `env` and returns a Constant expression with attached metadata including the maximally reduced expression in `:expr` and the environment in `:env`.
  - `env` must be a map with a content/variable in `expr` as a key"
  ([expr] (=> expr {}))
  ([expr env]
   (when-not (expr-tag? expr)
     (throw (ex-info "missing `:expr` tag on input expression" {:input expr})))
   (let [[r & more :as ctx] (ctx> expr env)
         v (when (empty? more)
             (case r
               (( )   :M) :M
               (nil   :N) :N
               (:mn   :U) :U
               ((:mn) :I) :I
               nil))
         m {:expr ctx :env env}]
     (if (some? v)
       (with-meta [ v ] m)
       (with-meta [ :_ ] m)))))

(defn =>*
  "Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.
  - if `as-dna-expr?` is false, returns a seq of results as returned by `=>` in the order of the corresponding `vspace` ordering"
  ([expr] (=>* expr true))
  ([expr as-dna-expr?]
   (when-not (expr-tag? expr)
     (throw (ex-info "missing `:expr` tag on input expression" {:input expr})))
   (let [vars (vars expr {:ordered? true})
         vspc (calc/vspace (count vars))
         all-envs (fn [vars]
                    (map (comp
                           (partial apply hash-map)
                           (partial interleave vars)) vspc))
         envs (all-envs vars)]
     (if as-dna-expr?
       (let [consts (mapv (comp first (partial => expr)) envs)]
         ;; ? dna or dna-seq (performance?)
         (dna-expr vars (calc/consts->dna (rseq consts)))
         ; ^:expr [ ^:expr [vars (rseq consts)] ]
         )
       (let [results (map (partial => expr) envs)]
         (with-meta results {:vars vars :vspc vspc}))))))


(defn eval-expr
  "Calls `=>` but instead of an expression returns the `const` value or nil."
  ([expr]     (eval-expr expr {}))
  ([expr env] (let [[x] (=> expr env)]
                (if (calc/const? x)
                  x
                  nil))))

(defn eval-all
  "Calls `=>*` but instead of an expression returns a `vdict`"
  [expr {:keys [sorted?] :or {sorted? true}}]
  (let [rs (=>* expr false)
        {:keys [vars vspc]} (meta rs)]
    (with-meta
      (->> rs
           (map first)
           (map vector vspc)
           (into (if sorted? (sorted-map-by calc/compare-dna) (hash-map))))
      {:vars vars})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression parsing

;; ! WIP
(defn str->expr
  [s]
  (let [->expr-str #(if (and (= \[ (first %)) (= \] (last %)))
                      %
                      (str "[" % "]"))
        escape-uncl (fn [s]
                      (string/replace s #"/.+?/" #(str "\"" % "\"")))]
    (-> s
        ->expr-str
        escape-uncl
        read-string)))


