(ns formform.expr
  (:require #_[clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [clojure.set :refer [map-invert]]
            [formform.calc :as calc]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen]))


;; Helper

(defn- merge-ctx
  ([ctx] (merge-ctx vector? ctx))
  ([pred ctx]
   (reduce (fn [acc x] (if (pred x)
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


;; Specs / Predicates

(def seq-reentry-defaults {:parity :any, :open? false, :interpr :rec-instr})

(def seq-reentry-sign->opts
  "Maps signatures for self-equivalent re-entry FORMs to their corresponding option-maps."
  (let [ds seq-reentry-defaults]
    {:<re      (merge ds {})
     :<..re    (merge ds {:parity :even})
     :<..re.   (merge ds {:parity :odd})
     :<re_     (merge ds {:open? true})
     :<..re_   (merge ds {:open? true :parity :even})
     :<..re._  (merge ds {:open? true :parity :odd})

     :<re'     (merge ds {:interpr :rec-ident})
     :<..re'   (merge ds {:interpr :rec-ident :parity :even})
     :<..re'.  (merge ds {:interpr :rec-ident :parity :odd})
     :<re'_    (merge ds {:interpr :rec-ident :open? true})
     :<..re'_  (merge ds {:interpr :rec-ident :open? true :parity :even})
     :<..re'._ (merge ds {:interpr :rec-ident :open? true :parity :odd})}))

(defn seq-reentry-opts->sign
  "Inverse map of seq-reentry-sign->opts with default args."
  [m]
  (let [opts (merge seq-reentry-defaults
               (select-keys m [:parity :open? :interpr]))
        opts->sign (map-invert seq-reentry-sign->opts)]
    (opts->sign opts)))

(s/def :formform.specs.expr/seq-reentry-signature
  #(some? (seq-reentry-sign->opts %)))

(s/def :formform.specs.expr/seq-reentry-opts
  #(some? ((set (vals seq-reentry-sign->opts)) %)))

(def seq-reentry-signature? (partial s/valid?
                              :formform.specs.expr/seq-reentry-signature))

(def seq-reentry-opts? (partial s/valid?
                         :formform.specs.expr/seq-reentry-opts))


(s/def :formform.specs.expr/expr-tag #(:expr (meta %)))

(def expr-tag? (partial s/valid? :formform.specs.expr/expr-tag))

;; ? underspecified
(s/def :formform.specs.expr/expr
  (s/and vector? :formform.specs.expr/expr-tag))

(s/def :formform.specs.expr/fdna
  (s/and vector? #(= (first %) :fdna)
    #(if-let [[_ vars dna] %] ;; too much overhead?
       (and
         (vector? vars)
         (calc/dna? dna)
         (== (count vars) (calc/dna-dim dna)))
       false)))

(s/def :formform.specs.expr/unclear
  (s/and vector? #(= (first %) :uncl)))

(s/def :formform.specs.expr/seq-reentry
  (s/and vector? #(s/valid? :formform.specs.expr/seq-reentry-signature
                    (first %))))

(def expr? (partial s/valid? :formform.specs.expr/expr))
(def form? seq?)

(defn variable? [x] (or (string? x) (symbol? x)))

(def unclear? (partial s/valid? :formform.specs.expr/unclear))
(def seq-reentry? (partial s/valid? :formform.specs.expr/seq-reentry))
(def fdna? (partial s/valid? :formform.specs.expr/fdna))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content

(def NONE nil)
(def MARK '())

(defn FORM
  ([] MARK)
  ([x & ys] (seq (merge-ctx #(and (expr? %) (not (map? (first %))))
                   (cons x ys)))))

(def UFORM :mn)
(def IFORM (FORM UFORM))

(defn VAR
  [x]
  (if (or (string? x) (symbol? x) #_(keyword? x))
    x
    (throw (ex-info "Invalid variable type" {}))))

(defn UNCLEAR
  [x & ys] [:uncl (apply str x ys)])

(def UNCLEAR->label second)

;; ? map every x to its own context
(defn SEQ-REENTRY
  [specs & xs]
  (let [signature
        (cond
          (keyword? specs) (if (seq-reentry-signature? specs)
                             specs
                             (throw
                               (ex-info "Invalid re-entry signature." {})))
          (map? specs) (seq-reentry-opts->sign specs)
          :else (throw (ex-info
                         "Invalid re-entry specifications." {})))]
    (apply vector signature xs)))

(def SEQ-REENTRY->sign first)
(def SEQ-REENTRY->ctx rest)

(defn FDNA
  ([] (FDNA [] calc/N))
  ([dna] (let [vars (vec (gen-vars (calc/dna-dim dna)))] (FDNA vars dna)))
  ([vars dna]
   {:pre [(== (count vars) (calc/dna-dim dna))]}
   [:fdna vars dna])
  ([vars c & cs]
   [:fdna vars (apply calc/make-dna c cs)]))

(def FDNA->varlist second)
(def FDNA->dna (comp second next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression / context

(defn make-expr
  ([] (with-meta [] {:expr true}))
  ([x & ctx]
   (let [[env ctx] (if (map? x) [x ctx] [nil (cons x ctx)])
         merged-ctx (let [ctx (merge-ctx #(and (expr? %) (not (map? (first %))))
                                ctx)]
                      (if (seq env) (into [env] ctx) ctx))]
     (with-meta merged-ctx {:expr true}))))


;; ? is this necessary
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

;; TODO
(defn filter-dna-seq-by-env
  [dna-seq env]
  (let [vpoint (mapv second (sort-by first (seq env)))]
    (calc/filter-dna-seq dna-seq vpoint)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression constructors

;;-------------------------------------------------------------------------
;; Atomic expressions

(def ·· make-expr)

(def ·      (comp make-expr FORM))
(def ·?     (comp make-expr VAR))
(def ·none  (make-expr NONE))
(def ·mark  (make-expr MARK))
(def ·uform (make-expr UFORM))
(def ·iform (make-expr IFORM))
(def ·uncl  (comp make-expr UNCLEAR))
(def ·dna   (comp make-expr FDNA))

(def ·N (make-expr calc/N))
(def ·M (make-expr calc/M))
(def ·U (make-expr calc/U))
(def ·I (make-expr calc/I))

(def const->expr {:N ·N :U ·U :I ·I :M ·M})
(def expr->const {·N :N ·U :U ·I :I ·M :M})

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
       (make-expr env (apply · f-chained))))))

;; ? find a different solution to represent special FORM syntrax
(def ·<  (partial nested-expr {:unmarked? false :rightwards? false}))
(def ·>  (partial nested-expr {:unmarked? false :rightwards? true}))
(def ··< (partial nested-expr {:unmarked? true :rightwards? false}))
(def ··> (partial nested-expr {:unmarked? true :rightwards? true}))
(def ·*  (partial chained-expr {:unmarked? false}))
(def ··* (partial chained-expr {:unmarked? true}))

(def ·seq-re (comp make-expr SEQ-REENTRY))

(def ·r     (comp make-expr (partial SEQ-REENTRY :<re)))
(def ·2r    (comp make-expr (partial SEQ-REENTRY :<..re)))
(def ·2r+1  (comp make-expr (partial SEQ-REENTRY :<..re.)))
(def ··r    (comp make-expr (partial SEQ-REENTRY :<re_)))
(def ··2r   (comp make-expr (partial SEQ-REENTRY :<..re_)))
(def ··2r+1 (comp make-expr (partial SEQ-REENTRY :<..re._)))

(def ·r'     (comp make-expr (partial SEQ-REENTRY :<re')))
(def ·2r'    (comp make-expr (partial SEQ-REENTRY :<..re')))
(def ·2r'+1  (comp make-expr (partial SEQ-REENTRY :<..re'.)))
(def ··r'    (comp make-expr (partial SEQ-REENTRY :<re'_)))
(def ··2r'   (comp make-expr (partial SEQ-REENTRY :<..re'_)))
(def ··2r'+1 (comp make-expr (partial SEQ-REENTRY :<..re'._)))

;;-------------------------------------------------------------------------
;; Compound expressions

;; Isolator FORMs/class
(defn ·N->M [x] (· (·r (· x)) (·2r (· x))))
(defn ·M->M [x] (· (·r x) (·2r x)))
(defn ·U->M [x] (· (· (·r (· x)) x) (· (·2r x) (· x))))
(defn ·I->M [x] (· (· (·r x) (· x)) (· (·2r (· x)) x)))

(def const->isolator {:N ·N->M :M ·M->M :U ·U->M :I ·I->M})

;; Selector FORMs/class
;; ? extend with flipped U/I for vcross (maybe a wrapper)
(defn ·sel
  ([vars->consts] (·sel vars->consts true))
  ([vars->consts simplify?]
   (if (and simplify? (every? #{calc/M calc/N} (vals vars->consts)))
     (apply · (map (fn [[v c]]
                     (if (= c calc/M) (· v) v)) vars->consts))
     (let [select-UI (fn [[v c]] (case c
                                   :N [(· v) (· v)]
                                   :M [v v]
                                   :U [v (· v)]
                                   :I [(· v) v]))
           all-selections (map select-UI vars->consts)]
       (·· (· (apply ·· (map first all-selections)) (· (·2r)))
           (· (apply ·· (map second all-selections)) (· (·r))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion (of special FORMs to expressions of simple FORMs)

;;-------------------------------------------------------------------------
;; content -> context

(def ·expanded-uform (·r NONE NONE))
(def ·expanded-iform (· ·expanded-uform))

(defn expand-unclear
  [uncl]
  {:pre [(unclear? uncl)]}
  (let [v (·? (UNCLEAR->label uncl))]
    (·r v v)))

(defn expand-fdna
  [fdna]
  {:pre [(fdna? fdna)]}
  (let [vars (FDNA->varlist fdna)
        dna  (FDNA->dna fdna)]
    (if (empty? vars)
      (·· dna)
      (let [vdict (calc/dna->vdict dna {})]
        (apply ·· (map (fn [[vpoint res]]
                         (apply ·
                           (· (const->expr res))
                           (map #(· ((const->isolator %1) %2)) vpoint vars)))
                    vdict))))))

(defn expand-seq-reentry
  [seq-re]
  {:pre [(seq-reentry? seq-re)]}
  (let [signature        (SEQ-REENTRY->sign seq-re)
        [x & ys :as ctx] (SEQ-REENTRY->ctx seq-re)
        {:keys [parity open? interpr]} (seq-reentry-sign->opts signature)
        ctx       (if (zero? (count ctx)) (conj ctx NONE) ctx)
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

;;-------------------------------------------------------------------------
;; context -> content

(defn expand-expr
  [expr]
  {:pre [(expr? expr)]}
  (if-let [env (when (map? (first expr)) (first expr))]
    (let [eqs (map (fn [[k v]]
                     (· (· k v) (· (· k) (· v)))) env)]
      (FORM (apply ·· eqs) (apply · (rest expr))))
    (FORM (apply · expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduction

;;-------------------------------------------------------------------------
;; content -> content

(declare reduce-context)

(defn- reduce-matching-content
  "Tries to reduce given content `x` to its simplest FORM if it matches a simple equivalent representation."
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
         ; (dna-expr? x) (let [r (expand-fdna )])
        ; (seq-reentry-tag? x) (let [ctx (expand-seq-reentry x)
        ;                            r   (reduce-context ctx env)]
        ;                        (reduce-matching-content r))
        (form? x) (let [r (reduce-context x env)]
                    (reduce-matching-content r))
        ; (mem-form? x) nil  ;; TODO
        :else (if-let [interpr (env x)]
                (cond
                  (keyword? interpr) interpr
                  :else (let [r (reduce-context interpr env)]
                          (if (<= (count r) 1)
                            (first r)
                            (list r))))
                x)))))

;;-------------------------------------------------------------------------
;; context -> context

;; ? kinda messy - how to make this more systematic
(defn- reduce-by-calling
  "Tries to reduce given context to `()` if it contains the equivalent of a `MARK` or both `UFORM` and `IFORM` are present in the context and/or given `env`.
  - remembers any occurrence of `UFORM` or `IFORM` in the `env`."
  [ctx env]
  ;; find mark in the expression
  (if (some #{'() :M '(nil) '(:N) '((())) '((:M)) '(((nil))) '(((:N)))}
        ctx)
    ['( () ) env]
    ;; else find and remember UFORM in the expression
    ;; match with IFORM if seen
    (let [[seen-U seen-I] [(:U env) (:I env)]
          [seen-U env]
          (if (and (not seen-U)
                (some #{:mn :U '(:I) '((:mn)) '((:U)) '(((:I)))} ctx))
            [true (assoc env :U true)]
            [seen-U env])]
      (if (and seen-U seen-I)
        ['( () ) env]
        ;; else find and remember IFORM in the expression
        ;; match with UFORM if seen
        (let [[seen-I env]
              (if (and (not seen-I)
                    (some #{'(:mn) :I '(:U) '(((:mn))) '((:I)) '(((:U)))} ctx))
                [true (assoc env :I true)]
                [seen-I env])]
          (if (and seen-U seen-I)
            ['( () ) env]
            ;; else return original expression with updated env
            [ctx env]))))))

(defn- reduce-by-crossing
  "Tries to reduce some `((x))` to `x` for each FORM in given context."
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
                        distinct ;; by law of calling
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

(def reduce-expr ctx>)


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
               (( )   :M) calc/M
               (nil   :N) calc/N
               (:mn   :U) calc/U
               ((:mn) :I) calc/I
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
         (·dna (vec vars) (calc/consts->dna (rseq consts)))
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
                  :_)))) ; nil?

;; ? return a map of map-keys var->interpr. instead
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


