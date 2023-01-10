(ns formform.expr-old-221119
  (:require #_[clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [formform.calc :as calc]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen]
            ))

;; ========================================================================
;;     formform expression module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================

;; Specs / Predicates

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
  #(some? (seq-reentry-sign->opts %)))

(s/def :formform.specs.expr/seq-reentry-opts
  #(some? ((set (vals seq-reentry-sign->opts)) %)))

(def seq-reentry-signature? (partial s/valid?
                              :formform.specs.expr/seq-reentry-signature))

(def seq-reentry-opts? (partial s/valid?
                         :formform.specs.expr/seq-reentry-opts))


(s/def :formform.specs.expr/expr-tag #(:expr (meta %)))

(def expr-tag? (partial s/valid? :formform.specs.expr/expr-tag))

(s/def :formform.specs.expr/variable (s/or :str string? :sym symbol?))

;; ? does it need to be a vector
(s/def :formform.specs.expr/expr
  (s/and vector? :formform.specs.expr/expr-tag))

(s/def :formform.specs.expr/fdna
  (s/and (s/nonconforming
          (s/cat :tag     #(= % :fdna)
                 :varlist (s/coll-of :formform.specs.expr/variable
                                    ; :count (calc/dna-dim (nth % 2))
                                     )
                 :dna     :formform.specs.calc/dna))
         #(== (count (second %)) (calc/dna-dim (nth % 2)))))

(s/def :formform.specs.expr/unclear
  (s/cat :tag   #(= % :uncl)
         :label string?))

(s/def :formform.specs.expr/seq-reentry
  (s/cat :tag  #(s/valid? :formform.specs.expr/seq-reentry-signature %)
         :ctxs (s/* vector?)))

(s/def :formform.specs.expr/rem-pairs
  (s/coll-of (s/tuple #(not (s/valid? :formform.specs.expr/expr %))
                      vector?) :into []))

(s/def :formform.specs.expr/memory
  (s/cat :tag  #(= % :mem)
         :rems :formform.specs.expr/rem-pairs
         :ctx  (s/* constantly)))

; (s/conform :formform.specs.expr/memory [:mem [['a (<-> :M)]] 'a nil :N '(a (:U))])

(def special-form-tags (into #{:mem :fdna :uncl}
                             (keys seq-reentry-sign->opts)))

(def expr? (partial s/valid? :formform.specs.expr/expr))
(def form? #(and (sequential? %) (not (expr? %))))
(def special-form? #(and (form? %) (some? (special-form-tags (first %)))))
(def pure-form? #(and (form? %) (not (special-form? %))))

(def variable? (partial s/valid? :formform.specs.expr/variable))

(def unclear? (partial s/valid? :formform.specs.expr/unclear))
(def seq-reentry? (partial s/valid? :formform.specs.expr/seq-reentry))
(def fdna? (partial s/valid? :formform.specs.expr/fdna))
(def memory? (partial s/valid? :formform.specs.expr/memory))

(def uform? #(= :mn %))
(def iform? #(= '(:mn) %))


;; Helper

(defn- merge-ctx
  ([ctx] (merge-ctx expr? ctx))
  ([pred ctx]
   (reduce (fn [acc x] (if (pred x)
                         (into acc x)
                         (conj acc x))) [] ctx)))

(defn gen-vars
  [n]
  (map #(str "v__" %) (range n)))

(defn rem-pairs? [xs]
  (and (seqable? xs) (every? seqable? xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content

(def NONE nil)
(def MARK '())


(defn FORM
  ([] MARK)
  ([x & ys] (sequence (merge-ctx (cons x ys)))))

(def UFORM :mn)
(def IFORM (FORM UFORM))


(defn VAR
  [x]
  (if (or (string? x) (symbol? x) #_(keyword? x))
    x
    (throw (ex-info "Invalid variable type" {:arg x}))))


(defn UNCLEAR
  [x & ys] [:uncl (apply str x ys)])

(def UNCLEAR->label second)


;; ? map every x to its own context
(defn SEQ-REENTRY
  "All contexts must be vectors!"
  [specs & ctxs]
  (let [signature
        (cond
          (keyword? specs) (if (seq-reentry-signature? specs)
                             specs
                             (throw
                              (ex-info "Invalid re-entry signature."
                                       {:arg specs})))
          (map? specs) (seq-reentry-opts->sign specs)
          :else (throw (ex-info "Invalid re-entry specifications."
                                {:arg specs})))
        ctxs (if (empty? ctxs) '(()) ctxs)]
    (apply vector signature (map merge-ctx ctxs))))

(def SEQ-REENTRY->sign first)
(def SEQ-REENTRY->ctxs rest)


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


(declare make-expr)

(defn MEMORY
  [kv-pairs & xs]
  ; {:pre [(rem-pairs? kv-pairs)]}
  (let [kv-pairs (map #(if (expr? (second %))
                         [(first %) (into [] (second %))]
                         [(first %) (vector (second %))]) kv-pairs)]
    (apply vector :mem kv-pairs (merge-ctx xs))))

(def MEMORY->rems second)
(def MEMORY->ctx (comp rest rest))

; (def MEMORY-peek (comp peek MEMORY->rems)) ;; [1 2 3] -> 3
; (def MEMORY-pop (comp pop MEMORY->rems)) ;; [1 2 3] -> [1 2]

(defn MEMORY-replace [[_ _ & ctx] & repl-pairs]
  {:pre [(rem-pairs? repl-pairs)]}
  (apply MEMORY (vec repl-pairs) ctx))

(defn MEMORY-extend [[_ rems & ctx] & ext-pairs]
  {:pre [(rem-pairs? ext-pairs)]}
  (apply MEMORY (vec (concat rems ext-pairs)) ctx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression / context

(defn make-expr
  [& ctx]
  (with-meta (vec (merge-ctx ctx)) {:expr true}))

(defn seq->expr [xs merge-ctx?]
  (if merge-ctx?
    (apply make-expr xs)
    (with-meta (vec xs) {:expr true})))

;; ! does not find nested re-entry FORMs
(defn find-vars
  "Finds all variables in an expresson.
  
  Options:
  - {:ordered true} to return variables in type order > alphanumeric order
  - {:vars #{…}} can be given a set of specific variables to find"
  [expr {:keys [ordered? vars] :or {ordered? false}}]
  (let [children (fn [x] (filter #(or (form? %)
                                      (if (nil? vars)
                                        (variable? %) (vars %))) x))
        vs (->> (seq expr)
                (tree-seq sequential? children)
                rest
                (filter (complement sequential?))
                distinct)]
    (if ordered?
      (sort utils/compare-names vs)
      vs)))

(defn- nest-left [ctxs]
  (if (empty? ctxs)
    nil
    (loop [r      (rest ctxs)
           nested (sequence (merge-ctx (first ctxs)))]
      (if (empty? r)
        nested
        (let [[ctx & r] r
              nested  (sequence (merge-ctx (concat [nested] ctx)))]
          (if (empty? r)
            nested
            (recur r nested)))))))

(defn- nest-right [ctxs]
  (if (empty? ctxs)
    nil
    (let [[ctx & r] ctxs]
      (if (empty? r)
        (sequence (merge-ctx ctx))
        (sequence (merge-ctx (concat ctx [(nest-right r)])))))))

(defn- chain [ctxs]
  (map #(sequence (merge-ctx %)) ctxs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression constructors

;;-------------------------------------------------------------------------
;; Atomic expressions

(def <-> make-expr)

(def <>      (comp make-expr FORM))
(def <?>     (comp make-expr VAR))
(def <none>  (make-expr NONE))
(def <mark>  (make-expr MARK))
(def <uform> (make-expr UFORM))
(def <iform> (make-expr IFORM))
(def <uncl>  (comp make-expr UNCLEAR))
(def <fdna>  (comp make-expr FDNA))
(def <mem>   (comp make-expr MEMORY))

(def <N> (make-expr calc/N))
(def <M> (make-expr calc/M))
(def <U> (make-expr calc/U))
(def <I> (make-expr calc/I))

(def const->expr {:N <N> :U <U> :I <I> :M <M>})
(def expr->const {<N> :N <U> :U <I> :I <M> :M
                  <none> :N <mark> :M <uform> :U <iform> :I})

;; ? "nested" or "chained"?
(defn nested-expr
  "Nests contexts leftwards `(((…)a)b)` or rightwards `(a(b(…)))` if `{:rightwards? true}`
  - a context must be either a vector `[x y …]` or an expression `(<-> x y …)`"
  [{:keys [unmarked? rightwards?]
    :or {unmarked? false rightwards? false}} ctx & ctxs]
  (let [ctxs (cons ctx ctxs)
        f-nested (if rightwards? (nest-right ctxs) (nest-left ctxs))]
    (apply make-expr (if unmarked? f-nested (list f-nested)))))

;; ? choose different term than “chain”
(defn chained-expr
  "Chains contexts like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`
  - a context must be either a vector `[x y …]` or an expression `(<-> x y …)`"
  [{:keys [unmarked?] :or {unmarked? false}} & ctxs]
  (let [f-chained (chain ctxs)]
    (if unmarked?
      (apply make-expr f-chained)
      (make-expr (apply <> f-chained)))))

(def <<  (partial nested-expr {:unmarked? false :rightwards? false}))
(def >>  (partial nested-expr {:unmarked? false :rightwards? true}))
(def <-< (partial nested-expr {:unmarked? true :rightwards? false}))
(def >-> (partial nested-expr {:unmarked? true :rightwards? true}))
(def <+> (partial chained-expr {:unmarked? false}))
(def <|> (partial chained-expr {:unmarked? true}))

(def <seq-re> (comp make-expr SEQ-REENTRY))

(def <r     (comp make-expr (partial SEQ-REENTRY :<r)))
(def <2r    (comp make-expr (partial SEQ-REENTRY :<..r)))
(def <2r+1  (comp make-expr (partial SEQ-REENTRY :<..r.)))
(def <r_    (comp make-expr (partial SEQ-REENTRY :<r_)))
(def <2r_   (comp make-expr (partial SEQ-REENTRY :<..r_)))
(def <2r+1_ (comp make-expr (partial SEQ-REENTRY :<..r._)))

(def <r'     (comp make-expr (partial SEQ-REENTRY :<r')))
(def <2r'    (comp make-expr (partial SEQ-REENTRY :<..r')))
(def <2r'+1  (comp make-expr (partial SEQ-REENTRY :<..r'.)))
(def <r'_    (comp make-expr (partial SEQ-REENTRY :<r'_)))
(def <2r'_   (comp make-expr (partial SEQ-REENTRY :<..r'_)))
(def <2r'+1_ (comp make-expr (partial SEQ-REENTRY :<..r'._)))

;;-------------------------------------------------------------------------
;; Compound expressions

;; Isolator FORMs/class
(defn <N>->M [x] (<> (<r [(<> x)]) (<2r [(<> x)])))
(defn <M>->M [x] (<> (<r [x]) (<2r [x])))
(defn <U>->M [x] (<> (<> (<r [(<> x)]) x) (<> (<2r [x]) (<> x))))
(defn <I>->M [x] (<> (<> (<r [x]) (<> x)) (<> (<2r [(<> x)]) x)))

(def const->isolator {:N <N>->M :M <M>->M :U <U>->M :I <I>->M})

;; Selector FORMs/class
;; ? extend with flipped U/I for vcross (maybe a wrapper)
(defn <sel>
  ([vars->consts] (<sel> vars->consts true))
  ([vars->consts simplify?]
   (if (and simplify? (every? #{calc/M calc/N} (vals vars->consts)))
     (apply <> (map (fn [[v c]]
                     (if (= c calc/M) (<> v) v)) vars->consts))
     (let [select-UI (fn [[v c]] (case c
                                   :N [(<> v) (<> v)]
                                   :M [v v]
                                   :U [v (<> v)]
                                   :I [(<> v) v]))
           all-selections (map select-UI vars->consts)]
       (<-> (<> (apply <-> (map first all-selections)) (<> (<2r)))
            (<> (apply <-> (map second all-selections)) (<> (<r))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion (of special FORMs to expressions of simple FORMs)

;;-------------------------------------------------------------------------
;; content -> context

(def <expanded-uform> (<r NONE NONE))
(def <expanded-iform> (<> <expanded-uform>))

(defn expand-unclear
  [uncl]
  {:pre [(unclear? uncl)]}
  (let [v (<?> (UNCLEAR->label uncl))]
    (<r v v)))

(defn expand-fdna
  [fdna]
  {:pre [(fdna? fdna)]}
  (let [vars (FDNA->varlist fdna)
        dna  (FDNA->dna fdna)]
    (if (empty? vars)
      (<-> dna)
      (let [vdict (calc/dna->vdict dna {})]
        (apply <-> (map (fn [[vpoint res]]
                          (apply <>
                                 (<> (const->expr res))
                                 (map #(<> ((const->isolator %1) %2)) vpoint vars)))
                        vdict))))))

(defn expand-seq-reentry
  [seq-re]
  {:pre [(seq-reentry? seq-re)]}
  (let [signature         (SEQ-REENTRY->sign seq-re)
        [x & ys :as ctxs] (SEQ-REENTRY->ctxs seq-re)
        {:keys [parity open?]} (seq-reentry-sign->opts signature)
        res-odd?  (odd? (count ctxs))
        pre-step? (and res-odd? (not (= parity :even)))
        re   [:f* (apply nested-expr {:unmarked? false :rightwards? false}
                         (vec (cons :f* x)) (if res-odd?
                                              (concat ys (cons x ys))
                                              ys))]
        pre  (when pre-step?
               [(if open? :f2 :f1)
                (apply nested-expr {:unmarked? false :rightwards? false}
                       (vec (cons :f* x)) ys)])
        open (when open?
               [:f1 (apply nested-expr {:unmarked? true :rightwards? false}
                           (vec (cons (if pre-step? :f2 :f*) x)) ys)])
        init (if (or pre-step? open?) :f1 :f*)]
    (<mem> (vec (remove nil? [re pre open])) init)))

(defn expand-memory
  [mem]
  {:pre [(memory? mem)]}
  (let [eqs (map (fn [[k v]] (<> (apply <> k v) (<> (<> k) (apply <> v))))
              (MEMORY->rems mem))]
    (FORM (apply <-> eqs) (apply <> (MEMORY->ctx mem)))))

;;-------------------------------------------------------------------------
;; context -> content

(defn expand-expr
  [expr]
  (if (<= (count expr) 1)
    (first expr)            ;; [x] => x | [] => nil
    (list (sequence expr))) ;; [x y …] => ((x y …))
  ; (FORM (apply FORM expr))
  )

; (defn ctx< [ctx]
;   (...expand context recursively))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduction

;; Observations are accumulated contents in reduction

(defn- observed-put [env x]
  (update env :observed #(if (nil? %) #{x} (conj % x))))

(defn- observed-disj [env x]
  (update env :observed #(if (nil? %) % (disj % x))))

(defn- observed-merge [env xs]
  (update env :observed #(if (nil? %)
                           (set xs)
                           (set/union % (set xs)))))

(defn- observed-exclude [env xs]
  (update env :observed #(if (nil? %)
                           %
                           (set/difference % (set xs)))))


(defn- observed-get [env]
  (get env :observed))

(defn- observed-has? [env x]
  (not= (get-in env [:observed x] :not-found)
        :not-found))


;;-------------------------------------------------------------------------
;; content -> content

(declare reduce-context)
(declare reduce-content)

(defn- reduce-matching-content
  "Tries to reduce given content `x` to its simplest FORM if it matches a simple equivalent representation."
  ([x] (reduce-matching-content x x))
  ([x default]
   (case x ;; [] = '() in cases! (cljs doesn’t like ((…)) in cases)
     ([] :M [nil] [:N] [[[]]] [[:M]] [[[nil]]] [[[:N]]]
         ((:U :I)) ((:I :U)))
     '()
     (nil :N [[]] [:M] [[nil]] [[:N]]
          (:U :I) (:I :U))
     nil
     (:mn :U [:I] [[:mn]] [[:U]] [[[:I]]])
     :mn
     ([:mn] :I [:U] [[[:mn]]] [[:I]] [[[:U]]])
     '(:mn)
     default)))

(defn reduce-form
  [form env]
  (let [ctx (reduce-context form env)]
    (reduce-matching-content (sequence ctx))))


(defn- reduce-rems
  [rems env]
  (if (empty? rems)
    [[] env]
    (loop [[[k v] & r] rems
           rems-reduced []
           env env]
      (assert (vector? v))
      (let [v (reduce-context v env)
            rems-reduced (conj rems-reduced [k v])
            env (assoc env k (if (== 1 (count v))
                               (first v) v))]
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
                (if (= (list v) x) ;; remove self-reference [x [x]]
                  [acc rem-vars (rest varlist-next)]
                  (let [opts {:vars (set varlist-next)}
                        vars (cond
                               (expr? x) (find-vars x opts)
                               (form? x) (find-vars (<-> x) opts)
                               :else '())]
                    [(conj acc rem)
                     (disj (into rem-vars vars) v)
                     (rest varlist-next)]))
                [acc rem-vars (rest varlist-next)]))
            [[] rem-vars (map first (rest rev-rems))]
            rev-rems))))

(defn reduce-memory
  [mem env]
  (let [[rems env] (reduce-rems (MEMORY->rems mem) env)
        ctx  (reduce-context (MEMORY->ctx mem) env)
        rems (filter-rems rems ctx)]
    (if (empty? rems)
      (expand-expr ctx)
      (apply MEMORY rems ctx))))


(declare reduce-context-chain)

(defn reduce-seq-reentry
  [seq-re env]
  (let [sign (SEQ-REENTRY->sign seq-re)
        ctxs (SEQ-REENTRY->ctxs seq-re)
        {:keys [parity open? interpr] :as specs} (seq-reentry-sign->opts sign)
        ;; ! check if mn from environment can be equivalent to mn in ctxs
        [x y z & r :as ctxs] (reduce-context-chain {:rtl? true} ctxs env)
        >< (fn [expr] (expand-expr (reduce-context expr env)))
        U UFORM, I IFORM]
    (if (= x [MARK])
      ;; rule of dominance applies for innermost mark:
      (>< (apply nested-expr {:unmarked? open? :rightwards? false}
                 ctxs))
      ;; try all possible cases for re-entry reduction:
      (if (empty? r)
        (match [interpr open? parity  x y z]
          ;; primitive cases:
          [_ _     :even [] nil nil] U ;; ((f))
          [_ _     _     [] nil nil] I ;; (f)
          [_ false _     [] []  nil] U ;; ((f))
          [_ true  _     [] []  nil] I ;; (((f))) => (f)

          ;; if interpretation of `mn` is “recursive identity”
          ;; and `f = ((f))` can be separated from the rest:
          ;; f x      |  (f) x 
          [:rec-ident true  :even [& xs] nil nil] (>< (apply <-> U xs))
          [:rec-ident true  _     [& xs] nil nil] (>< (apply <-> I xs))
          ;; (f) x    |  ((f x))
          [:rec-ident true  _     [] [& ys] nil]  (>< (apply <-> I ys))
          [:rec-ident false _     [& xs] [] nil]  (>< (apply <-> U xs))
          ;; ((f x))  |  (((f) x))
          [:rec-ident false :even [] [& ys] []]   (>< (apply <-> U ys))
          [:rec-ident false _     [] [& ys] []]   (>< (apply <-> I ys))

          ;; by case distinction:
          [_ _     _     [(:or :mn ([:mn] :seq))] [] nil]  I ;; ((f U/I))
          [_ _     _     [] [(:or :mn ([:mn] :seq))] nil]  U ;; ((f) U/I)

          [_ false :even [(:or :mn ([:mn] :seq))] nil nil] U ;; (f U/I)
          [_ true  :even [(:or :mn ([:mn] :seq))] nil nil] I ;; (f U/I)
          [_ true  _     [(:or :mn ([:mn] :seq))] nil nil] U ;; (f U/I)
          [_ false _     [(:or :mn ([:mn] :seq))] nil nil] I ;; (f U/I)

          [_ false :even [] [(:or :mn ([:mn] :seq))] []]   I ;; (((f) U/I))
          [_ true  :even [] [(:or :mn ([:mn] :seq))] []]   U ;; (((f) U/I))
          [_ true  _     [] [(:or :mn ([:mn] :seq))] []]   I ;; (((f) U/I))
          [_ false _     [] [(:or :mn ([:mn] :seq))] []]   U ;; (((f) U/I))

           ;; if nothing applies, return the reduced seq-reentry FORM:
          :else (apply SEQ-REENTRY specs ctxs))
        (apply SEQ-REENTRY specs ctxs)))))


(defn- filter-fdna
  "Filters the `dna` by values from given `env` whose keys match variables in the `varlist` of the formDNA."
  [fdna env]
  (let [;; assumes only constant values
        ;; ? what about unevaluated FORMs and formDNA?
        matches (map #(let [v (get env % :_)
                            v (if (or (= :_ v) (calc/const? v))
                                v
                                (let [expr (if (expr? v)
                                             v (make-expr v))]
                                  (expr->const expr)))]
                        (vector % v))
                     (FDNA->varlist fdna))
        vpoint (map second matches)]
    (FDNA (mapv first (filter #(= (second %) :_) matches))
          (calc/filter-dna (FDNA->dna fdna) vpoint))))

(defn reduce-fdna
  [fdna env]
  (let [fdna (filter-fdna fdna env)]
    (if (empty? (FDNA->varlist fdna))
      (FDNA->dna fdna)
      fdna)))

(defn reduce-unclear
  [uncl env]
  (let [lbl  (UNCLEAR->label uncl)
        fdna (FDNA [lbl] (calc/make-dna :N :U :U :U))]
    (reduce-fdna fdna env)))

(defn- reduce-content
  [x env]
  (let [res (reduce-matching-content x :failed)]
    (if (not= :failed res)
      res
      (cond
        (fdna? x)        (reduce-fdna x env)
        (memory? x)      (reduce-memory x env)
        (unclear? x)     (reduce-unclear x env)
        (seq-reentry? x) (reduce-seq-reentry x env)
        (pure-form? x)   (reduce-form x env)
        :else x))))


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
    (let [[seenU? seenI?] [(observed-has? env UFORM) (observed-has? env IFORM)]
          [seenU? env-next]
          (if (and (not seenU?)
                   (some #{:mn :U '(:I) '((:mn)) '((:U)) '(((:I)))} ctx))
            [true (observed-put env UFORM)]
            [seenU? env])]
      (if (and seenU? seenI?)
        ['( () ) env-next]
        ;; else find and remember IFORM in the expression
        ;; match with UFORM if seen
        (let [[seenI? env-next]
              (if (and (not seenI?)
                       (some #{'(:mn) :I '(:U) '(((:mn))) '((:I)) '(((:U)))} ctx))
                [true (observed-put env-next IFORM)]
                [seenI? env-next])]
          (if (and seenU? seenI?)
            ['( () ) env-next]
            ;; else return the input expression with distinct elements
            [(let [ctx (distinct ctx)
                   obs (observed-get env)]
               (if (nil? obs)
                 ctx
                 (remove #(and (obs %) (not (env %))) ctx)))
             env-next]))))))

(defn- reduce-by-crossing
  "Tries to reduce some `((x))` to `x` for each FORM in given context."
  [ctx]
  (let [f (fn [ctx x]
            (if (pure-form? x)
              (let [[x' & r] x]
                (if (and (pure-form? x') (empty? r))
                  (into [] (concat ctx x'))
                  (conj ctx x)))
              (conj ctx x)))]
    (reduce f [] ctx)))

(defn reduce-context-chain
  "Reduces a sequence of FORM contexts, intended to be linked in a nesting-chain, to a sequence of reduced contexts, possibly merged or shortened via inference.
  - assumes rightward-nesting, e.g. `(…(…(…)))`
  - for leftward-nesting, e.g. `(((…)…)…)`, pass `{:rtl? true}`"
  ([ctxs env] (reduce-context-chain {} ctxs env))
  ([{:keys [rtl?] :or {rtl? false}} ctxs env]
   (vec
    (loop [[ctx & r] (if rtl? (reverse ctxs) ctxs)
           env       env
           red-ctxs  (if rtl? '() [])]
      (let [ctx      (reduce-context ctx env)
            red-ctxs (if (and (> (count red-ctxs) 1)
                              (empty? ((if rtl? first last) red-ctxs)))
                       ;; reduce by crossing
                       (if rtl?
                         (let [[xs before] (split-at 2 red-ctxs)]
                           (conj before
                                 (vec (concat ctx (second xs)))))
                         (let [[before xs] (split-at (- (count red-ctxs) 2)
                                                     red-ctxs)]
                           (conj (vec before)
                                 (vec (concat (first xs) ctx)))))
                       (conj red-ctxs ctx))
            ;; needs upstream env to reduce with observed values
            ;; ? is there a cleaner approach than using meta
            env (:env-next (meta ctx))]
        (cond
          (= ctx '( () )) red-ctxs
          (empty? r)      red-ctxs
          :else (recur r env red-ctxs)))))))


(defn- substitute-from-env
  [ctx env]
  (loop [ctx  ctx
         i    0]
    (let [ctx' (map #(let [x (get env % :not-found)]
                       (if (= x :not-found) % x)) ctx)]
      (cond
        (= ctx' ctx) (merge-ctx ctx')
        (> i 400)    (throw (ex-info "Too many substitutions! Possibly caused by two mutually recursive associations in the env."
                                     {:type :infinite-substitution}))
        :else (recur ctx' (inc i))))))

;; ? add optim option to not observe values for (de)generation
;; ? add option for assumption mn ≠ mn to prevent relation of U/I
(defn- reduce-context
  [ctx env]
  (let [env (update env :depth #(if (nil? %) 0 (inc %)))]
    (if (< (:depth env) 400)
      ;; substitute matches from env upfront
      (loop [ctx (substitute-from-env ctx env)
             i   0]
        (let [[ctx env-next] (reduce-by-calling ctx env)]
          (if (or (= ctx '( () )) (= ctx '()))
            (vary-meta (seq->expr ctx false)
                       #(assoc % :env-next env-next))
            (let [env-next (observed-merge env-next ctx)
                  ctx' (->> (map #(reduce-content
                                   % (observed-disj env-next %))
                                 ctx)
                            reduce-by-crossing
                            (remove nil?))]
              (cond
                (= ctx' ctx) (vary-meta (seq->expr ctx' false)
                                        #(assoc % :env-next env-next))
                (> i 3)      (throw (ex-info "Too many reduction attempts!"
                                             {:type :infinite-reduction}))
                :else (recur ctx' (inc i)))))))
      (throw (ex-info "Context too deeply nested, possibly caused by a self-contradicting re-entry definition." {:type :stack-overflow})))))

(defn- reduce-env
  [env]
  (update-vals env #(if (expr? %)
                      (reduce-context % {})
                      (reduce-content % {}))))

;; ? should env be reduced completely before substitution?
(defn cnt>
  "Reduces a FORM content recursively until it cannot be further reduced.
  All reductions are justified by the axioms of FORM logic.
  - if `x` is a complex FORM, calls `reduce-context` on `x`
  - if no reduction applies, tries to retrieve the value from given `env`
  - if retrieval was unsuccessful, returns `x` as is"
  ([x]     (cnt> x {}))
  ([x env] (expand-expr
             (reduce-context (make-expr x) (reduce-env env)))))

(defn ctx>
  "Reduces a FORM context recursively until it cannot be further reduced.
  All reductions are justified by the axioms of FORM logic.
  - for complex expressions, calls `reduce-content` on every unique element"
  ([ctx]     (ctx> ctx {}))
  ([ctx env] (vec (reduce-context ctx (reduce-env env)))))

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
   (let [[res & more :as ctx] (ctx> expr env)
         v (when (empty? more)
             (case res
               ([ ]   :M) calc/M
               (nil   :N) calc/N
               (:mn   :U) calc/U
               ([:mn] :I) calc/I
               nil))
         m {:expr ctx :env env}]
     (if (some? v)
       (with-meta [ v ] m)
       (with-meta [ :_ ] m)))))

(defn =>*
  "Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.
  - if `to-fdna?` is false, returns a seq of results as returned by `=>` in the order of the corresponding `vspace` ordering"
  ([expr] (=>* expr {}))
  ([expr {:keys [to-fdna? vars] :or {to-fdna? true}}]
   (when-not (expr-tag? expr)
     (throw (ex-info "missing `:expr` tag on input expression" {:input expr})))
   (let [vars (if (nil? vars) (find-vars expr {:ordered? true}) vars)
         vspc (calc/vspace (count vars))
         all-envs (fn [vars]
                    (map (comp (partial apply hash-map)
                               (partial interleave vars)) vspc))
         envs (all-envs vars)]
     (if to-fdna?
       (let [consts (mapv (comp first (partial => expr)) envs)]
         ;; ? dna or dna-seq (performance?)
         (<fdna> (vec vars) (calc/consts->dna (rseq consts))))
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
  (let [rs (=>* expr {:to-fdna? false})
        {:keys [vars vspc]} (meta rs)]
    (with-meta
      (->> rs
           (map first)
           (map vector vspc)
           (into (if sorted? (sorted-map-by calc/compare-dna) (hash-map))))
      {:vars vars})))




(comment
  (ctx> (<r)) ;=> [(:mn)]
  (ctx> (expand-seq-reentry (first (<r))))
  ;; [:<re]
  ;; => [[:mem ([:f* [((:f*))]] [:f1 [(:f*)]]) :f1]]
  ;; => [(:f*)]

  (ctx> (<r_)) ;=> [(:mn)]
  (ctx> (expand-seq-reentry (first (<r_))))
  ;; => [[:mem ([:f* [((:f*))]] [:f2 [(:f*)]] [:f1 [:f2]]) :f1]]
  ;; => [(:f*)]

  (ctx> (<2r)) ;=> [:mn]
  (ctx> (expand-seq-reentry (first (<2r))))
  ;; => [[:mem ([:f* [((:f*))]]) :f*]]
  ;; => [:f*]

  (ctx> (<2r_)) ;=> [:mn]
  (ctx> (expand-seq-reentry (first (<2r_))))
  ;; => [[:mem ([:f* [((:f*))]] [:f1 [:f*]]) :f1]]
  ;; => [:f*]

  (ctx> (<2r+1)) ;=> [(:mn)]
  (ctx> (expand-seq-reentry (first (<2r+1))))
  ;; => [[:mem ([:f* [((:f*))]] [:f1 [(:f*)]]) :f1]]
  ;; => [(:f*)]

  (ctx> (<2r+1_)) ;=> [(:mn)]
  (ctx> (expand-seq-reentry (first (<2r+1_))))
  ;; => [[:mem ([:f* [((:f*))]] [:f2 [(:f*)]] [:f1 [:f2]]) :f1]]
  ;; => [(:f*)]


  (ctx> (<r ['a])) ;=> [[:<re [a]]]
  (ctx> (expand-seq-reentry (first (<r ['a]))))
  ;;=> [[:mem ([:f* [((:f* a) a)]] [:f1 [(:f* a)]]) :f1]]
  ;; ERROR too deeply nested

  (ctx> (<r [:U])) ;=> [(:mn)]
  (ctx> (expand-seq-reentry (first (<r [:U]))))
  ;;=> [[:mem ([:f* [((:f* :U) :U)]] [:f1 [(:f* :U)]]) :f1]]
  ;; ERROR too deeply nested

  )

(comment
  (<r ['l] ['e] ['r])
  (SEQ-REENTRY {} ['l] ['e] ['r])
  )