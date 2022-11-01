(ns formform.expr
  (:require #_[clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [clojure.set :refer [map-invert]]
            [clojure.core.match :refer [match]]
            [formform.calc :as calc]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen]
            ))


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

(s/def :formform.specs.expr/memory
  (s/and vector? #(= (first %) :mem)))

(def expr? (partial s/valid? :formform.specs.expr/expr))
(def form? seq?)

(defn variable? [x] (or (string? x) (symbol? x)))

(def unclear? (partial s/valid? :formform.specs.expr/unclear))
(def seq-reentry? (partial s/valid? :formform.specs.expr/seq-reentry))
(def fdna? (partial s/valid? :formform.specs.expr/fdna))
(def memory? (partial s/valid? :formform.specs.expr/memory))


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
  (some? (try (into {} xs) (catch Exception _ nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content

(def NONE nil)
(def MARK '())


(defn FORM
  ([] MARK)
  ([x & ys] (seq (merge-ctx (cons x ys)))))

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
  [specs & ctxs]
  (let [signature
        (cond
          (keyword? specs) (if (seq-reentry-signature? specs)
                             specs
                             (throw
                               (ex-info "Invalid re-entry signature." {})))
          (map? specs) (seq-reentry-opts->sign specs)
          :else (throw (ex-info
                         "Invalid re-entry specifications." {})))]
    (apply vector signature ctxs)))

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

(defn FDNA-filter-dna
  "Filters the `dna` by values from given `vdict` whose keys match variables in the `varlist` of the formDNA."
  [fdna vdict]
  (let [varlist (FDNA->varlist fdna)
        ;; assumes only constant values
        ;; what about unevaluated FORMs and formDNA?
        matches (remove nil? (map #(let [v (get vdict % :_)]
                                     (assert (or (= :_ v) (calc/const? v)))
                                     (vector % v)) varlist))
        vpoint  (map second matches)]
    (FDNA (mapv first (filter #(= (second %) :_) matches))
      (calc/filter-dna (FDNA->dna fdna) vpoint))))





(defn MEMORY
  [kv-pairs & xs]
  {:pre [(rem-pairs? kv-pairs)]}
  (apply vector :mem kv-pairs (merge-ctx xs)))

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

;; ? redundant
(defn get-ctx [expr] expr)

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

(defn- nest-left [ctxs]
  (if (empty? ctxs)
    '()
    (loop [r      (rest ctxs)
           nested (seq (merge-ctx (first ctxs)))]
      (if (empty? r)
        nested
        (let [[ctx & r] r
              nested  (seq (merge-ctx (concat [nested] ctx)))]
          (if (empty? r)
            nested
            (recur r nested)))))))

(defn- nest-right [ctxs]
  (if (empty? ctxs)
    '()
    (let [[ctx & r] ctxs]
      (if (empty? r)
        (seq (merge-ctx ctx))
        (seq (merge-ctx (concat ctx [(nest-right r)])))))))

(defn- chain [ctxs]
  (map #(seq (merge-ctx %)) ctxs))


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
(def ·mem   (comp make-expr MEMORY))

(def ·N (make-expr calc/N))
(def ·M (make-expr calc/M))
(def ·U (make-expr calc/U))
(def ·I (make-expr calc/I))

(def const->expr {:N ·N :U ·U :I ·I :M ·M})
(def expr->const {·N :N ·U :U ·I :I ·M :M})

(defn nested-expr
  "Nests contexts leftwards `(((…)a)b)` or rightwards `(a(b(…)))` if `{:rightwards? true}`
  - a context must be either a vector `[x y …]` or an expression `(·· x y …)`"
  [{:keys [unmarked? rightwards?]
    :or {unmarked? false rightwards? false}
    :as env} & ctxs]
  (let [f-nested (if rightwards? (nest-right ctxs) (nest-left ctxs))]
    (apply make-expr (if unmarked? f-nested (list f-nested)))))

;; ? choose different term than “chain”
(defn chained-expr
  "Chains contexts like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`
  - a context must be either a vector `[x y …]` or an expression `(·· x y …)`"
  [{:keys [unmarked?] :or {unmarked? false} :as opts} & ctxs]
  (let [f-chained (chain ctxs)]
    (if unmarked?
      (apply make-expr f-chained)
      (make-expr (apply · f-chained)))))

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
  (let [signature         (SEQ-REENTRY->sign seq-re)
        [x & ys :as ctxs] (SEQ-REENTRY->ctxs seq-re)
        {:keys [parity open? interpr]} (seq-reentry-sign->opts signature)
        ctxs      (if (zero? (count ctxs)) (cons [] ctxs) ctxs)
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
    (·mem (vec (remove nil? [re pre open])) init)))

(defn expand-memory
  [mem]
  {:pre [(memory? mem)]}
  (let [eqs (map (fn [[k v]] (· (· k v) (· (· k) (· v))))
              (MEMORY->rems mem))]
    (FORM (apply ·· eqs) (apply · (MEMORY->ctx mem)))))

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

(declare reduce-context)
(declare reduce-content)

(defn- reduce-rems
  [rems env]
  (if (empty? rems)
    [[] env]
    (loop [[[k v] & r] rems
           rems-reduced []
           env env]
      (let [v-red (if (expr? v)
                    (reduce-context v env)
                    (reduce-content v env))
            rems-reduced (conj rems-reduced [k v-red])
            env (assoc env k v-red)]
        (if (empty? r)
          [rems-reduced env]
          (recur r rems-reduced env))))))

(declare reduce-context-chain)

(defn- reduce-seq-reentry
  [seq-re env]
  (let [sign (SEQ-REENTRY->sign seq-re)
        ctxs (SEQ-REENTRY->ctxs seq-re)
        {:keys [parity open? interpr] :as specs} (seq-reentry-sign->opts sign)
        [x y z & r :as ctxs] (reduce-context-chain {:rtl? true} ctxs env)
        >< (fn [expr] (expand-expr (reduce-context expr env)))
        U UFORM, I IFORM]
    (if (= x [MARK])
      ;; rule of dominance applies for innermost mark:
      (>< (apply nested-expr {:unmarked? open? :rightwards? false}
                 (rest ctxs)))
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
          [:rec-ident true  :even [& xs] nil nil] (>< (apply ·· U xs))
          [:rec-ident true  _     [& xs] nil nil] (>< (apply ·· I xs))
          ;; (f) x    |  ((f x))
          [:rec-ident true  _     [] [& ys] nil]  (>< (apply ·· I ys))
          [:rec-ident false _     [& xs] [] nil]  (>< (apply ·· U xs))
          ;; ((f x))  |  (((f) x))
          [:rec-ident false :even [] [& ys] []]   (>< (apply ·· U ys)) 
          [:rec-ident false _     [] [& ys] []]   (>< (apply ·· I ys))

          ;; by case distinction:
          [_ _     _     [(:or U I)] [] nil]  I ;; ((f U/I))
          [_ _     _     [] [(:or U I)] nil]  U ;; ((f) U/I)

          [_ false :even [(:or U I)] nil nil] U ;; (f U/I)
          [_ true  _     [(:or U I)] nil nil] U ;; (f U/I)
          [_ true  :even [(:or U I)] nil nil] I ;; (f U/I)
          [_ false _     [(:or U I)] nil nil] I ;; (f U/I)

          [_ false :even [] [(:or U I)] []]   I ;; (((f) U/I))
          [_ true  _     [] [(:or U I)] []]   I ;; (((f) U/I))
          [_ true  :even [] [(:or U I)] []]   U ;; (((f) U/I))
          [_ false _     [] [(:or U I)] []]   U ;; (((f) U/I))

           ;; if nothing applies, return the reduced seq-reentry FORM:
          :else (apply SEQ-REENTRY specs ctxs))
        (apply SEQ-REENTRY specs ctxs)))))

(defn- reduce-content
  [x env]
  (let [res (reduce-matching-content x :failed)]
    (if (not= :failed res)
      res
      (cond
        (fdna? x)        (let [fdna (FDNA-filter-dna x env)]
                           (if (empty? (FDNA->varlist fdna))
                             (FDNA->dna fdna)
                             fdna))
        (memory? x)      (let [[rems env] (reduce-rems (MEMORY->rems x) env)
                               ctx (reduce-context (MEMORY->ctx x) env)]
                           (if (empty? rems)
                             (expand-expr ctx)
                             (apply MEMORY rems ctx)))
        (unclear? x)     (FDNA [(UNCLEAR->label x)]
                               (calc/make-dna :N :U :U :U))
        (seq-reentry? x) (let [res (reduce-seq-reentry x env)]
                           res)
        (form? x)        (let [ctx (reduce-context x env)]
                           (reduce-matching-content (sequence ctx)))
        :else (let [interpr (get env x :not-found)] ;; nil punning!
                (if (= interpr :not-found)
                  x
                  (let [env' (dissoc env x)]
                    (cond
                  ; (keyword? interpr) interpr
                  ;; ? reduce env at beginning, then just substitute value?
                      (expr? interpr) (let [ctx (reduce-context interpr env')]
                                        (expand-expr ctx))
                      :else (reduce-content interpr env')))))))))


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
                 (remove obs ctx)))
             env-next]))))))

(defn- reduce-by-crossing
  "Tries to reduce some `((x))` to `x` for each FORM in given context."
  [ctx]
  (let [f (fn [ctx x]
            (if (form? x)
              (let [[x' & r] x]
                (if (and (form? x') (empty? r))
                  (into [] (concat ctx x'))
                  (conj ctx x)))
              (conj ctx x)))]
    (reduce f [] ctx)))

;; ? obsolete
(defn merge-chain [ctxs]
  (reduce (fn [merged ctx]
            (if (and (> (count merged) 1) (empty? (last merged)))
              (let [[before xs] (split-at (- (count merged) 2) merged)]
                (conj (vec before)
                      (vec (concat (first xs) ctx))))
              (conj merged ctx)))
          (vector (first ctxs))
          (rest ctxs)))

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

;; ? add optim option to not observe values for (de)generation
(defn- reduce-context
  [ctx env]
  (let [env (update env :depth #(if (nil? %) 0 (inc %)))]
    (if (< (:depth env) 500)
      (loop [ctx ctx
             i   0]
        (let [[ctx env-next] (reduce-by-calling ctx env)]
          (if (= ctx '( () ))
            (vary-meta (seq->expr ctx false)
                       #(assoc % :env-next env-next))
            (let [env-next (observed-merge env-next ctx)
                  ctx' (->> (map #(reduce-content % (observed-disj env-next %))
                                 ctx)
                            reduce-by-crossing
                            (remove nil?))]
              (cond
                (= ctx' ctx) (vary-meta (seq->expr ctx' false)
                                        #(assoc % :env-next env-next))
                (> i 3)      (throw (ex-info "Too many reduction attempts!"
                                             {:expr (seq->expr ctx' false)}))
                :else (recur ctx' (inc i)))))))
      (throw (ex-info "Context too deeply nested, possibly caused by a self-contradicting re-entry definition." {:expr (seq->expr ctx false)})))))


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
   (let [[res & more :as ctx] (ctx> expr env)
         v (when (empty? more)
             (case res
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



(comment
  (and

   (= (reduce-context-chain [(·· 'a 'b) (·· 'a 'c)] {})
      '[[a b] [c]])

   (= (reduce-context-chain [(·· 'a 'b) (·· 'a 'c) (·· 'c 'a 'd)] {})
      '[[a b] [c] [d]])

   (= (reduce-context-chain [(·· ·uform 'b) (·· 'a ·iform)] {})
      '[[:mn b] [()]])

   (= (reduce-context-chain [(·· ·uform 'b) (·· 'a ·iform) (·· 'c)] {})
      '[[:mn b] [()]])

   (= (reduce-context-chain {:rtl? true}
                            [(·· 'c) (·· 'a ·iform) (·· ·uform 'b)] {})
      '[[()] [:mn b]])

   (= (reduce-context-chain {:rtl? true}
                            [(·· :f*) (·· ·uform 'a) (·· ·uform 'b)] {})
      '[[:f*] [a] [:mn b]])

   (= (reduce-context-chain {:rtl? true}
                            [(·· :f*) (·· ·uform) (·· ·uform)] {})
      '[[:f* :mn]])

   ))

(comment
  (and

   (= (ctx> (·dna ['a] :NUIM) {'a :U})
      '[(:mn)])

   (= (FDNA-filter-dna (FDNA ['a] :NUIM) {'x :M})
      '[:fdna [a] :NUIM])

   (= (FDNA-filter-dna (FDNA ['a] :NUIM) {'a :M})
      '[:fdna [] :N])

   (= (FDNA-filter-dna (FDNA ['a 'b] (calc/consts->dna [:N :U :I :M
                                                        :U :I :M :I
                                                        :I :M :I :U
                                                        :M :I :U :N]))
                       {'a :U})
      '[:fdna [b] :IMIU])
   ))

(comment
  (and

   (= (ctx> (·uncl "hey"))
      '[[:fdna ["hey"] :NUUU]])

   (= (ctx> (·uncl "hey") {"hey" :M})
      '[])

   (= (expand-unclear (first (·uncl "hey")))
      '[[:<re ["hey"] ["hey"]]])

   ))

(comment

  (and

   (= (ctx> (·mem [['a :M]] 'a))
      (ctx> (·mem [['a MARK]] 'a))
      (ctx> (·mem [['a ·mark]] 'a))
      (ctx> (·mem [['a (·· MARK)]] 'a))
      '[()])

   (= (ctx> (·mem [['a :N]] 'a 'b) {'b :U})
      '[:mn])

   (= (ctx> (·mem [['a :U]] (· 'b) 'a))
      '[(b) :mn])

   (= (ctx> (·mem [['a :U]] (· 'a) 'b))
      (ctx> (·· (·mem [['a :U]] (· 'a)) 'b))
      '[(:mn) b])

   (= (ctx> (·· (·mem [['a :U]] 'a) 'b))
      '[:mn b])

   (= (ctx> (·· (·mem [['x :N]] 'y)))
      '[y])

   (= (ctx> (·· 'x) {'x 'y})
      '[y])

   ;; ? should rems have priority over degeneration from env
   (= (ctx> (·· 'y (·mem [['y 'z]] 'y)))
      '[y])
   (= (ctx> (·· 'x (·mem [['y 'z]] 'x)) {'x 'y})
      (ctx> (·· 'x (·mem [['x 'z]] 'x)) {'x 'y})
      '[y z])

  ;; ? infinite recursion or dissoc from env on first interpretation
   (= (ctx> (·· 'x) {'x 'x})
      (ctx> (·· 'x) {'x (·· 'x)})
      '[x])
  ;; ? should an already interpreted variable be interpreted again in the 
  ;;   outer FORM
  ;; ? is this problematic for nested re-entries?
   (= (ctx> (·· 'x (·mem [['y 'x]] 'x)) {'x 'y})
      '[y])

   )

  (and

   (= (ctx> (·mem [[:x (·· (·· :x))]] :x))
      '[[:mem [[:x [:x]]] :x]])
   (= (ctx> (·mem [[:x (·· :x)]] :x))
      '[[:mem [[:x [:x]]] :x]])

  ;; (= ctx' ctx) because reduce-by-calling:
   (= (ctx> (·mem [[:x (·· :x 'a)]] :x))
      '[[:mem [[:x [:x a]]] :x a]])
   (= (ctx> (·mem [[:x (·· :x)]] :x))
      '[[:mem [[:x [:x]]] :x]])

  ;; infinite recursion because outer expr env nullifies previous dissoc:
   (= (ctx> (·mem [[:x (· :x)]] :x))
      '[[:mem [[:x [(:x)]]] (:x)]])
   (= (ctx> (·mem [[:x (·· (· :x))]] :x))
      (ctx> (·mem [[:x (· (·· :x))]] :x))
      '[[:mem [[:x [(:x)]]] (:x)]])

   ))

(comment
  (reduce-seq-reentry (first (·r)) {})
  (reduce-content (·r) {})
  (and

   (= (ctx> (·r)) '[(:mn)])
   (= (ctx> (·2r)) '[:mn])
   (= (ctx> (·2r+1)) '[(:mn)])
   (= (ctx> (··r)) '[(:mn)])
   (= (ctx> (··2r)) '[:mn])
   (= (ctx> (··2r+1)) '[(:mn)])

   )
  (and
   
   (= (ctx> (·r ['a])) '[[:<re [a]]])
   (= (ctx> (·2r ['a])) '[[:<..re [a]]])
   (= (ctx> (·2r+1 ['a])) '[[:<..re. [a]]])
   (= (ctx> (··r ['a])) '[[:<re_ [a]]])
   (= (ctx> (··2r ['a])) '[[:<..re_ [a]]])
   (= (ctx> (··2r+1 ['a])) '[[:<..re._ [a]]])
   ))

(comment
  (nested-expr {} ['a] ['b])
  (nested-expr {:unmarked? true} ['a] ['b])

  (nested-expr {} [:f 'a] [] ['b] [])
  (reduce-context-chain {:rtl? true}
                        [[:f 'a] [] []] {})

  (case [[1] [2] []]
   [[_] [_] []] true
    false)

  (expand-seq-reentry (first (··2r [] ['x] [])))


  (expand-seq-reentry (first (·r' [UFORM] [])))
  ;; [[:mem [[:f* [((:f* :mn))]]] :f*]]
  ;; ((f* U)) => f* U
  (reduce-seq-reentry (first (·r' [UFORM] [])) {})


  (expand-seq-reentry (first (·r' ['a])))
  '[[:mem [[:f* [((:f* a) a)]] [:f1 [(:f* a)]]] :f1]]
  ;; f* = ((f*) a), f1 = (f* a)
  ;; (((f*)) a) => (f* a)

  (reduce-seq-reentry (first (·r' ['a])) {})
  (reduce-seq-reentry (first (·2r' ['a])) {})
  (reduce-seq-reentry (first (·2r'+1 ['a])) {})
  ;; (f a)

  (expand-seq-reentry (first (··r' ['a])))
  '[[:mem [[:f* [((:f* a) a)]] [:f2 [(:f* a)]] [:f1 [:f2 a]]] :f1]]
  ;; f* = ((f*) a), f2 = (f* a), f1 = f2 a
  ;; (((f*))) a => (f*) a

  ;; f a
  (reduce-seq-reentry (first (··r' ['a])) {})
  (reduce-seq-reentry (first (··2r' ['a])) {})
  (reduce-seq-reentry (first (··2r'+1 ['a])) {})

  (reduce-seq-reentry (SEQ-REENTRY {} ['a] ['a]) {'a :N})

  (reduce-seq-reentry 
    (SEQ-REENTRY {} ['a] []) {'a :M})
  (reduce-seq-reentry
    (SEQ-REENTRY {} [MARK 'a] ['a]) {})
  (reduce-seq-reentry
    (SEQ-REENTRY {} ['b] [MARK 'a] ['a]) {})

  (reduce-seq-reentry
    (SEQ-REENTRY {} ['b] [IFORM 'a] ['a]) {})

  (reduce-seq-reentry
    (SEQ-REENTRY {} ['b IFORM] ['a] [UFORM]) {})


  (ctx> [ '((a) a) ])

  (reduce-context-chain {:rtl? true}
                        (SEQ-REENTRY->ctxs (SEQ-REENTRY {} ['a] ['a])) {})



  )

