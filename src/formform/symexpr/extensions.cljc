;; ========================================================================
;;     formform symbolic expression extensions module
;;     -- created 03/2023, (c) Peter Hofmann
;; ========================================================================

(ns formform.symexpr.extensions
  (:require
    [clojure.spec.alpha :as s]
    #?(:clj  [clojure.core.match :refer [match]]
       :cljs [cljs.core.match :refer-macros [match]])
    [formform.symexpr.common 
     :refer [tag_memory tag_formDNA tag_unclear tag_arrangement 
             tag_seq-reentry]]
    #?(:clj  [formform.symexpr.core :as symx 
              :refer [defoperator defsymbol
                      op-get op-data op-spec valid-op? interpret-op make-op
                      ]]
       :cljs [formform.symexpr.core :as symx 
              :refer [op-get op-data op-spec valid-op? interpret-op make-op]
              :refer-macros [defoperator defsymbol]])
    [formform.expr :as expr :refer [make form]]
    [formform.utils :as utils]))


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
                 (utils/nest-right expr/splice-ctx exprs)
                 (utils/nest-left expr/splice-ctx exprs))]
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
      (let [[expr m] (expr/ctx->cnt {:+meta? true}
                               (expr/simplify-context (expr/cnt->ctx expr) env))
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
                                      expr (expr/ctx->cnt
                                            {} ;; ? correct env
                                            (expr/simplify-context ctx {}))]
                                  (conj before expr))
                                (let [[before xs] (split-at
                                                   (- (count simpl-chain) 2)
                                                   simpl-chain)
                                      ctx  [(first xs) expr]
                                      expr (expr/ctx->cnt
                                            {} ;; ? correct env
                                            (expr/simplify-context ctx {}))]
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-defined Operators

;;-------------------------------------------------------------------------
;; syntactic operators

(defoperator :+ [& exprs] (into [] exprs))
(defoperator :* [& exprs] (apply vector tag_arrangement (map form exprs)))
(defoperator :| [& exprs] (mapv form exprs))

;; Nested expressions
(defoperator :<- [& exprs]
  (apply nest-exprs {:unmarked? false :ltr? false} exprs))
(defoperator :-> [& exprs]
  (apply nest-exprs {:unmarked? false :ltr? true} exprs))
(defoperator :< [& exprs]
  (apply nest-exprs {:unmarked? true :ltr? false} exprs))
(defoperator :> [& exprs]
  (apply nest-exprs {:unmarked? true :ltr? true} exprs))

;; Chained expressions
;; ? redundant because of :* and :|
(defoperator :<-> [& exprs]
  (apply mark-exprs {:unmarked? false} exprs))
(defoperator :<> [& exprs]
  (apply mark-exprs {:unmarked? true} exprs))


;;-------------------------------------------------------------------------
;; unclear FORMs

(s/def :formform.specs.expr/unclear
  (s/cat :tag   (partial = tag_unclear)
         :label #(and (string? %) ((complement empty?) %))))

(def unclear? (partial s/valid? :formform.specs.expr/unclear))

(defmethod op-spec tag_unclear [_] :formform.specs.expr/unclear)

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

(defoperator tag_unclear [label] [tag_seq-reentry :<r label label]
  :constructor construct-unclear
  :predicate unclear?
  :reducer
  (fn [[_ label] env]
    (let [fdna [tag_formDNA [label] [:N :U :U :U]]]
      (simplify-op fdna env))))

;;-------------------------------------------------------------------------
;; memory FORMs

(s/def :formform.specs.expr/rem-pair
  (s/tuple #(s/valid? :formform.specs.expr/expression %)
           #(s/valid? :formform.specs.expr/expression %)))

(s/def :formform.specs.expr/rems
  (s/coll-of :formform.specs.expr/rem-pair 
             :kind sequential?
             :into []))

(s/def :formform.specs.expr/memory
  (s/cat :tag  (partial = tag_memory)
         :rems :formform.specs.expr/rems
         :ctx  (s/* #(s/valid? :formform.specs.expr/expression %))))

(def rem-pairs? (partial s/valid? :formform.specs.expr/rems))

(def memory? (partial s/valid? :formform.specs.expr/memory))

(defmethod op-spec tag_memory [_] :formform.specs.expr/memory)

(defn memory-replace [[_ _ & ctx] & repl-pairs]
  {:pre [(rem-pairs? repl-pairs)]}
  (apply make-op tag_memory (vec repl-pairs) ctx))

(defn memory-extend [[_ rems & ctx] & ext-pairs]
  {:pre [(rem-pairs? ext-pairs)]}
  (apply make-op tag_memory (vec (concat rems ext-pairs)) ctx))

(defn- simplify-rems
  [rems env]
  (if (empty? rems)
    [[] env]
    (loop [[[k v] & r] rems
           rems-reduced []
           env env]
      (let [v (expr/simplify-content v env)
            rems-reduced (conj rems-reduced [k v])
            env (assoc env k v)]
        (if (empty? r)
          [rems-reduced env]
          (recur r rems-reduced env))))))

;; ? `& exprs` instead of `ctx`
(defn- filter-rems
  [rems ctx]
  (let [rem-keys (set (expr/find-subexprs ctx (set (map first rems))))
        rev-rems (reverse rems)]
    (first (reduce
            (fn [[acc rem-keys varlist-next] [v x :as rem]]
              (if (some? (rem-keys v))
                (if (= v x) ;; remove self-reference [x x]
                  [acc rem-keys (rest varlist-next)]
                  (let [new-ks (if (expr/struct-expr? x)
                                 (expr/find-subexprs x (set varlist-next))
                                 '())]
                    [(conj acc rem)
                     (disj (into rem-keys new-ks) v)
                     (rest varlist-next)]))
                [acc rem-keys (rest varlist-next)]))
            [[] rem-keys (map first (rest rev-rems))]
            rev-rems))))

(defn- simplify-memory
  [mem env]
  (let [[rems env] (simplify-rems (op-get mem :rems) env)
        ctx  (expr/simplify-context (op-get mem :ctx) env)
        rems (filter-rems rems ctx)]
    (if (empty? rems)
      (apply make ctx)
      (apply make-op tag_memory rems ctx))))

(defoperator tag_memory [rems & ctx]
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
  (make-op tag_memory rems exprs))

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

(s/def :opts.seq-reentry/parity #{:odd :even :any})
(s/def :opts.seq-reentry/open? boolean?)
(s/def :opts.seq-reentry/interpr #{:rec-ident :rec-instr})

(s/def :formform.specs.expr/seq-reentry-opts
  (s/keys :req-un [:opts.seq-reentry/parity
                   :opts.seq-reentry/open?
                   :opts.seq-reentry/interpr]))

(s/def :formform.specs.expr/seq-reentry
  (s/cat :tag (partial = tag_seq-reentry)
         :sign :formform.specs.expr/seq-reentry-signature
         :nested-exprs (s/+ :formform.specs.expr/expression)))

(def seq-reentry-signature?
  (partial s/valid? :formform.specs.expr/seq-reentry-signature))

(def seq-reentry-opts?
  (partial s/valid? :formform.specs.expr/seq-reentry-opts))

(def seq-reentry? (partial s/valid? :formform.specs.expr/seq-reentry))

(defmethod op-spec tag_seq-reentry [_] :formform.specs.expr/seq-reentry)

(defn simplify-seq-reentry
  [seq-re env]
  (let [env   (expr/observed-removeall env) ;; no de/generation across seq-re bounds
        sign  (op-get seq-re :sign)
        exprs (op-get seq-re :nested-exprs)
        {:keys [parity open? interpr] :as specs} (seq-reentry-sign->opts sign)
        ;; ! check if :U from environment can be equivalent to :U in exprs
        [re-entry? exprs]
        (let [re-expr     (if (symx/arrangement? (first exprs))
                            (symx/arr-prepend sign (first exprs))
                            [tag_arrangement sign (first exprs)]) ;; apply?
              [e & exprs] (simplify-expr-chain {:rtl? true}
                                               (cons re-expr (rest exprs))
                                               env)
              [x & r]     (if (symx/arrangement? e)
                            (op-get e :exprs)
                            [e])]
          (if (= x sign)
            [true  (cons (cond (empty? r) nil
                               (== 1 (count r)) (first r)
                               :else (apply make tag_arrangement r))
                         exprs)]
            [false (cons e exprs)]))
        >< (fn [expr] (expr/simplify-content expr env))] ; interpret ?
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
            :else (apply make tag_seq-reentry sign exprs)))
        (apply make tag_seq-reentry sign exprs))
      ;; re-entry vanished due to dominance of the mark
      ;; this is not a re-entry FORM anymore
      (>< (apply nest-exprs {:unmarked? open? :ltr? false}
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
        op (apply vector tag_seq-reentry signature nested-exprs)]
    (if (seq-reentry? op)
      op
      ;; ! redundant checks
      (throw (ex-info (str "Invalid operator arguments" op-k)
                      {:op op-k :args (cons specs nested-exprs)})))))

;; ? should even/odd construct redundant re-entries in interpretation?
(defoperator tag_seq-reentry [sign & nested-exprs]
  (let [[x & ys :as exprs] nested-exprs
        {:keys [parity open?]} (seq-reentry-sign->opts sign)
        res-odd?  (odd? (count exprs))
        pre-step? (and res-odd? (not (= parity :even)))
        re   [:f* (apply nest-exprs {:unmarked? false :ltr? false}
                         (make :f* x) (if res-odd?
                                        (concat ys (cons x ys))
                                        ys))]
        pre  (when pre-step?
               [(if open? :f2 :f1)
                (apply nest-exprs {:unmarked? false :ltr? false}
                       (make :f* x) ys)])
        open (when open?
               [:f1 (apply nest-exprs {:unmarked? true :ltr? false}
                           (make (if pre-step? :f2 :f*) x) ys)])
        init (if (or pre-step? open?) :f1 :f*)]
    (make-op tag_memory (vec (remove nil? [re pre open])) init))
  :constructor construct-seq-reentry
  :predicate seq-reentry?
  :reducer simplify-seq-reentry)

(defn seq-re
  "Constructs a self-equivalent re-entry FORM given the arguments:
  - `specs`: either a `seq-reentry-signature` or an options map
  - `nested-exprs`: zero or more expressions intended as a nested sequence"
  [specs & nested-exprs]
  (apply construct-seq-reentry tag_seq-reentry specs nested-exprs))

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
   (if (and simplify? (every? #{:M :N} (vals vars->consts)))
     (apply form (map (fn [[v c]]
                        (if (= c :M) (form v) v)) vars->consts))
     (let [select-UI (fn [[v c]] (case c
                                   :N [(form v) (form v)]
                                   :M [v v]
                                   :U [v (form v)]
                                   :I [(form v) v]))
           all-selections (map select-UI vars->consts)]
       (make (form (apply make (map first all-selections))
                   (form tag_seq-reentry :<..r))
             (form (apply make (map second all-selections))
                   (form tag_seq-reentry :<r)))))))


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
          (s/cat :tag      (partial = tag_formDNA)
                 :varorder :formform.specs.expr/varorder
                 :dna      :formform.specs.calc/dna))
         #(== (count (second %)) (calc/dna-dimension (nth % 2)))))

(def formDNA? (partial s/valid? :formform.specs.expr/formDNA))

(defmethod op-spec tag_formDNA [_] :formform.specs.expr/formDNA)

(defn- filter-formDNA
  "Filters the `dna` by values from given `env` whose keys match variables in the `varlist` of the formDNA."
  [fdna env]
  (let [;; assumes only constant values
        ;; ? what about unevaluated FORMs and formDNA?
        {:keys [varorder dna]} (op-data fdna)
        matches (map #(let [v (get env % calc/var-const)
                            v (if (or (= calc/var-const v) (calc/const? v))
                                v
                                (symx/expr->const v))]
                        (vector % v))
                     varorder)
        vpoint (map second matches)]
    (make tag_formDNA
          (mapv first (filter #(= (second %) calc/var-const) matches))
          (calc/filter-dna dna vpoint))))

(defn- simplify-formDNA
  [operator env]
  (let [filtered-fdna (filter-formDNA operator env)]
    (if (empty? (op-get filtered-fdna :varorder))
      (first (op-get filtered-fdna :dna))
      filtered-fdna)))

(defn- construct-formDNA
  ([op-k] (construct-formDNA op-k [] [:N]))
  ([op-k dna] (let [varorder (vec (expr/gen-vars (calc/dna-dimension dna)))]
                (construct-formDNA op-k varorder dna)))
  ([op-k varorder dna]
   (let [op (vector op-k varorder dna)]
     (if (formDNA? op)
       op
       (throw (ex-info (str "Invalid operator arguments" op-k)
                       {:op op-k :varorder varorder :dna dna}))))))

(defoperator tag_formDNA [varorder dna]
  (if (empty? varorder)
    (make (first dna))
    (let [vdict (calc/dna->vdict dna {})]
      (apply make
             (map (fn [[vpoint res]]
                    (apply form
                           (form res)
                           (map #(form ((const->isolator %1) %2))
                                vpoint varorder)))
                  vdict))))
  :constructor construct-formDNA
  :predicate formDNA?
  :reducer simplify-formDNA)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-defined Symbols

(defsymbol :0 :N)
(defsymbol :1 :U)
(defsymbol :2 :I)
(defsymbol :3 :M)

(defsymbol :mn :U)


(comment
  ;; ! this shouldnt work:
  (simplify-seq-reentry [:seq-re nil] {})
  (op-get [:seq-re 'a] :sign)
  (op-data [:seq-re 'a])


  (expr/simplify [:fdna ['a] [:N :U :I :M]]
                 {'a :U})

  (expr/simplify ['a [:fdna ['b] [:N :U :I :M]]]
                 {'a :U})
  ;=> (:U [:fdna [b] [:N :U :I :M]])
  '[[:fdna [b] [:U :U :M :M]]]

  (expr/simplify ['a [:fdna ['b] [:N :U :I :M]]])
  ;=> (a [:fdna [b] [:N :U :I :M]])
  '[[:fdna [a b] [:M :M :M :M
                  :I :M :I :M
                  :U :U :M :M
                  :N :U :I :M]]]

  (expr/simplify ['a 'b [:fdna ['c] [:N :U :I :M]]])
  ;=> (a b [:fdna [c] [:N :U :I :M]])
  '[[:fdna [a b c]
     [:M :M :M :M  :M :M :M :M  :M :M :M :M  :M :M :M :M
      :M :M :M :M  :I :M :I :M  :M :M :M :M  :I :M :I :M
      :M :M :M :M  :M :M :M :M  :U :U :M :M  :U :U :M :M
      :M :M :M :M  :I :M :I :M  :U :U :M :M  :N :U :I :M]]]

  (meta (expr/evaluate ['a 'b] {'b :U}))
  (expr/evaluate ['a 'b] {'a :M})
  (expr/eval-all ['a 'b] {'a :M})
  (expr/=> ['a 'b])
  (expr/=>* ['a 'b])
  (expr/=>* ['a 'b [:fdna ['c] [:N :U :I :M]]])

  (expr/=>* {:varorder ['l 'e 'r]}
            (make (seq-re :<r 'l 'e 'r)
                  (seq-re :<r 'l 'r 'e)) {})

  (defoperator :x5 [x] (repeat 5 x))
  (defoperator :xn [n x] (repeat n x))
  (defoperator :inv [x] (case x
                          :N :M
                          :U :I
                          :I :U
                          :M :N nil))

  ;; ? how to deal with uninterpretable operators
  (defoperator :if [pred x t f] [:if pred x t f]
    :reducer (fn [[_ pred x t f] env]
               (if (pred (simplify x env)) t f)))

  (expr/interpret [:x5 [:U]])
  (expr/interpret [:xn 6 :U])
  (expr/simplify [:inv :U])

  (expr/simplify [:mem [[:x []]] [:if #(= % []) :x :M :U]])

  (str (calc/vdict->vmap
        (calc/dna->vdict
         (op-get (expr/=>* nil) :dna) {})))

  )


  


