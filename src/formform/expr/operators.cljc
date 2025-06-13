;; ========================================================================
;;     formform expression operators module
;;     -- created 03/2023, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.expr.operators
  (:require
   [clojure.set :as set]
   #?(:clj  [clojure.core.match :refer [match]]
      :cljs [cljs.core.match :refer-macros [match]])
   [formform.calc :as calc]
   [formform.expr.common
    :refer [tag_memory tag_formDNA tag_unclear tag_arrangement
            tag_seq-reentry]]
   #?(:clj  [formform.expr.symexpr :as symx
             :refer [defoperator
                     op-get op-data op-symbol valid-op? make-op
                     interpret-op simplify-op]]
      :cljs [formform.expr.symexpr :as symx
             :refer [op-get op-data op-symbol valid-op? make-op
                     interpret-op simplify-op]
             :refer-macros [defoperator]])
   [formform.expr.core :as core :refer [make form permute-vars]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-defined Operators

;;-------------------------------------------------------------------------
;; syntactic operators

(defoperator :+ [& exprs] (into [] exprs))
(defoperator :* [& exprs] (apply vector tag_arrangement (map form exprs)))
(defoperator :| [& exprs] (mapv form exprs))

;; Nested expressions
(defoperator :<- [& exprs]
  (apply core/nest-exprs {:unmarked? false :ltr? false} exprs))
(defoperator :-> [& exprs]
  (apply core/nest-exprs {:unmarked? false :ltr? true} exprs))
(defoperator :< [& exprs]
  (apply core/nest-exprs {:unmarked? true :ltr? false} exprs))
(defoperator :> [& exprs]
  (apply core/nest-exprs {:unmarked? true :ltr? true} exprs))

;; Chained expressions
;; ? redundant because of :* and :|
(defoperator :<-> [& exprs]
  (apply core/mark-exprs {:unmarked? false} exprs))
(defoperator :<> [& exprs]
  (apply core/mark-exprs {:unmarked? true} exprs))


;;-------------------------------------------------------------------------
;; unclear FORMs

(def unclear? #(and (symx/operator? %)
                    (= tag_unclear (op-symbol %))))

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
    (let [fdna [tag_formDNA [label] [:U :U :U :N]]]
      (simplify-op fdna env))))

;;-------------------------------------------------------------------------
;; memory FORMs

(def rem-pair? #(and (sequential? %)
                     (== 2 (count %))
                     (every? core/expression? %)))

;; !! unchecked
(def rem-pairs? #(and (sequential? %)
                      (every? rem-pair? %)))

(def memory? #(and (symx/operator? %)
                   (= tag_memory (op-symbol %))
                   (rem-pairs? (second %))))

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
      (let [v (core/simplify-content v env)
            rems-reduced (conj rems-reduced [k v])
            env (assoc env k v)]
        (if (empty? r)
          [rems-reduced env]
          (recur r rems-reduced env))))))

;; ? `& exprs` instead of `ctx`
(defn filter-rems
  [rems ctx]
  (let [rem-keys (set (core/find-subexprs ctx (set (map first rems))))
        rev-rems (reverse rems)]
    (first (reduce
            (fn [[acc rem-keys varlist-next] [v x :as rem]]
              (if (some? (rem-keys v))
                (if (= v x) ;; remove self-reference [x x]
                  [acc rem-keys (rest varlist-next)]
                  (let [new-ks (if (core/struct-expr? x)
                                 (core/find-subexprs x (set varlist-next))
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
        ctx  (core/simplify-context (op-get mem :ctx) env)
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
  [rems & exprs]
  (make-op tag_memory rems exprs))

;;-------------------------------------------------------------------------
;; self-equivalent re-entry FORMs

(def seq-reentry-defaults {:parity :any, :open? false, :interpr :rec-instr})

(def seq-reentry-sign->opts
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
  [m]
  (let [opts (merge seq-reentry-defaults
                    (select-keys m [:parity :open? :interpr]))
        opts->sign (set/map-invert seq-reentry-sign->opts)]
    (opts->sign opts)))

(def seq-reentry-signature? (comp some? seq-reentry-sign->opts))

;; ! shallow predicate
(def seq-reentry-opts?
  #(and (map? %)
        (every? (partial contains? %) [:parity :interpr :open?])))

(def seq-reentry? #(and (symx/operator? %)
                        (= tag_seq-reentry (op-symbol %))))

(defn simplify-seq-reentry
  [seq-re env]
  (let [env   (core/observed-removeall env) ;; no de/generation across seq-re bounds
        sign  (op-get seq-re :sign)
        exprs (op-get seq-re :nested-exprs)
        {:keys [parity open? interpr]} (seq-reentry-sign->opts sign)
        ;; ! check if :U from environment can be equivalent to :U in exprs
        [re-entry? exprs]
        (let [re-expr     (if (symx/arrangement? (first exprs))
                            (symx/arr-prepend sign (first exprs))
                            [tag_arrangement sign (first exprs)]) ;; apply?
              [e & exprs] (core/simplify-expr-chain {:rtl? true}
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
        >< (fn [expr] (core/simplify-content expr env))] ; interpret ?
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
      (>< (apply core/nest-exprs {:unmarked? open? :ltr? false}
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
        re   [:f* (apply core/nest-exprs {:unmarked? false :ltr? false}
                         (make :f* x) (if res-odd?
                                        (concat ys (cons x ys))
                                        ys))]
        pre  (when pre-step?
               [(if open? :f2 :f1)
                (apply core/nest-exprs {:unmarked? false :ltr? false}
                       (make :f* x) ys)])
        open (when open?
               [:f1 (apply core/nest-exprs {:unmarked? true :ltr? false}
                           (make (if pre-step? :f2 :f*) x) ys)])
        init (if (or pre-step? open?) :f1 :f*)]
    (make-op tag_memory (vec (remove nil? [re pre open])) init))
  :constructor construct-seq-reentry
  :predicate seq-reentry?
  :reducer simplify-seq-reentry)

(defn seq-re
  [specs & nested-exprs]
  (apply construct-seq-reentry tag_seq-reentry specs nested-exprs))


(def tsds? #(and (symx/operator? %)
                 (= :tsds (op-symbol %))
                 (= 6 (count (op-get % :selection)))
                 (set/subset? #{:l :e :r} (symx/op-data %))))

;; Triple-selective decision system (see p. 90 (appendix) in uFORM iFORM)
(defoperator :tsds [selection l e r]
  (->> [[l e r] [r e l] [e r l] [l r e] [r l e] [e l r]]
       (mapv (fn [b exprs] (when-not (zero? b)
                             (apply seq-re :<r exprs)))
             selection)
       (filterv some?)
       (into [:-]))
  :predicate tsds?)


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
(defn selector
  ([vars->consts] (selector vars->consts true))
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

;; TODO
#_
(defoperator :sel [selections & exprs]
  (let [xs (mapv (comp selector zipmap)
                 (repeat (count selections) exprs) selections)]
    (apply make xs)))

(comment
  (selector {'a :U 'b :N 'c :M})
  '[:- [a [b] c [[:seq-re :<..r nil]]] [[a] [b] c [[:seq-re :<r nil]]]]

  (selector {'a :M [['a] 'b] :N})

  [:sel ['a :M] ['b :N] ['c :U]]

  [:sel ['a 'b 'c] [:M :N :U] [:U :M :N] [:U :U :I]]

  "[:sel [a,b,c] mnu umn uui]"

  "[:sel mnu a,b,c] [:sel umn a,b,c] [:sel uui a,b,c]"

  "[:mem [a [[x] y]] [b {@.. x,z}] ([:sel [a,b] mu] [:sel [a,b] ni])]"

  [:sel [[:M :N :U] [:U :M :N] [:U :U :I]] ['a 'b 'c]]
  "[:sel [mnu umn uui] a,b,c]"

  (let [selections [[:M :N :U] [:U :M :N] [:U :U :I]]
        exprs ['a 'b 'c]]
    (let [xs (mapv (comp selector zipmap)
                   (repeat (count selections) exprs) selections)]
      (apply make xs)))
  '[:-
    [a [b] c [[:seq-re :<..r nil]]] [a [b] [c] [[:seq-re :<r nil]]]
    [a b [c] [[:seq-re :<..r nil]]] [[a] b [c] [[:seq-re :<r nil]]]
    [a b [c] [[:seq-re :<..r nil]]] [[a] [b] c [[:seq-re :<r nil]]]]

  "[:sel [mnu] a,b,c]"

  ,)

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

(def formDNA? #(and (symx/operator? %)
                    (= tag_formDNA (op-symbol %))))

;; ? refactor (matches need not return a vector)
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
  ([op-k dna] (let [varorder (vec (core/gen-vars (calc/dna-dimension dna)))]
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
    (let [vdict (calc/dna->vdict {} dna)]
      (apply make
             (map (fn [[vpoint result]]
                    (apply form
                           (form result)
                           (map #(form ((const->isolator %1) %2))
                                vpoint varorder)))
                  vdict))))
  :constructor construct-formDNA
  :predicate formDNA?
  :reducer simplify-formDNA)

;; ! assumes equal order between `permute-vars` and `calc/dna-perspectives`
(defn formDNA-perspectives [fdna]
  (let [{:keys [dna varorder]} (op-data fdna)
        perms (permute-vars varorder)]
    (apply make
           (map (fn [varorder [_ dna]]
                  (make :fdna varorder dna))
                perms
                (calc/dna-perspectives dna)))))


(comment
  ;; ! this shouldnt work:
  (simplify-seq-reentry [:seq-re nil] {})
  (op-get [:seq-re 'a] :sign)
  (op-data [:seq-re 'a])


  (core/simplify [:fdna ['a] [:N :U :I :M]]
                 {'a :U})

  (core/simplify ['a [:fdna ['b] [:N :U :I :M]]]
                 {'a :U})
  ;;=> (:U [:fdna [b] [:N :U :I :M]])
  '[[:fdna [b] [:U :U :M :M]]]

  (core/simplify ['a [:fdna ['b] [:N :U :I :M]]])
  ;;=> (a [:fdna [b] [:N :U :I :M]])
  '[[:fdna [a b] [:M :M :M :M
                  :I :M :I :M
                  :U :U :M :M
                  :N :U :I :M]]]

  (core/simplify ['a 'b [:fdna ['c] [:N :U :I :M]]])
  ;;=> (a b [:fdna [c] [:N :U :I :M]])
  '[[:fdna [a b c]
     [:M :M :M :M  :M :M :M :M  :M :M :M :M  :M :M :M :M
      :M :M :M :M  :I :M :I :M  :M :M :M :M  :I :M :I :M
      :M :M :M :M  :M :M :M :M  :U :U :M :M  :U :U :M :M
      :M :M :M :M  :I :M :I :M  :U :U :M :M  :N :U :I :M]]]

  (meta (core/evaluate ['a 'b] {'b :U}))
  (core/evaluate ['a 'b] {'a :M})
  (core/eval-all ['a 'b] {'a :M})
  (core/=> ['a 'b])
  (core/=>* ['a 'b])
  (core/=>* ['a 'b [:fdna ['c] [:N :U :I :M]]])

  (core/=>* {:varorder ['l 'e 'r]}
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

  (core/interpret [:x5 [:U]])
  (core/interpret [:xn 6 :U])
  (core/simplify [:inv :U])

  (core/simplify [:mem [[:x []]] [:if #(= % []) :x :M :U]])

  (str (calc/vdict->vmap
        (calc/dna->vdict
         {} (op-get (core/=>* nil) :dna)))))


