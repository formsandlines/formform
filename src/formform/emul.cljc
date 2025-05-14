(ns formform.emul
  "API for the `emul` module of `formform`."
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.emul.core :as core]
            [formform.emul.interfaces :as i]
            [formform.calc.specs :as calc-sp]
            [formform.emul.specs :as sp]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen])
  #?(:cljs (:require-macros
            [formform.emul :refer [defini defumwelt defrule defspecies]])))


(defmacro defini
  "Defines a new type of ini pattern, to be specified with `make-ini`. Takes a keyword identifier, fields for data the user needs to provide, an optional docstring (to describe the fields and the pattern) and one or more implementations of:
  - `(make-gen [this w] …)` for a 1D pattern
  - `(make-gen [this w h] …)` for a 2D pattern"
  [type-k fields doc-string? & methods]
  (apply i/defini-impl type-k fields doc-string? methods))

(defmacro defumwelt
  "Defines a new type of umwelt pattern, to be specified with `make-umwelt`. Takes a keyword identifier, fields for data the user needs to provide, an optional docstring (to describe the fields and the pattern) and one or more implementations of:
  - `(observe-umwelt [this gen cell w] …)` for a 1D pattern
  - `(observe-umwelt [this gen cell w h] …)` for a 2D pattern
  - `gen` is a vector of the current generation (flat in 1D, nested in 2D)
  - `cell` is a vector `[[x …] v]` of the current cell coordinates and value"
  [type-k fields doc-string? & methods]
  (apply i/defumwelt-impl type-k fields doc-string? methods))

(defmacro defrule
  "Defines a new type of rule pattern, to be specified with `make-rule`. Takes a keyword identifier, fields for data the user needs to provide, an optional docstring (to describe the fields and the pattern) and one or more implementations of:
  - `(apply-rule [this umwelt self-v w] …)` for a 1D pattern
  - `(apply-rule [this umwelt self-v w h] …)` for a 2D pattern
  - `umwelt` is an ‘umwelt’ as provided by `observe-umwelt`
  - `self-v` is the value of the current cell"
  [type-k fields doc-string? & methods]
  (apply i/defrule-impl type-k fields doc-string? methods))

(defmacro defspecies
  "Defines a new type of species pattern, to be specified with `make-species`. Takes a keyword identifier, fields for data the user needs to provide, an optional docstring (to describe the fields and the pattern) and one or more implementations of:
  - `(specify-ca [this options w] …)` for a 1D pattern
  - `(specify-ca [this options w h] …)` for a 2D pattern"
  [type-k fields doc-string? & methods]
  (apply i/defspecies-impl type-k fields doc-string? methods))

(def !types i/!types)

(defn get-in-types
  [ks]
  (let [types @!types]
    (when-not ((set (keys types)) (first ks))
      (throw (ex-info (str "Unknown category `" (first ks) "`.")
                      {:keys ks})))
    (get-in types ks)))

(defn docs
  [cat-k type-k]
  (get-in-types [cat-k type-k :docs]))

(defn params
  [cat-k type-k]
  (get-in-types [cat-k type-k :params]))

(defn- make-instance
  [cat-k type-k & args]
  (let [{:keys [constructor params]}
        (if-let [m (get-in-types [cat-k type-k])]
          m
          (throw (ex-info (str "Type `" type-k "` is unknown in " cat-k ".")
                          {:type-key type-k})))
        spec-k (keyword (str (name cat-k) "/" (name type-k)))
        opts? (= :-opts (first params))
        args
        (cond
          ;; :opts is an optional argument for the api/make-x function,
          ;; but not for the record constructor
          opts? (cond
                  (= (count args) (dec (count params))) (cons {} args)
                  (and (= (count args) (count params))
                       (map? (first args))) args)
          (= (count args) (count params)) args
          :else (throw (ex-info (str "Wrong number of arguments. Expects: "
                                     params)
                                {:args args})))]
    (when (and (s/get-spec spec-k) (not (s/valid? spec-k args)))
      (throw (ex-info (str "Invalid arguments for " (name cat-k)
                           " `" type-k "`.")
                      {:args args
                       :spec-error (s/explain-data spec-k args)})))
    (apply constructor args)))

(def make-ini (partial make-instance :ini))
(def make-umwelt (partial make-instance :umwelt))
(def make-rule (partial make-instance :rule))
(def make-species (partial make-instance :species))


(s/fdef sys-ini
  :args (s/alt :ar1 (s/cat :ini-spec ::sp/ini-spec
                           :res-w    pos-int?)
               :ar2 (s/cat :ini-spec ::sp/ini-spec
                           :res-w    pos-int?
                           :res-h    pos-int?))
  :ret  ::sp/generation)
(def sys-ini
  "Returns a (initial) generation given an ini specification (via `make-ini`) and, depending on the arities it supports, one or two resolutions."
  core/sys-ini)

(s/fdef sys-next
  :args (s/cat :rule-spec   ::sp/rule-spec
               :umwelt-spec ::sp/umwelt-spec
               :generation  ::sp/generation)
  :ret  ::sp/generation)
(defn sys-next
  "Computes and returns the next generation given a rule specification (via `make-rule`), an umwelt specification (via `make-umwelt`) and a generation."
  [rule-spec umwelt-spec gen]
  (let [[w h :as res] (core/get-resolution-from-generation gen)]
    (core/sys-next res (range w) (when h (range h)) rule-spec umwelt-spec gen)))


(s/fdef observe-umwelt
  :args (s/alt :ar1 (s/cat :umwelt-spec ::sp/umwelt-spec
                           :generation  ::sp/generation
                           :cell        ::sp/cell))
  :ret  ::sp/umwelt)
(defn observe-umwelt
  "Returns a ‘umwelt’ given an umwelt specification (via `make-umwelt`), a generation and the current cell."
  [this gen cell]
  (let [res (core/get-resolution-from-generation gen)]
    (apply i/observe-umwelt this gen cell res)))

(s/fdef apply-rule
  :args (s/alt :ar1 (s/cat :rule-spec ::sp/rule-spec
                           :umwelt    ::sp/umwelt
                           :self-val  ::calc-sp/const?
                           :res-w     pos-int?)
               :ar2 (s/cat :rule-spec ::sp/rule-spec
                           :umwelt    ::sp/umwelt
                           :self-val  ::calc-sp/const?
                           :res-w     pos-int?
                           :res-h     pos-int?))
  :ret  ::calc-sp/const?)
(def apply-rule
  "Returns a cell value given a rule specification (via `make-rule`), a ‘umwelt’ (via `get-umwelt`), the current cell value and, depending on the arities the rule spec. supports, one or two resolutions."
  i/apply-rule)

(s/fdef specify-ca
  :args (s/alt :ar1 (s/cat :species-spec ::sp/species-spec
                           :options      map?
                           :res-w        pos-int?)
               :ar2 (s/cat :species-spec ::sp/species-spec
                           :options      map?
                           :res-w        pos-int?
                           :res-h        pos-int?))
  :ret  ::sp/ca-spec)
(def specify-ca
  "Returns a specification for a cellular automaton given a specification of its ‘species’ (via `make-species`) and, depending on the arities it supports, one or two resolutions."
  i/specify-ca)


(s/def ::tsds-selection
  (s/coll-of #{0 1} :kind vector? :count 6))

(defn tsds-sel->dna
  [selection]
  (expr/op-get (expr/=>* (expr/make :tsds selection 'a 'b 'c)) :dna))

#_
(defn- conform-tsds-dna-or-sel
  [dna-or-sel]
  (cond
    (s/valid? ::tsds-selection dna-or-sel) (tsds-sel->dna dna-or-sel)
    (and (calc/dna? dna-or-sel)
         (= 3 (calc/dna-dimension dna-or-sel))) dna-or-sel
    :else (throw
           (ex-info "Input must be either a formDNA of dimension 3 or a 6-element binary selection vector." {:input dna-or-sel}))))

(defn- exprs->dna
  [& exprs]
  (expr/op-get (expr/=>* (apply expr/make exprs)) :dna))

(def common-specimen
  "Common specimen to create cellular automata from (via `specify-ca`). Lists all the SelFis introduced by Ralf Peyn in ‘uFORM iFORM’."
  (let [selfi (partial make-species :selfi)
        ini-ball (make-ini :ball :N nil {:pos :center :align :center})
        ini-rand (make-ini :random)
        l 'a, e 'b, r 'c]
    {:Mark1
     (selfi (tsds-sel->dna [1 0 0 1 0 0]) ini-ball)
     :StripesD100000
     (selfi (tsds-sel->dna [1 0 0 0 0 0]) ini-rand)
     :StripesL000100
     (selfi (tsds-sel->dna [0 0 0 1 0 0]) ini-rand)
     :Mono000101
     (selfi (tsds-sel->dna [0 0 0 1 0 1]) ini-rand)
     :Rhythm101101
     (selfi (tsds-sel->dna [1 0 1 1 0 1]) ini-rand)
     :NewSense
     (selfi (tsds-sel->dna [1 1 0 1 0 0]) ini-rand)
     :Slit
     (selfi (exprs->dna [[l] r] [[r] l]) ini-ball)
     :xor4vRnd
     (selfi (exprs->dna [[l] r] [[r] l]) ini-rand)
     :or4v
     (selfi (exprs->dna l r) ini-ball)
     :xorReId
     (selfi (exprs->dna (expr/seq-re :<r' l, r)
                        (expr/seq-re :<r' r, l)) ini-ball)
     :xorReIdRnd
     (selfi (exprs->dna (expr/seq-re :<r' l, r)
                        (expr/seq-re :<r' r, l)) ini-rand)
     :Rule4v30
     (selfi (exprs->dna [[l] e r] [[e] l] [[r] l]) ini-ball)
     :Rule4v111
     (selfi (exprs->dna [[[l] e] r] [[[l] r] e] [[[e] r] l]) ini-ball)
     :Structure111Re
     (selfi (tsds-sel->dna [1 0 1 1 0 0]) ini-ball)
     :CoOneAnother
     (selfi (tsds-sel->dna [1 0 1 1 0 0]) ini-rand)
     :Rule4v110
     (selfi (exprs->dna [[e] r] [[r] e] [[r] l]) ini-ball)
     :uniTuringReRnd
     (selfi (exprs->dna [[(tsds-sel->dna [1 0 1 1 0 0])] [l e r]]) ini-rand)}))


(s/fdef ca-iterator
  :args (s/or :ar1 (s/cat :ca-spec ::sp/ca-spec)
              :ar2 (s/cat :ca-spec ::sp/ca-spec
                          :steps   pos-int?))
  :ret  ::sp/iterator)
(defn ca-iterator
  "Returns a lazy seq that iteratively computes the next generation for the given cellular automaton specification (via `specify-ca`). Optionally, the last argument can be a number to just get the first `n` steps in its evolution."
  ([ca-spec]
   (core/ca-iterator ca-spec))
  ([ca-spec steps]
   (take steps (core/ca-iterator ca-spec))))

(s/fdef step
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  any?)
(defn step
  "Given a stateful `CellularAutomaton` object, computes its next generation and appends it to its evolution state."
  [ca-obj]
  (i/step ca-obj))

(s/fdef restart
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  any?)
(defn restart
  "Given a stateful `CellularAutomaton` object, resets its evolution to the initial generation."
  [ca-obj]
  (i/restart ca-obj))

#_#_
(s/fdef get-evolution
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  ::sp/evolution)
(defn get-evolution
  "Given a stateful `CellularAutomaton` object, returns an immutable copy of its current evolution state."
  [ca-obj]
  (i/get-evolution ca-obj))

(s/fdef get-current-generation
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  ::sp/generation)
(defn get-current-generation
  "Given a stateful `CellularAutomaton` object, returns its current generation either as a native array (if `optimized?` is true) or a vector."
  [ca-obj optimized?]
  (i/get-current-generation ca-obj optimized?))

(s/fdef get-cached-history
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  ::sp/evolution)
(defn get-cached-history
  "Given a stateful `CellularAutomaton` object, returns its cached history/evolution, where all generations are either native arrays (if `optimized?` is true) or vectors."
  [ca-obj optimized?]
  (i/get-cached-history ca-obj optimized?))

(s/fdef get-system-time
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  pos-int?)
(defn get-system-time
  "Given a stateful `CellularAutomaton` object, returns its generation index (aka “system-time”)."
  [ca-obj]
  (i/get-system-time ca-obj))

(s/fdef get-history-cache-limit
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  pos-int?)
(defn get-history-cache-limit
  "Given a stateful `CellularAutomaton` object, returns the max. number of generations it caches (stored in its history)."
  [ca-obj]
  (i/get-history-cache-limit ca-obj))

(s/fdef get-resolution
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  ::sp/resolution)
(defn get-resolution
  "Given a stateful `CellularAutomaton` object, returns its resolution."
  [ca-obj]
  (i/get-resolution ca-obj))

(s/fdef create-ca
  :args (s/or :ar1 (s/cat :ca-spec ::sp/ca-spec)
              :ar2 (s/cat :ca-spec ::sp/ca-spec
                          :history-cache-limit pos-int?))
  :ret  ::sp/automaton)
(defn create-ca
  "Returns a stateful `CellularAutomaton` object for the given cellular automaton specification (via `specify-ca`). Callable methods are:
  - `step` to compute the next generation which gets added to the evolution
  - `restart` to re-initialize the CA with its first generation
  - `get-evolution` to obtain an immutable copy of the current evolution
  - `get-resolution` to obtain the resolution of the CA"
  ([ca-spec]
   (core/create-ca ca-spec nil))
  ([ca-spec history-cache-limit]
   (core/create-ca ca-spec history-cache-limit)))


(comment
  (keys (get-in-types [:ini]))
  (params :ini :fill-center)

  (make-ini :fill-center {:res [10 4] :val :U} :M)
  (make-rule :match (calc/rand-dna 2))
  (make-instance :rule :match (calc/rand-dna 2))
  (make-species :selfi (calc/rand-dna 2) (make-ini :random))
  (make-species :lifeform (calc/rand-dna 2))

  ,)

(comment
  (docs :species :selfi)
  (docs :rule :match)
  (take 6 (ca-iterator (specify-ca (common-specimen :Mark1) {} 10)))
  ,)

(comment
  (def ca (create-ca
           (specify-ca (make-species :mindform
                                     (calc/rand-dna 2)
                                     (make-ini :rand-center 10))
                       {}
                       40 40)))
  (.get-resolution ca)
  (step ca)
  (get-current-generation ca)
  (restart ca)
  ,)

(comment
  (sys-ini (make-ini :random) 10)
  (sys-ini (make-ini :random) 10 10)
  (sys-ini (make-ini :rand-center 2) 10)
  (sys-ini (make-ini :rand-center 2) 10 10)
  (sys-ini (make-ini :ball) 10)
  (sys-ini (make-ini :ball) 10 10)
  (sys-ini (make-ini :fill-all :N) 10)
  (sys-ini (make-ini :fill-all :N) 10 4)
  (sys-ini (make-ini :fill-all :rand) 10)
  (sys-ini (make-ini :fill-all :rand) 10 4)
  (sys-ini (make-ini :fill-all [:N :U :I :M]) 4)
  (sys-ini (make-ini :fill-all [[:_ :_ :_ :N]
                                [:_ :_ :U :_]
                                [:_ :I :_ :_]
                                [:M :_ :_ :_]]) 4 4)
  (sys-ini (make-ini :fill-all (fn [x] (if (= 0 (mod x 3))
                                        :U :_))) 10)
  (sys-ini (make-ini :fill-all (fn [x y] (if (and (= 0 (mod x 3))
                                                 (= 0 (mod y 3)))
                                          :U :_))) 10 10)
  (sys-ini (make-ini :fill-center {:res [4] :val :U} :_) 10)
  (sys-ini (make-ini :fill-center {:res [4 3] :val :rand} :_) 10 10)
  (sys-ini (make-ini :fill-center [:U :I :M] :_) 10)
  (sys-ini (make-ini :fill-center [[:U :I :M] [:M :N :I] [:I :U :M]] :_) 5 5)

  (apply-rule (make-rule :match [:N :N :N :N
                                 :N :N :N :N
                                 :N :M :N :N
                                 :N :N :N :N])
              (observe-umwelt (make-umwelt :select-ltr 2)
                              (sys-ini (make-ini :fill-center
                                                 [:U :N :I] :N) 5)
                              ;;=> [:N :U :N :I :N]
                              [[2] :_]) ;;=> [:U :I]
              :_) ;;=> :M
  
  ,)

(comment
  (sys-ini ((:constructor (get-in @!types [:ini :fill-all])) :U) 10 3)
  (make-ini :fill-all :I)
  (docs :ini :fill-all)
  (params :ini :fill-all)
  ,)

(comment
  (def selfi (specify-ca (make-species :selfi
                                       (calc/rand-dna 2)
                                       (make-ini :random))
                         {}
                         40))
  (meta selfi)
  
  (take 3 (ca-iterator selfi))
  (def ca (create-ca selfi))
  (get-resolution ca)
  (seq (get-current-generation ca))
  (step ca)
  (restart ca)

  (.get-resolution ca)
  (.get-evolution ca)
  (.step ca)
  (.restart ca)
  (type ca)
  
  (def slit (i/specify-ca (common-specimen :Slit)
                          {}
                          20))
  (meta slit)
  (take 10 (ca-iterator slit))
  '([:N :N :N :N :N :N :N :N :I :U :M :U :I :N :N :N :N :N :N :N]
    [:N :N :N :N :N :N :N :I :U :U :N :U :U :I :N :N :N :N :N :N]
    [:N :N :N :N :N :N :I :U :M :U :N :U :M :U :I :N :N :N :N :N]
    [:N :N :N :N :N :I :U :U :N :M :N :M :N :U :U :I :N :N :N :N]
    [:N :N :N :N :I :U :M :U :I :N :N :N :I :U :M :U :I :N :N :N]
    [:N :N :N :I :U :U :N :U :U :I :N :I :U :U :N :U :U :I :N :N]
    [:N :N :I :U :M :U :N :U :M :U :N :U :M :U :N :U :M :U :I :N]
    [:N :I :U :U :N :M :N :M :N :M :N :M :N :M :N :M :N :U :U :I]
    [:N :U :M :U :I :N :N :N :N :N :N :N :N :N :N :N :I :U :M :U]
    [:N :M :N :U :U :I :N :N :N :N :N :N :N :N :N :I :U :U :N :M])
  (assoc slit :label "Slit")
  ,)

(comment
  
  (observe-umwelt (make-umwelt :select-ltr 3)
                  [:N :M :U :I :N :M :I]
                  [[3] :U])
  (observe-umwelt (make-umwelt :self-select-ltr 3)
                  [[:N :I :N :U :I]
                   [:U :M :N :I :M]
                   [:M :N :I :M :U]
                   [:N :N :U :I :M]
                   [:M :U :I :I :N]]
                  [[2 2] :I])

  (let [gen [:N :M :U :I :N :M :I]
        cell [[3] :U]
        dna [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
             :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
             :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]
        env (observe-umwelt (make-umwelt :select-ltr 3) gen cell)]
    (apply-rule (make-rule :match dna) env (last cell)))

  (let [dna [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
             :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
             :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]]
    (sys-next (make-rule :match dna)
              (make-umwelt :select-ltr 3)
              (sys-ini (make-ini :fill-center {:res [1] :val :M} :N) 7)))
  ;; ini: [:N :N :N :M :N :N :N]
  ;;=>    [:M :M :N :M :N :M :M]

  (let [dna [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
             :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
             :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]
        selfi (specify-ca
               (make-species :selfi dna
                             (make-ini :fill-center {:res [1] :val :M} :N))
               {}
               7)]
    (take 5 (ca-iterator selfi)))
  '([:N :N :N :M :N :N :N]
    [:M :M :N :M :N :M :M]
    [:N :M :N :M :N :N :N]
    [:N :M :N :M :N :M :M]
    [:N :M :N :M :N :N :M])
  ,)

#_
(comment
  (require '[clojure.string :as string])
  (require '[clojure.pprint :refer [pprint]])
  (require '[formform.expr :as expr])

  (defn pp-gen [gen]
    (let [pp-val (fn [c]
                   (case c
                     :N "⁠█" :U "▒" :I "░" :M "▓" "?"))]
      (string/join "" (mapv pp-val gen))))

  (def coa (expr/make '[:seq-re :<r a b c]
                      '[:seq-re :<r c b a]
                      '[:seq-re :<r a c b]))

  (conform-tsds-dna-or-sel (expr/op-get (expr/=>* coa) :dna))
  (conform-tsds-dna-or-sel [0 1 1 0 1 0])

  (let [dna (expr/op-get (expr/=>* coa) :dna)
        selfi (make-selfi [251] dna [:ball])]
    #?(:clj
       (spit "./out.txt" (string/join "\n" (map pp-gen (take 2000 selfi))))))

  (let [dna (expr/op-get (expr/=>* coa) :dna)
        mindform (make-mindform [100 50] dna [:rand-center 10])]
    #?(:clj
       (spit "./out.txt" (string/join "\n\n"
                                      (map (fn [evol]
                                             (string/join "\n"
                                                          (map pp-gen evol)))
                                           (take 80 mindform))))))

  (let [dna (expr/op-get (expr/=>* coa) :dna)
        lifeform (make-lifeform [100 50] dna)]
    #?(:clj
       (spit "./out.txt" (string/join "\n\n"
                                      (map (fn [evol]
                                             (string/join "\n"
                                                          (map pp-gen evol)))
                                           (take 80 lifeform))))))

  (let [dna (expr/op-get (expr/=>* coa) :dna)
        decisionform (make-decisionform [100 50] dna 20)]
    #?(:clj
       (spit "./out.txt" (string/join "\n\n"
                                      (map (fn [evol]
                                             (string/join "\n"
                                                          (map pp-gen evol)))
                                           (take 80 decisionform))))))
  ,)
