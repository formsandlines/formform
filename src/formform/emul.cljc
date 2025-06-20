(ns formform.emul
  "API for the `emul` module of `formform`."
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.emul.core :as core]
            [formform.emul.interfaces :as i]
            [formform.calc.specs :as calc-sp]
            [formform.emul.specs :as sp]
            [formform.utils :as utils]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen])
  #?(:cljs (:require-macros
            [formform.emul :refer [defini defumwelt defrule]])))


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

(def !types i/!types)

(defn get-in-types
  [ks]
  (let [types @!types]
    (when-not ((set (keys types)) (first ks))
      (throw (ex-info (str "Unknown category `" (first ks) "`.")
                      {:keys ks})))
    (get-in types ks)))

(defn help-type
  ([cat-k type-k] (help-type {} cat-k type-k))
  ([{:keys [no-docs? trim?]} cat-k type-k]
   (let [{:keys [params docs]} (get-in-types [cat-k type-k])
         docs-str (when-not no-docs?
                    (if trim?
                      (str (subs (str/replace docs #"\n" " ")
                                 0 (min 80 (count docs)))
                           "…")
                      docs))
         params-str (str/join " " (mapv #(if (= :-opts %)
                                           "?opts" (name %)) params))]
     (str "`" type-k " [" params-str "]`"
          (when-not no-docs? (str "\n\n" docs-str))))))

(defn help-cat
  [cat-k]
  (str "Registered `" cat-k "` types:\n"
       (str/join "\n"
                 (mapv (fn [type-k]
                         (str "- " (help-type {:no-docs? true} cat-k type-k)))
                       (keys (get @!types cat-k))))))

(defn- make-instance
  [cat-k type-k & args]
  (if (= :help type-k)
    (println (help-cat cat-k))
    (let [{:keys [constructor params]}
          (if-let [m (get-in-types [cat-k type-k])]
            m
            (throw (ex-info (str "Type `" type-k "` is unknown in " cat-k ".")
                            {:type-key type-k})))
          spec-k (keyword (str (name cat-k) "/" (name type-k)))]
      (if (= :help (first args))
        (println (help-type cat-k type-k))
        (let [opts? (= :-opts (first params))
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
          (apply constructor args))))))

(def make-ini
  "Creates an instance of a CA ini (initial conditions) specification of a given type (as a keyword) and with given parameters as required by the type.
  - `(make-ini :help)` prints a list of all ini types and their parameters
  - `(make-ini :t :help)` prints the parameters and docs for given type `:t`"
  (partial make-instance :ini))

(def make-umwelt
  "Creates an instance of a CA umwelt (neighborhood) specification of a given type (as a keyword) and with given parameters as required by the type.
  - `(make-umwelt :help)` prints a list of all ini types and their parameters
  - `(make-umwelt :t :help)` prints the parameters and docs for given type `:t`"
  (partial make-instance :umwelt))

(def make-rule
  "Creates an instance of a CA rule specification of a given type (as a keyword) and with given parameters as required by the type.
  - `(make-rule :help)` prints a list of all ini types and their parameters
  - `(make-rule :t :help)` prints the parameters and docs for given type `:t`"
  (partial make-instance :rule))

(comment
  (make-ini :ball :help)
  (make-umwelt :help)
  ,)


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
                           ;; :res-w     pos-int?
                           )
               :ar2 (s/cat :rule-spec ::sp/rule-spec
                           :umwelt    ::sp/umwelt
                           :self-val  ::calc-sp/const?
                           ;; :res-w     pos-int?
                           ;; :res-h     pos-int?
                           ))
  :ret  ::calc-sp/const?)
(def apply-rule
  "Returns a cell value given a rule specification (via `make-rule`), a ‘umwelt’ (via `get-umwelt`), the current cell value and, depending on the arities the rule spec. supports, one or two resolutions."
  i/apply-rule)

#_
(comment
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
    i/specify-ca))


#_
(comment
  ;; (def specify-ca nil)
  (defmulti specify-ca (fn [id-k & _] id-k))

  (defmethod specify-ca :default
    [label specs]
    (core/map->CASpec
     (assoc specs :label label)))

  (defmethod specify-ca :selfi
    [_ {:keys [overwrites]} dna ini]
    (let [umwelt-size (calc/dna-dimension dna)]
      (core/map->CASpec
       (merge
        {:label       "SelFi"
         :rule-spec   (core/->Rule-Match {} dna)
         :umwelt-spec (core/->Umwelt-SelectLtr {} umwelt-size)
         :ini-spec    ini}
        overwrites))))

  (defmethod specify-ca :mindform
    [_ {:keys [overwrites]} dna ini]
    (let [umwelt-size (calc/dna-dimension dna)]
      (core/map->CASpec
       (merge
        {:label       "MindFORM"
         :rule-spec   (core/->Rule-Match {} dna)
         :umwelt-spec (core/->Umwelt-SelfSelectLtr {} umwelt-size)
         :ini-spec    ini}
        overwrites))))

  (defmethod specify-ca :lifeform
    [_ {:keys [overwrites ini-opts]} dna rand-distr]
    (core/map->CASpec
     (merge
      {:label       "LifeFORM"
       :rule-spec   (core/->Rule-Life {} dna)
       :umwelt-spec (core/->Umwelt-Moore {} :column-first false)
       :ini-spec    (core/->Ini-Random ini-opts (or rand-distr 0.5))}
      overwrites)))

  (defmethod specify-ca :decisionform
    [_ {:keys [overwrites ini-opts]} dna rand-distr init-size]
    (core/map->CASpec
     (merge
      {:label       "DecisionFORM"
       :rule-spec   (core/->Rule-Life {} dna)
       :umwelt-spec (core/->Umwelt-Moore {} :column-first false)
       :ini-spec    (core/->Ini-RandFigure ini-opts
                                           (core/->Ini-Constant ini-opts :n)
                                           init-size
                                           {:pos :center
                                            :align :center})}
      overwrites))))

(defn specify-ca
  [label specs-map]
  (core/map->CASpec
   (assoc specs-map :label label)))

;; Constructors for common CA specifications

(defn make-selfi
  "1D cellular automaton. Takes a `dna` for its rule function (type `:match`) and an `ini` type (via `make-ini`). Its ‘umwelt’ is of type `:select-ltr`."
  ([{:keys [overwrites]} dna ini]
   (let [umwelt-size (calc/dna-dimension dna)]
     (core/map->CASpec
      (merge
       {:label       "SelFi"
        :rule-spec   (core/->Rule-Match {} dna)
        :umwelt-spec (core/->Umwelt-SelectLtr {} umwelt-size)
        :ini-spec    ini}
       overwrites))))
  ([dna ini]
   (make-selfi {} dna ini)))

(defn make-mindform
  "2D cellular automaton. Takes a `dna` for its rule function (type `:match`) and an `ini` type (via `make-ini`). Its ‘umwelt’ is of type `:self-select-ltr`."
  ([{:keys [overwrites]} dna ini]
   (let [umwelt-size (calc/dna-dimension dna)]
     (core/map->CASpec
      (merge
       {:label       "MindFORM"
        :rule-spec   (core/->Rule-Match {} dna)
        :umwelt-spec (core/->Umwelt-SelfSelectLtr {} umwelt-size)
        :ini-spec    ini}
       overwrites))))
  ([dna ini]
   (make-mindform {} dna ini)))

(defn make-lifeform
  "2D cellular automaton. Takes a `dna` as part of its rule function, which is of type `:life`. Its ‘umwelt’ is of type `:moore`."
  ([{:keys [overwrites ini-opts]} dna]
   (core/map->CASpec
    (merge
     {:label       "LifeFORM"
      :rule-spec   (core/->Rule-Life {} dna)
      :umwelt-spec (core/->Umwelt-Moore {} :column-first false)
      :ini-spec    (core/->Ini-Random ini-opts)}
     overwrites)))
  ([dna] (make-lifeform {} dna)))

(defn make-decisionform
  "2D cellular automaton. Takes a `dna` as part of its rule function, which is of type `:life`, and an initial size for its `:rand-center` type ini. Its ‘umwelt’ is of type `:moore`."
  ([{:keys [overwrites ini-opts]} dna init-size]
   (core/map->CASpec
    (merge
     {:label       "DecisionFORM"
      :rule-spec   (core/->Rule-Life {} dna)
      :umwelt-spec (core/->Umwelt-Moore {} :column-first false)
      :ini-spec    (core/->Ini-RandFigure ini-opts
                                          (core/->Ini-Constant ini-opts :n)
                                          init-size
                                          {:pos :center
                                           :align :center})}
     overwrites)))
  ([dna init-size] (make-decisionform {} dna init-size)))
,


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

(defn exprs->dna
  [& exprs]
  (expr/op-get (expr/=>* (apply expr/make exprs)) :dna))

(def common-specimen
  "Common specimen to specify cellular automata. Lists all the SelFis introduced by Ralf Peyn in ‘uFORM iFORM’."
  (let [selfi #(apply make-selfi {:overwrites {:label (str "SelFi/" %1)}} %&)
        ini-ball (make-ini :ball :n nil {:pos :center :align :center})
        ini-rand (make-ini :random)
        l 'a, e 'b, r 'c]
    {:Mark1
     (selfi "Mark1"
            (tsds-sel->dna [1 0 0 1 0 0]) ini-ball)
     :StripesD100000
     (selfi "StripesD100000"
            (tsds-sel->dna [1 0 0 0 0 0]) ini-rand)
     :StripesL000100
     (selfi "StripesL000100"
            (tsds-sel->dna [0 0 0 1 0 0]) ini-rand)
     :Mono000101
     (selfi "Mono000101"
            (tsds-sel->dna [0 0 0 1 0 1]) ini-rand)
     :Rhythm101101
     (selfi "Rhythm101101"
            (tsds-sel->dna [1 0 1 1 0 1]) ini-rand)
     :NewSense
     (selfi "NewSense"
            (tsds-sel->dna [1 1 0 1 0 0]) ini-rand)
     :Slit
     (selfi "Slit"
            (exprs->dna [[l] r] [[r] l]) ini-ball)
     :xor4vRnd
     (selfi "xor4vRnd"
            (exprs->dna [[l] r] [[r] l]) ini-rand)
     :or4v
     (selfi "or4v"
            (exprs->dna l r) ini-ball)
     :xorReId
     (selfi "xorReId"
            (exprs->dna (expr/seq-re :<r' l, r)
                        (expr/seq-re :<r' r, l)) ini-ball)
     :xorReIdRnd
     (selfi "xorReIdRnd"
            (exprs->dna (expr/seq-re :<r' l, r)
                        (expr/seq-re :<r' r, l)) ini-rand)
     :Rule4v30
     (selfi "Rule4v30"
            (exprs->dna [[l] e r] [[e] l] [[r] l]) ini-ball)
     :Rule4v111
     (selfi "Rule4v111"
            (exprs->dna [[[l] e] r] [[[l] r] e] [[[e] r] l]) ini-ball)
     :Structure111Re
     (selfi "Structure111Re"
            (tsds-sel->dna [1 0 1 1 0 0]) ini-ball)
     :CoOneAnother
     (selfi "CoOneAnother"
            (tsds-sel->dna [1 0 1 1 0 0]) ini-rand)
     :Rule4v110
     (selfi "Rule4v110"
            (exprs->dna [[e] r] [[r] e] [[r] l]) ini-ball)
     :uniTuringReRnd
     (selfi "uniTuringReRnd"
            (exprs->dna [[(tsds-sel->dna [1 0 1 1 0 0])] [l e r]]) ini-rand)}))


(s/fdef ca-iterator
  :args (s/or :ar1 (s/cat :ca-spec ::sp/ca-spec
                          :resolution ::resolution)
              :ar2 (s/cat :ca-spec ::sp/ca-spec
                          :resolution ::resolution
                          :steps   pos-int?))
  :ret  ::sp/iterator)
(defn ca-iterator
  "Returns a lazy seq that iteratively computes the next generation for the given cellular automaton specification (via `specify-ca`, etc.). Optionally, the last argument can be a number to just get the first `n` steps in its evolution."
  ([ca-spec resolution]
   (core/ca-iterator ca-spec resolution))
  ([ca-spec resolution steps]
   (take steps (core/ca-iterator ca-spec resolution))))

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
  :args (s/or :ar1 (s/cat :ca-obj ::sp/automaton)
              :ar2 (s/cat :ca-obj ::sp/automaton
                          :optimized? boolean?))
  :ret  ::sp/generation)
(defn get-current-generation
  "Given a stateful `CellularAutomaton` object, returns its current generation either as a native array (if `optimized?` is true) or a vector."
  ([ca-obj]
   (i/get-current-generation ca-obj false))
  ([ca-obj optimized?]
   (i/get-current-generation ca-obj optimized?)))

(s/fdef get-cached-history
  :args (s/or :ar1 (s/cat :ca-obj ::sp/automaton)
              :ar2 (s/cat :ca-obj ::sp/automaton
                          :optimized? boolean?))
  :ret  ::sp/evolution)
(defn get-cached-history
  "Given a stateful `CellularAutomaton` object, returns its cached history/evolution, where all generations are either native arrays (if `optimized?` is true) or vectors."
  ([ca-obj]
   (i/get-cached-history ca-obj false))
  ([ca-obj optimized?]
   (i/get-cached-history ca-obj optimized?)))

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
  :args (s/or :ar1 (s/cat :ca-spec ::sp/ca-spec
                          :resolution ::resolution)
              :ar2 (s/cat :ca-spec ::sp/ca-spec
                          :history-cache-limit pos-int?
                          :resolution ::resolution))
  :ret  ::sp/automaton)
(defn create-ca
  "Returns a stateful `CellularAutomaton` object for the given cellular automaton specification (via `specify-ca`, etc.). Callable methods are:
  - `step` to compute the next generation which gets added to the evolution
  - `restart` to re-initialize the CA with its first generation
  - `get-evolution` to obtain an immutable copy of the current evolution
  - `get-resolution` to obtain the resolution of the CA"
  ([ca-spec resolution]
   (core/create-ca ca-spec nil resolution))
  ([ca-spec history-cache-limit resolution]
   (core/create-ca ca-spec history-cache-limit resolution)))


(comment
  (keys (get-in-types [:ini]))
  (make-ini :fill-center :help)

  (make-ini :fill-center {:res [10 4] :val :u} :m)
  (make-rule :match (calc/rand-dna 2))
  (make-instance :rule :match (calc/rand-dna 2))
  (make-selfi (calc/rand-dna 2) (make-ini :random))
  (make-lifeform (calc/rand-dna 2))

  ,)

(comment
  (make-rule :match :help)
  (take 6 (ca-iterator (common-specimen :Mark1) [10]))
  ,)

(comment
  (def ca (create-ca
           (make-mindform (calc/rand-dna 2)
                          (make-ini :rand-center 10))
           [40 40]))
  (.get-resolution ca)
  (step ca)
  (get-current-generation ca)
  (restart ca)
  ,)

(comment
  (make-ini :help)
  (frequencies (sys-ini (make-ini :random {:seed 32 :weights 0.8}) 100))
  (sys-ini (make-ini :random {:seed 10}) 10)
  (sys-ini (make-ini :random) 10 10)
  (sys-ini (make-ini :rand-figure {:weights 0.2} :n 10 {:pos :center :align :center}) 40)

  (sys-ini (make-ini :rand-center 2) 10)
  (sys-ini (make-ini :rand-center 2) 10 10)
  (sys-ini (make-ini :ball) 10)
  (sys-ini (make-ini :ball) 10 10)
  (sys-ini (make-ini :fill-all :n) 10)
  (sys-ini (make-ini :fill-all :n) 10 4)
  (sys-ini (make-ini :fill-all :rand) 10)
  (sys-ini (make-ini :fill-all :rand) 10 4)
  (sys-ini (make-ini :fill-all [:n :u :i :m]) 4)
  (sys-ini (make-ini :fill-all [[:_ :_ :_ :n]
                                [:_ :_ :u :_]
                                [:_ :i :_ :_]
                                [:m :_ :_ :_]]) 4 4)
  (sys-ini (make-ini :fill-all (fn [x] (if (= 0 (mod x 3))
                                         :u :_))) 10)
  (sys-ini (make-ini :fill-all (fn [x y] (if (and (= 0 (mod x 3))
                                                  (= 0 (mod y 3)))
                                           :u :_))) 10 10)
  (sys-ini (make-ini :fill-center {:res [4] :val :u} :_) 10)
  (sys-ini (make-ini :fill-center {:res [4 3] :val :rand} :_) 10 10)
  (sys-ini (make-ini :fill-center [:u :i :m] :_) 10)
  (sys-ini (make-ini :fill-center [[:u :i :m] [:m :n :i] [:i :u :m]] :_) 5 5)

  (apply-rule (make-rule :match [:n :n :n :n
                                 :n :n :n :n
                                 :n :m :n :n
                                 :n :n :n :n])
              (observe-umwelt (make-umwelt :select-ltr 2)
                              (sys-ini (make-ini :fill-center
                                                 [:u :n :i] :n) 5)
                              ;;=> [:n :u :n :i :n]
                              [[2] :_]) ;;=> [:u :i]
              :_) ;;=> :m
  
  ,)

(comment
  (sys-ini (make-ini :random)
           10 10)
  ;; [[:m :i :m :n :n :u :m :u :u :u]
  ;;  [:m :i :m :i :n :m :i :i :u :m]
  ;;  [:n :i :n :i :m :u :u :n :i :u]
  ;;  [:u :n :m :u :i :u :u :u :u :i]
  ;;  [:u :n :m :u :m :i :m :n :u :m]
  ;;  [:i :i :i :m :m :u :m :m :n :i]
  ;;  [:i :i :n :n :n :n :i :n :n :m]
  ;;  [:m :n :n :u :n :u :i :i :n :u]
  ;;  [:n :i :m :i :n :n :n :m :i :m]
  ;;  [:i :n :n :u :n :u :m :u :i :n]]

  (ca-iterator (make-lifeform {:seed 100}
                              (calc/rand-dna 2))
               [10 10]
               1)
  
  (def ca (create-ca (make-lifeform {:seed 100}
                                    (calc/rand-dna 2))
                     [10 10]))

  (get-current-generation ca false)
  

  (sys-ini ((:constructor (get-in @!types [:ini :fill-all])) :u) 10 3)
  (make-ini :fill-all :i)
  (make-ini :fill-all :help)
  (make-ini :fill-all :help)
  ,)

(comment
  (def selfi (make-selfi (calc/rand-dna 2)
                         (make-ini :random)))
  (meta selfi)
  
  (take 3 (ca-iterator selfi [40]))
  (def ca (create-ca selfi [40]))
  (get-resolution ca)
  (seq (get-current-generation ca))
  (step ca)
  (restart ca)

  (.get-resolution ca)
  (.get-evolution ca)
  (.step ca)
  (.restart ca)
  (type ca)
  
  (def slit (common-specimen :Slit))
  (meta slit)
  (take 10 (ca-iterator slit [20]))
  '([:n :n :n :n :n :n :n :n :i :u :m :u :i :n :n :n :n :n :n :n]
    [:n :n :n :n :n :n :n :i :u :u :n :u :u :i :n :n :n :n :n :n]
    [:n :n :n :n :n :n :i :u :m :u :n :u :m :u :i :n :n :n :n :n]
    [:n :n :n :n :n :i :u :u :n :m :n :m :n :u :u :i :n :n :n :n]
    [:n :n :n :n :i :u :m :u :i :n :n :n :i :u :m :u :i :n :n :n]
    [:n :n :n :i :u :u :n :u :u :i :n :i :u :u :n :u :u :i :n :n]
    [:n :n :i :u :m :u :n :u :m :u :n :u :m :u :n :u :m :u :i :n]
    [:n :i :u :u :n :m :n :m :n :m :n :m :n :m :n :m :n :u :u :i]
    [:n :u :m :u :i :n :n :n :n :n :n :n :n :n :n :n :i :u :m :u]
    [:n :m :n :u :u :i :n :n :n :n :n :n :n :n :n :i :u :u :n :m])
  (assoc slit :label "Slit")
  ,)

(comment
  
  (observe-umwelt (make-umwelt :select-ltr 3)
                  [:n :m :u :i :n :m :i]
                  [[3] :u])
  (observe-umwelt (make-umwelt :self-select-ltr 3)
                  [[:n :i :n :u :i]
                   [:u :m :n :i :m]
                   [:m :n :i :m :u]
                   [:n :n :u :i :m]
                   [:m :u :i :i :n]]
                  [[2 2] :i])

  (let [gen [:n :m :u :i :n :m :i]
        cell [[3] :u]
        dna [:n :u :i :m  :n :n :i :i  :n :u :n :u  :n :n :n :n
             :n :u :i :m  :n :u :i :m  :n :u :n :u  :n :u :n :u
             :n :u :i :m  :n :n :i :i  :n :u :i :m  :n :n :i :i
             :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m]
        env (observe-umwelt (make-umwelt :select-ltr 3) gen cell)]
    (apply-rule (make-rule :match dna) env (last cell)))

  (let [dna [:n :u :i :m  :n :n :i :i  :n :u :n :u  :n :n :n :n
             :n :u :i :m  :n :u :i :m  :n :u :n :u  :n :u :n :u
             :n :u :i :m  :n :n :i :i  :n :u :i :m  :n :n :i :i
             :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m]]
    (sys-next (make-rule :match dna)
              (make-umwelt :select-ltr 3)
              (sys-ini (make-ini :fill-center {:res [1] :val :m} :n) 7)))
  ;; ini: [:n :n :n :m :n :n :n]
  ;;=>    [:m :m :n :m :n :m :m]

  (let [dna [:n :u :i :m  :n :n :i :i  :n :u :n :u  :n :n :n :n
             :n :u :i :m  :n :u :i :m  :n :u :n :u  :n :u :n :u
             :n :u :i :m  :n :n :i :i  :n :u :i :m  :n :n :i :i
             :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m]
        selfi (make-selfi dna
                          (make-ini :fill-center {:res [1] :val :m} :n))]
    (take 5 (ca-iterator selfi [7])))
  '([:n :n :n :m :n :n :n]
    [:m :m :n :m :n :m :m]
    [:n :m :n :m :n :n :n]
    [:n :m :n :m :n :m :m]
    [:n :m :n :m :n :n :m])
  ,)

#_
(comment
  (require '[clojure.string :as string])
  (require '[clojure.pprint :refer [pprint]])
  (require '[formform.expr :as expr])

  (defn pp-gen [gen]
    (let [pp-val (fn [c]
                   (case c
                     :n "⁠█" :u "▒" :i "░" :m "▓" "?"))]
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
