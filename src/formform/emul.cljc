(ns formform.emul
  "API for the `emul` module of `formform`.

  ## Concepts

  ### Cellular Automata

  **cellular automaton**  
  → system that _iteratively_ produces the next _generation_

  * _automaton_ → model for the description of _rule_-based _state_ changes  
  * _iteration_ → reproduce the operation  
  * _state_ → correspondence between time and _value_ of the system

  **evolution**  
  → causal chain of _generations_
  
  **generation**  
  → _state_ in an _evolution_  
  → n-dimensional relation between _cells_
  
  **cell**  
  → system that indicates a _calc/value_

  ### Specification

  **ini {sys-ini}**  
  → initial _generation_ of the _cellular automaton_  
  → _configuration_ of _cells_ by a pattern

  * _configuration_ → _arrangement_ by a perspective
  * _arrangement_ → selection in space

  **umwelt**  
  → _arrangement_ of _cells_ observable from/by the _cell_  
  → environment for _rule_ application

  **rule {sys-next}**  
  → function to determine the next _state_ of the _cell_  
  → correspondence between _umwelt_ / self _(→Cell)_ and _calc/value_
  "
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.emul.core :as core]
            [formform.emul.interfaces :as i]
            [formform.calc.specs :as calc-sp]
            [formform.expr.specs :as expr-sp]
            [formform.emul.specs :as sp]
            [formform.utils :as utils]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen])
  #?(:cljs (:require-macros
            [formform.emul :refer [defini defumwelt defrule]])))

#?(:clj (set! *warn-on-reflection* true))


(defmacro defini
  "Defines a new type of ini pattern, to be specified with [[make-ini]].
  Takes a keyword identifier (`type-k`), `fields` for data the user needs to provide, an optional `doc-string?` (to describe the fields and the pattern) and one or more implementations of:

  * `(make-gen [this opts w] …)` for a 1D pattern
  * `(make-gen [this opts w h] …)` for a 2D pattern
  * `opts` is an options map that can have keys such as `:seed`."
  [type-k fields doc-string? & methods]
  (apply i/defini-impl type-k fields doc-string? methods))

(defmacro defumwelt
  "Defines a new type of umwelt pattern, to be specified with [[make-umwelt]].
  Takes a keyword identifier (`type-k`), `fields` for data the user needs to provide, an optional `doc-string?` (to describe the fields and the pattern) and one or more implementations of:

  * `(observe-umwelt [this gen1d cell w] …)` for a 1D pattern
  * `(observe-umwelt [this gen2d cell w h] …)` for a 2D pattern
  * `gen1d`/`gen2d` is a vector of the current generation (flat in 1D, nested in 2D)
  * `cell` is a vector `[[x …] v]` of the current cell coordinates and value"
  [type-k fields doc-string? & methods]
  (apply i/defumwelt-impl type-k fields doc-string? methods))

(defmacro defrule
  "Defines a new type of rule pattern, to be specified with [[make-rule]].
  Takes a keyword identifier (`type-k`), `fields` for data the user needs to provide, an optional `doc-string?` (to describe the fields and the pattern) and one or more implementations of:

  * `(apply-rule [this umwelt self-v] …)`
  * `umwelt` is an ‘umwelt’ as provided by [[observe-umwelt]]
  * `self-v` is the value of the currently selected cell"
  [type-k fields doc-string? & methods]
  (apply i/defrule-impl type-k fields doc-string? methods))

(def !types "All registered ini/umwelt/rule types." i/!types)

(defn- get-in-types
  [ks]
  (let [types @!types]
    (when-not ((set (keys types)) (first ks))
      (throw (ex-info (str "Unknown category `" (first ks) "`.")
                      {:keys ks})))
    (get-in types ks)))

(defn- help-type
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

(defn- help-cat
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
          (apply i/->rec constructor args))))))

(def make-ini
  "Creates an instance of a CA ini (initial conditions) specification of a given type (as a keyword) and with given parameters as required by the type.

  * `(make-ini :help)` prints a list of all ini types and their parameters
  * `(make-ini :t :help)` prints the parameters and docs for given type `:t`"
  (partial make-instance :ini))

(def make-umwelt
  "Creates an instance of a CA umwelt (neighborhood) specification of a given type (as a keyword) and with given parameters as required by the type.

  * `(make-umwelt :help)` prints a list of all umwelt types and their parameters
  * `(make-umwelt :t :help)` prints the parameters and docs for given type `:t`"
  (partial make-instance :umwelt))

(def make-rule
  "Creates an instance of a CA rule specification of a given type (as a keyword) and with given parameters as required by the type.

  * `(make-rule :help)` prints a list of all rule types and their parameters
  * `(make-rule :t :help)` prints the parameters and docs for given type `:t`"
  (partial make-instance :rule))


(s/fdef sys-ini
  :args (s/alt :ar2 (s/cat :ini-spec   ::sp/ini-spec
                           :resolution ::sp/resolution)
               :ar3 (s/cat :ini-spec   ::sp/ini-spec
                           :resolution ::sp/resolution
                           :opts       (s/keys :opt-un [:rand/seed])))
  :ret  ::sp/generation)
(defn sys-ini
  "Returns an (initial) generation given an `ini-spec` (via [[make-ini]]) and a `resolution`.
  `resolution` must be a vector `[width]` (1D) or `[width height]` (2D).

  The last argument can be an options map with keys:

  * `:seed` → an integer number to provide a seed for reproducable randomness"
  ([ini-spec resolution] (apply i/make-gen ini-spec {} resolution))
  ([ini-spec resolution opts] (apply i/make-gen ini-spec opts resolution)))

(s/fdef sys-next
  :args (s/cat :rule-spec   ::sp/rule-spec
               :umwelt-spec ::sp/umwelt-spec
               :generation  ::sp/generation)
  :ret  ::sp/generation)
(defn sys-next
  "Computes and returns the next generation given a `rule-spec` (via [[make-rule]]), an `umwelt-spec` (via [[make-umwelt]]) and a `generation`."
  [rule-spec umwelt-spec generation]
  (let [[w h :as res] (core/get-resolution-from-generation generation)]
    (core/sys-next res (range w) (when h (range h))
                   rule-spec umwelt-spec generation)))


(s/fdef observe-umwelt
  :args (s/cat :umwelt-spec ::sp/umwelt-spec
               :generation  ::sp/generation
               :cell        ::sp/cell)
  :ret  ::sp/umwelt)
(defn observe-umwelt
  "Returns an ‘umwelt’ given a `umwelt-spec` (via [[make-umwelt]]), a `generation` and the current `cell` (a vector of the shape `[[x ?y] value]`)."
  [umwelt-spec generation cell]
  (let [res (core/get-resolution-from-generation generation)]
    (apply i/observe-umwelt umwelt-spec generation cell res)))

(s/fdef apply-rule
  :args (s/cat :rule-spec ::sp/rule-spec
               :umwelt    ::sp/umwelt
               :cell      ::sp/cell)
  :ret  ::calc-sp/const_)
(defn apply-rule
  "Returns a cell value given a `rule-spec` (via [[make-rule]]), an `umwelt` (via [[observe-umwelt]]) and the current `cell` (a vector of the shape `[[x ?y] value]`)."
  [rule-spec umwelt cell]
  (let [self-v (second cell)]
    (i/apply-rule rule-spec umwelt self-v)))


(s/fdef specify-ca
  :args (s/alt :ar1 (s/cat :specs-map (s/keys :req-un [::sp/rule-spec
                                                       ::sp/umwelt-spec
                                                       ::sp/ini-spec]))
               :ar2 (s/cat :specs-map (s/keys :req-un [::sp/rule-spec
                                                       ::sp/umwelt-spec
                                                       ::sp/ini-spec])
                           :label string?))
  :ret  ::sp/ca-spec)
(defn specify-ca
  "Returns a `CASpec` record, given a `specs-map` (which is a map of specifications for a custom cellular automaton) and an optional `label`.
  The `CASpec` can be used with [[ca-iterator]] or [[create-ca]]. The input map needs to have all of the following entries:

  * `:rule-spec`: a rule specification as per [[make-rule]]
  * `:umwelt-spec`: an umwelt specification as per [[make-umwelt]]
  * `:ini-spec`: an ini specification as per [[make-ini]]

  Note that formform.emul has constructors for common ca specs via [[make-selfi]], [[make-mindform]], [[make-lifeform]] and [[make-decisionform]]. Furthermore, there are predefined ca specs in [[common-specimen]]."
  ([specs-map]
   (i/->rec core/map->CASpec specs-map))
  ([specs-map label]
   (i/->rec core/map->CASpec (assoc specs-map :label label))))


;; Constructors for common CA specifications

(s/def :ca-spec/overwrites map?)

(s/fdef make-selfi
  :args (s/alt :ar2 (s/cat :dna ::calc-sp/dna
                           :ini  ::sp/ini-spec)
               :ar3 (s/cat :dna  ::calc-sp/dna
                           :ini  ::sp/ini-spec
                           :opts (s/keys :opt-un [:ca-spec/overwrites])))
  :ret  ::sp/ca-spec)
(defn make-selfi
  "Returns a 1D cellular automaton known as a “SelFi” given a `dna` for its rule function and an `ini-spec` (via [[make-ini]]).
  Its ‘umwelt’ is of type `:select-ltr`.

  The last argument can be an options map with keys:

  * `:overwrites` → a map to overwrite any part of the returned CA spec"
  ([dna ini-spec] (make-selfi dna ini-spec {}))
  ([dna ini-spec {:keys [overwrites]}]
   (let [umwelt-size (calc/dna-dimension dna)]
     (i/->rec core/map->CASpec
              (merge
               {:label       "SelFi"
                :rule-spec   (i/->rec core/->Rule-Match {} dna)
                :umwelt-spec (i/->rec core/->Umwelt-SelectLtr {} umwelt-size)
                :ini-spec    ini-spec}
               overwrites)))))

(s/fdef make-mindform
  :args (s/alt :ar2 (s/cat :dna ::calc-sp/dna
                           :ini  ::sp/ini-spec)
               :ar3 (s/cat :dna  ::calc-sp/dna
                           :ini  ::sp/ini-spec
                           :opts (s/keys :opt-un [:ca-spec/overwrites])))
  :ret  ::sp/ca-spec)
(defn make-mindform
  "Returns a 2D cellular automaton known as a “mindFORM” given a `dna` for its rule function and an `ini-spec` (via [[make-ini]]).

  The last argument can be an options map with keys:

  * `:overwrites` → a map to overwrite any part of the returned CA spec "
  ([dna ini-spec] (make-mindform dna ini-spec {}))
  ([dna ini-spec {:keys [overwrites]}]
   (let [umwelt-size (calc/dna-dimension dna)]
     (i/->rec core/map->CASpec
              (merge
               {:label       "MindFORM"
                :rule-spec   (i/->rec core/->Rule-Match {} dna)
                :umwelt-spec (i/->rec core/->Umwelt-SelfSelectLtr
                                      {} umwelt-size)
                :ini-spec    ini-spec}
               overwrites)))))

(s/fdef make-lifeform
  :args (s/alt :ar1 (s/cat :dna ::calc-sp/dna)
               :ar2 (s/cat :dna ::calc-sp/dna
                           :opts (s/keys :opt-un [:ca-spec/overwrites
                                                  ::sp/ini-opts])))
  :ret  ::sp/ca-spec)
(defn make-lifeform
  "Returns a 2D cellular automaton known as a “lifeFORM” given a `dna` for its rule function.

  The last argument can be an options map with keys:

  * `:overwrites` → a map to overwrite any part of the returned CA spec"
  ([dna] (make-lifeform dna {}))
  ([dna {:keys [overwrites ini-opts]}]
   (i/->rec core/map->CASpec
            (merge
             {:label       "LifeFORM"
              :rule-spec   (i/->rec core/->Rule-Life {} dna)
              :umwelt-spec (i/->rec core/->Umwelt-Moore {} :column-first false)
              :ini-spec    (i/->rec core/->Ini-Random ini-opts)}
             overwrites))))

(s/fdef make-decisionform
  :args (s/alt :ar2 (s/cat :dna ::calc-sp/dna
                           :init-size pos-int?)
               :ar3 (s/cat :dna ::calc-sp/dna
                           :init-size pos-int?
                           :opts (s/keys :opt-un [:ca-spec/overwrites
                                                  ::sp/ini-opts])))
  :ret  ::sp/ca-spec)
(defn make-decisionform
  "Returns a 2D cellular automaton known as a “decisionFORM” given a `dna` for its rule function and an `initial-size` for its ini (type `:rand-figure`).

  The last argument can be an options map with keys:

  * `:overwrites` → a map to overwrite any part of the returned CA spec"
  ([dna initial-size] (make-decisionform dna initial-size {}))
  ([dna initial-size {:keys [overwrites ini-opts]}]
   (i/->rec core/map->CASpec
            (merge
             {:label       "DecisionFORM"
              :rule-spec   (i/->rec core/->Rule-Life {} dna)
              :umwelt-spec (i/->rec core/->Umwelt-Moore {} :column-first false)
              :ini-spec    (i/->rec core/->Ini-RandFigure ini-opts
                                    (i/->rec core/->Ini-Constant ini-opts :n)
                                    initial-size
                                    :center)}
             overwrites))))


(def ini-patterns
  "A collection of predefined patterns for use with `:figure` inis."
  {:ball
   [:i :u :m :u :i]
   :hole
   [:u :i :n :i :u]
   :ball2d
   [[:_ :_ :i :_ :_]
    [:_ :i :u :i :_]
    [:i :u :m :u :i]
    [:_ :i :u :i :_]
    [:_ :_ :i :_ :_]]
   :hole2d
   [[:_ :_ :u :_ :_]
    [:_ :u :i :u :_]
    [:u :i :n :i :u]
    [:_ :u :i :u :_]
    [:_ :_ :u :_ :_]]
   :ball2d-square
   [[:i :i :i :i :i]
    [:i :u :u :u :i]
    [:i :u :m :u :i]
    [:i :u :u :u :i]
    [:i :i :i :i :i]]
   :hole2d-square
   [[:u :u :u :u :u]
    [:u :i :i :i :u]
    [:u :i :n :i :u]
    [:u :i :i :i :u]
    [:u :u :u :u :u]]})


;; ? replace with evaluated dna/selfi data or leave as is for reference
(def common-specimen
  "Map of common specimen to specify cellular automata. Contains all the SelFis introduced by Ralf Peyn in ‘uFORM iFORM’."
  (let [selfi #(make-selfi %2 %3 {:overwrites {:label (str "SelFi/" %1)}})
        ini-ball (make-ini :figure :n (ini-patterns :ball) :center)
        ini-rand (make-ini :random)
        l 'a, e 'b, r 'c]
    {:Mark1
     (selfi "Mark1"
            (expr/ts==>* 1 0 0 1 0 0) ini-ball)
     :StripesD100000
     (selfi "StripesD100000"
            (expr/ts==>* 1 0 0 0 0 0) ini-rand)
     :StripesL000100
     (selfi "StripesL000100"
            (expr/ts==>* 0 0 0 1 0 0) ini-rand)
     :Mono000101
     (selfi "Mono000101"
            (expr/ts==>* 0 0 0 1 0 1) ini-rand)
     :Rhythm101101
     (selfi "Rhythm101101"
            (expr/ts==>* 1 0 1 1 0 1) ini-rand)
     :NewSense
     (selfi "NewSense"
            (expr/ts==>* 1 1 0 1 0 0) ini-rand)
     :Slit
     (selfi "Slit"
            (expr/==>* [:- [[l] r] [[r] l]]) ini-ball)
     :xor4vRnd
     (selfi "xor4vRnd"
            (expr/==>* [:- [[l] r] [[r] l]]) ini-rand)
     :or4v
     (selfi "or4v"
            (expr/==>* [:- l r]) ini-ball)
     :xorReId
     (selfi "xorReId"
            (expr/==>* [:-
                        (expr/seq-re :<r' l, r)
                        (expr/seq-re :<r' r, l)]) ini-ball)
     :xorReIdRnd
     (selfi "xorReIdRnd"
            (expr/==>* [:-
                        (expr/seq-re :<r' l, r)
                        (expr/seq-re :<r' r, l)]) ini-rand)
     :Rule4v30
     (selfi "Rule4v30"
            (expr/==>* [:- [[l] e r] [[e] l] [[r] l]]) ini-ball)
     :Rule4v111
     (selfi "Rule4v111"
            (expr/==>* [:- [[[l] e] r] [[[l] r] e] [[[e] r] l]]) ini-ball)
     :Structure111Re
     (selfi "Structure111Re"
            (expr/ts==>* 1 0 1 1 0 0) ini-ball)
     :CoOneAnother
     (selfi "CoOneAnother"
            (expr/ts==>* 1 0 1 1 0 0) ini-rand)
     :Rule4v110
     (selfi "Rule4v110"
            (expr/==>* [:- [[e] r] [[r] e] [[r] l]]) ini-ball)
     :uniTuringReRnd
     (selfi "uniTuringReRnd"
            (expr/==>* [[(expr/ts==>* 1 0 1 1 0 0)] [l e r]])
            ini-rand)}))


;; ? steps really necessary
(s/fdef ca-iterator
  :args (s/alt :ar2 (s/cat :ca-spec    ::sp/ca-spec
                           :resolution ::sp/resolution)
               :ar3 (s/cat :ca-spec    ::sp/ca-spec
                           :resolution ::sp/resolution
                           :opts       (s/keys :opt-un [:rand/seed]))
               :ar4 (s/cat :ca-spec    ::sp/ca-spec
                           :resolution ::sp/resolution
                           :opts       (s/keys :opt-un [:rand/seed])
                           :steps      pos-int?))
  :ret  ::sp/iterator)
(defn ca-iterator
  "Returns a lazy seq that iteratively computes the next generation for the given a `ca-spec` (cellular automaton specification, via [[specify-ca]], etc.) and a `resolution` vector.

  May take an additional argument as an options map with keys:

  * `:seed` → an integer number to provide a seed for reproducable randomness (in case of randomness in the ini spec)

  May take as the final argument (after the options map) a number to just get the first _n_ `steps` in its evolution."
  ([ca-spec resolution]
   (core/ca-iterator ca-spec {} resolution))
  ([ca-spec resolution opts]
   (core/ca-iterator ca-spec opts resolution))
  ([ca-spec resolution opts steps]
   (take steps (core/ca-iterator ca-spec opts resolution))))

(s/fdef step
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  any?)
(defn step
  "Given a stateful `ca-object` (of type `CellularAutomaton`), computes its next generation and appends it to its evolution state."
  [ca-object]
  (i/step ca-object))

(s/fdef restart
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  any?)
(defn restart
  "Given a stateful `ca-object` (of type `CellularAutomaton`), resets its evolution to the initial generation."
  [ca-obj]
  (i/restart ca-obj))

#_#_
(s/fdef get-evolution
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  ::sp/evolution)
(defn get-evolution
  "Given a stateful `ca-object` (of type `CellularAutomaton`), returns an immutable copy of its current evolution state."
  [ca-obj]
  (i/get-evolution ca-obj))

(s/def ::optimized? boolean?)

(s/fdef get-current-generation
  :args (s/alt :ar1 (s/cat :ca-obj ::sp/automaton)
               :ar2 (s/cat :ca-obj ::sp/automaton
                           :opts (s/keys :opt-un [::optimized?])))
  :ret  ::sp/generation)
(defn get-current-generation
  "Given a stateful `ca-object` (of type `CellularAutomaton`), returns its current generation either as a vector or a native array (if `optimized?` is true)."
  ([ca-obj] (i/get-current-generation ca-obj false))
  ([ca-obj {:keys [optimized?] :or {optimized? false}}]
   (i/get-current-generation ca-obj optimized?)))

(s/fdef get-cached-history
  :args (s/alt :ar1 (s/cat :ca-obj ::sp/automaton)
               :ar2 (s/cat :ca-obj ::sp/automaton
                           :opts (s/keys :opt-un [::optimized?])))
  :ret  ::sp/evolution)
(defn get-cached-history
  "Given a stateful `ca-object` (of type `CellularAutomaton`), returns its cached history/evolution, where all generations are either vectors or native arrays (if `optimized?` is true)."
  ([ca-obj] (i/get-cached-history ca-obj false))
  ([ca-obj {:keys [optimized?] :or {optimized? false}}]
   (i/get-cached-history ca-obj optimized?)))

(s/fdef get-system-time
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  nat-int?)
(defn get-system-time
  "Given a stateful `ca-object` (of type `CellularAutomaton`), returns its generation index (aka “system-time”)."
  [ca-obj]
  (i/get-system-time ca-obj))

(s/fdef get-history-cache-limit
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  nat-int?)
(defn get-history-cache-limit
  "Given a stateful `ca-object` (of type `CellularAutomaton`), returns the max. number of generations it caches (stored in its history)."
  [ca-obj]
  (i/get-history-cache-limit ca-obj))

(s/fdef get-resolution
  :args (s/cat :ca-obj ::sp/automaton)
  :ret  ::sp/resolution)
(defn get-resolution
  "Given a stateful `ca-object` (of type `CellularAutomaton`), returns its resolution."
  [ca-obj]
  (i/get-resolution ca-obj))

(s/def ::history-cache-limit pos-int?)

(s/fdef create-ca
  :args (s/alt :ar2 (s/cat :ca-spec ::sp/ca-spec
                           :resolution ::sp/resolution)
               :ar3 (s/cat :ca-spec ::sp/ca-spec
                           :resolution ::sp/resolution
                           :opts (s/keys :opt-un
                                         [:rand/seed ::history-cache-limit])))
  :ret  ::sp/automaton)
(defn create-ca
  "Returns a stateful `CellularAutomaton` object for the given `ca-spec` (cellular automaton specification, via [[specify-ca]], etc.) and a `resolution` vector.

  Callable methods are `step`, `restart`, `get-resolution`, `get-current-generation`, `get-cached-history`, `get-system-time` and `get-history-cache-limit` (see docs for further explanation).

  The last argument can be an options map with keys:

  * `history-cache-limit` → manually set the limit for how many generations will be cached (otherwise an heuristic algorithm is used)
  * `:seed` → an integer number to provide a seed for reproducable randomness (in case of randomness in the ini spec)"
  ([ca-spec resolution]
   (core/create-ca ca-spec {} resolution))
  ([ca-spec resolution opts]
   (core/create-ca ca-spec opts resolution)))


(def ^:no-doc fns-with-specs
  (utils/list-fn-specs "formform.emul"))

