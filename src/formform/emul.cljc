(ns formform.emul
  "API for the `emul` module of `formform`."
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.emul.core :as core]
            [formform.calc.specs :as calc-sp]
            [formform.emul.specs :as sp]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen]))

(s/fdef sys-ini
  :args (s/cat :resolution ::sp/resolution
               :ini-spec   ::sp/ini-spec)
  :ret  ::sp/generation)
(def sys-ini
  "Multimethod that returns a generation from a registered ini-pattern, given a resolution (which defines the CA dimension) and an ini-spec.
  - the ini-spec must be a vector of the pattern type (keyword) and 0 or more arguments, as specified by the pattern
  - you may register a custom pattern by defining a new method on `sys-ini`"
  core/sys-ini)

(s/fdef get-umwelt
  :args (s/cat :generation  ::sp/generation
               :cell        ::sp/cell
               :umwelt-spec ::sp/umwelt-spec)
  :ret  ::sp/umwelt)
(def get-umwelt
  "Multimethod that returns a ‘umwelt’ (vector of neighbor cell values) from a registered umwelt-pattern, given a generation, the current cell and an umwelt-spec.
  - the umwelt-spec must be a vector of the pattern type (keyword) and 0 or more arguments, as specified by the pattern
  - you may register a custom pattern by defining a new method on `get-umwelt`"
  core/get-umwelt)

(s/fdef apply-rule
  :args (s/cat :umwelt    ::sp/umwelt
               :cell-val  ::calc-sp/const?
               :rule-spec ::sp/rule-spec)
  :ret  ::calc-sp/const?)
(def apply-rule
  "Multimethod that returns a cell value from a registered rule, given an ‘umwelt’ (see `get-umwelt`), the current cell value and a rule-spec.
  - the rule-spec must be a vector of the rule type (keyword) and 0 or more arguments, as specified by the rule
  - you may register a custom rule by defining a new method on `apply-rule`"
  core/apply-rule)

(s/fdef sys-next
  :args (s/cat :rule-spec   ::sp/rule-spec
               :umwelt-spec ::sp/umwelt-spec
               :generation  ::sp/generation)
  :ret  ::sp/generation)
(defn sys-next
  "Given a ‘rule-spec’, an ‘umwelt-spec’ and a generation, returns the next generation, computed from the rule and umwelt specifications."
  ([rule-spec umwelt-spec generation]
   (core/sys-next (core/get-resolution generation)
                  rule-spec umwelt-spec generation)))


(s/fdef make-selfi
  :args (s/alt :ar1 (s/cat :resolution ::sp/resolution
                           :dna        ::calc-sp/dna
                           :ini-spec   ::sp/ini-spec)
               :ar2 (s/cat :resolution ::sp/resolution
                           :dna        ::calc-sp/dna
                           :ini-spec   ::sp/ini-spec
                           :steps      pos-int?))
  :ret  (s/or ::sp/automaton-1d
              ::sp/evolution-1d))
(defn make-selfi
  "Initializes a ‘SelFi’: a 1D 4-valued cellular automaton. Takes a resolution `width`, a formDNA (as the rule function) and initial parameters.

  Returns a lazy seq that iteratively computes the next generation. Each generation consists of constants arranged in a 2D (nested) vector. Optionally, the last argument can be a number to just get the first `n` steps of evolution."
  ([res dna ini-spec]
   (core/make-selfi res dna ini-spec))
  ([res dna ini-spec steps]
   (take steps (core/make-selfi res dna ini-spec))))

(s/fdef make-mindform
  :args (s/alt :ar1 (s/cat :resolution ::sp/resolution
                           :dna        ::calc-sp/dna
                           :ini-spec   ::sp/ini-spec)
               :ar2 (s/cat :resolution ::sp/resolution
                           :dna        ::calc-sp/dna
                           :ini-spec   ::sp/ini-spec
                           :steps      pos-int?))
  :ret  (s/or ::sp/automaton-2d
              ::sp/evolution-2d))
(defn make-mindform
  "Initializes a ‘mindFORM’: a 2D 4-valued cellular automaton. Takes a resolution `[width height]`, a formDNA (as the rule function) and initial parameters.

  Returns a lazy seq that iteratively computes the next generation. Each generation consists of constants arranged in a 2D (nested) vector. Optionally, the last argument can be a number to just get the first `n` steps of evolution."
  ([res dna ini-spec]
   (core/make-mindform res dna ini-spec))
  ([res dna ini-spec steps]
   (take steps (core/make-mindform res dna ini-spec))))


(s/def ::tsds-selection
  (s/coll-of #{0 1} :kind vector? :count 6))

(comment
  (s/conform ::tsds-selection [0 1 1 0 1 0])
  ,)

(defn- tsds-sel->dna
  [selection]
  (expr/op-get (expr/=>* (expr/make :tsds selection 'a 'b 'c)) :dna))

(defn- conform-tsds-dna-or-sel
  [dna-or-sel]
  (cond
    (s/valid? ::tsds-selection dna-or-sel) (tsds-sel->dna dna-or-sel)
    (and (calc/dna? dna-or-sel)
         (= 3 (calc/dna-dimension dna-or-sel))) dna-or-sel
    :else (throw
           (ex-info "Input must be either a formDNA of dimension 3 or a 6-element binary selection vector." {:input dna-or-sel}))))

(s/fdef make-lifeform
  :args (s/alt :ar1 (s/cat :resolution ::sp/resolution
                           :dna-or-sel (s/or ::calc-sp/dna
                                             ::tsds-selection))
               :ar2 (s/cat :resolution ::sp/resolution
                           :dna-or-sel (s/or ::calc-sp/dna
                                             ::tsds-selection)
                           :steps      pos-int?))
  :ret  (s/or ::sp/automaton-2d
              ::sp/evolution-2d))
(defn make-lifeform
  "Initializes a ‘lifeFORM’: a 2D 4-valued cellular automaton. Takes a resolution `[width height]` and either a formDNA of dimension 3 or a 6-element vector of binary digits (for a ‘triple-selective decision system’). The initial generation is fully randomized.
  
  Returns a lazy seq that iteratively computes the next generation. Each generation consists of constants arranged in a 2D (nested) vector. Optionally, the last argument can be a number to just get the first `n` steps of evolution."
  ([res dna-or-sel]
   (core/make-lifeform res (conform-tsds-dna-or-sel dna-or-sel)))
  ([res dna-or-sel steps]
   (take steps (core/make-lifeform
                res (conform-tsds-dna-or-sel dna-or-sel)))))

(s/fdef make-decisionform
  :args (s/alt :ar1 (s/cat :resolution ::sp/resolution
                           :dna-or-sel (s/or ::calc-sp/dna
                                             ::tsds-selection)
                           :init-size  pos-int?)
               :ar2 (s/cat :resolution ::sp/resolution
                           :dna-or-sel (s/or ::calc-sp/dna
                                             ::tsds-selection)
                           :init-size  pos-int?
                           :steps      pos-int?))
  :ret  (s/or ::sp/automaton-2d
              ::sp/evolution-2d))
(defn make-decisionform
  "Initializes a ‘decisionFORM’: a 2D 4-valued cellular automaton. Takes a resolution `[width height]`, either a formDNA of dimension 3 or a 6-element vector of binary digits (for a ‘triple-selective decision system’) and the size of the initial square of random cell values. This square on a background of unmarked values sets up the initial generation.
  
  Returns a lazy seq that iteratively computes the next generation. Each generation consists of constants arranged in a 2D (nested) vector. Optionally, the last argument can be a number to just get the first `n` steps of evolution."
  ([res dna-or-sel init-size]
   (core/make-decisionform res (conform-tsds-dna-or-sel dna-or-sel) init-size))
  ([res dna-or-sel init-size steps]
   (take steps (core/make-decisionform
                res (conform-tsds-dna-or-sel dna-or-sel) init-size))))


(comment
  (do (require '[clojure.pprint :refer [pprint]])
      (pprint (methods sys-ini)))
  (sys-ini [5] [:fill-all :M])
  (sys-ini [3 5] [:fill-all :M])
  (sys-ini [5] [:random])
  (sys-ini [3 5] [:random])
  (sys-ini [10] [:rand-center 3])
  (sys-ini [5 5] [:rand-center 3])
  (sys-ini [10] [:fill-center {:res 4 :val :rand} :_])
  (sys-ini [5 5] [:fill-center {:res [3 3] :val :rand} :_])
  (sys-ini [10] [:fill-center [:U :I :M] :_])
  (sys-ini [5 5] [:fill-center [[:U :I :M] [:M :N :I] [:I :U :M]] :_])
  (sys-ini [17] [:ball])
  (sys-ini [13 13] [:ball])
  (sys-ini [5] nil)
  ,)

(comment
  
  (get-umwelt [:N :M :U :I :N :M :I] [[3] :U] [:select-ltr 3])
  (get-umwelt [[:N :I :N :U :I]
               [:U :M :N :I :M]
               [:M :N :I :M :U]
               [:N :N :U :I :M]
               [:M :U :I :I :N]] [[2 2] :I] [:self-select-ltr 3])

  (let [gen [:N :M :U :I :N :M :I]
        cell [[3] :U]
        dna [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
             :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
             :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]
        env (get-umwelt gen cell [:select-ltr 3])]
    (apply-rule env (last cell) [:match dna]))

  (let [dna [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
             :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
             :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]]
    (sys-next [:match dna] [:select-ltr 3]
              (sys-ini [7] [:fill-center {:res 1 :val :M} :N])))
  ;; ini: [:N :N :N :M :N :N :N]
  ;;=>    [:M :M :N :M :N :M :M]

  (let [dna [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
             :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
             :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]
        selfi (make-selfi [7] dna [:fill-center {:res 1 :val :M} :N])]
    (take 5 selfi))
  '([:N :N :N :M :N :N :N]
    [:M :M :N :M :N :M :M]
    [:N :M :N :M :N :N :N]
    [:N :M :N :M :N :M :M]
    [:N :M :N :M :N :N :M])
  ,)

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
