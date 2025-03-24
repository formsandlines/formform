(ns formform.emul.specs
  (:require [formform.calc.specs :as calc-sp]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            ;; [clojure.spec.gen.alpha :as gen]
            [formform.emul.interfaces :as i]
            #?(:cljs [formform.emul.core :refer [CellularAutomaton]]))
  #?(:clj (:import [formform.emul.core CellularAutomaton])))

(s/def ::cell
  (s/cat :coords (s/coll-of int? :kind vector?)
         :value  ::calc-sp/const?))

(s/def ::resolution (s/or :1d (s/cat :w pos-int?)
                          :2d (s/cat :w pos-int? :h pos-int?)))
(s/def ::res ::resolution)

(s/def ::generation-1d
  (s/coll-of ::calc-sp/const? :kind vector?))

(s/def ::generation-2d
  (s/and (s/coll-of (s/coll-of ::calc-sp/const? :kind vector?)
                    :kind vector?)
         #(apply = (mapv count %))))

(s/def ::generation (s/or :1d ::generation-1d
                          :2d ::generation-2d))

(s/def ::evolution-1d
  (s/and (s/coll-of ::generation-1d)
         #(apply = (map count %))))

(s/def ::evolution-2d
  (s/and (s/coll-of ::generation-2d)
         #(apply = (map count %))))

(s/def ::evolution (s/or :1d ::evolution-1d
                         :2d ::evolution-2d))

(s/def ::umwelt
  (s/coll-of ::calc-sp/const? :kind vector?))



(s/def ::umwelt-spec
  #(satisfies? i/Umwelt %))

(s/def ::ini-spec
  #(satisfies? i/Ini %))

(s/def ::rule-spec
  #(satisfies? i/Rule %))

(s/def ::species-spec
  #(satisfies? i/Specifier %))


(s/def ::val (s/or :random #{:rand}
                   :const? ::calc-sp/const?))

(s/def :ini/fill-all (s/cat :bg ::val))
(s/def :ini/fill-center
  (s/cat :area (s/or :template (s/keys :req-un [::res ::val])
                     :explicit ::generation)
         :bg ::val))
(s/def :ini/random (s/cat))
(s/def :ini/rand-center (s/cat :size pos-int?))
(s/def :ini/ball (s/cat))

(s/def :umwelt/select-ltr (s/cat :size pos-int?))
(s/def :umwelt/self-select-ltr (s/cat :size pos-int?))
(s/def :umwelt/moore (s/cat :self? boolean?))
(s/def :umwelt/von-neumann (s/cat :self? boolean?))

(s/def :rule/match (s/cat :dna ::calc-sp/dna))
(s/def :rule/life (s/cat :dna ::calc-sp/dna))

(s/def :species/selfi (s/cat :dna ::calc-sp/dna :ini ::ini-spec))
(s/def :species/mindform (s/cat :dna ::calc-sp/dna :ini ::ini-spec))
(s/def :species/lifeform (s/cat :dna ::calc-sp/dna))
(s/def :species/decisionform (s/cat :dna ::calc-sp/dna :init-size pos-int?))


(s/def ::ca-spec
  (s/keys :req-un [::resolution ::rule-spec ::umwelt-spec ::ini-spec]))

(s/def ::ca-constructor
  #(satisfies? i/Specifier %))


(binding [clojure.spec.alpha/*coll-check-limit* 3]
  ;; only realize 3 generation samples
  (s/def ::iterator-1d
    (s/every ::generation-1d :kind utils/iterate?))

  (s/def ::iterator-2d
    (s/every ::generation-2d :kind utils/iterate?))

  (s/def ::iterator (s/or :1d ::iterator-1d
                          :2d ::iterator-2d)))

(s/def ::automaton
  (partial instance? CellularAutomaton))


(comment
  (require '[formform.calc :as calc])
  (require '[formform.emul :as emul])

  (s/conform ::rule-spec (emul/make-rule :match (calc/rand-dna 3)))
  (s/conform ::umwelt-spec (emul/make-umwelt :moore false))
  (s/conform ::ini-spec (emul/make-ini :ball))

  (every? s/invalid?
          [(s/conform ::ini-spec (emul/make-rule :life (calc/rand-dna 3)))
           (s/conform ::rule-spec (emul/make-umwelt :moore false))
           (s/conform ::umwelt-spec (emul/make-ini :ball))])

  (s/conform ::generation-1d [:N :M :I :M :M :U])
  (s/conform ::generation-2d [[:N :M] [:I :M] [:M :U]])

  (s/valid? ::iterator-2d (iterate (fn [xs] (conj xs [:U :I])) [[:N :M]]))
  (s/valid? ::iterator-1d (iterate (fn [xs] (conj xs :U)) [:N :M]))

  (def mindform (emul/make-species :mindform
                                   (calc/rand-dna 3)
                                   (emul/make-ini :random)))
  (s/conform ::ca-constructor mindform)
  (def ca (emul/specify-ca mindform 40 30))
  (s/conform ::ca-spec ca)
  (s/conform ::automaton (emul/create-ca ca))

  (s/conform ::evolution
             (take 10 (iterate (fn [xs] (into [] (repeatedly 2 calc/rand-const)))
                               [:N :N])))
  (s/conform ::evolution
             (take 10 (iterate (fn [xs]
                                 (into []
                                       (repeatedly
                                        2 #(into [] (repeatedly
                                                     2 calc/rand-const)))))
                               [[:N :N] [:N :N]])))
  ,)
