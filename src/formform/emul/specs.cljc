(ns formform.emul.specs
  (:require [formform.calc.specs :as calc-sp]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            ;; [clojure.spec.gen.alpha :as gen]
            ))

(s/def ::cell
  (s/cat :coords (s/coll-of int? :kind vector?)
         :value  ::calc-sp/const?))

(s/def ::resolution (s/or :1d (s/cat :w int?)
                          :2d (s/cat :w int? :h int?)))

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
  (s/cat :pattern keyword?
         :args    (s/* any?)))

(s/def ::ini-spec
  (s/cat :pattern keyword?
         :args    (s/* any?)))

(s/def ::rule-spec
  (s/cat :rule keyword?
         :args (s/* any?)))

(binding [clojure.spec.alpha/*coll-check-limit* 3]
  ;; only realize 3 generation samples
  (s/def ::automaton-1d
    (s/every ::generation-1d :kind utils/iterate?))

  (s/def ::automaton-2d
    (s/every ::generation-2d :kind utils/iterate?)))


(comment
  (require '[formform.calc :as calc])

  (s/conform ::rule-spec [:foo])
  (s/conform ::rule-spec [:foo 1 {:a 2} 'b])

  (s/conform ::generation-1d [:N :M :I :M :M :U])
  (s/conform ::generation-2d [[:N :M] [:I :M] [:M :U]])

  (s/valid? ::automaton-2d (iterate (fn [xs] (conj xs [:U :I])) [[:N :M]]))
  (s/valid? ::automaton-1d (iterate (fn [xs] (conj xs :U)) [:N :M]))

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
