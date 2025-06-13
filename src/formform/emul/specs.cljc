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
         :value  ::calc-sp/const))

(s/def ::resolution (s/or :1d (s/cat :w pos-int?)
                          :2d (s/cat :w pos-int? :h pos-int?)))
(s/def ::res ::resolution)

(s/def ::generation-1d
  (s/coll-of ::calc-sp/const :kind vector?))

(s/def ::generation-2d
  (s/and (s/coll-of (s/coll-of ::calc-sp/const :kind vector?)
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
  (s/coll-of ::calc-sp/const :kind vector?))



(s/def ::umwelt-spec
  #(satisfies? i/Umwelt %))

(s/def ::ini-spec
  #(satisfies? i/Ini %))

(s/def ::ini-transducer
  (s/and ::ini-spec
         #(satisfies? i/IniTransducer %)))

(s/def ::rule-spec
  #(satisfies? i/Rule %))

(s/def ::val (s/or :random #{:?}
                   :const  ::calc-sp/const))

(s/def ::bg (s/or :value  ::val
                  :bg-ini ::ini-transducer))

(s/def ::hole #{:_})

(s/def ::figure-1d
  (s/coll-of (s/or :val  ::val
                   :hole ::hole) :kind vector?))

(s/def ::figure-2d
  (s/and (s/coll-of (s/coll-of (s/or :val  ::val
                                     :hole ::hole) :kind vector?)
                    :kind vector?)
         #(apply = (mapv count %))))

(s/def ::figure (s/or :1d ::figure-1d
                      :2d ::figure-2d))

(s/def ::seed pos-int?) ;; random seed for reproducability
(s/def ::ini-opts (s/keys :opt-un [::seed]))

(s/def :ini-pattern/f fn?)
(s/def :ini-pattern/w pos-int?)
(s/def :ini-pattern/h pos-int?)

(s/def ::ini-pattern
  (s/or :explicit ::figure
        :implicit (s/keys :req-un [:ini-pattern/f
                                   (or :ini-pattern/w
                                       :ini-pattern/h)])))

(s/def :anchor/pos (s/or :index nat-int?
                         :relative keyword?))
(s/def :anchor/align keyword?)
(s/def :anchor/offset int?)

(s/def ::anchor
  (s/keys :opt-un [:anchor/pos :anchor/align :anchor/offset]))

(s/def :rand/N float?)
(s/def :rand/U float?)
(s/def :rand/I float?)
(s/def :rand/M float?)

(s/def ::random-distribution
  (s/or :against-n float?
        :explicit (s/keys :req-un [:rand/N :rand/U :rand/I :rand/M])))

;; Note: the `x/y` specs below don’t apply directly to the records, but to the arguments as provided in `emul/make-x` constructors, where they are validated against the specs.

(s/def :ini/constant (s/cat :-opts ::ini-opts :const ::calc-sp/const))
(s/def :ini/random (s/cat :-opts ::ini-opts :distr ::random-distribution))
(s/def :ini/cycle (s/cat :-opts ::ini-opts :pattern vector?))

(s/def :ini/figure (s/cat :-opts ::ini-opts
                          :bg ::bg
                          :pattern ::ini-pattern
                          :anchor ::anchor))

(s/def :ini/rand-figure (s/cat :-opts ::ini-opts
                               :bg ::bg
                               :size pos-int?
                               :anchor ::anchor))

(s/def :ini/ball (s/cat :-opts ::ini-opts
                        :bg ::bg
                        :style (s/nilable keyword?)
                        :anchor ::anchor))

(s/def :ini/comp-figure (s/cat :-opts ::ini-opts
                               :bg ::bg
                               :figure-inis (s/coll-of ::ini-transducer)))

(s/def :ini/figure-repeat (s/cat :-opts ::ini-opts
                                 :bg ::bg
                                 :pattern ::ini-pattern
                                 :anchor ::anchor
                                 :copies (s/or :num pos-int?
                                               :per-dim (s/coll-of pos-int?))
                                 :spacing (s/or :num nat-int?
                                                :per-dim (s/coll-of nat-int?))))


(s/def ::umwelt-opts map?)
(s/def ::umwelt-order #{:column-first :row-first})

(s/def :umwelt/select-ltr (s/cat :-opts ::umwelt-opts :size pos-int?))
(s/def :umwelt/self-select-ltr (s/cat :-opts ::umwelt-opts :size pos-int?))
(s/def :umwelt/moore (s/cat :-opts ::umwelt-opts
                            :order ::umwelt-order
                            :self? boolean?))
(s/def :umwelt/von-neumann (s/cat :-opts ::umwelt-opts
                                  :order ::umwelt-order
                                  :self? boolean?))


(s/def ::rule-opts map?)

(s/def :rule/match (s/cat :-opts ::rule-opts :dna ::calc-sp/dna))
(s/def :rule/life (s/cat :-opts ::rule-opts :dna ::calc-sp/dna))


#_
(comment
  (s/def :species/overwrites map?)
  (s/def ::species-opts (s/keys :opt-un [:species/overwrites]))

  (s/def :species/selfi (s/cat :-opts ::species-opts
                               :dna ::calc-sp/dna
                               :ini ::ini-spec))

  (s/def :species/mindform (s/cat :-opts ::species-opts
                                  :dna ::calc-sp/dna
                                  :ini ::ini-spec))

  (s/def :species/lifeform (s/cat :-opts ::species-opts
                                  :dna ::calc-sp/dna))

  (s/def :species/decisionform (s/cat :-opts ::species-opts
                                      :dna ::calc-sp/dna
                                      :init-size pos-int?)))

(comment

  (s/valid? :rule/match [{} [:N]])

  ,)

(s/def ::ca-spec
  (s/keys :req-un [::resolution ::rule-spec ::umwelt-spec ::ini-spec]))

;; (s/def ::ca-constructor
;;   #(satisfies? i/Specifier %))


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
  (require '[formform.emul.core :as core])

  (s/conform ::bg (core/->Ini-Constant {} :N))
  (s/conform ::figure [:N :_ :?])
  (s/conform ::ini-pattern {:f (fn [] nil)
                            :w 12})
  (s/conform ::anchor {:pos :center
                       :align :left
                       :offset 0})

  (s/explain :ini/figure
             (core/->Ini-Figure {}
                                (core/->Ini-Constant {} :N)
                                [:M :_ :I] {:pos :center :align :center}))
  (s/conform :ini/constant
             (into {} (core/->Ini-Constant {} :N)))

  ,)

(comment
  (require '[formform.calc :as calc])
  (require '[formform.emul :as emul])

  (emul/params :ini :ball)
  (emul/make-ini :ball (emul/make-ini :constant :N) nil {})

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

  (def mindform (emul/make-mindform
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
