(ns formform.emul-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [clojure.edn :as edn]
            [formform.emul :refer :all]
            [orchestra.spec.test :as stest]))

;; (doseq [fsym fns-with-specs] (stest/instrument fsym))


;; Notes about the test data:

;; All CA snapshots are loaded as EDN data and compared to the generated evolutions for each test case.

;; SelFi snapshots are generated with initial conditions from various examples in the appendix of uFORM iFORM (trimmed down to small excerpts) and carefully compared visually to the original images (some only partially).

;; Snapshots for mindforms, lifeforms and decisionforms have been extracted (using my `ffemul-img2data` tool) from screenshots of videos from:
;; - https://uformiform.info/en/downloads (mindforms)
;; - https://www.youtube.com/channel/UCZjKMohiTT1Tjo5z2BmB18g/videos (lifeforms, decisionforms)

;; The Clojure library Quil (https://clojars.org/quil) is useful to visualize the snapshot data if you want to verify for yourself.


(def selfi-snaps (edn/read-string
                  (slurp "./test/formform/emul/snapshots/selfi_snapshots.edn")))

(def mindform-snaps
  {:100000mindFORM1-0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100000mindFORM1_0000s.edn"))
   :100000mindFORM1-0036s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100000mindFORM1_0036s.edn"))
   :100101mindFORM_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101mindFORM_0000s.edn"))
   :100101mindFORM_0015s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101mindFORM_0015s.edn"))})

(def lifeform-snaps
  {:100101lifeFORMs_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101lifeFORMs_0000s.edn"))
   :101101circulator_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101lifeFORMs_0000s.edn"))})

(def decisionform-snaps
  {:100101decisionFORMs_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101decisionFORMs_0000s.edn"))})

(defn snapshots->ca
  [species-type snapshots dna]
  (let [snapshot-evol (mapv :data snapshots)
        {:keys [data size]} (snapshots 0)
        ini (make-ini :fill-all data)
        species (condp = species-type
                  :lifeform (make-species species-type dna)
                  :decisionform (make-species species-type dna 10)
                  :selfi (make-species species-type dna ini)
                  :mindform (make-species species-type dna ini)
                  :else (throw (ex-info "Invalid species type."
                                        {:species species-type})))
        ca (apply specify-ca species
                  (if (#{:lifeform :decisionform} species-type)
                    ;; these species have no ini params, so we overwrite:
                    {:overwrites {:ini-spec ini}}
                    {})
                  size)]
    [snapshot-evol ca]))

(defn equiv-iterator-evolution?
  [evol ca]
  (let [iterator (ca-iterator ca)]
    (= (take (count evol) iterator) evol)))

(defn equiv-automaton-evolution?
  [evol ca]
  (let [automaton (create-ca ca (count evol))]
    (dotimes [_ (count (rest evol))] (step automaton))
    (= (get-cached-history automaton false)
       evol)))

;; alternative method:
#_
(defn equiv-automaton-evolution?
  [evol ca]
  (let [automaton (create-ca ca 0)
        init-gen (get-current-generation automaton false)]
    (= (cons init-gen (repeatedly (count (rest evol))
                                  (fn []
                                    (step automaton)
                                    (get-current-generation automaton false))))
       evol)))


(deftest mindform-congruence
  (testing "100000mindFORM1 snapshot congruence with iterator"
    (let [[evol ca] (snapshots->ca :mindform
                                   (mindform-snaps :100000mindFORM1-0000s)
                                   (tsds-sel->dna [1 0 0 0 0 0]))]
      (is (equiv-iterator-evolution? evol ca))
      (is (equiv-automaton-evolution? evol ca)))

    ;; tests should pass, but left out since they’re too slow:
    #_
    (let [[evol ca] (snapshots->ca :mindform
                                   (mindform-snaps :100000mindFORM1-0036s)
                                   (tsds-sel->dna [1 0 0 0 0 0]))]
      (is (equiv-iterator-evolution? evol ca))
      (is (equiv-automaton-evolution? evol ca))))

  (testing "100101mindFORM snapshot congruence with iterator"
    (let [[evol ca] (snapshots->ca :mindform
                                   (mindform-snaps :100101mindFORM_0000s)
                                   (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (equiv-iterator-evolution? evol ca))
      (is (equiv-automaton-evolution? evol ca)))

    ;; tests should pass, but left out since they’re too slow:
    #_
    (let [[evol ca] (snapshots->ca :mindform
                                   (mindform-snaps :100101mindFORM_0015s)
                                   (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (equiv-iterator-evolution? evol ca))
      (is (equiv-automaton-evolution? evol ca)))))

(deftest lifeform-congruence
  (testing "100101lifeFORMs_0000s snapshot congruence with iterator"
    (let [[evol ca] (snapshots->ca :lifeform
                                   (lifeform-snaps :100101lifeFORMs_0000s)
                                   (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (equiv-iterator-evolution? evol ca))
      (is (equiv-automaton-evolution? evol ca))))

  (testing "101101circulator_0000s snapshot congruence with iterator"
    (let [[evol ca] (snapshots->ca :lifeform
                                   (lifeform-snaps :101101circulator_0000s)
                                   ;; the TSDS is NOT 101101 but 100101!
                                   (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (equiv-iterator-evolution? evol ca))
      (is (equiv-automaton-evolution? evol ca)))))

(deftest decisionform-congruence
  (testing "100101decisionFORMs_0000s snapshot congruence with iterator"
    (let [[evol ca] (snapshots->ca :decisionform
                                   (decisionform-snaps
                                    :100101decisionFORMs_0000s)
                                   (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (equiv-iterator-evolution? evol ca))
      (is (equiv-automaton-evolution? evol ca)))))

(deftest selfi-congruence
  (testing "Mark1 snapshot congruence"
    (is (= (selfi-snaps :Mark1-r25t20) ;; roughly verified
           (take 20 (ca-iterator (specify-ca (common-specimen :Mark1)
                                             {} 25))))))
  (testing "Slit snapshot congruence"
    (is (= (selfi-snaps :Slit-r27t20) ;; roughly verified
           (take 20 (ca-iterator (specify-ca (common-specimen :Slit)
                                             {} 27))))))

  (testing "or4v snapshot congruence"
    (is (= (selfi-snaps :or4v-r21t19) ;; fully verified
           (take 19 (ca-iterator (specify-ca (common-specimen :or4v)
                                             {} 21))))))

  (testing "Rule4v110 snapshot congruence"
    (is (= (selfi-snaps :xorReId-r27t20) ;; fully verified
           (take 20 (ca-iterator (specify-ca (common-specimen :xorReId)
                                             {} 27))))))

  (testing "Rule4v30 snapshot congruence"
    (is (= (selfi-snaps :Rule4v30-r27t11) ;; fully verified
           (take 11 (ca-iterator (specify-ca (common-specimen :Rule4v30)
                                             {} 27))))))

  (testing "Rule4v111 snapshot congruence"
    (is (= (selfi-snaps :Rule4v111-r19t19) ;; fully verified
           (take 19 (ca-iterator (specify-ca (common-specimen :Rule4v111)
                                             {} 19))))))

  (testing "Structure111Re snapshot congruence"
    (is (= (selfi-snaps :Structure111Re-r27t20) ;; roughly verified
           (take 20 (ca-iterator (specify-ca (common-specimen :Structure111Re)
                                             {} 27))))))

  (testing "Rule4v110 snapshot congruence"
    (is (= (selfi-snaps :Rule4v110-r25t20) ;; fully verified
           (take 20 (ca-iterator (specify-ca (common-specimen :Rule4v110)
                                             {} 25)))))))



(comment

  (require '[clojure.pprint :as pprint :refer [pprint]])

  (binding [pprint/*print-right-margin* 80
            pprint/*print-miser-width* 80]
    (pprint (take 20 (ca-iterator (specify-ca (common-specimen :Mark1)
                                              {} 25)))))

  ,)

(comment

  (let [[evol ca] (snapshots->ca :lifeform
                                 (lifeform-snaps :101101circulator_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
    (equiv-iterator-evolution? evol ca))

  
  (let [[evol ca] (snapshots->ca :lifeform
                                 (lifeform-snaps :100101lifeFORMs_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
    ;; (equiv-iterator-evolution? evol ca)
    (equiv-automaton-evolution? evol ca))

  (def snapshot ((lifeform-snaps :100101lifeFORMs_0000s) 0))
  (def ini-gen (:data snapshot))
  (def res (:size snapshot))

  (= (->> (ca-iterator
           (apply specify-ca
                  (make-species :lifeform (tsds-sel->dna [1 0 0 1 0 1]))
                  {:overwrites {:ini-spec
                                (make-ini :fill-all ini-gen)}}
                  res))
          (take 10))
     (map :data (lifeform-snaps :100101lifeFORMs_0000s)))

  (require '[clojure.math.combinatorics :as combo])
  
  (let [perms (combo/selections [0 1] 6)]
    (some
     (fn [perm]
       (= (->> (ca-iterator
                (apply specify-ca
                       (make-species :lifeform (tsds-sel->dna perm))
                       {:overwrites {:ini-spec
                                     (make-ini :fill-all ini-gen)}}
                       res))
               (take 2)
               last)
          (:data ((lifeform-snaps :100101lifeFORMs_0000s) 1))))
     perms))

  
  (let [[evol ca] (snapshots->ca :mindform
                                 (mindform-snaps :100000mindFORM1-0000s)
                                 (tsds-sel->dna [1 0 0 0 0 0]))]
    (equiv-automaton-evolution? evol ca))

  (def atm (create-ca
            (specify-ca (make-species :selfi
                                      [:N :N :N :N
                                       :N :I :N :I
                                       :I :I :I :I
                                       :I :U :I :U]
                                      (make-ini :ball))
                        {}
                        10)))
  (get-system-time atm)
  (get-resolution atm)
  (step atm)
  (get-current-generation atm false)
  ,)
