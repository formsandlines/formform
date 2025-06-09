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
        ini (make-ini :figure :N data {})
        ca-spec
        (condp = species-type
          :lifeform (make-lifeform {:overwrites {:ini-spec ini}}
                                   dna 0.5)
          :decisionform (make-decisionform {:overwrites {:ini-spec ini}}
                                           dna 0.5 10)
          :selfi (make-selfi dna ini)
          :mindform (make-mindform dna ini)
          :else (throw (ex-info "Invalid species type."
                                {:species species-type})))]
    [snapshot-evol ca-spec size]))

(defn equiv-iterator-evolution?
  [evol ca res]
  (let [iterator (ca-iterator ca res)]
    (= (take (count evol) iterator) evol)))

(defn equiv-automaton-evolution?
  [evol ca res]
  (let [automaton (create-ca ca (count evol) res)]
    (dotimes [_ (count (rest evol))] (step automaton))
    (= (get-cached-history automaton false)
       evol)))

;; alternative method:
#_
(defn equiv-automaton-evolution?
  ca-data
  (let [automaton (create-ca ca 0)
        init-gen (get-current-generation automaton false)]
    (= (cons init-gen (repeatedly (count (rest evol))
                                  (fn []
                                    (step automaton)
                                    (get-current-generation automaton false))))
       evol)))


(deftest mindform-congruence
  (testing "100000mindFORM1 snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100000mindFORM1-0000s)
                                 (tsds-sel->dna [1 0 0 0 0 0]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))

    ;; tests should pass, but left out since they’re too slow:
    #_
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100000mindFORM1-0036s)
                                 (tsds-sel->dna [1 0 0 0 0 0]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data))))

  (testing "100101mindFORM snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100101mindFORM_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))

    ;; tests should pass, but left out since they’re too slow:
    #_
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100101mindFORM_0015s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))))

(deftest lifeform-congruence
  (testing "100101lifeFORMs_0000s snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :lifeform
                                 (lifeform-snaps :100101lifeFORMs_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data))))

  (testing "101101circulator_0000s snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :lifeform
                                 (lifeform-snaps :101101circulator_0000s)
                                 ;; the TSDS is NOT 101101 but 100101!
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))))

(deftest decisionform-congruence
  (testing "100101decisionFORMs_0000s snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :decisionform
                                 (decisionform-snaps
                                  :100101decisionFORMs_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))))

(deftest selfi-congruence
  (testing "Mark1 snapshot congruence"
    (is (= (selfi-snaps :Mark1-r25t20) ;; roughly verified
           (take 20 (ca-iterator (common-specimen :Mark1)
                                 [25])))))
  (testing "Slit snapshot congruence"
    (is (= (selfi-snaps :Slit-r27t20) ;; roughly verified
           (take 20 (ca-iterator (common-specimen :Slit)
                                 [27])))))

  (testing "or4v snapshot congruence"
    (is (= (selfi-snaps :or4v-r21t19) ;; fully verified
           (take 19 (ca-iterator (common-specimen :or4v)
                                 [21])))))

  (testing "Rule4v110 snapshot congruence"
    (is (= (selfi-snaps :xorReId-r27t20) ;; fully verified
           (take 20 (ca-iterator (common-specimen :xorReId)
                                 [27])))))

  (testing "Rule4v30 snapshot congruence"
    (is (= (selfi-snaps :Rule4v30-r27t11) ;; fully verified
           (take 11 (ca-iterator (common-specimen :Rule4v30)
                                 [27])))))

  (testing "Rule4v111 snapshot congruence"
    (is (= (selfi-snaps :Rule4v111-r19t19) ;; fully verified
           (take 19 (ca-iterator (common-specimen :Rule4v111)
                                 [19])))))

  (testing "Structure111Re snapshot congruence"
    (is (= (selfi-snaps :Structure111Re-r27t20) ;; roughly verified
           (take 20 (ca-iterator (common-specimen :Structure111Re)
                                 [27])))))

  (testing "Rule4v110 snapshot congruence"
    (is (= (selfi-snaps :Rule4v110-r25t20) ;; fully verified
           (take 20 (ca-iterator (common-specimen :Rule4v110)
                                 [25]))))))



(comment

  (require '[clojure.pprint :as pprint :refer [pprint]])

  (binding [pprint/*print-right-margin* 80
            pprint/*print-miser-width* 80]
    (pprint (take 20 (ca-iterator (common-specimen :Mark1)
                                  [25]))))

  ,)

(comment

  (let [ca-data (snapshots->ca :lifeform
                               (lifeform-snaps :101101circulator_0000s)
                               (tsds-sel->dna [1 0 0 1 0 1]))]
    (apply equiv-iterator-evolution? ca-data))

  
  (let [ca-data (snapshots->ca :lifeform
                               (lifeform-snaps :100101lifeFORMs_0000s)
                               (tsds-sel->dna [1 0 0 1 0 1]))]
    ;; (apply equiv-iterator-evolution? ca-data)
    (apply equiv-automaton-evolution? ca-data))

  (def snapshot ((lifeform-snaps :100101lifeFORMs_0000s) 0))
  (def ini-gen (:data snapshot))
  (def res (:size snapshot))

  (= (->> (ca-iterator
           (make-lifeform {:overwrites {:ini-spec
                                        (make-ini :fill-all ini-gen)}}
                          (tsds-sel->dna [1 0 0 1 0 1]))
                  
           [res])
          (take 10))
     (map :data (lifeform-snaps :100101lifeFORMs_0000s)))

  (require '[clojure.math.combinatorics :as combo])
  
  (let [perms (combo/selections [0 1] 6)]
    (some
     (fn [perm]
       (= (->> (ca-iterator
                (make-lifeform {:overwrites {:ini-spec
                                             (make-ini :fill-all ini-gen)}}
                               (tsds-sel->dna perm))
                [res])
               (take 2)
               last)
          (:data ((lifeform-snaps :100101lifeFORMs_0000s) 1))))
     perms))

  
  (let [ca-data (snapshots->ca :mindform
                               (mindform-snaps :100000mindFORM1-0000s)
                               (tsds-sel->dna [1 0 0 0 0 0]))]
    (apply equiv-automaton-evolution? ca-data))

  (def atm (create-ca (make-species :selfi
                                    [:N :N :N :N
                                     :N :I :N :I
                                     :I :I :I :I
                                     :I :U :I :U]
                                    (make-ini :ball))
                      [10]))
  (get-system-time atm)
  (get-resolution atm)
  (step atm)
  (get-current-generation atm false)
  ,)
