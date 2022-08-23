(ns formform.specs.calc
  (:require [formform.calc :as fc]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs

(s/def ::const-int (s/int-in 0 4))
(s/def ::dna-seq-dim nat-int?)

(s/fdef formform.calc/int->const
  :args (s/alt
          :ar1 (s/cat
                 :n ::const-int)
          :ar2 (s/cat
                 :n ::const-int
                 :sort-code ::sort-code))
  :ret  ::const)

(s/fdef formform.calc/const->int
  :args (s/alt
          :ar1 (s/cat
                 :c ::const)
          :ar2 (s/cat
                 :c ::const
                 :sort-code ::sort-code))
  :ret  ::const-int)

(s/fdef formform.calc/dna-seq-dim
  :args ::dna-seq
  :ret  ::dna-seq-dim)

(stest/instrument 'formform.calc/int->const)
(stest/instrument 'formform.calc/const->int)


(comment

  (gen/sample (s/gen :formform.specs.calc/const))
  (gen/sample (s/gen :formform.specs.calc/sort-code))
  (gen/sample (s/gen :formform.specs.calc/dna-seq-dim))
  (gen/sample (s/gen :formform.specs.calc/dna-seq))

  (let [input [true :wU!? 'J 'J]
        data (s/conform :formform.specs.calc/dna-seq input)]
    (if (s/invalid? data)
      (s/explain :formform.specs.calc/dna-seq input)
      data))

  (let [input "yes"]
    (if (s/valid? :formform.specs.calc/dna-seq input)
      input
      (s/explain-data :formform.specs.calc/dna-seq input)))

  (s/exercise ::const)
  (s/exercise ::sort-code)

  (stest/check 'formform.calc/int->const)
  (stest/check 'formform.calc/const->int)

  (s/valid? ::const-int 0)
  (s/exercise ::const-int)

  (fc/int->const 2)
  (fc/int->const 2 fc/nmui-code)
  (fc/int->const 3 [:M])

  (fc/const->int :O)

  )
