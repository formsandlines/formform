(ns formform.calc-notes
  (:require [nextjournal.clerk :as clerk] 
            [formform.calc :as calc
             :refer [nuim-code nmui-code
                     make-dna vspace vdict
                     N U I M]]))

;; # Calc module

;; ## Constants

[N U I M]

;; ### conversion

nuim-code

nmui-code

(calc/int->const 3)

(calc/int->const 3 nmui-code)


;; ## formDNA

(make-dna N U I M)

(let [dna1 (make-dna M I U N)
      dna2 (make-dna I M N U)
      dna3 (make-dna U N M I)
      dna4 (make-dna N U I M)]
  (make-dna dna4 dna3 dna2 dna1))

;; ### conversion

(calc/digits->dna [0 1 2 3])
(calc/digits->dna [0 1 2 3] nmui-code)

(let [dna (calc/rand-dna 3)]
  {:dna dna
   :dna-seq (calc/dna->consts dna)
   :digits-nuim (calc/dna->digits dna)
   :digits-nmui (calc/dna->digits dna nmui-code)
   :quaternary-number (calc/dna->quaternary dna)})


;; ## vspace

(vspace 4)

(calc/vpoint? [:M :N :U])

