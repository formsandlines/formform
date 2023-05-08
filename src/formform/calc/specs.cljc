(ns formform.calc.specs
  (:require
    [formform.calc.core :as core]
    [formform.utils :as utils]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen])
  #?(:cljs (:require-macros
            [formform.calc.specs :refer [spec--dna-args spec--dna-seq-args]])))

;;-------------------------------------------------------------------------
;; constant

(s/def ::const
  (s/with-gen
    #(case % (:N :U :I :M) true false)
    #(gen/elements [:N :U :I :M])))

(s/def ::consts (s/every ::const
                         :kind sequential?
                         :min-count 1))

(s/def ::var-const #(= % core/var-const))

(s/def ::const?
  (s/with-gen
    (s/or :const     ::const
          :var-const ::var-const)
    #(gen/elements [:N :U :I :M core/var-const])))

(s/def ::const-int (s/int-in 0 4))
(s/def ::const-int? (s/or :int ::const-int
                          :var-int #(and (int? %) (== % -1))))

(s/def ::const-char #{\N \U \I \M, \n \u \i \m, \0 \1 \2 \3})
(s/def ::const-char? (s/or :char ::const-char
                           :var-char #(= % \_)))

(s/def ::sort-code
  (s/with-gen
    (s/and vector?
           #(== 4 (count %))
           #(= #{:N :U :I :M} (set %)))
    #(gen/shuffle [:N :U :I :M])))

;;-------------------------------------------------------------------------
;; formDNA

(s/def ::dna-length
  (s/with-gen
    #(some? (core/dna-length->dim %))
    #(gen/elements (take 12 core/dna-lengths)))) 

(s/def ::dna-dimension nat-int?)

;; ? necessary
(s/def ::dna-count #(some? (core/dna-dimension %)))

(s/def ::dna
  (s/and (s/coll-of core/consts
                    :kind sequential?
                    :min-count 1)
         (comp (partial s/valid? ::dna-length)
               count)))

(s/def ::dna-seq
  (s/and sequential? #(<= (count (set %)) 4)
         ::dna-count))

(s/def ::dna-seq-elem
  (s/or :const ::const
        :char  ::const-char
        :int   ::const-int))

(s/def ::dna-seq-elem-tree ;; required by `core/make-dna`
  (s/coll-of (s/or :leaf   ::dna-seq-elem
                   :branch ::dna-seq-elem-tree)
             :kind (complement map?)))


;;-------------------------------------------------------------------------
;; formDNA perspective

(s/def ::permutation-order
  (s/coll-of nat-int?
             :distinct true
             :kind sequential?))

(s/def ::dna-perspective
  (s/cat :perm-order ::permutation-order
         :dna        ::dna))

(s/def ::dna-perspective-group
  (s/map-of ::permutation-order ::dna))


;;-------------------------------------------------------------------------
;; vpoint

(s/def ::vpoint
  (s/every ::const?
           :kind sequential?))

;;-------------------------------------------------------------------------
;; vspace

(s/def ::vspace
  (s/and (s/coll-of ::vpoint
                    :min-count 1
                    :distinct true)
         (fn [vspc] (let [vs-dim (core/dna-dimension (seq vspc))
                          vp-dim (count (first vspc))]
                      (and (some? vs-dim)
                           (== vs-dim vp-dim)
                           (every? #(== vp-dim (count %)) vspc))))))
;; ? add spec ordered-vspace

;;-------------------------------------------------------------------------
;; vdict

(s/def ::vdict
  (s/and map?
         #(s/valid? ::vspace (keys %))
         #(s/valid? ::dna    (vals %))))

;;-------------------------------------------------------------------------
;; vmap

(s/def ::vmap
  (s/or :vmap (s/map-of ::const
                        ::vmap
                        :count 4)
        :res ::const))


;;-------------------------------------------------------------------------
;; function specs (impl)


(let [digits (set (map (comp first str) (range 4)))]
  (s/def ::quaternary-str (s/and string?
                                 #(= "4r" (subs % 0 2))
                                 #(every? digits (subs % 2)))))

(s/fdef core/consts->quaternary
  :args (s/cat :consts ::consts)
  :ret  ::quaternary-str)


(defmacro spec--dna-seq-args [spec]
  `(s/alt :ar1 (s/cat :dna-seq ~(if (nil? spec)
                                  `::dna-seq
                                  `(s/and ::dna-seq ~spec)))
          :ar2 (s/cat :sort-code ::sort-code
                      :dna-seq ~(if (nil? spec)
                                  `::dna-seq
                                  `(s/and ::dna-seq ~spec)))))

(defmacro spec--dna-args []
  `(s/alt :ar1 (s/cat :dna       ::dna)
          :ar2 (s/cat :sort-code ::sort-code
                      :dna       ::dna)))

(s/fdef core/prod=dna-seq->dna
  :args (s/fspec :args (s/cat :sort-code ::sort-code
                              :x         any?)
                 :ret  ::const)
  :ret  (s/fspec :args (spec--dna-seq-args nil)
                 :ret  ::dna))

(s/fdef core/prod=dna->dna-seq
  :args (s/fspec :args (s/cat :sort-code ::sort-code
                              :const     ::const)
                 :ret  any?)
  :ret  (s/fspec :args (spec--dna-args)
                 :ret  ::dna-seq))


(def ^:no-doc fns-with-specs (utils/list-fn-specs "formform.calc"))

