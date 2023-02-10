(ns formform.specs.calc
  (:require [formform.calc :as calc]
            [clojure.math :as math]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs

(s/def :opts/sorted? boolean?)
(s/def :opts.safety/limit? boolean?)
(s/def :opts.safety/unsafe? boolean?)


(s/def ::const-int (s/int-in 0 4))
(s/def ::const-int? (s/or :int ::const-int
                          :var-int #(== % -1)))

(s/def ::const-char #{\N \U \I \M, \n \u \i \m, \0 \1 \2 \3})
(s/def ::const-char? (s/or :char ::const-char
                           :var-char #(= % \_)))

(s/def ::dna-seq-elem
  (s/or :const ::const
        :char  ::const-char
        :int   ::const-int))

;; specifically for make-dna

(s/def ::dna-seq-elem-tree 
  (s/coll-of (s/or :leaf   ::dna-seq-elem
                   :branch ::dna-seq-elem-tree)
             :kind (complement map?)))


(let [digits (set (map (comp first str) (range 10)))]
  (s/def ::quaternary-str (s/and string?
                                 #(= "4r" (subs % 0 2))
                                 #(every? digits (subs % 2)))))

(s/fdef formform.calc/digit->const
  :args (s/alt :ar1 (s/cat :int ::const-int?)
               :ar2 (s/cat :int ::const-int?
                           :sort-code ::sort-code))
  :ret  ::const?)

(s/fdef formform.calc/char->const
  :args (s/alt :ar1 (s/cat :char ::const-char?)
               :ar2 (s/cat :char (s/nonconforming ::const-char?)
                           :sort-code ::sort-code))
  :ret  ::const?)

(s/fdef formform.calc/const->digit
  :args (s/alt :ar1 (s/cat :const ::const?)
               :ar2 (s/cat :const ::const?
                           :sort-code ::sort-code))
  :ret  ::const-int?)


(s/fdef formform.calc/dna-dimension
  :args (s/cat :xs sequential?)
  :ret  (s/or :invalid nil?
              :dim ::dna-dimension))

(s/fdef formform.calc/rand-dna
  :args (s/alt :ar1 (s/cat :dim ::dna-dimension)
               :ar2 (s/cat :dim ::dna-dimension
                           :elems (s/or :seq (s/and sequential?
                                                    #(<= 1 (count %) 4))
                                        :nil nil?)))
  :ret  ::dna-seq)

(s/fdef formform.calc/dna->quaternary
  :args (s/cat :dna ::dna)
  :ret  ::quaternary-str)


;; for some reason, spec/orchestra needs a custom generator for `map-entry?`

(s/def ::const-map-entry
  (s/with-gen
    (s/and map-entry?
           #(s/valid? ::const (first %)))
    #(gen/fmap first (gen/map (s/gen ::const) (gen/simple-type)))))

(s/def ::dna-map-entry
  (s/with-gen
    (s/and map-entry?
           #(s/valid? ::dna (first %)))
    #(gen/fmap first (gen/map (s/gen ::dna) (gen/simple-type)))))

(s/fdef formform.calc/make-compare-dna
  :args (s/cat :sort-code ::sort-code)
  :ret  (s/fspec :args (s/or :const-or-dna
                             (s/tuple (s/or :const ::const
                                            :dna   ::dna)
                                      (s/or :const ::const
                                            :dna   ::dna))
                             :map-entries
                             (s/tuple (s/or :const ::const-map-entry
                                            :dna   ::dna-map-entry)
                                      (s/or :const ::const-map-entry
                                            :dna   ::dna-map-entry)))
                 :ret  #(#{-1 0 1} %))
  ;; ! doesn’t work yet:
  ; :fn   (fn [{:keys [args ret]}]
  ;         (try (sort (ret (:sort-code args))
  ;                    [:N :U :I :M])
  ;              (catch Exception e false)))
  )

(s/fdef formform.calc/reorder-dna-seq
  :args (s/cat :dna-seq        ::dna-seq
               :sort-code-from ::sort-code
               :sort-code-to   ::sort-code)
  :ret  ::dna-seq)


(defmacro spec--dna-seq-args [spec]
  `(s/alt :ar1 (s/cat :dna-seq ~(if (nil? spec)
                                  `::dna-seq
                                  `(s/and ::dna-seq ~spec)))
          :ar2 (s/cat :dna-seq ~(if (nil? spec)
                                  `::dna-seq
                                  `(s/and ::dna-seq ~spec))
                      :sort-code ::sort-code)))

(defmacro spec--dna-args []
  `(s/alt :ar1 (s/cat :dna       ::dna)
          :ar2 (s/cat :dna       ::dna
                      :sort-code ::sort-code)))

(s/fdef prod=dna-seq->dna
  :args (s/fspec :args (s/cat :sort-code ::sort-code
                              :x         any?)
                 :ret  ::const)
  :ret  (s/fspec :args (spec--dna-seq-args nil)
                 :ret  ::dna))

(s/fdef prod=dna->dna-seq
  :args (s/fspec :args (s/cat :sort-code ::sort-code
                              :const     ::const)
                 :ret  any?)
  :ret  (s/fspec :args (spec--dna-args)
                 :ret  ::dna-seq))

(s/fdef formform.calc/digits->dna
  :args (spec--dna-seq-args (s/coll-of ::const-int 
                                       :min-count 1
                                       :kind sequential?))
  :ret  ::dna)

(s/fdef formform.calc/chars->dna
  :args (spec--dna-seq-args (s/coll-of ::const-char 
                                       :min-count 1
                                       :kind sequential?))
  :ret  ::dna)

(s/fdef formform.calc/dna->digits
  :args (spec--dna-args)
  :ret  (s/coll-of ::const-int
                   :min-count 1
                   :kind sequential?))


(s/fdef formform.calc/expand-dna-seq
  :args (s/or :ar2 (s/cat :dna-seq ::dna-seq
                          :ext-dim ::dna-dimension)
              :ar3 (s/and (s/cat :dna-seq ::dna-seq
                                 :dim     ::dna-dimension
                                 :ext-dim ::dna-dimension)
                          #(<= (:dim %) (:ext-dim %))))
  :ret  ::dna-seq
  :fn   #(== (-> % :args second :ext-dim)
             (calc/dna-dimension (-> % :ret))))

(s/fdef formform.calc/reduce-dna-seq
  :args (s/or :ar1 (s/cat :dna-seq ::dna-seq)
              :ar2 (s/and (s/cat :terms   sequential?
                                 :dna-seq ::dna-seq)
                          #(== (count (:terms %))
                               (calc/dna-dimension (:dna-seq %)))))
  :ret  (s/and (s/cat :terms   sequential?
                      :dna-seq ::dna-seq)
               #(== (calc/dna-dimension (-> % :dna-seq))
                    (count (-> % :terms)))))

(s/fdef formform.calc/make-dna
  :args (s/and (s/nonconforming ::dna-seq-elem-tree)
               #(calc/dna-dimension? (flatten %)))
  :ret  ::dna
  :fn   #(== (count (-> % :ret))
             (count (flatten (-> % :args)))))

(s/fdef formform.calc/filter-dna-seq
  :args (s/and (s/cat :dna-seq          ::dna-seq
                      :depth-selections (s/coll-of ::const-int?
                                                   :kind sequential?))
               #(== (calc/dna-dimension (-> % :dna-seq))
                    (count (-> % :depth-selections))))
  :ret  ::dna-seq)

(s/fdef formform.calc/filter-dna
  :args (s/and (s/cat :dna    ::dna
                      :vpoint ::vpoint)
               #(== (calc/dna-dimension (-> % :dna))
                    (count (-> % :vpoint))))
  :ret  ::dna)


(s/def ::permutation-order
  (s/coll-of nat-int?
             :distinct true
             :kind sequential?))

(s/def ::dna-seq-perspective
  (s/cat :perm-order ::permutation-order
         :dna-seq    ::dna-seq))

(s/def ::dna-perspective
  (s/cat :perm-order ::permutation-order
         :dna        ::dna))

(s/def ::dna-perspective-group
  (s/map-of ::permutation-order ::dna))

(s/fdef formform.calc/permute-dna-seq
  :args (s/and (s/alt :ar2 (s/cat :dna-seq    ::dna-seq
                                  :perm-order ::permutation-order)
                      :ar3 (s/cat :dna-seq    ::dna-seq
                                  :perm-order ::permutation-order
                                  :opts (s/keys :opt-un [:opts.safety/limit?])))
               #(let [{:keys [dna-seq perm-order]} (-> % second)]
                  (== (calc/dna-dimension dna-seq)
                      (count perm-order))))
  :ret  ::dna-seq-perspective)

(s/fdef formform.calc/dna-seq-perspectives
  :args (s/alt :ar1 (s/cat :dna-seq ::dna-seq)
               :ar2 (s/cat :dna-seq ::dna-seq
                           :opts (s/keys :opt-un [:opts.safety/limit?])))
  :ret  (s/coll-of ::dna-seq-perspective
                   :kind sequential?))

(s/fdef formform.calc/dna-perspectives
  :args (s/alt :ar1 (s/cat :dna ::dna)
               :ar2 (s/cat :dna ::dna
                           :opts (s/keys :opt-un [:opts.safety/limit?])))
  :ret  ::dna-perspective-group)

(s/fdef formform.calc/equal-dna
  :args (s/every ::dna :min-count 1)
  :ret  boolean?)

(s/fdef formform.calc/equiv-dna
  :args (s/every ::dna :min-count 1)
  :ret  boolean?)


(s/fdef formform.calc/rand-vpoint
  :args (s/? nat-int?)
  :ret  ::vpoint)

(s/fdef formform.calc/vspace
  :args (s/alt :ar1 (s/cat :dim ::dna-dimension)
               :ar2 (s/cat :dim ::dna-dimension
                           :sort-code ::sort-code))
  :ret  ::vspace)

(s/def :vdict.opts/default-result ::const)

(s/fdef formform.calc/vdict
  :args (s/cat :vp->r (s/map-of ::vpoint ::const)
               :opts  (s/keys :opt-un [:vdict.opts/default-result
                                       :opts/sorted?]))
  :ret  ::vdict)

(s/fdef formform.calc/dna->vdict
  :args (s/cat :dna  ::dna
               :opts (s/keys :opt-un [:opts/sorted?
                                      :opts.safety/unsafe?]))
  :ret  ::vdict)

(s/fdef formform.calc/vdict->vmap
  :args (s/alt :ar1 (s/cat :vdict ::vdict)
               :ar2 (s/cat :fmap
                           (s/nilable
                            ;; lightweight spec to prevent recursion issues
                            (s/fspec :args (s/cat :vmap map?
                                                  :vspace sequential?
                                                  :depth nat-int?
                                                  :dim ::dna-dimension)
                                     :ret  map?))
                           :vdict ::vdict))
  :ret  ::vmap)

(s/fdef formform.calc/dna->vmap
  :args (s/cat :dna ::dna)
  :ret  ::vmap)


(s/fdef formform.calc/rel
  :args (s/* (s/or :const ::const
                   :dna   ::dna))
  :ret  (s/or :const ::const
              :dna   ::dna))

(s/fdef formform.calc/inv
  :args (s/* (s/or :const ::const
                   :dna   ::dna))
  :ret  (s/or :const ::const
              :dna   ::dna))


(comment
  (s/conform (let [const-or-dna    (s/or :const ::const
                                         :dna   ::dna)
                   const-map-entry (s/and map-entry?
                                          #(s/valid? const-or-dna
                                                     (first %)))]
               (s/or :const-or-dna (s/tuple const-or-dna
                                            const-or-dna)
                     :map-entries  (s/tuple const-map-entry
                                            const-map-entry)))
             [(first {[:M :U :I :N] 0 :M 1}) (second {[:M :U :I :N] 0 :M 1})])

  (gen/sample (s/gen ::const))
  (gen/sample (s/gen ::sort-code))
  (gen/sample (s/gen ::dna-dimension))
  (gen/sample (s/gen ::dna-length))
  (gen/sample (s/gen ::dna-seq))
  (gen/sample (s/gen ::dna))
  (gen/sample (s/gen ::vpoint))
  ; (gen/sample (s/gen ::vspace))
  ; (gen/sample (s/gen ::vdict))
  ; (gen/sample (s/gen ::vmap))

  )
