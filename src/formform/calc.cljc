(ns formform.calc
  "API for the `calc` module of `formform`."
  (:require [formform.calc.core :as core]
            #?(:clj  [formform.utils :as utils :refer [defapi]]
               :cljs [formform.utils :as utils :refer-macros [defapi]])
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [orchestra.spec.test :as stest]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data specs

(s/def ::const
  (s/with-gen
    #(case %
       (:N :U :I :M) true
       false)
    #(gen/elements [:N :U :I :M])))

(def rand-const
  "Generates a random constant."
  #(gen/generate (s/gen ::const)))

(s/def ::const?
  (s/with-gen
    (s/or :const     ::const
          :var-const ::var-const)
    #(gen/elements [:N :U :I :M core/var-const])))

(s/def ::var-const #(= % core/var-const))

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


(s/def ::dna-seq-elem-tree ;; specifically for make-dna
  (s/coll-of (s/or :leaf   ::dna-seq-elem
                   :branch ::dna-seq-elem-tree)
             :kind (complement map?)))

(s/def ::sort-code
  (s/with-gen
    (s/and
      vector?
      #(== 4 (count %))
      #(= #{:N :U :I :M} (set %)))
    #(gen/shuffle [:N :U :I :M])))

(def sort-code? (partial s/valid? ::sort-code))


(let [digits (set (map (comp first str) (range 4)))]
  (s/def ::quaternary-str (s/and string?
                                 #(= "4r" (subs % 0 2))
                                 #(every? digits (subs % 2)))))


(s/def ::dna-length
  (s/with-gen
    #(some? (core/dna-length->dim %))
    #(gen/elements (take 12 core/dna-lengths)))) 

(s/def ::dna-dimension nat-int?)

(def dna-dimension? (partial s/valid? ::dna-count))
(def dna? (partial s/valid? ::dna))


;; ? necessary
(s/def ::dna-count #(some? (core/dna-dimension %)))

(s/def ::dna-seq
  (s/and sequential? #(<= (count (set %)) 4)
         ::dna-count))

(s/def ::dna
  (s/and (s/coll-of core/consts 
                    :kind sequential? 
                    :min-count 1)
         (comp (partial s/valid? ::dna-length)
               count)))


(s/def ::vpoint
  (s/every ::const?
           :kind sequential?))

(def vpoint? (partial s/valid? ::vpoint))


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

(def vspace? (partial s/valid? ::vspace))


(s/def ::vdict
  (s/and map?
         #(s/valid? ::vspace (keys %))
         #(s/valid? ::dna    (vals %))))

(def vdict? (partial s/valid? ::vdict))


(s/def ::vmap
  (s/or :vmap (s/map-of ::const
                        ::vmap
                        :count 4)
        :res ::const))

(def vmap? (partial s/valid? ::vmap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs & API

(s/def :opts/sorted? boolean?)
(s/def :opts.safety/limit? boolean?)
(s/def :opts.safety/unsafe? boolean?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant
;; -> element of formDNA representing a primitive FORM value
;; -> single-digit formDNA

(defapi core/const?
  "Checks if the argument is a valid constant.")

(defapi core/nuim-code)
(defapi core/nmui-code)

(s/fdef core/digit->const
  :args (s/alt :ar1 (s/cat :int ::const-int?)
               :ar2 (s/cat :int ::const-int?
                           :sort-code ::sort-code))
  :ret  ::const?)
(defapi core/digit->const
  "Converts a digit to its corresponding constant representation.")

(s/fdef core/char->const
  :args (s/alt :ar1 (s/cat :char ::const-char?)
               :ar2 (s/cat :char (s/nonconforming ::const-char?)
                           :sort-code ::sort-code))
  :ret  ::const?)
(defapi core/char->const
  "Coerces a char to a corresponding constant.")

(s/fdef core/const->digit
  :args (s/alt :ar1 (s/cat :const ::const?)
               :ar2 (s/cat :const ::const?
                           :sort-code ::sort-code))
  :ret  ::const-int?)
(defapi core/const->digit
  "Converts a constant to a digit corresponding to an optional `sort-code` or the default `nuim-code`.")

(s/fdef core/consts->quaternary
  :args (s/cat :consts ::consts)
  :ret  ::quaternary-str)
(defapi core/consts->quaternary
  "Converts a sequence of constants to a corresponding quaternary number (as a string, prefixed by '4r').
  - use `read-string` to obtain the decimal value as a BigInt")


;; for some reason, spec/orchestra needs a custom generator for `map-entry?`

(s/def ::const-map-entry
  (s/with-gen
    (s/and map-entry?
           #(s/valid? ::const (first %)))
    #(gen/fmap first (gen/map (s/gen ::const) (gen/simple-type)))))

(s/def ::consts-map-entry
  (s/with-gen
    (s/and map-entry?
           #(s/valid? ::consts (first %)))
    #(gen/fmap first (gen/map (s/gen ::consts) (gen/simple-type)))))

(s/fdef core/make-compare-consts
  :args (s/cat :sort-code ::sort-code)
  :ret  (s/fspec :args (s/or :const-or-consts
                             (s/tuple (s/or :const  ::const
                                            :consts ::consts)
                                      (s/or :const  ::const
                                            :consts ::consts))
                             :map-entries
                             (s/tuple (s/or :const  ::const-map-entry
                                            :consts ::consts-map-entry)
                                      (s/or :const  ::const-map-entry
                                            :consts ::consts-map-entry)))
                 :ret  #(#{-1 0 1} %))
  ;; ! doesn’t work yet:
  ; :fn   (fn [{:keys [args ret]}]
  ;         (try (sort (ret (:sort-code args))
  ;                    [:N :U :I :M])
  ;              (catch Exception e false)))
  )
(defapi core/make-compare-consts
  "Given a `sort-code` (try `calc.nuim-code` or `calc.nmui-code`), returns a comparator function to sort single constants, formDNA or arbitrary sequences of constants (can be mixed).
  - can also compare map-entries by keys of comparable types")
(defapi core/compare-consts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formDNA
;; -> quaternary number of constants representing value structures in FORMs
;; - represented in `dna?` as a `keyword?` with `name` length of 4^dim
;;
;; dna-seq
;; -> generalized implementation format for formDNA
;; - a `dna?` is a `dna-seq?`, since underneath a Clojure keyword is just
;;   a string, which is always a sequence of some sort
;; - operations on formDNA often have dna-seq as input to avoid conversion

(s/fdef core/dna-dimension
  :args (s/cat :xs sequential?)
  :ret  (s/or :invalid nil?
              :dim ::dna-dimension))
(defapi core/dna-dimension
  "Calculates the dimension of a formDNA/`dna-seq` (corresponds to the number of variables in a FORM). The length of a `dna-seq` is 4^d for its dimension d.
  - the input sequence can have any type of elements")

(s/fdef core/rand-dna
  :args (s/alt :ar1 (s/cat :dim ::dna-dimension)
               :ar2 (s/cat :dim ::dna-dimension
                           :elems (s/or :seq (s/and sequential?
                                                    #(<= 1 (count %) 4))
                                        :nil nil?)))
  :ret  ::dna-seq)
(defapi core/rand-dna
  "Generates a random formDNA/`dna-seq` of dimension `dim`. A vector of 4 custom elements can be provided as a second argument.")

(defapi core/reverse-dna
  "Reverses a formDNA (returns an rseq)
  - make sure the input is a vector for constant-time reverse")

(s/def ::consts (s/every ::const
                         :kind sequential?
                         :min-count 1))


(s/fdef core/reorder-dna-seq
  :args (s/cat :dna-seq        ::dna-seq
               :sort-code-from ::sort-code
               :sort-code-to   ::sort-code)
  :ret  ::dna-seq)
(defapi core/reorder-dna-seq
  "Reorders given formDNA/`dna-seq` from `sort-code-from` to `sort-code-to`.
  
  Note:
  - `dna-seq` can have any type of elements (not only constants)
  - does NOT change the encoding of the elements, just their ordering")


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

(s/fdef core/digits->dna
  :args (spec--dna-seq-args (s/coll-of ::const-int 
                                       :min-count 1
                                       :kind sequential?))
  :ret  ::dna)
(defapi core/digits->dna
  "Converts a `seqable?` of digits (as string/char or integer) to formDNA.
  
  Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `digits` will be reordered to match the code.")

(s/fdef core/chars->dna
  :args (spec--dna-seq-args (s/coll-of ::const-char 
                                       :min-count 1
                                       :kind sequential?))
  :ret  ::dna)
(defapi core/chars->dna
  "Converts a `seqable?` of chars to formDNA.
  
  Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `digits` will be reordered to match the code.")

(s/fdef core/dna->digits
  :args (spec--dna-args)
  :ret  (s/coll-of ::const-int
                   :min-count 1
                   :kind sequential?))
(defapi core/dna->digits
  "Converts formDNA to a sequence of digits corresponding to a `sort-code`.
  
  Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `dna` will be reordered to match the code.")


(s/fdef core/expand-dna-seq
  :args (s/or :ar2 (s/cat :dna-seq ::dna-seq
                          :ext-dim ::dna-dimension)
              :ar3 (s/and (s/cat :dna-seq ::dna-seq
                                 :dim     ::dna-dimension
                                 :ext-dim ::dna-dimension)
                          #(<= (:dim %) (:ext-dim %))))
  :ret  ::dna-seq
  :fn   #(== (-> % :args second :ext-dim)
             (core/dna-dimension (-> % :ret))))
(defapi core/expand-dna-seq
  "Expands a `dna-seq` to a given target dimension by repeating elements.
  
  Note: `dna-seq` can have any type of elements (not only constants)")

(s/fdef core/reduce-dna-seq
  :args (s/or :ar1 (s/cat :dna-seq ::dna-seq)
              :ar2 (s/and (s/cat :terms   sequential?
                                 :dna-seq ::dna-seq)
                          #(== (count (:terms %))
                               (core/dna-dimension (:dna-seq %)))))
  :ret  (s/and (s/cat :terms   sequential?
                      :dna-seq ::dna-seq)
               #(== (core/dna-dimension (-> % :dna-seq))
                    (count (-> % :terms)))))
(defapi core/reduce-dna-seq
  "Reduces a `dna-seq` by eliminating redundant/contingent terms.
  - returns a tuple `[terms dna-seq]`, where `terms` is a sequence that represents the remaining terms after reduction
  - takes an optional `terms` sequence of any kind of items that will be used instead of the default arithmetic sequence `[0 1 2 …]` to represent each term (length has to match the formDNA dimension)
  
  Note: `dna-seq` can have any type of elements (not only constants)")

(s/fdef core/make-dna
  :args (s/and (s/nonconforming ::dna-seq-elem-tree)
               #(dna-dimension? (flatten %)))
  :ret  ::dna
  :fn   #(== (count (-> % :ret))
             (count (flatten (-> % :args)))))
(defapi core/make-dna
  "Creates a formDNA from arguments, which may be valid chars, keywords, integers or sequences thereof.
  - valid chars are: \\n \\u \\i \\m (upper- or lowercase) and \\0 \\1 \\2 \\3
  - valid integers are: 0 1 2 3
  - valid keywords are: :N :U :I :M
  - total argument count (including count of sequence args) must match a valid formDNA length, which is 4^d, where d is a natural number")

(s/fdef core/filter-dna-seq
  :args (s/and (s/cat :dna-seq          ::dna-seq
                      :depth-selections (s/coll-of ::const-int?
                                                   :kind sequential?))
               #(== (core/dna-dimension (-> % :dna-seq))
                    (count (-> % :depth-selections))))
  :ret  ::dna-seq)
(defapi core/filter-dna-seq)

(s/fdef core/filter-dna
  :args (s/and (s/cat :dna    ::dna
                      :vpoint ::vpoint)
               #(== (core/dna-dimension (-> % :dna))
                    (count (-> % :vpoint))))
  :ret  ::dna)
(defapi core/filter-dna
  "Filters a `dna` by selecting specific parts corresponding to a given `vpoint`, which acts as a coordinate vector in its value space.
  - use holes `:_` to indicate a variable selection")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formDNA perspectives

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

(s/fdef core/permute-dna-seq
  :args (s/and (s/alt :ar2 (s/cat :dna-seq    ::dna-seq
                                  :perm-order ::permutation-order)
                      :ar3 (s/cat :dna-seq    ::dna-seq
                                  :perm-order ::permutation-order
                                  :opts (s/keys :opt-un [:opts.safety/limit?])))
               #(let [{:keys [dna-seq perm-order]} (-> % second)]
                  (== (core/dna-dimension dna-seq)
                      (count perm-order))))
  :ret  ::dna-seq-perspective)
(defapi core/permute-dna-seq)
(defapi core/permute-dna)

(s/fdef core/dna-seq-perspectives
  :args (s/alt :ar1 (s/cat :dna-seq ::dna-seq)
               :ar2 (s/cat :dna-seq ::dna-seq
                           :opts (s/keys :opt-un [:opts.safety/limit?])))
  :ret  (s/coll-of ::dna-seq-perspective
                   :kind sequential?))
(defapi core/dna-seq-perspectives)

(s/fdef core/dna-perspectives
  :args (s/alt :ar1 (s/cat :dna ::dna)
               :ar2 (s/cat :dna ::dna
                           :opts (s/keys :opt-un [:opts.safety/limit?])))
  :ret  ::dna-perspective-group)
(defapi core/dna-perspectives)

(s/fdef core/equal-dna
  :args (s/every ::dna :min-count 1)
  :ret  boolean?)
(defapi core/equal-dna
  "Equality check for formDNA. Two formDNAs are considered equal, if they contain the same constants in the same order. Stricter than `equiv-dna`, where permutations are considered equal.")

(s/fdef core/equiv-dna
  :args (s/every ::dna :min-count 1)
  :ret  boolean?)
(defapi core/equiv-dna
  "Equivalence check for formDNA. Two formDNAs are considered equivalent, if they belong to the same equivalence-class of `dna-perspectives` (i.e. if they are permutations of each other).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value structures

;;-------------------------------------------------------------------------
;; `vpoint` -> value point -> vector of `const`-coordinates in a `vspace`

(s/fdef core/rand-vpoint
  :args (s/? nat-int?)
  :ret  ::vpoint)
(defapi core/rand-vpoint
  "Generates a random vpoint either as an infinite lazy seq or with given dimension `dim`.")

;;-------------------------------------------------------------------------
;; `vspace` -> value space -> vector of all `n`-dimensional `vpoint`s

(s/fdef core/vspace
  :args (s/alt :ar1 (s/cat :dim ::dna-dimension)
               :ar2 (s/cat :dim ::dna-dimension
                           :sort-code ::sort-code))
  :ret  ::vspace)
(defapi core/vspace
  "Generates a vspace of dimension `dim`, optionally with custom `sort-code`.
  - returns a lazy-seq which may be too memory-expensive to fully realize for dimensions greater than 11 (> 200 Mio. elements in total!)")

(s/def :vdict.opts/default-result ::const)

;;-------------------------------------------------------------------------
;; `vdict` -> value dictionary -> (sorted) k-v map from `vspace` to `dna`
;; - for value table generation
;; - like a flat vmap

(s/fdef core/vdict
  :args (s/cat :vp->r (s/map-of ::vpoint ::const)
               :opts  (s/keys :opt-un [:vdict.opts/default-result
                                       :opts/sorted?]))
  :ret  ::vdict)
(defapi core/vdict
  "Generates a vdict given a map from vpoint to result (constant).
  - if the corresponding vspace is not a subset of the set of keys from `vp->r`, the remaining results will be filled with :N or a given default constant
  - optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive")

(s/fdef core/dna->vdict
  :args (s/cat :dna  ::dna
               :opts (s/keys :opt-un [:opts/sorted?
                                      :opts.safety/unsafe?]))
  :ret  ::vdict)
(defapi core/dna->vdict
  "Generates a vdict from a given dna.
  - optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive")

(s/fdef core/vdict->vmap
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
(defapi core/vdict->vmap
  "Generates a vmap from a given vdict.")

;;-------------------------------------------------------------------------
;; `vmap` -> value map -> mapping from `vspace` topology to `dna`

(s/fdef core/dna->vmap
  :args (s/cat :dna ::dna)
  :ret  ::vmap)
(defapi core/dna->vmap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formDNA arithmetic

(s/fdef core/rel
  :args (s/* (s/or :const ::const
                   :dna   ::dna))
  :ret  (s/or :const ::const
              :dna   ::dna))
(defapi core/rel
  "Relates the values of 2 constants in a formDNA to each other.")
;; alias
(def -- rel)

(s/fdef core/inv
  :args (s/* (s/or :const ::const
                   :dna   ::dna))
  :ret  (s/or :const ::const
              :dna   ::dna))
(defapi core/inv
  "Inverts the value of a every constant in a formDNA.")
;; alias
(def | inv)

(def fns-with-specs (utils/list-fn-specs "formform.calc"))


(comment

  ; (s/form ::dna)

  ; (s/registry)


  

  )
