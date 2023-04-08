(ns formform.calc
  "API for the `calc` module of `formform`."
  (:require [formform.calc.core :as core]
            ; #?(:clj  [formform.utils :as utils :refer [defapi]]
            ;    :cljs [formform.utils :as utils :refer-macros [defapi]])
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))


;; ? common specs
(s/def :opts/sorted? boolean?)
(s/def :opts.safety/limit? boolean?)
(s/def :opts.safety/unsafe? boolean?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structures

;;-------------------------------------------------------------------------
;; `constant`
;; -> representation of a primitive value in FORM logic
;;
;; `sort-code` specifies a numeric ordering for constants, which is
;; useful for conversion to integers and `formDNA` interpretation order.
;; 
;; It is always assumed to be in `nuim-code` by default:
;; ```
;;   0 = :N -> unmarked
;;   1 = :U -> undetermined
;;   2 = :I -> imaginary
;;   3 = :M -> marked
;; ```
;; Make sure you convert to/from `nuim-code` when using different codes!

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
                          :var-int #(== % -1)))

(s/def ::const-char #{\N \U \I \M, \n \u \i \m, \0 \1 \2 \3})
(s/def ::const-char? (s/or :char ::const-char
                           :var-char #(= % \_)))

(def consts
  "Set of all 4 constants"
  core/consts)

(def var-const
  "Variable constant (like a hole/placeholder)"
  core/var-const)

(def const?
  "Checks if the argument is a valid constant."
  (partial s/valid? ::const))

(def rand-const
  "Generates a random constant."
  #(gen/generate (s/gen ::const)))


;; Compare constants

(s/def ::sort-code
  (s/with-gen
    (s/and vector?
           #(== 4 (count %))
           #(= #{:N :U :I :M} (set %)))
    #(gen/shuffle [:N :U :I :M])))

(def nuim-code core/nuim-code)
(def nmui-code core/nmui-code)

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

(s/fdef make-compare-consts
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
                 :ret  #(#{-1 0 1} %)))
(defn make-compare-consts
  "Given a `sort-code` (try `calc.nuim-code` or `calc.nmui-code`), returns a comparator function to sort single constants, formDNA or arbitrary sequences of constants (can be mixed).

  * can also compare map-entries by keys of comparable types"
  [sort-code]
  (core/make-compare-consts sort-code))

(def compare-consts
  "Comparator for constants using the default `nuim-code`."
  core/compare-consts)


;; Convert constants

(s/fdef digit->const
  :args (s/alt :ar1 (s/cat :int ::const-int?)
               :ar2 (s/cat :sort-code ::sort-code
                           :int ::const-int?))
  :ret  ::const?)
(defn digit->const
  "Converts a digit to its corresponding constant representation."
  ([n] (digit->const nuim-code n))
  ([sort-code n]
   (core/digit->const sort-code n)))

(s/fdef char->const
  :args (s/alt :ar1 (s/cat :char ::const-char?)
               :ar2 (s/cat :sort-code ::sort-code
                           :char (s/nonconforming ::const-char?)))
  :ret  ::const?)
(defn char->const
  "Coerces a `char` to a corresponding `constant`."
  ([c] (char->const nuim-code c))
  ([sort-code c]
   (core/char->const sort-code c)))

(s/fdef const->digit
  :args (s/alt :ar1 (s/cat :const ::const)
               :ar2 (s/cat :sort-code ::sort-code
                           :const ::const))
  :ret  ::const-int?)
(defn const->digit
  "Converts a `constant` to a `digit` corresponding to an optional `sort-code` or the default `nuim-code`."
  ([c] (const->digit nuim-code c))
  ([sort-code c]
   (core/const->digit sort-code c)))


;;-------------------------------------------------------------------------
;; formDNA
;; -> quaternary code/number made of constants
;; -> representation of a complex value structure in FORM logic

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

(def dna-dimension? (partial s/valid? ::dna-count))
(def dna? (partial s/valid? ::dna))

(s/fdef dna-dimension
  :args (s/cat :xs sequential?)
  :ret  (s/or :invalid nil?
              :dim ::dna-dimension))
(defn dna-dimension
  "Calculates the dimension of a `formDNA`/`dna-seq` (corresponds to the number of variables in a FORM). The length of a `dna-seq` is 4^d for its dimension d.

  * the input sequence can have any type of elements"
  [xs]
  (core/dna-dimension xs))

(s/def ::dna-seq-elem
  (s/or :const ::const
        :char  ::const-char
        :int   ::const-int))

(s/def ::dna-seq-elem-tree ;; specifically for make-dna
  (s/coll-of (s/or :leaf   ::dna-seq-elem
                   :branch ::dna-seq-elem-tree)
             :kind (complement map?)))

(s/fdef make-dna
  :args (s/and (s/nonconforming ::dna-seq-elem-tree)
               #(dna-dimension? (flatten %)))
  :ret  ::dna
  :fn   #(== (count (-> % :ret))
             (count (flatten (-> % :args)))))
(defn make-dna
  "Creates a formDNA from arguments, which may be valid chars, keywords, integers or sequences thereof.

  * valid chars are: \\n \\u \\i \\m (upper- or lowercase) and \\0 \\1 \\2 \\3
  * valid integers are: 0 1 2 3
  * valid keywords are: :N :U :I :M
  * total argument count (including count of sequence args) must match a valid formDNA length, which is 4^d, where d is a natural number"
  [& xs]
  (apply core/make-dna xs))

(s/fdef rand-dna
  :args (s/alt :ar1 (s/cat :dim ::dna-dimension)
               :ar2 (s/cat :dim ::dna-dimension
                           :elems (s/or :seq (s/and sequential?
                                                    #(<= 1 (count %) 4))
                                        :nil nil?)))
  :ret  ::dna-seq)
(defn rand-dna
  "Generates a random formDNA/`dna-seq` of dimension `dim`. A vector of 4 custom elements can be provided as a second argument."
  ([dim] (rand-dna dim nil))
  ([dim elems]
   (let [len    (apply * (repeat dim 4))
         gen-fn #(rand-nth (if (and (some? elems) (<= (count elems) 4))
                             elems
                             nuim-code))]
     (vec (repeatedly len gen-fn)))))


;; Sort formDNA

(s/fdef reorder-dna-seq
  :args (s/cat :dna-seq        ::dna-seq
               :sort-code-from ::sort-code
               :sort-code-to   ::sort-code)
  :ret  ::dna-seq)
(defn reorder-dna-seq
  "Reorders given formDNA/`dna-seq` from `sort-code-from` to `sort-code-to`.

  Note:

  * `dna-seq` can have any type of elements (not only constants)
  * does NOT change the encoding of the elements, just their ordering"
  [dna-seq sort-code-from sort-code-to]
  (core/reorder-dna-seq dna-seq sort-code-from sort-code-to))


;; Compare formDNA

(s/fdef equal-dna
  :args (s/every ::dna :min-count 1)
  :ret  boolean?)
(defn equal-dna
  "Equality check for formDNA. Two formDNAs are considered equal, if they contain the same constants in the same order. Stricter than `equiv-dna`, where permutations are considered equal."
  [& dnas]
  (apply core/equal-dna dnas))

(s/fdef equiv-dna
  :args (s/every ::dna :min-count 1)
  :ret  boolean?)
(defn equiv-dna
  "Equivalence check for formDNA. Two formDNAs are considered equivalent, if they belong to the same equivalence-class of `dna-perspectives` (i.e. if they are permutations of each other)."
  [& dnas]
  (apply core/equiv-dna dnas))


;; Transform formDNA

(s/fdef expand-dna-seq
  :args (s/or :ar2 (s/cat :dna-seq ::dna-seq
                          :ext-dim ::dna-dimension)
              :ar3 (s/and (s/cat :dna-seq ::dna-seq
                                 :dim     ::dna-dimension
                                 :ext-dim ::dna-dimension)
                          #(<= (:dim %) (:ext-dim %))))
  :ret  ::dna-seq
  :fn   #(== (-> % :args second :ext-dim)
             (core/dna-dimension (-> % :ret))))
(defn expand-dna-seq
  "Expands a `dna-seq` to a given target dimension by repeating elements.

Note: `dna-seq` can have any type of elements (not only constants)"
  ([dna-seq ext-dim]
   (let [dim (core/dna-dimension dna-seq)]
     (expand-dna-seq dna-seq dim ext-dim)))
  ([dna-seq dim ext-dim]
   (core/expand-dna-seq dna-seq dim ext-dim)))

(s/fdef reduce-dna-seq
  :args (s/or :ar1 (s/cat :dna-seq ::dna-seq)
              :ar2 (s/and (s/cat :terms   sequential?
                                 :dna-seq ::dna-seq)
                          #(== (count (:terms %))
                               (core/dna-dimension (:dna-seq %)))))
  :ret  (s/and (s/cat :terms   sequential?
                      :dna-seq ::dna-seq)
               #(== (core/dna-dimension (-> % :dna-seq))
                    (count (-> % :terms)))))
(defn reduce-dna-seq
  "Reduces a `dna-seq` by eliminating redundant/contingent terms.

  * returns a tuple `[terms dna-seq]`, where `terms` is a sequence that represents the remaining terms after reduction
  * takes an optional `terms` sequence of any kind of items that will be used instead of the default arithmetic sequence `[0 1 2 …]` to represent each term (length has to match the formDNA dimension)

  Note: `dna-seq` can have any type of elements (not only constants)"
  ([dna-seq]
   (core/reduce-dna-seq dna-seq))
  ([terms dna-seq]
   (core/reduce-dna-seq terms dna-seq)))

; (s/fdef filter-dna-seq
;   :args (s/and (s/cat :dna-seq          ::dna-seq
;                       :depth-selections (s/coll-of ::const-int?
;                                                    :kind sequential?))
;                #(== (core/dna-dimension (-> % :dna-seq))
;                     (count (-> % :depth-selections))))
;   :ret  ::dna-seq)
; (defn filter-dna-seq
;   "Filters a `dna-seq` by matching each of its “depth indices” (which corresponds to the interpretation order of terms) with the integers from a given `depth-selections` sequence.
  
;   This is a generalized form of `filter-dna`."
;   ;; ! needs better explanation and examples
;   [dna-seq depth-selections]
;   (core/filter-dna-seq dna-seq depth-selections))

(s/fdef filter-dna
  :args (s/and (s/cat :dna    ::dna
                      :vpoint ::vpoint)
               #(== (core/dna-dimension (-> % :dna))
                    (count (-> % :vpoint))))
  :ret  ::dna)
(defn filter-dna
  "Filters a `dna` by selecting specific parts corresponding to a given `vpoint`, which acts as a coordinate vector in its value space.

  * use holes `:_` to indicate a variable selection"
  [dna vpoint]
  (core/filter-dna dna vpoint))


;; Convert to/from formDNA

(defmacro ^:private spec--dna-seq-args [spec]
  `(s/alt :ar1 (s/cat :dna-seq ~(if (nil? spec)
                                  `::dna-seq
                                  `(s/and ::dna-seq ~spec)))
          :ar2 (s/cat :sort-code ::sort-code
                      :dna-seq ~(if (nil? spec)
                                  `::dna-seq
                                  `(s/and ::dna-seq ~spec)))))

(defmacro ^:private spec--dna-args []
  `(s/alt :ar1 (s/cat :dna       ::dna)
          :ar2 (s/cat :sort-code ::sort-code
                      :dna       ::dna)))

(s/fdef digits->dna
  :args (spec--dna-seq-args (s/coll-of ::const-int 
                                       :min-count 1
                                       :kind sequential?))
  :ret  ::dna)
(defn digits->dna
  "Converts a `seqable?` of digits (as string/char or integer) to formDNA.
  
Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `digits` will be reordered to match the code."
  ([dna-seq] (core/digits->dna dna-seq))
  ([sort-code dna-seq]
   (core/digits->dna sort-code dna-seq)))

(s/fdef dna->digits
  :args (spec--dna-args)
  :ret  (s/coll-of ::const-int
                   :min-count 1
                   :kind sequential?))
(defn dna->digits
  "Converts formDNA to a sequence of digits corresponding to a `sort-code`.
  
Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `dna` will be reordered to match the code."
  ([dna] (core/dna->digits dna))
  ([sort-code dna]
   (core/dna->digits sort-code dna)))


;;-------------------------------------------------------------------------
;; formDNA perspective
;; -> permutation of a formDNA
;; -> representation of a different “perspective” on a value structure

(s/def ::permutation-order
  (s/coll-of nat-int?
             :distinct true
             :kind sequential?))

(s/def ::dna-perspective
  (s/cat :perm-order ::permutation-order
         :dna        ::dna))

(s/fdef permute-dna
  :args (s/and (s/alt :ar2 (s/cat :dna        ::dna
                                  :perm-order ::permutation-order)
                      :ar3 (s/cat :opts (s/keys :opt-un [:opts.safety/limit?])
                                  :dna        ::dna
                                  :perm-order ::permutation-order))
               #(let [{:keys [dna-seq perm-order]} (-> % second)]
                  (== (core/dna-dimension dna-seq)
                      (count perm-order))))
  :ret  ::dna-perspective)
(defn permute-dna
  ([dna perm-order] (permute-dna {} dna perm-order))
  ([opts dna perm-order]
   (core/permute-dna-seq opts dna perm-order)))

(s/def ::dna-perspective-group
  (s/map-of ::permutation-order ::dna))

(s/fdef dna-perspectives
  :args (s/alt :ar1 (s/cat :dna ::dna)
               :ar2 (s/cat :opts (s/keys :opt-un [:opts.safety/limit?])
                           :dna ::dna))
  :ret  ::dna-perspective-group)
(defn dna-perspectives
  "Given a formDNA, generates all of its permutations and returns a map from permuted term order to the corresponding formDNA."
  ([dna] (dna-perspectives {} dna))
  ([opts dna]
   (into {} (core/dna-seq-perspectives opts dna))))


;;-------------------------------------------------------------------------
;; vpoint
;; -> value point
;; -> vector of const-coordinates in a vspace

(s/def ::vpoint
  (s/every ::const?
           :kind sequential?))

(def vpoint? (partial s/valid? ::vpoint))

(s/fdef rand-vpoint
  :args (s/? nat-int?)
  :ret  ::vpoint)
(defn rand-vpoint
  "Generates a random vpoint either as an infinite lazy seq or with given dimension `dim`."
  ([]    (repeatedly #(rand-nth nuim-code)))
  ([dim] (repeatedly dim #(rand-nth nuim-code))))


;;-------------------------------------------------------------------------
;; vspace
;; -> value space
;; -> vector of all `n`-dimensional vpoints

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

(s/fdef vspace
  :args (s/alt :ar1 (s/cat :dim ::dna-dimension)
               :ar2 (s/cat :sort-code ::sort-code
                           :dim ::dna-dimension))
  :ret  ::vspace)
(defn vspace
  "Generates a vspace of dimension `dim`, optionally with custom `sort-code`.

  * returns a lazy-seq which may be too memory-expensive to fully realize for dimensions greater than 11 (> 200 Mio. elements in total!)"
  ([dim] (core/vspace dim))
  ([sort-code dim]
   (core/vspace sort-code dim)))


;;-------------------------------------------------------------------------
;; vdict
;; -> value dictionary
;; -> (sorted) k-v map from `vspace` to `dna`
;; - for value table generation
;; - like a flat vmap

(s/def ::vdict
  (s/and map?
         #(s/valid? ::vspace (keys %))
         #(s/valid? ::dna    (vals %))))

(def vdict? (partial s/valid? ::vdict))

(s/def :vdict.opts/default-result ::const)

(s/fdef vdict
  :args (s/alt :ar1 (s/cat :vp->r (s/map-of ::vpoint ::const))
               :ar2 (s/cat :opts  (s/keys :opt-un [:vdict.opts/default-result
                                                   :opts/sorted?])
                           :vp->r (s/map-of ::vpoint ::const)))
  :ret  ::vdict)
(defn vdict
  "Generates a vdict given a map vpoint->result (result is a constant).

  * if the corresponding vspace is not a subset of the set of keys from `vp->r`, the remaining results will be filled with :N or a given default constant
  * optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive"
  ([vpoint->result] (vdict {} vpoint->result))
  ([opts vpoint->result]
   (core/vdict opts vpoint->result)))

(s/fdef dna->vdict
  :args (s/cat :opts (s/keys :opt-un [:opts/sorted? :opts.safety/unsafe?])
               :dna  ::dna)
  :ret  ::vdict)
(defn dna->vdict
  "Generates a vdict from a given dna.

  * optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive"
  ([dna] (core/dna->vdict {} dna))
  ([opts dna]
   (core/dna->vdict opts dna)))


;;-------------------------------------------------------------------------
;; vmap
;; -> value map
;; -> mapping from `vspace` topology to `dna`

(s/def ::vmap
  (s/or :vmap (s/map-of ::const
                        ::vmap
                        :count 4)
        :res ::const))

(def vmap? (partial s/valid? ::vmap))

(s/fdef vdict->vmap
  :args (s/cat :vdict ::vdict)
  ;; ? fmap arg needed
  ; :args (s/alt :ar1 (s/cat :vdict ::vdict)
  ;              :ar2 (s/cat :fmap
  ;                          (s/nilable
  ;                           ;; lightweight spec to prevent recursion issues
  ;                           (s/fspec :args (s/cat :vmap map?
  ;                                                 :vspace sequential?
  ;                                                 :depth nat-int?
  ;                                                 :dim ::dna-dimension)
  ;                                    :ret  map?))
  ;                          :vdict ::vdict))
  :ret  ::vmap)
(defn vdict->vmap
  "Generates a vmap from a given vdict."
  ;; ? needs more documentation
  [vdict]
  (core/vdict->vmap vdict))

(s/fdef dna->vmap
  :args (s/cat :dna ::dna)
  :ret  ::vmap)
(defn dna->vmap
  [dna]
  (core/dna->vmap dna))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arithmetic

(s/fdef rel
  :args (s/* (s/or :const ::const
                   :dna   ::dna))
  :ret  (s/or :const ::const
              :dna   ::dna))
(defn rel
  "Relates the values of 2 constants in a formDNA to each other."
  [& consts-or-dnas]
  (apply core/rel consts-or-dnas))
;; alias
(def -- "Alias to `rel`." rel)

(s/fdef inv
  :args (s/* (s/or :const ::const
                   :dna   ::dna))
  :ret  (s/or :const ::const
              :dna   ::dna))
(defn inv
  "Inverts the value of a every constant in a formDNA."
  [& consts-or-dnas]
  (apply core/inv consts-or-dnas))
;; alias
(def | "Alias to `inv`." inv)


(def ^:no-doc fns-with-specs (utils/list-fn-specs "formform.calc"))

(comment

  )

