(ns formform.calc
  "API for the `calc` module of `formform`.

  ## Concepts

  ### Values
  
  **constant**  
  → representation of a _value_ in FORM logic

  * _value_ → element of a _value system_
  * _value_ → state indicated by an _expression_
  * _value system_ → system of differences

  A `sort-code` specifies a numeric ordering for _constants_, which is useful for conversion to integers and _formDNA_ interpretation order. It is always assumed to be in _nuim-code_ by default:

      0 = :n → unmarked
      1 = :u → undetermined
      2 = :i → imaginary
      3 = :m → marked

  Make sure you convert to/from `nuim-code` when using different codes!


  ### Value Structures

  **formDNA**  
  → representation of a _value structure_ in FORM logic  

  * becomes a quaternary number when reversed and converted to digits
  * _value structure_ → specific structure in _value system_

  **formDNA perspective**  
  → permutation of _formDNA_  
  → representation of a different perspective on the _value structure_

  **vpoint**  
  → relate _values_ as a point  
  → vector of _constant_-coordinates in a _vspace_

  **vspace**  
  → relate _vpoints_ as a space  
  → vector of all n-dimensional _vpoints_

  **vdict**  
  → map _vpoints_ to _values_ in a dictionary  
  → (sorted) key-value map from _vspace_ to _formDNA_
  
  - for value table generation
  - like a flat _vmap_

  **vmap**  
  → map recursively decomposed _vspace_ to _value structure_  
  → mapping from _vspace_ topology to _formDNA_
  "
  (:require [formform.calc.core :as core]
            [formform.calc.specs :as sp]
            [formform.utils :as utils]
            [clojure.test.check.generators]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))


;; ? common specs
(s/def :opts/sorted? boolean?)
(s/def :opts.safety/limit? boolean?)
(s/def :opts.safety/unsafe? boolean?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures

;;-------------------------------------------------------------------------
;; Constants

(def nuim-code core/nuim-code)
(def nmui-code core/nmui-code)

(def consts
  "Set of all 4 constants."
  core/consts)

(def val-hole
  "“Value hole” – placeholder for an unknown/missing result. Denotes a constant, but like a black box, we cannot know which one."
  core/val-hole)

(def const?
  "Checks if the argument is a valid constant."
  (partial s/valid? ::sp/const))

(s/fdef rand-const
  :args (s/alt :ar0 (s/cat)
               :ar1 (s/cat :seed :rand/seed))
  :ret  ::sp/const)
(defn rand-const
  "Generates a random constant. A seed (an integer) can be provided as a second argument for reproducability."
  ([] (core/rand-const (utils/make-rng)))
  ([seed] (core/rand-const (utils/make-rng seed))))

(s/fdef rand-const-weighted
  :args (s/alt :ar1 (s/cat :weights ::sp/const-weights)
               :ar2 (s/cat :weights ::sp/const-weights
                           :seed :rand/seed))
  :ret  ::sp/const)
(defn rand-const-weighted
  "Same as `rand-const`, but takes a `weights` argument to specify the relative probability of each of the four constants to be randomly chosen.

  Weights can be provided either as:
  * a sequence of 4 non-negative numbers (e.g. `[1 0 2 5]`) in n-u-i-m order
  * a map (e.g. `{:i 1 :u 2}`), where missing weights are 0
  * a single number in the interval [0.0, 1.0] that represents the ratio of `:u`/`:i`/`m` against `:n` (whose weight is 1 - x)"
  ([const-weights] (core/rand-const (utils/make-rng) const-weights))
  ([const-weights seed] (core/rand-const (utils/make-rng seed) const-weights)))


;; Compare constants

(def sort-code? (partial s/valid? ::sp/sort-code))

;; for some reason, spec/orchestra needs a custom generator for `map-entry?`
(s/def ::const-map-entry
  (s/with-gen
    (s/and map-entry?
           #(s/valid? ::sp/const (first %)))
    #(gen/fmap first (gen/map (s/gen ::sp/const) (gen/simple-type)))))

(s/def ::consts-map-entry
  (s/with-gen
    (s/and map-entry?
           #(s/valid? ::sp/consts (first %)))
    #(gen/fmap first (gen/map (s/gen ::sp/consts) (gen/simple-type)))))

(s/fdef make-compare-consts
  :args (s/cat :sort-code ::sp/sort-code)
  :ret  (s/fspec :args (s/or :const-or-consts
                             (s/tuple (s/or :const  ::sp/const
                                            :consts ::sp/consts)
                                      (s/or :const  ::sp/const
                                            :consts ::sp/consts))
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
  :args (s/alt :ar1 ::sp/const_-int
               :ar2 (s/cat :sort-code ::sp/sort-code
                           :int ::sp/const_-int))
  :ret  ::sp/const_)
(defn digit->const
  "Converts a digit to its corresponding constant representation."
  ([n] (core/digit->const nuim-code n))
  ([sort-code n]
   (core/digit->const sort-code n)))

(s/fdef char->const
  :args (s/alt :ar1 ::sp/const_-char
               :ar2 (s/cat :sort-code ::sp/sort-code
                           :char (s/nonconforming ::sp/const_-char)))
  :ret  ::sp/const_)
(defn char->const
  "Coerces a `char` to a corresponding `constant`."
  ([c] (core/char->const nuim-code c))
  ([sort-code c]
   (core/char->const sort-code c)))

(s/fdef const->digit
  :args (s/alt :ar1 ::sp/const
               :ar2 (s/cat :sort-code ::sp/sort-code
                           :const ::sp/const))
  :ret  ::sp/const_-int)
(defn const->digit
  "Converts a `constant` to a `digit` corresponding to an optional `sort-code` or the default `nuim-code`."
  ([c] (core/const->digit nuim-code c))
  ([sort-code c]
   (core/const->digit sort-code c)))


;;-------------------------------------------------------------------------
;; formDNA

(def dna-dimension? (partial s/valid? ::sp/dna-count))
(def dna? (partial s/valid? ::sp/dna))
(def partial-dna? (partial s/valid? (s/and (fn [xs] (some #{:_} xs))
                                           ::sp/dna_)))


(s/fdef dna-dimension
  :args (s/cat :xs sequential?)
  :ret  (s/or :invalid nil?
              :dim ::sp/dna-dimension))
(defn dna-dimension
  "Calculates the dimension of a `formDNA`/`dna-seq` (corresponds to the number of variables in a FORM). The length of a `dna-seq` is 4^d for its dimension d.

  * the input sequence can have any type of elements"
  [xs]
  (core/dna-dimension xs))

;; ? extend to allow holes `:_` / `-1`
(s/fdef make-dna
  :args (s/and (s/nonconforming ::sp/dna-seq-elem-tree)
               #(dna-dimension? (flatten %)))
  :ret  ::sp/dna
  :fn   #(== (count (-> % :ret))
             (count (flatten (-> % :args)))))
(defn make-dna
  "Creates a formDNA from arguments, which may be valid chars, keywords, integers or sequences thereof.

  * valid chars are: \\n \\u \\i \\m (upper- or lowercase) and \\0 \\1 \\2 \\3
  * valid integers are: 0 1 2 3
  * valid keywords are: :n :u :i :m
  * total argument count (including count of sequence args) must match a valid formDNA length, which is 4^d, where d is a natural number"
  [& xs]
  (apply core/make-dna xs))

(s/fdef rand-dna
  :args (s/alt :ar1 (s/cat :dim ::sp/dna-dimension)
               :ar2 (s/cat :dim ::sp/dna-dimension
                           :seed :rand/seed))
  :ret  ::sp/dna)
(defn rand-dna
  "Generates a random formDNA of dimension `dim`. A seed (an integer) can be provided as a second argument for reproducability."
  ([dim] (core/rand-dna (utils/make-rng) dim))
  ([dim seed] (core/rand-dna (utils/make-rng seed) dim)))

(s/fdef rand-dna-weighted
  :args (s/alt :ar2 (s/cat :dim ::sp/dna-dimension
                           :weights ::sp/const-weights)
               :ar3 (s/cat :dim ::sp/dna-dimension
                           :weights ::sp/const-weights
                           :seed :rand/seed))
  :ret  ::sp/dna)
(defn rand-dna-weighted
  "Same as `rand-dna`, but takes a `weights` argument to specify the relative probability of each of the four constants to be randomly chosen.

  Weights can be provided either as:
  * a sequence of 4 non-negative numbers (e.g. `[1 0 2 5]`) in n-u-i-m order
  * a map (e.g. `{:i 1 :u 2}`), where missing weights are 0
  * a single number in the interval [0.0, 1.0] that represents the ratio of `:u`/`:i`/`m` against `:n` (whose weight is 1 - x)"
  ([dim const-weights]
   (core/rand-dna (utils/make-rng) dim const-weights))
  ([dim const-weights seed]
   (core/rand-dna (utils/make-rng seed) dim const-weights)))

#_#_
(s/fdef rand-dna-seq-from
  :args (s/alt :ar2 (s/cat :dim ::sp/dna-dimension
                           :elems (s/or :seq (s/and sequential?
                                                    #(<= 1 (count %) 4))
                                        :nil nil?))
               :ar3 (s/cat :dim ::sp/dna-dimension
                           :elems (s/or :seq (s/and sequential?
                                                    #(<= 1 (count %) 4))
                                        :nil nil?)
                           :seed :rand/seed))
  :ret  ::sp/dna-seq)
(defn rand-dna-seq-from
  "Like `rand-dna` but selects from a vector of 1 (min) to 4 (max) custom elements instead of `[:n :m :u :i]` (e.g. to restrict generated values). A random seed can be provided as a second argument for reproducability."
  ([dim elems] (core/rand-dna (utils/make-rng) dim elems))
  ([dim elems seed]
   (core/rand-dna (utils/make-rng seed) dim elems)))


;; Sort formDNA

(s/fdef reorder-dna-seq
  :args (s/cat :dna-seq        ::sp/dna-seq_
               :sort-code-from ::sp/sort-code
               :sort-code-to   ::sp/sort-code)
  :ret  ::sp/dna-seq_)
(defn reorder-dna-seq
  "Reorders given formDNA/`dna-seq` from `sort-code-from` to `sort-code-to`.

  Note:
  * `dna-seq` can have any type of elements (not only constants)
  * does NOT change the encoding of the elements, just their ordering"
  [dna-seq sort-code-from sort-code-to]
  (core/reorder-dna-seq dna-seq sort-code-from sort-code-to))


;; Compare formDNA

(s/fdef equal-dna?
  :args (s/every ::sp/dna :min-count 1)
  :ret  boolean?)
(defn equal-dna?
  "Equality check for formDNA. Two formDNAs are considered equal, if they contain the same constants in the same order. Stricter than `equiv-dna?`, where permutations are considered equal.

  Note: partial formDNA (which includes holes (`:_`)) cannot be compared and thus are not valid input. If you know/assume equality for holes, use `equal-partial-dna-assuming-holes-equal?`."
  [& dnas]
  {:pre [(not (some (partial some #(= :_ %)) dnas))]}
  (apply core/equal-dna? dnas))

(s/fdef equiv-dna?
  :args (s/every ::sp/dna :min-count 1)
  :ret  boolean?)
(defn equiv-dna?
  "Equivalence check for formDNA. Two formDNAs are considered equivalent, if they belong to the same equivalence-class of `dna-perspectives` (i.e. if they are permutations of each other).

  Note: partial formDNA (which includes holes (`:_`)) cannot be compared and thus are not valid input. If you know/assume equality for holes, use `equiv-partial-dna-assuming-holes-equal?`."
  [& dnas]
  {:pre [(not (some (partial some #(= :_ %)) dnas))]}
  (apply core/equiv-dna? dnas))


;; ? or `unsafe-equal-dna?`

(s/fdef equal-partial-dna-assuming-holes-equal?
  :args (s/every ::sp/dna_ :min-count 1)
  :ret  boolean?)
(defn equal-partial-dna-assuming-holes-equal?
  "Equality check for partial formDNA (derived from `equal-dna?`), under the assumption that all holes (`:_`) originate from the same expression and thus their supposed value would be equal."
  [& dnas]
  (apply core/equal-dna? dnas))

(s/fdef equiv-partial-dna-assuming-holes-equal?
  :args (s/every ::sp/dna_ :min-count 1)
  :ret  boolean?)
(defn equiv-partial-dna-assuming-holes-equal?
  "Equivalence check for partial formDNA (derived from `equiv-dna?`), under the assumption that all holes (`:_`) originate from the same expression and thus their supposed value would be equal."
  [& dnas]
  (apply core/equiv-dna? dnas))


;; Transform formDNA

(s/fdef expand-dna-seq
  :args (s/alt :ar2 (s/cat :dna-seq ::sp/dna-seq_
                           :ext-dim ::sp/dna-dimension)
               :ar3 (s/& (s/cat :dna-seq ::sp/dna-seq_
                                :dim     ::sp/dna-dimension
                                :ext-dim ::sp/dna-dimension)
                         #(<= (:dim %) (:ext-dim %))))
  :ret  ::sp/dna-seq_
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

;; ? `unsafe-` variant for partial dna
(s/fdef reduce-dna-seq
  :args (s/alt :ar1 (s/cat :dna-seq ::sp/dna-seq_)
               :ar2 (s/& (s/cat :dna-seq ::sp/dna-seq_
                                :terms   sequential?)
                         #(== (count (:terms %))
                              (core/dna-dimension (:dna-seq %)))))
  :ret  (s/and (s/cat :terms   sequential?
                      :dna-seq ::sp/dna-seq_)
               #(== (core/dna-dimension (-> % :dna-seq))
                    (count (-> % :terms)))))
(defn reduce-dna-seq
  "Reduces a `dna-seq` by eliminating redundant/contingent terms.

  * returns a tuple `[terms dna-seq]`, where `terms` is a sequence that represents the remaining terms after reduction
  * takes an optional `terms` sequence of any kind of items that will be used instead of the default arithmetic sequence `[0 1 2 …]` to represent each term (length has to match the formDNA dimension)

  Note: `dna-seq` can have any type of elements (not only constants)"
  ([dna-seq]
   (core/reduce-dna-seq dna-seq))
  ([dna-seq terms]
   (core/reduce-dna-seq dna-seq terms {})))

;; (s/fdef filter-dna-seq
;;   :args (s/and (s/cat :dna-seq          ::sp/dna-seq
;;                       :depth-selections (s/coll-of ::sp/const_-int
;;                                                    :kind sequential?))
;;                #(== (core/dna-dimension (-> % :dna-seq))
;;                     (count (-> % :depth-selections))))
;;   :ret  ::sp/dna-seq)
;; (defn filter-dna-seq
;;   "Filters a `dna-seq` by matching each of its “depth indices” (which corresponds to the interpretation order of terms) with the integers from a given `depth-selections` sequence.

;;   This is a generalized form of `filter-dna`."
;;   ;; ! needs better explanation and examples
;;   [dna-seq depth-selections]
;;   (core/filter-dna-seq dna-seq depth-selections))

(s/fdef filter-dna
  :args (s/& (s/cat :dna    ::sp/dna_
                    :vpoint ::sp/vpoint)
             #(== (core/dna-dimension (-> % :dna))
                  (count (-> % :vpoint))))
  :ret  ::sp/dna_)
(defn filter-dna
  "Filters a `dna` by selecting specific parts corresponding to a given `vpoint`, which acts as a coordinate vector in its value space.

  * use holes `:_` in `vpoint` to indicate a variable selection"
  [dna vpoint]
  (core/filter-dna dna vpoint))

(s/fdef dna-get
  :args (s/& (s/cat :dna    ::sp/dna_
                    :vpoint (s/every ::sp/const
                                     :kind sequential?))
             #(== (core/dna-dimension (-> % :dna))
                  (count (-> % :vpoint))))
  :ret  ::sp/const)
(defn dna-get
  "Extracts a single value from a `dna` according to a given `vpoint` index, which is a sequence of constants corresponding to each term→value association."
  [dna vpoint]
  (let [subdna (core/filter-dna dna vpoint)]
    (when (= 1 (count subdna))
      (first subdna))))


;; Convert to/from formDNA

(s/fdef digits->dna
  :args (sp/spec--dna-seq-args (s/coll-of ::sp/const-int 
                                          :min-count 1
                                          :kind sequential?))
  :ret  ::sp/dna_)
(defn digits->dna
  "Converts a `seqable?` of digits (as string/char or integer) to formDNA. 
  
  Note that `nuim-code` is the default ordering and is always assumed for _formDNA_. If a different `sort-code` is specified, the input sequence (expected in this code) will be mapped and reordered to match `nuim-code`. If you just want the mapping without reordering, use something like `(mapv digit->const dna)`."
  ([dna-seq] (core/digits->dna dna-seq))
  ([from-sort-code dna-seq]
   (core/digits->dna from-sort-code dna-seq)))

(s/fdef dna->digits
  :args (sp/spec--dna-args)
  :ret  (s/coll-of ::sp/const-int
                   :min-count 1
                   :kind sequential?))
(defn dna->digits
  "Converts formDNA to a sequence of digits corresponding to a `sort-code`.
  
  Note that `nuim-code` is the default ordering and is always assumed for _formDNA_. If a different `sort-code` is specified, the input dna (expected in `nuim-code`) will be mapped and reordered to match this code."
  ([dna] (core/dna->digits dna))
  ([to-sort-code dna]
   (core/dna->digits to-sort-code dna)))

;; ? not needed
(s/fdef chars->dna
  :args (sp/spec--dna-seq-args (s/coll-of ::sp/const-char
                                          :min-count 1
                                          :kind sequential?))
  :ret  ::sp/dna_)
(defn chars->dna
  "Converts a `seqable?` of chars to formDNA.
  
  Note that `nuim-code` is the default ordering and is always assumed for _formDNA_. If a different `sort-code` is specified, the input sequence (expected in this code) will be reordered to match `nuim-code`."
  ([dna-seq] (core/chars->dna dna-seq))
  ([from-sort-code dna-seq]
   (core/chars->dna from-sort-code dna-seq)))


;;-------------------------------------------------------------------------
;; formDNA perspective

(s/fdef permute-dna
  :args (s/& (s/alt :ar2 (s/cat :dna        ::sp/dna_
                                :perm-order ::sp/permutation-order)
                    :ar3 (s/cat :dna        ::sp/dna_
                                :perm-order ::sp/permutation-order
                                :opts (s/keys :opt-un [:opts.safety/limit?])))
             #(let [{:keys [dna perm-order]} (-> % second)]
                (== (core/dna-dimension dna)
                    (count perm-order))))
  :ret  ::sp/dna-perspective)
(defn permute-dna
  "Given a formDNA (`dna`), generates its permutation (called a “perspective”) that matches the given `perm-order`.

  `perm-order` must be a sequence of indices that correspond to each term/subdna of the `dna`. Think of each place in the sequence as a depth (shallowest → deepest) and each index as an identifier of the term in that depth.

  For example, `[0 1 2]` is the default order of terms in a 3-dimensional formDNA. `[1 2 0]` would permute the terms, such that in the resulting formDNA term 0 is in depth 2, term 1 in depth 0 and term 2 in depth 1."
  ([dna perm-order] (core/permute-dna-seq {} dna perm-order))
  ([dna perm-order opts] (core/permute-dna-seq opts dna perm-order)))

(s/fdef dna-perspectives
  :args (s/alt :ar1 (s/cat :dna ::sp/dna_)
               :ar2 (s/cat :dna ::sp/dna_
                           :opts (s/keys :opt-un [:opts.safety/limit?])))
  :ret  ::sp/dna-perspective-group)
(defn dna-perspectives
  "Given a formDNA (`dna`), generates all of its permutations and returns a map from permuted term order to the corresponding formDNA."
  ([dna] (dna-perspectives dna {}))
  ([dna opts]
   (let [dna-seq-psps (core/dna-seq-perspectives opts dna)]
     (with-meta (into {} dna-seq-psps)
       ;; retains key order from metadata (if available)
       (meta dna-seq-psps)))))


;;-------------------------------------------------------------------------
;; vpoint

(def vpoint? (partial s/valid? ::sp/vpoint))

(s/fdef rand-vpoint
  :args (s/alt :ar1 (s/cat :dim nat-int?)
               :ar2 (s/cat :dim nat-int?
                           :seed :rand/seed))
  :ret  ::sp/vpoint)
(defn rand-vpoint
  "Generates a random vpoint with given dimension `dim` (= length of the vpoint). A seed (an integer) can be provided as a second argument for reproducability."
  ([dim] (core/rand-vpoint (utils/make-rng) dim))
  ([dim seed] (core/rand-vpoint (utils/make-rng seed) dim)))

(s/fdef rand-vpoint-weighted
  :args (s/alt :ar2 (s/cat :dim nat-int?
                           :weights ::sp/const-weights)
               :ar3 (s/cat :dim nat-int?
                           :weights ::sp/const-weights
                           :seed :rand/seed))
  :ret  ::sp/vpoint)
(defn rand-vpoint-weighted
  "Same as `rand-vpoint`, but takes a `weights` argument to specify the relative probability of each of the four constants to be randomly chosen.

  Weights can be provided either as:
  * a sequence of 4 non-negative numbers (e.g. `[1 0 2 5]`) in n-u-i-m order
  * a map (e.g. `{:i 1 :u 2}`), where missing weights are 0
  * a single number in the interval [0.0, 1.0] that represents the ratio of `:u`/`:i`/`m` against `:n` (whose weight is 1 - x)"
  ([dim const-weights]
   (core/rand-vpoint (utils/make-rng) dim const-weights))
  ([dim const-weights seed]
   (core/rand-vpoint (utils/make-rng seed) dim const-weights)))


;;-------------------------------------------------------------------------
;; vspace

(def vspace? (partial s/valid? ::sp/vspace))

(s/fdef vspace
  :args (s/alt :ar1 (s/cat :dim ::sp/dna-dimension)
               :ar2 (s/cat :sort-code ::sp/sort-code
                           :dim ::sp/dna-dimension))
  :ret  ::sp/vspace)
(defn vspace
  "Generates a vspace of dimension `dim`, optionally with custom `sort-code`.

  * returns a lazy-seq which may be too memory-expensive to fully realize for dimensions greater than 11 (> 200 Mio. elements in total!)"
  ([dim] (core/vspace dim))
  ([sort-code dim]
   (core/vspace sort-code dim)))


;;-------------------------------------------------------------------------
;; vdict

(def vdict? (partial s/valid? ::sp/vdict))

(s/def :vdict.opts/default-result ::sp/const)

(s/fdef vdict
  :args (s/alt :ar1 (s/cat :vp->r (s/map-of ::sp/vpoint ::sp/const))
               :ar2 (s/cat :vp->r (s/map-of ::sp/vpoint ::sp/const)
                           :opts  (s/keys :opt-un [:vdict.opts/default-result
                                                   :opts/sorted?])))
  :ret  ::sp/vdict)
(defn vdict
  "Generates a vdict given a map `vpoint->result` (result is a constant).

  * if the corresponding vspace is not a subset of the set of keys from `vp->r`, the remaining results will be filled with :n or a given default constant
  * optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive"
  ([vpoint->result] (core/vdict {} vpoint->result))
  ([vpoint->result opts] (core/vdict opts vpoint->result)))

(s/fdef dna->vdict
  :args (s/alt :ar1 (s/cat :dna  ::sp/dna_)
               :ar2 (s/cat :dna  ::sp/dna_
                           :opts (s/keys :opt-un [:opts/sorted?
                                                  :opts.safety/unsafe?])))
  :ret  ::sp/vdict)
(defn dna->vdict
  "Generates a vdict from a given formDNA (`dna`).

  * optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive"
  ([dna] (core/dna->vdict {} dna))
  ([dna opts] (core/dna->vdict opts dna)))


;;-------------------------------------------------------------------------
;; vmap

(def vmap? (partial s/valid? ::sp/vmap))

(s/fdef vdict->vmap
  :args (s/cat :vdict ::sp/vdict)
  ;; ? fmap arg needed
  ;; :args (s/alt :ar1 (s/cat :vdict ::sp/vdict)
  ;;              :ar2 (s/cat :fmap
  ;;                          (s/nilable
  ;;                           ;; lightweight spec to prevent recursion issues
  ;;                           (s/fspec :args (s/cat :vmap map?
  ;;                                                 :vspace sequential?
  ;;                                                 :depth nat-int?
  ;;                                                 :dim ::sp/dna-dimension)
  ;;                                    :ret  map?))
  ;;                          :vdict ::sp/vdict))
  :ret  ::sp/vmap)
(defn vdict->vmap
  "Generates a vmap from a given vdict."
  ;; ? needs more documentation
  [vdict]
  (core/vdict->vmap vdict))

(s/fdef dna->vmap
  :args (s/cat :dna ::sp/dna_)
  :ret  ::sp/vmap)
(defn dna->vmap
  [dna]
  (core/dna->vmap dna))

(s/fdef vmap-dimension
  :args (s/cat :vmap ::sp/vmap)
  :ret  int?)
(defn vmap-dimension
  "Returns the dimension of a vmap (equivalent to `dna-dimension` of the corresponding formDNA)."
  [vmap]
  (core/vmap-dimension vmap))

;; ? is this a good name
(s/fdef vmap-perspectives
  :args (s/cat :dna-psps ::sp/dna-perspective-group)
  :ret  ::sp/vmap-perspective-group)
(defn vmap-perspectives
  "Given a group of all perspectives from a formDNA, returns these perspectives as vmaps."
  [dna-psps]
  (update-vals dna-psps dna->vmap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arithmetic

(s/fdef rel
  :args (s/* (s/or :const ::sp/const_
                   :dna   ::sp/dna_))
  :ret  (s/or :const ::sp/const_
              :dna   ::sp/dna_))
(defn rel
  "Relates the values of all given arguments (which must either be all constants or all formDNAs) to each other.

  Note: relations with value holes (`:_`) can only be dominated by the mark (`:m`) and will otherwise result in value holes."
  [& consts-or-dnas]
  (apply core/rel consts-or-dnas))
;; alias
(def -- "Alias to `rel`." rel)

(s/fdef inv
  :args (s/* (s/or :const ::sp/const_
                   :dna   ::sp/dna_))
  :ret  (s/or :const ::sp/const_
              :dna   ::sp/dna_))
(defn inv
  "Inverts the value of a given constant or formDNA. With multiple arguments, will relate all arguments (via `rel`) and then invert the result.

  Note: value holes (`:_`) will invert to value holes."
  [& consts-or-dnas]
  (apply core/inv consts-or-dnas))
;; alias
(def | "Alias to `inv`." inv)



(def ^:no-doc fns-with-specs (utils/list-fn-specs "formform.calc"))



(comment
  (rel [:_ :u :i :_] [:n :_ :i :m])
  (rel [:n :_ :i :m] :m)

  (inv [:_ :u :i :_] [:n :_ :i :m])

  ,)
