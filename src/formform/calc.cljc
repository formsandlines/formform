;; ========================================================================
;;     formform calculation module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================

(ns formform.calc
  (:require [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant
;; -> element of formDNA representing a primitive FORM value
;; -> single-digit formDNA

(def consts #{:N :U :I :M})
(def var-const :_)

(s/def :formform.specs.calc/const
  (s/with-gen
    #(case %
       (:N :U :I :M) true
       false)
    #(gen/elements [:N :U :I :M])))

(s/def :formform.specs.calc/var-const #(= % var-const))

(s/def :formform.specs.calc/const?
  (s/with-gen
    (s/or :const     :formform.specs.calc/const
          :var-const :formform.specs.calc/var-const)
    #(gen/elements [:N :U :I :M var-const])))

(s/def :formform.specs.calc/sort-code
  (s/with-gen
    (s/and
      vector?
      #(== 4 (count %))
      #(= #{:N :U :I :M} (set %)))
    #(gen/shuffle [:N :U :I :M])))

(def const? (partial s/valid? :formform.specs.calc/const))
(def rand-const #(gen/generate (s/gen :formform.specs.calc/const)))

(def sort-code? (partial s/valid? :formform.specs.calc/sort-code))

;; predefined sort-codes
(def nuim-code [:N :U :I :M])
(def nmui-code [:N :M :U :I])

(defn digit->const
  ([n] (digit->const n nuim-code))
  ([n sort-code]
   (if (== n -1)
     var-const ;; “hole” or variable value
     (sort-code n))))

(defn char->const
  ([c] (char->const c nuim-code))
  ([c sort-code]
   (case c
     (\n \N) :N
     (\u \U) :U
     (\i \I) :I
     (\m \M) :M
     \_      var-const
     (when-let [n (edn/read-string (str c))]
       (digit->const n sort-code)))))


(defn const->digit
  ([c] (const->digit c nuim-code))
  ([c sort-code]
   (if (= c var-const)
     -1 ;; “hole” or variable value
     ((zipmap sort-code (range)) c))))

(defn consts->quaternary
  "Converts a sequence of constants to a corresponding quaternary number (as a string, prefixed by '4r').
  - use `read-string` to obtain the decimal value as a BigInt"
  [consts]
  (if (seq consts)
    (let [digits (map const->digit consts)]
      (apply str "4r" digits))
    (throw (ex-info "Must contain at least one element." {:arg consts}))))

(defn make-compare-consts
  "Given a `sort-code` (try `calc.nuim-code` or `calc.nmui-code`), returns a comparator function to sort single constants, formDNA or arbitrary sequences of constants (can be mixed).
  - can also compare map-entries by keys of comparable types"
  [sort-code]
  (fn [a b]
    (let [sort-map  (zipmap sort-code (range))
          convert   (fn convert [x]
                      (cond
                        ;; ! not interoperable for cljs will not
                        ;;   parse BigInt here
                        ;;   -> maybe use interop with js/BigInt
                        ;;   or a different approach
                        ; (dna? x)   (edn/read-string (consts->quaternary x))
                        (sequential? x) (edn/read-string
                                          (consts->quaternary x))
                        (const? x) (sort-map x)
                        ; (coll? x)  (mapv convert x)
                        :else (throw (ex-info "Incompatible type: " {:x x}))))]
      (if (and (map-entry? a) (map-entry? b))
        (compare (convert (first a)) (convert (first b)))
        (compare (convert a) (convert b))))))

;; convenience function
(def compare-consts (make-compare-consts nuim-code))


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

;; ? not sure if keyword representation is really needed here
;;   and a seqable should rather always be used for better performance

(def dna-lengths (iterate (partial * 4) 1))

;; ? true for JavaScript
(def max-safe-dna-dim 25)

(def max-safe-dna-len
  (last (take max-safe-dna-dim dna-lengths)))

(def cached_dna-length->dim
  (into {} (map vector (take max-safe-dna-dim dna-lengths) (range))))

(defn dna-length->dim
  [n]
  (if (<= n max-safe-dna-len)
    (cached_dna-length->dim n)
    (let [dim (/ (math/log n) ;; ? can count return a BigInt
                 (math/log 4.0))]
      (if (or (infinite? dim) (utils/has-decimal? dim))
        nil
        (int dim)))))

(defn dna-dimension
  "Calculates the dimension of a formDNA/`dna-seq` (corresponds to the number of variables in a FORM). The length of a `dna-seq` is 4^d for its dimension d.
  - the input sequence can have any type of elements"
  [xs]
  (dna-length->dim (count xs)))

(s/def :formform.specs.calc/dna-length
  (s/with-gen
    #(some? (dna-length->dim %))
    #(gen/elements (take 12 dna-lengths)))) 

(s/def :formform.specs.calc/dna-dimension nat-int?)

;; ? necessary
(s/def :formform.specs.calc/dna-count #(some? (dna-dimension %)))

(s/def :formform.specs.calc/dna-seq
  (s/and sequential? #(<= (count (set %)) 4)
         :formform.specs.calc/dna-count))

(s/def :formform.specs.calc/dna
  (s/and (s/coll-of consts 
                    :kind sequential? 
                    :min-count 1)
         (comp (partial s/valid? :formform.specs.calc/dna-length)
               count)))

(def dna-dimension? (partial s/valid? :formform.specs.calc/dna-count))
(def dna? (partial s/valid? :formform.specs.calc/dna))

(defn rand-dna
  "Generates a random formDNA/`dna-seq` of dimension `dim`. A vector of 4 custom elements can be provided as a second argument."
  ([dim] (rand-dna dim nil))
  ([dim elems]
   (let [len    (apply * (repeat dim 4))
         gen-fn #(rand-nth (if (and (some? elems) (<= (count elems) 4))
                             elems
                             nuim-code))]
     (vec (repeatedly len gen-fn)))))


(declare dna->digits)

(def reverse-dna
  "Reverses a formDNA (returns an rseq)
  - make sure the input is a vector for constant-time reverse"
  (comp rseq vec))

(defn reorder-dna-seq
  "Reorders given formDNA/`dna-seq` from `sort-code-from` to `sort-code-to`.
  
  Note:
  - `dna-seq` can have any type of elements (not only constants)
  - does NOT change the encoding of the elements, just their ordering"
  [dna-seq sort-code-from sort-code-to]
  (if (= sort-code-from sort-code-to)
    dna-seq
    (let [sort-idxs (->> (range 3 -1 -1)
                         (zipmap sort-code-from)
                         (sort (make-compare-consts sort-code-to))
                         reverse
                         (map second))
          ;; ! recursion can cause OutOfMemoryError with dim > 12
          aux (fn reorder [dna-subseq]
                (let [len      (count dna-subseq)
                      part-len (/ len 4)]
                  (if (< len 4)
                    (vec dna-subseq)
                    (->> sort-idxs
                         (map (fn [i]
                                (let [index   (* i part-len)
                                      cs-part (take part-len
                                                    (drop index dna-subseq))]
                                  (reorder cs-part))))
                         (reduce into)))))]
      (aux dna-seq))))


;; ? almost useless to abstract these functions for a single use case::

(defn prod=dna-seq->dna
  "Produces a converter function from a `dna-seq` of any type to `dna`.
  Requires a mapping function `sort+x->const` that takes a `sort-code` and an item `x` of the type expected in a to-be-converted `dna-seq` and returns a constant.
  - if `sort+x->const` is `nil`, the expected type of `x` is `const`"
  [sort+x->const]
  (fn f
    ([dna-seq] (f dna-seq nuim-code))
    ([dna-seq sort-code]
     (let [dna-seq (if (= sort-code nuim-code)
                     dna-seq
                     (reorder-dna-seq dna-seq sort-code nuim-code))]
       (mapv (partial sort+x->const sort-code) dna-seq)))))

(defn prod=dna->dna-seq
  "Produces a converter function from `dna` to a `dna-seq` of any type.
  Requires a mapping function `sort+const->x` that takes a `sort-code` and a constant from the to-be-converted `dna` and returns an item of the desired type.
  - if `sort+x->const` is `nil`, the expected type of `x` is `const`"
  [sort+const->x]
  (fn f
    ([dna] (f dna nuim-code))
    ([dna sort-code]
     (let [dna-seq (mapv (partial sort+const->x sort-code) dna)]
       (if (= sort-code nuim-code)
         dna-seq
         (reorder-dna-seq dna-seq nuim-code sort-code))))))

(def digits->dna
  "Converts a `seqable?` of digits (as string/char or integer) to formDNA.
  
  Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `digits` will be reordered to match the code."
  (prod=dna-seq->dna (fn [sort-code x] (digit->const x sort-code))))

(def chars->dna
  "Converts a `seqable?` of chars to formDNA.
  
  Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `digits` will be reordered to match the code."
  (prod=dna-seq->dna (fn [sort-code x] (char->const x sort-code))))

(def dna->digits
  "Converts formDNA to a sequence of digits corresponding to a `sort-code`.
  
  Note that `nuim-code` is the default ordering. If a different `sort-code` is specified, `dna` will be reordered to match the code."
  (prod=dna->dna-seq (fn [sort-code x] (const->digit x sort-code))))


;; ? is this correct expansion for dim > 2 (see `rel`)
(defn expand-dna-seq
  "Expands a `dna-seq` to a given target dimension by repeating elements.
  
  Note: `dna-seq` can have any type of elements (not only constants)"
  ([dna-seq ext-dim]
   (let [dim (dna-dimension dna-seq)]
     (expand-dna-seq dna-seq dim ext-dim)))
  ([dna-seq dim ext-dim]
   (reduce
    (fn [seq-expanded x]
      (into seq-expanded
            (repeat (utils/pow-nat 4 (- ext-dim dim)) x)))
    []
    dna-seq)))

(defn reduce-dna-seq
  "Reduces a `dna-seq` by eliminating redundant/contingent terms.
  - returns a tuple `[terms dna-seq]`, where `terms` is a sequence that represents the remaining terms after reduction
  - takes an optional `terms` sequence of any kind of items that will be used instead of the default arithmetic sequence `[0 1 2 …]` to represent each term (length has to match the formDNA dimension)
  
  Note: `dna-seq` can have any type of elements (not only constants)"
  ([dna-seq]
   (let [dim (dna-dimension dna-seq)]
     (reduce-dna-seq (vec (range dim)) dna-seq)))
  ([terms dna-seq]
   (loop [dna-seq dna-seq
          terms   (utils/reversev terms)
          dim     (dna-dimension dna-seq)
          subdim  0]
     (if (>= subdim dim)
       [(utils/reversev terms) dna-seq]
       (let [quad-len   (utils/pow-nat 4 subdim)
             parts      (if (== 1 quad-len)
                          dna-seq
                          (partition quad-len dna-seq))
             simplified (loop [[quad r] (split-at 4 parts)
                               result   []]
                          (when (apply = quad)
                            (let [result (if (== 1 quad-len)
                                           (conj result (first quad))
                                           (into result (first quad)))]
                              (if (< (count r) 4)
                                result
                                (recur (split-at 4 r) result)))))]
         (if (nil? simplified)
           (recur dna-seq terms dim (inc subdim))
           (recur simplified
                  (utils/dissocv terms subdim)
                  (dec dim) subdim)))))))


(defn make-dna
  "Creates a formDNA from arguments, which may be valid chars, keywords, integers or sequences thereof.
  - valid chars are: \\n \\u \\i \\m (upper- or lowercase) and \\0 \\1 \\2 \\3
  - valid integers are: 0 1 2 3
  - valid keywords are: :N :U :I :M
  - total argument count (including count of sequence args) must match a valid formDNA length, which is 4^d, where d is a natural number"
  [& xs]
  (let [x->const
        (fn [x] (if-let [c (condp #(%1 %2) x
                             keyword?    (when (const? x) x)
                             char?       (char->const x)
                             integer?    (digit->const x)
                             sequential? (apply make-dna x)
                             (throw (ex-info "unsupported type" {:val x})))]
                  c
                  (throw (ex-info "invalid value" {:val x}))))
        dna (->> xs
                 (map x->const)
                 (reduce #(if (sequential? %2)
                            (into %1 %2)
                            (conj %1 %2)) []))]
    (if (some? (dna-dimension dna))
      dna
      (throw (ex-info "invalid dna length" {:dna dna})))))


(defn filter-dna-seq
  [dna-seq depth-selections]
  (let [dim (dna-dimension dna-seq)]
    (if (== dim (count depth-selections))
      (let [f (fn [pos depth] (* pos (utils/pow-nat 4 (- dim depth))))
            depth-offsets (map (fn [pos depth]
                                 (if (== pos -1)
                                   (map #(f % depth) (range 0 4))
                                   (list (f pos depth))))
                               depth-selections (drop 1 (range)))
            idxs (set (map #(apply + %)
                           (apply combo/cartesian-product depth-offsets)))
            n (dec (count dna-seq))]
        (->> dna-seq
             (mapv vector (range))
             (filterv (fn [[i c]] (idxs (- n i))))
             (mapv second)))
      (throw
       (ex-info "Size of selection vector must be equal to dna-seq dimension!"
                {:expected dim :actual (count depth-selections)})))))

(defn filter-dna
  "Filters a `dna` by selecting specific parts corresponding to a given `vpoint`, which acts as a coordinate vector in its value space.
  - use holes `:_` to indicate a variable selection"
  [dna vpoint]
  (filter-dna-seq dna (map const->digit vpoint)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formDNA perspectives

(defn permute-dna-seq
  ([dna-seq perm-order] (permute-dna-seq dna-seq perm-order {}))
  ([dna-seq perm-order {:keys [limit?] :or {limit? true}}]
   (let [dim     (dna-dimension dna-seq)
         dna-vec (vec dna-seq)]
     (cond
       (not= dim (count perm-order)) nil
       (< dim 2)                     [perm-order dna-vec]
       (= perm-order (range dim))    [perm-order dna-vec]
       ;; fast-ish for up to 10 dimensions, have not tested beyond 12
       (and limit? (> dim 12))       (throw
                                      (ex-info "Aborted: operation too expensive for dimensions greater than 12. Set `:limit?` to false to proceed."
                                               {:input [dna-seq perm-order]}))
       :else
       (let [int->quat-str (fn [n] (utils/pad-left (utils/int->nbase n 4)
                                                   dim "0"))
             dim-ns (range dim)
             perm-dna-seq
             (mapv (fn [i]
                     (let [qtn-key  (mapv (comp edn/read-string str)
                                          (int->quat-str i))
                           perm-key (apply str "4r"
                                           (map #(qtn-key (perm-order %))
                                                dim-ns))
                           i-perm   (edn/read-string perm-key)]
                       (dna-vec i-perm)))
                   (range (count dna-seq)))]
         [perm-order perm-dna-seq])))))

(defn dna-seq-perspectives
  ([dna-seq] (dna-seq-perspectives dna-seq {}))
  ([dna-seq {:keys [limit?] :or {limit? true}}]
   (let [dim (dna-dimension dna-seq)]
     (if (and limit? (> dim 6))
       ;; fast-ish for up to 5 dimensions, have not tested beyond 6.
       (throw (ex-info "Aborted: operation too expensive for dimensions greater than 6. Set `:limit?` to false to proceed, but be aware that the combinatorial space explodes quickly!" {:input dna-seq}))
       (let [dna-vec (vec dna-seq)]
         (map (partial permute-dna-seq dna-vec)
              (combo/permutations (range dim))))))))

;; ? redundant
(defn permute-dna
  ([dna perm-order] (permute-dna dna perm-order {}))
  ([dna perm-order opts]
   (permute-dna-seq dna perm-order opts)))

;; ? redundant
(defn dna-perspectives
  ([dna] (dna-perspectives dna {}))
  ([dna opts]
   (into {}
         (map #(let [[order dna-psp] %] [order dna-psp])
              (dna-seq-perspectives dna opts)))))


(def equal-dna
  "Equality check for formDNA. Two formDNAs are considered equal, if they contain the same constants in the same order. Stricter than `equiv-dna`, where permutations are considered equal."
  =)

(defn equiv-dna
  "Equivalence check for formDNA. Two formDNAs are considered equivalent, if they belong to the same equivalence-class of `dna-perspectives` (i.e. if they are permutations of each other)."
  ([_] true)
  ([a b] (let [->psps (comp dna-perspectives second reduce-dna-seq)
               ->set  (comp set vals)
               a-psps (-> a ->psps ->set)
               b-psps (-> b ->psps ->set)]
           (= a-psps b-psps)))
  ([a b & more]
   (if (equiv-dna a b)
     (if (next more)
       (recur b (first more) (next more))
       (equiv-dna b (first more)))
     false)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value structures

;;-------------------------------------------------------------------------
;; `vpoint` -> value point -> vector of `const`-coordinates in a `vspace`

(s/def :formform.specs.calc/vpoint
  (s/every :formform.specs.calc/const?
           :kind sequential?))

(def vpoint? (partial s/valid? :formform.specs.calc/vpoint))

(defn rand-vpoint
  "Generates a random vpoint either as an infinite lazy seq or with given dimension `dim`."
  ([]    (repeatedly #(rand-nth nuim-code)))
  ([dim] (repeatedly dim #(rand-nth nuim-code))))

;;-------------------------------------------------------------------------
;; `vspace` -> value space -> vector of all `n`-dimensional `vpoint`s

(s/def :formform.specs.calc/vspace
  (s/and (s/coll-of :formform.specs.calc/vpoint
                    :min-count 1
                    :distinct true)
         (fn [vspc] (let [vs-dim (dna-dimension (seq vspc))
                          vp-dim (count (first vspc))]
                      (and (some? vs-dim)
                           (== vs-dim vp-dim)
                           (every? #(== vp-dim (count %)) vspc))))))
;; ? add spec ordered-vspace

(def vspace? (partial s/valid? :formform.specs.calc/vspace))

(defn vspace
  "Generates a vspace of dimension `dim`, optionally with custom `sort-code`.
  - returns a lazy-seq which may be too memory-expensive to fully realize for dimensions greater than 11 (> 200 Mio. elements in total!)"
  ([dim] (vspace dim nuim-code))
  ([dim sort-code]
   (let [vs sort-code]
     (apply combo/cartesian-product (repeat dim sort-code)))))

;;-------------------------------------------------------------------------
;; `vdict` -> value dictionary -> (sorted) k-v map from `vspace` to `dna`
;; - for value table generation
;; - like a flat vmap

(s/def :formform.specs.calc/vdict
  (s/and map?
         #(s/valid? :formform.specs.calc/vspace (keys %))
         #(s/valid? :formform.specs.calc/dna    (vals %))))

(def vdict? (partial s/valid? :formform.specs.calc/vdict))

(defn vdict
  "Generates a vdict given a map from vpoint to result (constant).
  - if the corresponding vspace is not a subset of the set of keys from `vp->r`, the remaining results will be filled with :N or a given default constant
  - optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive"
  [vp->r {:keys [default-result sorted?]
          :or {default-result :N sorted? false}}]
  (let [dim   (count (ffirst vp->r))
        vspc  (vspace dim)
        def-r (if (const? default-result) default-result :N)]
    (->> (map #(let [r (vp->r %)]
                 (if (const? r)
                   [% r]
                   [% def-r])) vspc)
         (into (if sorted? (sorted-map-by compare-consts) (hash-map))))))

(defn dna->vdict
  "Generates a vdict from a given dna.
  - optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive"
  [dna {:keys [sorted? unsafe?] :or {sorted? false unsafe? false}}]
  (let [dim     (dna-dimension dna)
        _       (when (and (not unsafe?) (> dim 11))
                  (throw (ex-info "Aborted: operation may freeze for formDNA dimensions above 11. Set option `unsafe?` to true if you still want to proceed." {:dimension dim})))
        dna-rev (reverse-dna dna)
        vspc    (vspace dim)]
    (into (if sorted? (sorted-map-by compare-consts) (hash-map))
          (map vector vspc dna-rev))))

;;-------------------------------------------------------------------------
;; `vmap` -> value map -> mapping from `vspace` topology to `dna`

(s/def :formform.specs.calc/vmap
  (s/map-of :formform.specs.calc/const
            (s/or :vmap :formform.specs.calc/vmap
                  :res  :formform.specs.calc/const)
            :count 4))

(def vmap? (partial s/valid? :formform.specs.calc/vmap))

;; fast-ish with up to 10 dimensions
(defn vdict->vmap
  "Generates a vmap from a given vdict."
  ([vdict] (vdict->vmap nil vdict))
  ([fmap vdict]
   (let [vspc (keys vdict)
         dim  (count (first vspc))
         aux  (fn f [depth vspc]
                (if (< depth dim)
                  (let [group (group-by #(nth % depth) vspc)
                        vmap  (update-vals group #(f (inc depth) %))]
                    (if (nil? fmap)
                      vmap
                      (fmap vmap vspc depth dim)))
                  (vdict (first vspc))))]
     (with-meta (aux 0 vspc) {:dim dim}))))

;; ? can a custom algo directly from formDNA be more efficient here
(defn dna->vmap
  [dna]
  (vdict->vmap (dna->vdict dna {})))

; (vmap? (dna->vmap (rand-dna 1)))
; (vdict->vmap {[] :U})
; (vdict->vmap
;  (fn [vmap vspc depth dim] (assoc vmap :depth depth :vspace vspc))
;  (dna->vdict (rand-dna 2) {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formDNA arithmetic

(defn- relc [a b]
  (case a
    :M :M
    :N b
    (case b
      :M :M
      :N a
      (case [a b]
        [:U :U] :U
        [:I :I] :I
        ([:U :I] [:I :U]) :M))))

(defn rel
  "Relates the values of 2 constants in a formDNA to each other."
  ([]  :N)
  ([a] a)
  ([a b] (if (and (const? a) (const? b))
           (relc a b)
           (let [adim (dna-dimension a)
                 bdim (dna-dimension b)]
             (cond
               (= adim bdim) (mapv rel a b)
               (> adim bdim) (mapv rel a (expand-dna-seq b adim))
               :else         (mapv rel (expand-dna-seq a bdim) b)))))
  ([a b & xs] (rel a (reduce rel b xs))))
;; alias
(def -- rel)


(defn- invc [a]
  (case a
    :N :M
    :U :I
    :I :U
    :M :N))

(defn inv
  "Inverts the value of a every constant in a formDNA."
  ([]  :M)
  ([a] (if (const? a)
         (invc a)
         (mapv inv a)))
  ([a & xs] (inv (apply rel (cons a xs)))))
;; alias
(def | inv)


(comment
  ; (set! *print-length* 50)
  ; (require '[criterium.core :as crt])

  (sort compare-consts [[:N :U] [:I :M]])


  )

