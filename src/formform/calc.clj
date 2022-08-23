(ns formform.calc
  (:require [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [formform.utils :as utils]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant
;; -> element of formDNA representing a primitive FORM value
;; -> single-digit formDNA


(s/def :formform.specs.calc/const
  (s/with-gen
    #(case %
       (:N :U :I :M) true
       false)
    #(gen/elements [:N :U :I :M])))

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

; (defn const?
;   [x] (case x
;         (:N :U :I :M) true
;         false))

; (defn sort-code?
;   [x] (and
;         (vector? x)
;         (== 4 (count x))
;         (= #{:N :U :I :M} (set x))))

(def nuim-code [:N :U :I :M])
(def nmui-code [:N :M :U :I])

; (defn rand-const
;   "Generates a random constant. Given a natural number `nat` and an optional `sort-code`, returns the correpsonding `const`."
;   [] (rand-nth nuim-code))

(defn int->const
  ([n] (int->const n nuim-code))
  ([n sort-code]
   (sort-code n)))

(defn const->int
  ([c] (const->int c nuim-code))
  ([c sort-code]
   ((zipmap sort-code (range)) c)))


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

(defn dna-seq-dim
  "Calculates the dimension of a `dna-seq`. The length of `dna-seq` is 4^d for its dimension d."
  [dna-seq]
  ; {:pre  [(or (sequential? dna-seq) (string? dna-seq))]
  ;  :post [(or (nil? %) (int? %))]}
  (let [len (count dna-seq)
        dim (/ (math/log len)
               (math/log 4.0))]
    (if (or (infinite? dim) (utils/has-decimal? dim))
      nil
      (int dim))))

(s/def :formform.specs.calc/dna-seq
  (s/and
    (s/nonconforming (s/or :sequential? sequential? :string? string?))
    #(<= (count (distinct %)) 4)
    dna-seq-dim))

(defn dna-seq?
  "True if `x` is a `dna-seq`: must be a `seqable?` of no more than 4 distinct elements and must have a `dna-seq-dim`.
  - can be given an optional set/collection of no more than 4 specific elements that `x` should consist of"
  ([x] (dna-seq? x nil))
  ([x elems]
   (and
     (s/valid? :formform.specs.calc/dna-seq x)
     (if (coll? elems)
       (if-let [elem-set (set elems)]
         (and
           (<= 4 (count elem-set))
           (every? (partial contains? elem-set) x))
         false)
       true))))

; (defn dna-seq?
;   "True if `x` is a `dna-seq`: must be a `seqable?` of no more than 4 distinct elements and must have a `dna-seq-dim`.
;   - can be given an optional set/collection of no more than 4 specific elements that `x` should consist of"
;   ([x] (dna-seq? x nil))
;   ([x elems]
;    (and
;      (seqable? x)
;      (some? (dna-seq-dim x))
;      (if (coll? elems)
;        (if-let [elem-set (set elems)]
;          (and
;            (<= 4 (count elem-set))
;            (every? (partial contains? elem-set) x))
;          false)
;        (<= (count (distinct x)) 4)))))

(defn rand-dna-seq
  "Generates a random `dna-seq?` of `elems` (defaults to digits 0-3) with dimension `dim`."
  ([dim] (rand-dna-seq dim nil))
  ([dim elems]
   ; {:pre  [(int? dim) (>= dim 0)]
   ;  :post [(dna-seq? %)]}
   (let [len    (apply * (repeat dim 4))
         gen-fn (if (and (some? elems) (<= (count elems) 4))
                  #(rand-nth elems)
                  #(rand-int 4))]
     (repeatedly len gen-fn))))

(defn dna-dim
  "Calculates the dimension of a formDNA (corresponds to the number of variables in a FORM). The length of `dna` is 4^d for its dimension d."
  [dna]
  {:pre [(keyword? dna)]}
  (dna-seq-dim (name dna)))

(defn dna?
  [x]
  (and (keyword? x)
    (some? (dna-dim x))
    (every? #{\N \U \I \M} (name x))))

(defn rand-dna
  "Generates a random formDNA of dimension `dim`."
  ([dim]
   {:pre  [(int? dim) (>= dim 0)]
    :post [(dna? %)]}
   (->> (rand-dna-seq dim nuim-code)
        (map name)
        (#(apply str %))
        keyword)))

(declare dna->digits)

(defn dna->quaternary
  "Converts formDNA to its corresponding quaternary number (as a string, prefixed by '4r').
  - use `read-string` to obtain the decimal value as a BigInt"
  [dna]
  {:pre  [(dna? dna)]
   :post [(string? %)]}
  (let [digits (dna->digits dna)]
    (apply str "4r" digits)))

;; ? can this be generalized to dna-seq?
(defn make-compare-dna
  "Given a `sort-code` (try `calc.nuim-code` or `calc.nmui-code`), returns a comparator function to sort formDNA.
  - compares simple and composite constants
  - compares collections of formDNA (not recursively!)"
  [sort-code]
  {:pre  [(sort-code? sort-code)]}
  (fn [a b]
    (let [sort-map  (zipmap sort-code (range))
          comp-dna? #(and (dna? %) (> (dna-dim %) 0))
          convert   (fn [x] (if (comp-dna? x)
                              ;; ! not interoperable for cljs will not
                              ;;   parse BigInt here
                              ;;   -> maybe use interop with js/BigInt
                              ;;   or a different approach
                              (read-string (dna->quaternary x))
                              (sort-map x)))]
      (cond
        (and (coll? a) (coll? b))
        (let [ns-a (mapv convert a)
              ns-b (mapv convert b)]
          (compare ns-a ns-b))

        (and ((complement coll?) a) ((complement coll?) b))
        (compare (convert a) (convert b))

        :else (throw (ex-info "Cannot compare: " {:a a :b b}))))))

;; convenience function
(def compare-dna (make-compare-dna nuim-code))

(defn dna->consts
  [dna]
  {:pre [(dna? dna)]}
  (->> (name dna)
       (map (comp keyword str))))

(defn consts->dna
  [cs]
  (keyword
    (apply str (map name cs))))

(defn reorder-dna-seq
  "Reorders a `dna-seq` from `sort-code-from` to `sort-code-to`.
  
  Note:
  - `dna-seq` can have any type of elements (not only constants)
  - does NOT change the encoding of the elements, just their ordering"
  [dna-seq sort-code-from sort-code-to]
  (let [sort-idxs (->> (range 3 -1 -1)
                       (zipmap sort-code-from)
                       (sort (make-compare-dna sort-code-to))
                       reverse
                       (map second))
        aux (fn reorder [dna-subseq]
              (let [len      (count dna-subseq)
                    part-len (/ len 4)]
                (if (< len 4)
                  dna-subseq
                  (apply concat
                    (map (fn [i]
                           (let [index   (* i part-len)
                                 cs-part (take part-len
                                           (drop index dna-subseq))]
                             (reorder cs-part)))
                      sort-idxs)))))]
    (aux dna-seq)))

(defn expand-dna-seq
  "Expands a `dna-seq` to a given target dimension by repeating elements.
  
  Note: `dna-seq` can have any type of elements (not only constants)"
  ([dna-seq ext-dim]
   (let [dim (int (/ (math/log (count dna-seq))
                     (math/log 4.0)))]
     (expand-dna-seq dna-seq dim ext-dim)))
  ([dna-seq dim ext-dim]
   (reduce
     (fn [seq-expanded x]
       (concat
         seq-expanded
         (repeat (utils/pow-nat 4 (- ext-dim dim)) x)))
     '()
     dna-seq)))

(defn dna->digits
  "Converts formDNA to a string of digits.
  
  Note that `nuim-code` is the default ordering."
  ([dna] (dna->digits dna nuim-code))
  ([dna sort-code]
   (let [dna-seq (map #(-> ((comp keyword str) %)
                           (const->int sort-code)) (name dna))]
     (if (= sort-code nuim-code)
       dna-seq
       (reorder-dna-seq dna-seq nuim-code sort-code)))))

(defn digits->dna
  "Converts a `seqable?` of digits to formDNA.
  
  Note that `sort-code` will only affect digit interpretation, not ordering."
  ([digits] (digits->dna digits nuim-code))
  ([digits sort-code]
   (let [dna-seq (if (= sort-code nuim-code)
                   digits
                   (reorder-dna-seq digits sort-code nuim-code))]
     (->> dna-seq
          (map #(name (int->const % sort-code)))
          (apply str)
          keyword))))

;; TODO
(defn filter-dna-seq
  [dna-seq vpoint]
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formDNA perspectives

(defn permute-dna-seq
  ([dna-seq perm-order] (permute-dna-seq dna-seq perm-order {}))
  ([dna-seq perm-order {:keys [limit?] :or {limit? true}}]
   (let [dim     (dna-seq-dim dna-seq)
         dna-vec (vec dna-seq)]
     (cond
       (not= dim (count perm-order)) nil
       (< dim 2)                     [perm-order dna-seq]
       (= perm-order (range dim))    [perm-order dna-seq]
       ;; fast-ish for up to 10 dimensions, have not tested beyond 12
       (and limit? (> dim 12))       (throw (ex-info "Aborted: operation too expensive for dimensions greater than 12. Set `:limit?` to false to proceed." {:input [dna-seq perm-order]}))
       :else
       (let [int->quat-str (fn [n] (utils/pad-left (utils/int->nbase n 4)
                                                   dim "0"))
             perm-dna-seq
             (map (fn [i]
                    (let [qtn-key (mapv (comp read-string str)
                                        (int->quat-str i))
                          perm-key (apply str "4r"
                                          (map #(qtn-key (perm-order %))
                                               (range dim)))
                          i-perm (read-string perm-key)]
                      (dna-vec i-perm)))
                  (range (count dna-seq)))]
         [perm-order perm-dna-seq])))))

(defn dna-seq-perspectives
  ([dna-seq] (dna-seq-perspectives dna-seq {}))
  ([dna-seq {:keys [limit?] :or {limit? true}}]
   (let [dim (dna-seq-dim dna-seq)]
     (if (and limit? (> dim 6))
       ;; fast-ish for up to 5 dimensions, have not tested beyond 6.
       (throw (ex-info "Aborted: operation too expensive for dimensions greater than 6. Set `:limit?` to false to proceed, but be aware that the combinatorial space explodes quickly!" {:input dna-seq}))
       (let [dna-vec (vec dna-seq)]
         (map (partial permute-dna-seq dna-vec)
              (combo/permutations (range dim))))))))

(defn permute-dna
  ([dna perm-order] (permute-dna dna perm-order {}))
  ([dna perm-order opts]
   (consts->dna
     (permute-dna-seq (dna->consts dna) perm-order opts))))

(defn dna-perspectives
  ([dna] (dna-perspectives dna {}))
  ([dna opts]
   (into {}
         (map #(let [[order cs] %] [order (consts->dna cs)])
              (dna-seq-perspectives (dna->consts dna) opts)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value structures

;;-------------------------------------------------------------------------
;; `vpoint` -> value point -> vector of `const`-coordinates in a `vspace`

(defn vpoint?
  [x]
  (every? (set nuim-code) x))

(defn rand-vpoint
  "Generates a random vpoint either as a lazy seq or with given dimension `dim`."
  ([]    (repeatedly #(rand-nth nuim-code)))
  ([dim] (repeatedly dim #(rand-nth nuim-code))))

;;-------------------------------------------------------------------------
;; `vspace` -> value space -> vector of all `n`-dimensional `vpoint`s

(defn vspace?
  [x]
  (and (dna-seq-dim x) (every? vpoint? x)))

(defn vspace 
  "Generates a vspace of dimension `dim`, optionally with custom `sort-code`."
  ([dim] (vspace dim nuim-code))
  ([dim sort-code]
   (let [vs sort-code]
     (apply combo/cartesian-product (repeat dim sort-code)))))

;;-------------------------------------------------------------------------
;; `vdict` -> value dictionary -> (sorted) k-v map from `vspace` to `dna`
;; - for value table generation
;; - like a flat vmap

(defn vdict?
  [x]
  (and (map? x) (vspace? (keys x)) (dna-seq? (vals x))))

(defn vdict
  "Generates a vdict given a map from vpoint to result (constant).
  - if the corresponding vspace is not a subset of the set of keys from `vp->r, the remaining results will be filled with :N or a given default constant.`
  - optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive."
  [vp->r {:keys [default-result sorted?]
          :or {default-result :N sorted? false}}]
  (let [dim   (count (ffirst vp->r))
        vspc  (vspace dim)
        def-r (if (const? default-result) default-result :N)]
    (->> (map #(let [r (vp->r %)]
                 (if (const? r)
                   [% r]
                   [% def-r])) vspc)
         (into (if sorted? (sorted-map-by compare-dna) (hash-map))))))

(defn dna->vdict
  "Generates a vdict from a given dna.
  - optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive."
  [dna {:keys [sorted?] :or {sorted? false}}]
  (let [dna-seq (reverse (dna->consts dna))
        vspc    (vspace  (dna-seq-dim dna-seq))]
    (into (if sorted? (sorted-map-by compare-dna) (hash-map))
      (map vector vspc dna-seq))))

;;-------------------------------------------------------------------------
;; `vmap` -> value map -> mapping from `vspace` topology to `dna`

;; ? should this really be a map? if yes, must it be sorted?

(defn vmap?
  ;; ! insufficient -> checks only root form
  [x]
  (and (map? x) (== (count x) 4)
    (every? const? (keys x))
    (every? map? (vals x))))


;; fast with up to 9 dimensions
(defn vdict->vmap
  "Generates a vmap from a given vdict."
  [vdict]
  (let [vspc (keys vdict)
        dim  (count (first vspc))
        aux  (fn f [i vspc]
               (if (< i dim)
                 (let [group (group-by #(nth % i) vspc)]
                   (update-vals group
                     #(f (inc i) %)))
                 (vdict (first vspc))))]
    (aux 0 vspc)))


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
  ([] :N)
  ([a] a)
  ([a b] (if (and (const? a) (const? b))
           (relc a b)
           (let [acs  (dna->consts a)
                 bcs  (dna->consts b)
                 adim (dna-dim a)
                 bdim (dna-dim b)]
             (consts->dna
               (cond
                 (= adim bdim) (map rel acs bcs)
                 (> adim bdim) (map rel acs (expand-dna-seq bcs adim))
                 :else     (map rel (expand-dna-seq acs bdim) bcs))))))
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
  ([] :M)
  ([a] (if (const? a)
         (invc a)
         (consts->dna (map inv (dna->consts a)))))
  ([a & xs] (inv (apply rel (cons a xs)))))
;; alias
(def | inv)





