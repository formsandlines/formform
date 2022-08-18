(ns formform.calc
  (:require [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [formform.utils :as utils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant
;; -> element of formDNA representing a primitive FORM value
;; -> single-digit formDNA

(def nuim-code [:N :U :I :M])
(def nmui-code [:N :M :U :I])

(defn rand-const
  "Generates a random constant. Given a natural number `nat` and an optional `sort-code`, returns the correpsonding `const`."
  [] (rand-nth nuim-code))

(defn const?
  [x] (case x
        (:N :U :I :M) true
        false))

(defn int->const
  ([n] (int->const n nuim-code))
  ([n sort-code] (sort-code n)))

(defn const->int
  ([c] (const->int c nuim-code))
  ([c sort-code] ((zipmap sort-code (range)) c)))


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
  (let [len (count dna-seq)
        dim (/ (math/log len)
               (math/log 4.0))]
    (if (utils/has-decimal? dim)
      nil
      (int dim))))

(defn dna-seq?
  "True if `x` is a `dna-seq`: must be a `seqable?` of no more than 4 distinct elements and must have a `dna-seq-dim`.
  - can be given an optional set/collection of no more than 4 specific elements that `x` should consist of"
  ([x] (dna-seq? x nil))
  ([x elems]
   (and
     (seqable? x)
     (some? (dna-seq-dim x))
     (if (coll? elems)
       (if-let [elem-set (set elems)]
         (and
           (<= 4 (count elem-set))
           (every? elem-set x))
         false)
       (<= (count (distinct x)) 4)))))

(defn rand-dna-seq
  "Generates a random `dna-seq?` of `elems` (defaults to digits 0-3) with dimension `dim`."
  ([dim] (rand-dna-seq dim nil))
  ([dim elems]
   (let [len    (apply * (repeat dim 4))
         gen-fn (if (and (some? elems) (<= (count elems) 4))
                  #(rand-nth elems)
                  #(rand-int 4))]
     (repeatedly len gen-fn))))

(defn rand-dna
  "Generates a random formDNA of dimension `dim`."
  ([dim] (->> (rand-dna-seq dim nuim-code)
              (map name)
              (#(apply str %))
              keyword)))

(defn dna-dim
  "Calculates the dimension of a formDNA (corresponds to the number of variables in a FORM). The length of `dna` is 4^d for its dimension d."
  [dna]
  (dna-seq-dim (name dna)))

(defn dna?
  [x]
  (and (keyword? x)
    (some? (dna-dim x))
    (every? #{\N \U \I \M} (name x))))

(declare dna->digits)

(defn dna->quaternary
  "Converts formDNA to its corresponding quaternary number (as a string, prefixed by '4r').
  - use `read-string` to obtain the decimal value as a BigInt"
  [dna]
  (let [digits (dna->digits dna)]
    (apply str "4r" digits)))

;; ? can this be generalized to dna-seq?
(defn make-compare-dna
  "Given a `sort-code` (try `calc.nuim-code` or `calc.nmui-code`), returns a comparator function to sort formDNA.
  - compares simple and composite constants
  - compares collections of formDNA (not recursively!)"
  [sort-code]
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

(defn rand-vpoint
  "Generates a random vpoint either as a lazy seq or with given dimension `dim`."
  ([]    (repeatedly #(rand-nth nuim-code)))
  ([dim] (repeatedly dim #(rand-nth nuim-code))))

;;-------------------------------------------------------------------------
;; `vspace` -> value space -> vector of all `n`-dimensional `vpoint`s

(defn vspace 
  "Generates a vspace of dimension `dim`, optionally with custom `sort-code`."
  ([dim] (vspace dim nuim-code))
  ([dim sort-code]
   (let [vs sort-code]
     (apply combo/cartesian-product (repeat dim sort-code)))))

;;-------------------------------------------------------------------------
;; `vdict` -> value dictionary -> sorted k-v map from `vspace` to `dna`
;; - for value table generation
;; - like a flat vmap

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



(comment
  (rand-vpoint 5)
  (vspace 2 nmui-code)

  (rand-dna 3)

  (rand-const)

  (const->int :M)
  (const->int :M nmui-code)
  (int->const 3)
  (int->const 3 nmui-code)

  (--)
  (-- :I)
  (-- :U :I)
  (-- :U :I :U :N)

  (|)
  (| :I)
  (| :U :I)
  (| :U :I :U :N)

  (-- :U (| (-- (| :N) (| :I))))

  (= :NMUIMUUMMIIIMMMM (rel :NMUIIUNMMIIIUUMN :NUIM :NUIM))
  (= :MIUN (inv :NUIM))

  (dna->consts :NUIM)
  (consts->dna [:N :U :I :M])

  (const? [])

  (dna? :NUIM)
  (dna? (rand-dna 10)) ; 16 mio. digits!

  (dna->digits :NUIM)
  (dna->digits :NMUI nmui-code)
  (digits->dna [0 1 2 3])
  (digits->dna [0 1 2 3] nmui-code)

  (rand-dna-seq 3 nuim-code)
  (rand-dna-seq 3 [:N :M])
  (rand-dna 3)

  (rel :NMUI :MNIN)

  (dna-dim :MMMMIIIIUUUUNNNN)
  (dna? :MMMMIIIIUUUUNNNN)
  (dna-seq? [1 0 3 2
             3 2 0 2
             1 2 0 1
             0 0 1 2])

  (consts->dna
    (reorder-dna-seq
      (dna->consts :MMMMIIIIUUUUNNNN)
      nuim-code nmui-code))
  ;=> :IIIIUUUUMMMMNNNN

  (consts->dna
    (reorder-dna-seq
      (dna->consts :IIIIUUUUMMMMNNNN)
      nmui-code nuim-code))
  ;=> :MMMMIIIIUUUUNNNN

  (def d1 (digits->dna (map (comp read-string str) "2301200223012002030323012301030303032002230100002301230123012301") nmui-code))
  (def d2 (digits->dna (map (comp read-string str) "1032012332102301") nmui-code))
  (def r (rel d1 d2))
  (apply str (dna->digits r nmui-code))

  (read-string "4r2301200223012002030323012301030303032002230100002301230123012301")

  (dna->quaternary :NUIM)
  (dna->quaternary :NMUI)
  (dna->quaternary (rand-dna 7))
  (read-string (dna->quaternary (rand-dna 7)))
  (dna->quaternary :IM)

  (sort compare-dna [:NMUI :NUIM :M])
  (sort compare-dna [[:NUIM :IM] [:UMI :MNU]])
  (sort compare-dna [(sort compare-dna [:NUIM :IM])
                     (sort compare-dna [:UMI :MNU])])

  (expand-dna-seq (dna->consts (rand-dna 1)) 2)


  (let [dna-seq (dna->consts :MMMMIIIIUUUUNNNN)
        perm-order [1 0]]
    (permute-dna-seq dna-seq perm-order {}))

  (dna-seq-perspectives (dna->consts :MMMMIIIIUUUUNNNN) {})

  (dna-perspectives :MMMMIIIIUUUUNNNNMMMMIIIIUUUUNNNNMMMMIIIIUUUUNNNNMMMMIIIIUUUUNNNN {})

  (dna-perspectives (rand-dna 6) {})
  (dna-seq-perspectives (rand-dna-seq 7) {})

  (permute-dna-seq (rand-dna-seq 8) [1 0 2 3 4 5 6 7] {})
  (permute-dna (rand-dna 8) [1 0 2 3 4 5 6 7] {})

  (permute-dna-seq (rand-dna-seq 10) [1 0 2 3 4 5 6 7 8 9] {})

  )

(comment

  (let [vp->r {[:N :M] :M
               [:U :U] :I
               [:X :Y] :M
               [:U :U :I] :N}]
    (vdict vp->r {:default-result :U}))

  (dna->vdict (rand-dna 4) {:sorted? true})

  (vdict->vmap (dna->vdict (rand-dna 8) {}))

  )



