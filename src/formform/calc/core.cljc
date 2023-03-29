;; ========================================================================
;;     formform calculation module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================

(ns formform.calc.core
  (:require [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [formform.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant

(def consts #{:N :U :I :M})
(def var-const :_)

(def const? (comp some? consts))

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
  [consts]
  (if (seq consts)
    (let [digits (map const->digit consts)]
      (apply str "4r" digits))
    (throw (ex-info "Must contain at least one element." {:arg consts}))))


(defn make-compare-consts
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
  [xs]
  (dna-length->dim (count xs)))


(defn rand-dna
  ([dim] (rand-dna dim nil))
  ([dim elems]
   (let [len    (apply * (repeat dim 4))
         gen-fn #(rand-nth (if (and (some? elems) (<= (count elems) 4))
                             elems
                             nuim-code))]
     (vec (repeatedly len gen-fn)))))


(declare dna->digits)

(def reverse-dna (comp rseq vec))

(defn reorder-dna-seq
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


;; ? almost useless to abstract these functions for a single use case:

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
  (prod=dna-seq->dna (fn [sort-code x] (digit->const x sort-code))))

(def chars->dna
  (prod=dna-seq->dna (fn [sort-code x] (char->const x sort-code))))

(def dna->digits
  (prod=dna->dna-seq (fn [sort-code x] (const->digit x sort-code))))


;; ? is this correct expansion for dim > 2 (see `rel`)
(defn expand-dna-seq
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

;; ? redundant
(defn permute-dna
  ([dna perm-order] (permute-dna dna perm-order {}))
  ([dna perm-order opts]
   (permute-dna-seq dna perm-order opts)))

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
(defn dna-perspectives
  ([dna] (dna-perspectives dna {}))
  ([dna opts]
   (into {}
         (map #(let [[order dna-psp] %] [order dna-psp])
              (dna-seq-perspectives dna opts)))))


(def equal-dna =)

(defn equiv-dna
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
;; vpoint

(defn rand-vpoint
  ([]    (repeatedly #(rand-nth nuim-code)))
  ([dim] (repeatedly dim #(rand-nth nuim-code))))

;;-------------------------------------------------------------------------
;; vspace

(defn vspace
  ([dim] (vspace dim nuim-code))
  ([dim sort-code]
   (let [vs sort-code]
     (apply combo/cartesian-product (repeat dim sort-code)))))

;;-------------------------------------------------------------------------
;; vdict

(defn vdict
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
  [dna {:keys [sorted? unsafe?] :or {sorted? false unsafe? false}}]
  (let [dim     (dna-dimension dna)
        _       (when (and (not unsafe?) (> dim 11))
                  (throw (ex-info "Aborted: operation may freeze for formDNA dimensions above 11. Set option `unsafe?` to true if you still want to proceed." {:dimension dim})))
        dna-rev (reverse-dna dna)
        vspc    (vspace dim)]
    (into (if sorted? (sorted-map-by compare-consts) (hash-map))
          (map vector vspc dna-rev))))

;;-------------------------------------------------------------------------
;; vmap

;; fast-ish with up to 10 dimensions
(defn vdict->vmap
  ([vdict] (vdict->vmap nil vdict))
  ([fmap vdict]
   (if (== 1 (count vdict))
     (vdict '())
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
       (with-meta (aux 0 vspc) {:dim dim})))))

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


(defn- invc [a]
  (case a
    :N :M
    :U :I
    :I :U
    :M :N))

(defn inv
  ([]  :M)
  ([a] (if (const? a)
         (invc a)
         (mapv inv a)))
  ([a & xs] (inv (apply rel (cons a xs)))))


(comment
  ; (set! *print-length* 50)
  ; (require '[criterium.core :as crt])

  )
