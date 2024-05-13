;; ========================================================================
;;     formform calculation module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.calc.core
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
  ([n] (digit->const nuim-code n))
  ([sort-code n]
   (if (== n -1)
     var-const ;; “hole” or variable value
     (sort-code n))))

(defn char->const
  ([c] (char->const nuim-code c))
  ([sort-code c]
   (case c
     (\n \N) :N
     (\u \U) :U
     (\i \I) :I
     (\m \M) :M
     \_      var-const
     (when-let [n (edn/read-string (str c))]
       (digit->const sort-code n)))))

(defn const->digit
  ([c] (const->digit nuim-code c))
  ([sort-code c]
   {:pre [(consts c)]}
   ((zipmap sort-code (range)) c)))

(defn const?->digit
  ([c] (const?->digit nuim-code c))
  ([sort-code c]
   (if (= c var-const)
     -1 ;; “hole” or variable value
     (const->digit sort-code c))))

(defn consts->quaternary
  "Converts a sequence of constants to a corresponding quaternary number (as a string, prefixed by '4r'). Used for comparison.

  * use `read-string` to obtain the decimal value as a BigInt"
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
(def ^:private max-safe-dna-dim 25)

(def ^:private max-safe-dna-len
  (last (take max-safe-dna-dim dna-lengths)))

(def ^:private cached_dna-length->dim
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


(def reverse-dna
  "Reverses a formDNA (returns an rseq)

  * make sure the input is a vector for constant-time reverse"
  (comp rseq vec))

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
    ([dna-seq] (f nuim-code dna-seq))
    ([sort-code dna-seq]
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
    ([dna] (f nuim-code dna))
    ([sort-code dna]
     (let [dna-seq (mapv (partial sort+const->x sort-code) dna)]
       (if (= sort-code nuim-code)
         dna-seq
         (reorder-dna-seq dna-seq nuim-code sort-code))))))

(def digits->dna
  (prod=dna-seq->dna (fn [sort-code x] (digit->const sort-code x))))

(def chars->dna
  (prod=dna-seq->dna (fn [sort-code x] (char->const sort-code x))))

(def dna->digits
  (prod=dna->dna-seq (fn [sort-code x] (const?->digit sort-code x))))


;; ? is this correct expansion for dim > 2 (see `rel`)
(defn expand-dna-seq
  [dna-seq dim ext-dim]
  (reduce
    (fn [seq-expanded x]
      (into seq-expanded
            (repeat (utils/pow-nat 4 (- ext-dim dim)) x)))
    []
    dna-seq))

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

(defn rand-dna
  ([dim] (rand-dna dim nil))
  ([dim elems]
   (let [len    (apply * (repeat dim 4))
         gen-fn #(rand-nth (if (and (some? elems) (<= (count elems) 4))
                             elems
                             nuim-code))]
     (vec (repeatedly len gen-fn)))))


(defn filter-dna-seq
  [dna-seq depth-selections]
  (let [dim (dna-dimension dna-seq)]
    (if (== dim (count depth-selections))
      (if (every? nat-int? depth-selections)
        ;; for fully determined selections, use an efficient quaternary index
        ((comp vector (vec dna-seq))
         (- (count dna-seq) 1 (edn/read-string
                               (apply str "4r" depth-selections))))
        ;; otherwise, we need more machinery
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
               (filterv (fn [[i _]] (idxs (- n i))))
               (mapv second))))
      (throw
       (ex-info "Size of selection vector must be equal to dna-seq dimension!"
                {:expected dim :actual (count depth-selections)})))))

(defn filter-dna
  [dna vpoint]
  (filter-dna-seq dna (map const?->digit vpoint)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formDNA perspectives

(defn permute-dna-seq
  [{:keys [limit?] :or {limit? true}} dna-seq perm-order]
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
            ; perm-dna-seq
            ; (mapv (fn [i]
            ;         (let [qtn-key  (mapv (comp edn/read-string str)
            ;                              (int->quat-str i))
            ;               perm-key (apply str "4r"
            ;                               (map #(qtn-key (perm-order %))
            ;                                    dim-ns)) ;; ? just map perm-order
            ;               i-perm   (edn/read-string perm-key)]
            ;           (dna-vec i-perm)))
            ;       (range (count dna-seq)))
            ;; ? make this platform-agnostic
            perm-dna-arr #?(:clj  (make-array clojure.lang.Keyword
                                              (count dna-seq))
                            :cljs (js/Array. (count dna-seq)))]
        (dotimes [i (count dna-seq)]
          (let [qtn-key  (mapv (comp edn/read-string str)
                               (int->quat-str i))
                perm-key (apply str "4r"
                                (map #(qtn-key (perm-order %))
                                     dim-ns)) ;; ? just map perm-order
                i-perm   (edn/read-string perm-key)]
            (aset perm-dna-arr i-perm (dna-vec i))))
        [perm-order (vec perm-dna-arr)]))))

(comment
  (def int->quat-str (fn [n] (utils/pad-left (utils/int->nbase n 4)
                                             3 "0")))
  (let [perm [1 2 0]
        qtns (map #(mapv (comp edn/read-string str) (int->quat-str %)) 
                  (range (utils/pow-nat 4 3)))]
    (map #(vector % (mapv % perm)) qtns))

  (mapv (comp edn/read-string str)
        (int->quat-str 24))

  (dotimes [i 10]
    (println i))
  )

(defn dna-seq-perspectives
  [{:keys [limit?] :or {limit? true}} dna-seq]
  (let [dim (dna-dimension dna-seq)]
    (if (and limit? (> dim 6))
       ;; fast-ish for up to 5 dimensions, have not tested beyond 6.
      (throw (ex-info "Aborted: operation too expensive for dimensions greater than 6. Set `:limit?` to false to proceed, but be aware that the combinatorial space explodes quickly!" {:input dna-seq}))
      (let [dna-vec (vec dna-seq)]
        (map (partial permute-dna-seq {} dna-vec)
             (combo/permutations (range dim)))))))


(def equal-dna =)

(defn equiv-dna
  ([_] true)
  ([a b] (let [->psps (comp (partial dna-seq-perspectives {})
                            second reduce-dna-seq)
               ->set  (comp set (partial map second))
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

(defn vspace
  ([dim] (vspace nuim-code dim))
  ([sort-code dim]
   (let [vs sort-code]
     (apply combo/cartesian-product (repeat dim sort-code)))))


(defn vdict
  [{:keys [default-result sorted?] :or {default-result :N sorted? false}}
   vp->r]
  (let [dim   (count (ffirst vp->r))
        vspc  (vspace dim)
        def-r (if (const? default-result) default-result :N)]
    (->> (map #(let [r (vp->r %)]
                 (if (const? r)
                   [% r]
                   [% def-r])) vspc)
         (into (if sorted? (sorted-map-by compare-consts) (hash-map))))))

(defn dna->vdict
  [{:keys [sorted? unsafe?] :or {sorted? false unsafe? false}}
   dna]
  (let [dim     (dna-dimension dna)
        _       (when (and (not unsafe?) (> dim 11))
                  (throw (ex-info "Aborted: operation may freeze for formDNA dimensions above 11. Set option `unsafe?` to true if you still want to proceed." {:dimension dim})))
        dna-rev (reverse-dna dna)
        vspc    (vspace dim)]
    (into (if sorted? (sorted-map-by compare-consts) (hash-map))
          (map vector vspc dna-rev))))


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
  (vdict->vmap nil (dna->vdict {} dna)))

(defn vmap-dimension
  [vmap]
  (loop [vmap   vmap
         cached (:dim (meta vmap))
         dim    0]
    (cond
      (some? cached)  cached
      (keyword? vmap) dim
      :else           (let [vmap-part (:N vmap)]
                        (recur vmap-part
                               (:dim (meta vmap-part))
                               (inc dim))))))


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
               (> adim bdim) (mapv rel a (expand-dna-seq b bdim adim))
               :else         (mapv rel (expand-dna-seq a adim bdim) b)))))
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


(comment)
  
  (inv :M)
      
  ; (set! *print-length* 50)
  ; (require '[criterium.core :as crt])

  

