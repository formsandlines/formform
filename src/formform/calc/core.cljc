;; ========================================================================
;;     formform calculation module
;;     -- created 08/2022, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.calc.core
  (:require [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [formform.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant

(def consts #{:n :u :i :m})
(def consts_ #{:n :u :i :m :_})
(def val-hole :_)

(def const? (comp some? consts))
(def const_? (comp some? consts_))

;; predefined sort-codes
(def nuim-code [:n :u :i :m])
(def nmui-code [:n :m :u :i])

(defn conform-nuim-weights
  [w]
  (cond
    (number? w)
    (if (<= 0.0 w 1.0)
      [(- 1.0 w) w w w]
      (throw (ex-info "Single weight must be between 0.0 and 1.0."
                      {:weight w})))

    (sequential? w)
    (if (= (count w) 4)
      w
      (throw (ex-info "Must be exactly 4 weights: [n u i m]"
                      {:weights w})))

    (and (map? w) (seq w))
    (if (every? consts (keys w))
      (mapv #(get w % 0) nuim-code)
      (throw (ex-info "Keys for map weights must be in `#{:n :u :i :m}.`"
                      {:weights w})))

    :else (throw (ex-info "Invalid weights format."
                          {:weights w}))))

(defn rand-const
  ([rng] (rand-const rng nil))
  ([rng weights]
   (if weights
     (let [weights (conform-nuim-weights weights)]
       (utils/rng-select rng nuim-code weights))
     (utils/rng-select rng nuim-code))))

(defn rand-consts
  ([rng n] (rand-consts rng n nil))
  ([rng n weights]
   (if weights
     (let [weights (conform-nuim-weights weights)]
       (vec (utils/rng-select-n rng nuim-code n weights)))
     (vec (utils/rng-select-n rng nuim-code n)))))


(defn digit->const
  ([n] (digit->const nuim-code n))
  ([sort-code n]
   (if (== n -1)
     val-hole
     (sort-code n))))

(defn char->const
  ([c] (char->const nuim-code c))
  ([sort-code c]
   (case c
     (\n \N) :n
     (\u \U) :u
     (\i \I) :i
     (\m \M) :m
     \_      val-hole
     (when-let [n (utils/parse-int-maybe (str c))]
       (digit->const sort-code n)))))

(defn const->digit
  ([c] (const->digit nuim-code c))
  ([sort-code c]
   ;; {:pre [(consts c)]}
   (condp = sort-code
     nuim-code (case c :n 0 :u 1 :i 2 :m 3)
     nmui-code (case c :n 0 :m 1 :u 2 :i 3)
     ((zipmap sort-code (range)) c))))

(defn const_->digit
  ([c] (const_->digit nuim-code c))
  ([sort-code c]
   (if (= c val-hole)
     -1 ;; int representation of `:_`
     (const->digit sort-code c))))

(defn consts->quaternary
  "Converts a sequence of constants to a corresponding quaternary number (as a string). Keep in mind that the order (by default) is reversed, because place values in numbers are from right to left. To change this (e.g. for use in a comparator), set `rtl?` (“right-to-left”) to `false`.

  * use `read-string` and prepend `\"4r\"` to the number string to obtain the decimal value as a BigInt"
  ([consts]
   (consts->quaternary consts true))
  ([consts rtl?]
   (if (seq consts)
     (let [consts (if rtl? (reverse consts) consts)
           digits (mapv const->digit consts)]
       (apply str digits))
     (throw (ex-info "Must contain at least one element." {:arg consts})))))

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
                        (sequential? x) (utils/parse-int
                                         (consts->quaternary x false) 4)
                        (const? x) (sort-map x)
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

;; ? rewrite to use transduce instead of map+reduce and vectors instead of seqs
;;   maybe also subvec instead of take/drop
(defn reorder-dna-seq
  [dna-seq sort-code-from sort-code-to]
  (if (= sort-code-from sort-code-to)
    dna-seq
    (let [sort-idxs (->> [0 1 2 3] ;; indexes, not values!
                         (zipmap sort-code-from)
                         (sort (make-compare-consts sort-code-to))
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


(comment

  (reorder-dna-seq [:n :u :i :m  :u :i :m :n  :i :m :n :u  :m :n :u :i]
                   nuim-code nmui-code)
  [:n :m :u :i  :m :i :n :u  :u :n :i :m  :i :u :m :n]

  (let [sort-code-from nuim-code
        sort-code-to nmui-code]
    (->> [0 1 2 3] ;; indexes, not values!
         (zipmap sort-code-from)
         (sort (make-compare-consts sort-code-to))
         (map second)))

  ,)


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
  (prod=dna->dna-seq (fn [sort-code x] (const_->digit sort-code x))))


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
  ([dna-seq] (reduce-dna-seq dna-seq {}))
  ([dna-seq opts] (let [dim (dna-dimension dna-seq)]
                    (reduce-dna-seq dna-seq (vec (range dim)) opts)))
  ([dna-seq terms
    {:keys [assume-holes-equal?] :or {assume-holes-equal? false}}]
   (if (and (not assume-holes-equal?) ;; ? check in loop may be more efficient
            (some #(or (= :_ %) (= -1 %)) dna-seq))
     [terms dna-seq]
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
                    (dec dim) subdim))))))))

(comment

  (reduce-dna-seq [:n :_ :i :m  :n :_ :i :m  :n :_ :i :m  :n :_ :i :n
                   :n :_ :i :m  :n :_ :i :m  :n :_ :i :m  :n :_ :i :n
                   :n :_ :i :m  :n :_ :i :m  :n :_ :i :m  :n :_ :i :n
                   :n :_ :i :m  :n :_ :i :m  :n :_ :i :m  :n :_ :i :n]
                  {:assume-holes-equal? false})
  ,)

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
  ([rng dim] (rand-dna rng dim nil))
  ([rng dim weights]
   (let [len (apply * (repeat dim 4))]
     (if weights
       (let [weights (conform-nuim-weights weights)]
         (rand-consts rng len weights))
       (rand-consts rng len)))))

(defn rand-vpoint
  ([rng dim] (rand-vpoint rng dim nil))
  ([rng dim weights]
   (if weights
     (let [weights (conform-nuim-weights weights)]
       (rand-consts rng dim weights))
     (rand-consts rng dim))))

#_
(defn rand-dna
  ([rng dim] (rand-dna rng dim nil))
  ([rng dim elems]
   (let [len  (apply * (repeat dim 4))
         coll (if (and (some? elems) (<= (count elems) 4))
                elems
                nuim-code)]
     (vec (utils/rng-select-n rng coll len)))))


(defn filter-dna-seq
  [dna-seq depth-selections]
  (let [dim (dna-dimension dna-seq)]
    (if (== dim (count depth-selections))
      (if (every? nat-int? depth-selections)
        ;; for fully determined selections, use an efficient quaternary index
        ((comp vector (vec dna-seq))
         (let [qtn (apply str (if (empty? depth-selections)
                                [0] depth-selections))]
           (utils/parse-int qtn 4)))
        ;; otherwise, we need more machinery
        (let [f (fn [pos depth] (* pos (utils/pow-nat 4 (- dim depth))))
              depth-offsets (map (fn [pos depth]
                                   (if (== pos -1)
                                     (map #(f % depth) (range 0 4))
                                     (list (f pos depth))))
                                 depth-selections (drop 1 (range)))
              idxs (set (map #(apply + %)
                             (apply combo/cartesian-product depth-offsets)))]
          (->> dna-seq
               (mapv vector (range)) ;; ? map-indexed
               (filterv (fn [[i _]] (idxs i)))
               (mapv second))))
      (throw
       (ex-info "Size of selection vector must be equal to dna-seq dimension!"
                {:expected dim :actual (count depth-selections)})))))

(defn filter-dna
  [dna vpoint]
  ;; ! mapping vpoint to digits seems to drag down performance in CAs
  ;;   so either not use for CA rules or …?
  (filter-dna-seq dna (mapv const_->digit vpoint)))

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
            ;; perm-dna-seq
            ;; (mapv (fn [i]
            ;;         (let [qtn-key  (mapv (comp edn/read-string str)
            ;;                              (int->quat-str i))
            ;;               perm-key (apply str "4r"
            ;;                               (map #(qtn-key (perm-order %))
            ;;                                    dim-ns)) ;; ? just map perm-order
            ;;               i-perm   (edn/read-string perm-key)]
            ;;           (dna-vec i-perm)))
            ;;       (range (count dna-seq)))
            ;; ? make this platform-agnostic
            perm-dna-arr #?(:clj  (make-array clojure.lang.Keyword
                                              (count dna-seq))
                            :cljs (js/Array. (count dna-seq)))]
        (dotimes [i (count dna-seq)]
          (let [qtn-key  (mapv (comp utils/parse-int str) (int->quat-str i))
                perm-key (apply str (map #(qtn-key (perm-order %))
                                         dim-ns)) ;; ? just map perm-order
                i-perm   (utils/parse-int perm-key 4)]
            (aset perm-dna-arr i-perm (dna-vec i))))
        [perm-order (vec perm-dna-arr)]))))

(defn dna-seq-perspectives
  [{:keys [limit?] :or {limit? true}} dna-seq]
  (let [dim (dna-dimension dna-seq)]
    (if (and limit? (> dim 6))
      ;; fast-ish for up to 5 dimensions, have not tested beyond 6.
      (throw (ex-info "Aborted: operation too expensive for dimensions greater than 6. Set `:limit?` to false to proceed, but be aware that the combinatorial space explodes quickly!" {:input dna-seq}))
      (let [dna-vec (vec dna-seq)
            perms (combo/permutations (range dim))]
        (with-meta
          (map (partial permute-dna-seq {} dna-vec) perms)
          {:sorted-keys perms})))))


(def equal-dna? =)

(defn equiv-dna?
  ([_] true)
  ([a b] (let [->psps (comp (partial dna-seq-perspectives {})
                            second reduce-dna-seq)
               ->set  (comp set (partial map second))
               a-psps (-> a ->psps ->set)
               b-psps (-> b ->psps ->set)]
           (= a-psps b-psps)))
  ([a b & more]
   (if (equiv-dna? a b)
     (if (next more)
       (recur b (first more) (next more))
       (equiv-dna? b (first more)))
     false)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value structures

(defn vspace
  ([dim] (vspace nuim-code dim))
  ([sort-code dim]
   (apply combo/cartesian-product (repeat dim sort-code))))


(defn vdict
  [{:keys [default-result sorted?] :or {default-result :n sorted? false}}
   vp->r]
  (let [dim   (count (ffirst vp->r))
        vspc  (vspace dim)
        def-r (if (const? default-result) default-result :n)]
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
        vspc    (vspace dim)]
    (into (if sorted?
            (sorted-map-by compare-consts)
            (hash-map))
          (mapv vector vspc dna))))


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
      :else           (let [vmap-part (:n vmap)]
                        (recur vmap-part
                               (:dim (meta vmap-part))
                               (inc dim))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; const/formDNA arithmetic

(defn relc [a b]
  (case a
    :m :m
    :n b
    (case b
      :m :m
      :n a
      (case [a b]
        [:u :u] :u
        [:i :i] :i
        ([:u :i] [:i :u]) :m
        ;; value holes are black boxes, we never look inside!
        ([:_ :_] [:_ :u] [:u :_] [:_ :i] [:i :_]) :_))))

(defn rel
  ([]  :n)
  ([a] a)
  ([a b] (if (and (const_? a) (const_? b))
           (relc a b)
           (let [adim (dna-dimension a)
                 bdim (dna-dimension b)]
             (cond
               (= adim bdim) (mapv rel a b)
               (> adim bdim) (mapv rel a (expand-dna-seq b bdim adim))
               :else         (mapv rel (expand-dna-seq a adim bdim) b)))))
  ([a b & xs] (rel a (reduce rel b xs))))


(defn invc [a]
  (case a
    :n :m
    :u :i
    :i :u
    :m :n
    :_ :_)) ;; <- value holes can never be compared, so this is fine

;; ? is implicit relation confusing
(defn inv
  ([]  :m)
  ([a] (if (const_? a)
         (invc a)
         (mapv inv a)))
  ([a & xs] (inv (apply rel (cons a xs)))))

