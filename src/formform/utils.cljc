(ns ^:no-doc formform.utils
  (:require
   [clojure.math :as math]
   [clojure.string :as string]
   [clojure.spec.alpha :as s]))

(defn has-decimal? [n] (< (int n) n))

(defn pow-nat [x n] (apply * (repeat n x)))

(defn geom-seq [k r] (map (fn [n] (* k (pow-nat r n))) (range)))

;; ? might not be worth it - performance diff. to split-at minimal
;; (unused by now)
(defn splitv-atv
  "Alternative to `clojure.core/splitv-at` that only takes vector input.

* performance improvement might be minimal."
  [i v]
  (vector (into [] (subvec v 0 i))
          (into [] (subvec v i (count v)))))

(defn dissocv [v i]
  (into (into [] (subvec v 0 i))
        (subvec v (inc i))))

(defn reversev [xs]
  (vec (if (vector? xs)
         (rseq xs)
         (reverse xs))))

;; -> see https://stackoverflow.com/questions/26082594/converting-a-number-from-base-10-to-another-base-in-clojure for other suggestions
(defn int->nbase [n base]
  #?(:clj
     (Integer/toString n base)
     :cljs
     (.toString n base)))

;;-------------------------------------------------------------------------
;; Modified from source:
;; https://github.com/expez/superstring/blob/master/src/superstring/core.clj

(defn- gen-padding
  "Generate the necessary padding to fill s upto width."
  [s padding width]
  (let [missing (- width (count s))
        full-lengths (int (math/floor (/ missing (count padding))))
        remaining (if (zero? full-lengths) (- width (count s))
                      (rem missing (* full-lengths (count padding))))]
    (str (apply str (repeat full-lengths padding))
         (subs padding 0 remaining))))

(defn pad-left
  "Pad the beginning of s with padding, or spaces, until the length of
  s matches width."
  [s width padding]
  (if (<= width (count s))
    s
    (str (gen-padding s padding width) s)))
;;-------------------------------------------------------------------------

(defn compare-names [a b]
  (let [to-str (fn [x] (cond
                         (symbol? x)  (str x)
                         (keyword? x) (name x)
                         (string? x)   x
                         :else (assert false)))
        astr (to-str a)
        bstr (to-str b)]
    (compare astr bstr)))

; (defn extend-meta [x metadata]
;   (with-meta x (merge (meta x) metadata)))

;; ? get rid of `nil`
(defn nest-left
  ([xs] (nest-left identity identity xs))
  ([fnode xs] (nest-left fnode identity xs))
  ([fnode fx xs]
   (if (empty? xs)
     nil
     (loop [r      (rest xs)
            nested (fnode [(fx (first xs))])]
       (if (empty? r)
         nested
         (let [[x & r] r
               nested  (fnode (concat [nested] [(fx x)]))]
           (if (empty? r)
             nested
             (recur r nested))))))))

;; ? get rid of `nil`
(defn nest-right
  ([xs] (nest-right identity identity xs))
  ([fnode xs] (nest-right fnode identity xs))
  ([fnode fx xs]
   (if (empty? xs)
     nil
     (let [[x & r] xs
           x       (fx x)]
       (if (empty? r)
         (fnode [x])
         (fnode (concat [x] [(nest-right fnode fx r)])))))))

(defn list-fn-specs [ns-root]
  (for [[k _] (s/registry)
        :when (and (symbol? k)
                   (string/starts-with? (namespace k) ns-root))]
    k))


