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

;; because edn/read-string is too slow
(defn parse-int
  "Parses an integer from a string with an optional base, using the implementation function of the respective host language."
  ([s] (parse-int s nil))
  ([s base]
   (if base
     #?(:clj  (Integer/parseInt s base)
        :cljs (js/parseInt s base))
     #?(:clj  (Integer/parseInt s)
        :cljs (js/parseInt s)))))

(defn parse-int-maybe
  "Like `parse-int`, but return `nil` on invalid input instead of throwing an exception."
  ([s] (parse-int-maybe s nil))
  ([s base]
   (if base
     #?(:clj  (try (. Integer parseInt s base) (catch Exception _ nil))
        :cljs (try (js/parseInt s base) (catch js/Error _ nil)))
     #?(:clj  (try (. Integer parseInt s) (catch Exception _ nil))
        :cljs (try (js/parseInt s) (catch js/Error _ nil))))))

;; -> see https://stackoverflow.com/questions/26082594/converting-a-number-from-base-10-to-another-base-in-clojure for other suggestions
(defn int->nbase
  "Converts an integer to a string of that number in the given base."
  [n base]
  #?(:clj  (Integer/toString n base)
     :cljs (.toString n base)))

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

;; (defn extend-meta [x metadata]
;;   (with-meta x (merge (meta x) metadata)))

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

(defn lazy-seq? [x]
  #?(:clj  (instance? clojure.lang.LazySeq x)
     :cljs (instance? cljs.core.LazySeq x)))

(defn iterate? [x]
  #?(:clj  (instance? clojure.lang.Iterate x)
     :cljs (instance? cljs.core.Iterate x)))

(defn kebab->camel
  "Converts a kebab-case string to camelCase.
   Example: 'hello-world' -> 'helloWorld'"
  ([kebab-str capitalize?]
   (if (or (nil? kebab-str) (empty? kebab-str))
     ""
     (let [parts (clojure.string/split kebab-str #"-")]
       (if capitalize?
         (apply str (map clojure.string/capitalize parts))
         (apply str (first parts) 
                (map clojure.string/capitalize (rest parts)))))))
  ([kebab-str] (kebab->camel kebab-str false)))

(defn keywords-to-array
  "Converts a sequence of keywords to an array, using an optimized implementation for the respective host language."
  [xs]
  #?(:clj  (let [n (count xs)
                 ^"[Lclojure.lang.Keyword;" arr
                 (make-array clojure.lang.Keyword n)]
             (dotimes [i n]
               (aset arr i (nth xs i)))
             arr)
     :cljs (let [rows (count xs)
                 arr #js []]
             (dotimes [i rows]
               (.push arr (nth xs i)))
             arr)))

(defn keywords-to-array-2d
  "Converts a 2D sequence of keywords to a 2D array, using an optimized implementation for the respective host language."
  [xxs]
  #?(:clj  (let [rows (count xxs)
                 cols (count (first xxs))
                 ^"[[Lclojure.lang.Keyword;" arr
                 (make-array clojure.lang.Keyword rows cols)]
             (dotimes [i rows]
               (let [row (nth xxs i)]
                 (dotimes [j cols]
                   (aset arr i j (nth row j)))))
             arr)
     :cljs (let [rows (count xxs)
                 arr #js []]
             (dotimes [i rows]
               (let [row (nth xxs i)
                     js-row #js []]
                 (doseq [item row]
                   (.push js-row item))
                 (.push arr js-row)))
             arr)))

(defn is-array?
  [coll]
  #?(:clj  (.isArray (class coll))
     :cljs (js/Array.isArray coll))) ;; ! test in cljs

(defn array-to-vector
  [arr]
  (if (is-array? arr)
    (mapv array-to-vector arr)
    arr))

(comment
  (aget (keywords-to-array [:N :U :I :M]) 1)
  (aget (keywords-to-array-2d [[:N :U] [:I :M]]) 0 1)

  (is-array? (keywords-to-array [:N :U :I :M]))

  (array-to-vector (keywords-to-array [:N :U :I :M]))
  (array-to-vector (keywords-to-array-2d [[:N :U] [:I :M]]))

  ,)
