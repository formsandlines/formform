(ns ^:no-doc formform.utils
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.test.check.random :as random]
   [clojure.spec.alpha :as s]
   #?(:cljs [goog.math.Long :as glong])))

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
                   (str/starts-with? (namespace k) ns-root))]
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
  ([kebab-str] (kebab->camel kebab-str false))
  ([kebab-str capitalize?]
   (if (or (nil? kebab-str) (empty? kebab-str))
     ""
     (let [parts (str/split kebab-str #"-")]
       (if capitalize?
         (apply str (mapv str/capitalize parts))
         (apply str (first parts) 
                (mapv str/capitalize (rest parts))))))))

(defn camel->kebab
  "Converts a camelCase string to kebab-case.
   Example: 'helloWorld' -> 'hello-world'"
  ([camel-str]
   (if (or (nil? camel-str) (empty? camel-str))
     ""
     (let [camel-str (str (str/upper-case (first camel-str)) (subs camel-str 1))
           parts (re-seq #"[A-Z][^A-Z]+" camel-str)]
       (str/join "-" (mapv str/lower-case parts))))))

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


;; RNG (random number generator) Wrapper

(defn make-rng
  "Returns a new random number generator with an optional `seed`."
  ([] (make-rng nil))
  ([seed] (if seed
            (random/make-random seed)
            (random/make-random))))

(defn rng-split-n
  "Splits a given random number generator into `n` different random number generators."
  [rng n]
  (random/split-n rng n))

(defn rng-rand-long
  "Generates a random integer given a random number generator."
  [rng]
  (random/rand-long rng))

(defn rng-rand-double
  "Generates a random double in the interval [0.0 1.0) given a random number generator."
  [rng]
  (random/rand-double rng))

(defn long-modulo
  "Returns the result of the modulo operation on a Long integer (native to the host platform) modulo some integer `n`."
  #?(:clj  [native-long n]
     :cljs [^goog.math.Long native-long n])
  #?(:clj  (mod native-long n)
     ;; goog.math.Long `.modulo` behaves more like `rem` in Clojure
     ;; so we need to do some math to make it behave like `mod`:
     :cljs (let [^goog.math.Long gl
                 (.modulo native-long
                          ^goog.math.Long (glong/fromNumber n))
                 ^js/Number nrem (.toInt gl)]
             (if (< nrem 0) (+ nrem n) nrem))))

;; Random functions

(defn rng-select
  "Selects a random item from `coll`, given a random number generator."
  ([rng coll]
   (let [n (long-modulo (rng-rand-long rng) 4)]
     (coll n)))
  ([rng coll weights]
   (when (some #(< % 0) weights)
     (throw (ex-info "Weights cannot be negative!" {:weights weights})))
   (let [total (+ 0.0 (reduce + weights))]
     (if (zero? total)
       (throw (ex-info "Sum of weights must be positive!" {:weights weights}))
       (let [normal-weights (mapv #(/ % total) weights)
             cumulative (vec (reductions + normal-weights))
             r (rng-rand-double rng)]
         (loop [i 0]
           (let [threshold (cumulative i)]
             (if (or (<= r threshold)
                     (>= i (count weights)))
               (coll i)
               (recur (inc i))))))))))

(defn rng-rand-n
  "Generates `n` different random numbers given a random number generator."
  [rng n]
  (mapv rng-rand-long (random/split-n rng n)))

(defn rng-select-n
  "Selects `n` different random items from `coll`, given a random number generator."
  ([rng coll n]
   (into [] (map #(rng-select % coll))
         (rng-split-n rng n)))
  ([rng coll n weights]
   (into [] (map #(rng-select % coll weights))
         (rng-split-n rng n))))

(comment
  (->> (repeatedly 100000 #(rng-select (make-rng) [:n :u :i :m] [1 2 3 4]))
       (frequencies))

  (->> (rng-select-n (make-rng) [:n :u :i :m] 100000 [1 2 3 4])
       (frequencies))

  (let [coll [:n :u :i :m]
        weights [1 0 4 3]
        total (reduce + weights)
        normal-weights (mapv #(/ (+ % 0.0) total) weights)
        cumulative (vec (reductions + normal-weights))
        freqs (frequencies
               (for [_ (range 100000)
                     :let [r (rand)]]
                 (loop [i 0]
                   (let [threshold (cumulative i)]
                     (if (or (<= r threshold)
                             (>= i (count weights)))
                       (coll i)
                       (recur (inc i)))))))
        freq-total (reduce + (vals freqs))]
    [(mapv (fn [[k n]]
             [k (+ 0.0 (/ n freq-total))])
           freqs)
     normal-weights])

  ,)

(comment

  (def rng (random/make-random 40))

  (rng-split-n rng 4)
  (long-modulo (rng-rand rng) 4)
  (rng-select rng [:a :b :c :d])
  (mapv #(long-modulo % 4) (rng-rand-n rng 100))
  (rng-select-n rng [:a :b :c :d] 10)
  
  (random/rand-long ((random/split-n rng 3) 2))

  (random/rand-long rng)
  (random/rand-long (random/make-random (random/rand-long rng)))

  (let [[init-rng next-rng] (random/split rng)]
    [(random/rand-long init-rng)
     next-rng])

  (defn random-nums
    [rng]
    (lazy-seq
     (let [[rng1 rng2] (random/split rng)
           x (random/rand-long rng1)]
       (cons x (random-nums rng2)))))

  (take 10 (random-nums rng))

  ,)

(comment
  (aget (keywords-to-array [:n :u :i :m]) 1)
  (aget (keywords-to-array-2d [[:n :u] [:i :m]]) 0 1)

  (is-array? (keywords-to-array [:n :u :i :m]))

  (array-to-vector (keywords-to-array [:n :u :i :m]))
  (array-to-vector (keywords-to-array-2d [[:n :u] [:i :m]]))

  ,)
