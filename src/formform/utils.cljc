(ns formform.utils)

(defn has-decimal? [n] (< (int n) n))

(defn pow-nat [x n] (apply * (repeat n x)))

(defn geom-seq [k r] (map (fn [n] (* k (pow-nat r n))) (range)))

;; ? might not be worth it - performance diff. to split-at minimal
(defn splitv-at [i v]
  (vector (into [] (subvec v 0 i))
          (into [] (subvec v i (count v)))))

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
        full-lengths (clojure.math/floor (/ missing (count padding)))
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

(comment

  ; (meta (extend-meta (with-meta ['x] {:foo true}) {:bar true}))
  (pad-left "abc" 6 "0")

  (splitv-at 2 [1 2 3 4 5 6])

  
  )

