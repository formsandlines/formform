(ns formform.utils
  (:require [superstring.core :as string]))

(defn has-decimal? [n] (< (int n) n))

(defn pow-nat [x n] (apply * (repeat n x)))

;; needs interop
;; -> see https://stackoverflow.com/questions/26082594/converting-a-number-from-base-10-to-another-base-in-clojure for other suggestions
(defn int->nbase [n base] (Integer/toString n base))

;; ? drop dependency on superstring in the future?
(def pad-left superstring.core/pad-left)

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

  
  )

