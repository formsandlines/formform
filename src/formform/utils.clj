(ns formform.utils
  (:require [superstring.core :as string]))

(defn has-decimal? [n] (< (int n) n))

(defn pow-nat [x n] (apply * (repeat n x)))

;; needs interop
;; -> see https://stackoverflow.com/questions/26082594/converting-a-number-from-base-10-to-another-base-in-clojure for other suggestions
(defn int->nbase [n base] (Integer/toString n base))

;; ? drop dependency on superstring in the future?
(def pad-left superstring.core/pad-left)




