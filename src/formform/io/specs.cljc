(ns formform.io.specs
  (:require [formform.calc.specs :as calc-sp]
            [formform.expr.specs :as expr-sp]
            [formform.io.core :as core]
            [formform.utils :as utils]
            [instaparse.core :as insta]
            [clojure.spec.alpha :as s]))


(s/def ::parse-tree (s/or :tree vector?
                          :fail insta/failure?))


(s/def :uniform/type
  #{:empty :variable :constant :reEntryPoint :symbol :form :unclear :operator})

(s/def :uniform/label string?)
(s/def :uniform/value (s/or :string string? :keyword keyword?))
(s/def :uniform/unmarked boolean?)
(s/def :uniform/children
  (s/coll-of ::uniform-expr :kind vector?))
(s/def :uniform/space :uniform/children)

(def ^:private has-type #(fn [uniform] (= % (:type uniform))))

(s/def ::uniform-expr
  (s/or :empty        (s/and (s/keys :req-un [:uniform/type])
                             (has-type :empty))
        :variable     (s/and (s/keys :req-un [:uniform/type :uniform/label])
                             (has-type :variable))
        :constant     (s/and (s/keys :req-un [:uniform/type :uniform/value])
                             (has-type :constant))
        :reEntryPoint (s/and (s/keys :req-un [:uniform/type :uniform/label])
                             (has-type :reEntryPoint))
        :symbol       (s/and (s/keys :req-un [:uniform/type :uniform/label])
                             (has-type :symbol))
        :form         (s/and (s/keys :req-un [(or :uniform/children
                                                  :uniform/space)
                                              :uniform/type]
                                     :opt-un [:uniform/unmarked])
                             (has-type :form))
        :unclear      (s/and (s/keys :req-un [:uniform/type :uniform/value
                                              :uniform/label])
                             (has-type :unclear))
        :operator     (s/and (s/keys :req-un [:uniform/type :uniform/label])
                             (has-type :operator))))


;;-------------------------------------------------------------------------
;; function specs (impl.)

(s/fdef core/parse-tree
  :args (s/alt :ar1 (s/cat :tree ::parse-tree)
               :ar2 (s/cat :opts (s/keys :opt-un [::calc-sp/sort-code])
                           :tree ::parse-tree))
  :ret  (s/or :expr ::expr-sp/expression
              :fail insta/failure?))
; (defn parse-tree
;   "Parses the given instaparse tree and returns a FORM expression."
;   ([tree] (core/parse-tree {} tree))
;   ([opts tree]
;    (core/parse-tree opts tree)))
  

(def ^:no-doc fns-with-specs (utils/list-fn-specs "formform.io"))


