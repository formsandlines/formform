(ns formform.io
  "API for the `io` module of `formform`."
  (:require
   [formform.calc :as calc]
   [formform.expr :as expr]
   [formform.io.core :as core]
   #?(:clj  [formform.utils :as utils :refer [defapi]]
      :cljs [formform.utils :as utils :refer-macros [defapi]])
   [instaparse.core :as insta]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs

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

(def has-type #(fn [uniform] (= % (:type uniform))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function specs & API

(s/fdef core/parse-tree
  :args (s/alt :ar1 (s/cat :tree ::parse-tree)
               :ar2 (s/cat :opts (s/keys :opt-un [:formform.calc/sort-code])
                           :tree ::parse-tree))
  :ret  (s/or :expr :formform.expr/expression
              :fail insta/failure?))
(defapi core/parse-tree)

(s/fdef core/formula->expr
  :args (s/alt :ar1 (s/cat :s string?)
               :ar2 (s/cat :opts (s/keys :opt-un [:formform.calc/sort-code])
                           :s string?))
  :ret  (s/or :expr :formform.expr/expression
              :fail insta/failure?))
(defapi core/read-expr)

(s/fdef core/print-expr
  :args (s/cat :expr :formform.expr/expression)
  :ret  string?)
(defapi core/print-expr)

(s/def :opts.uniform/legacy? boolean?)
(s/def :opts.uniform/branchname string?)
(s/def :opts.uniform/use-unmarked? boolean?)
(s/def :opts.uniform/unclear? boolean?)
(s/def :opts.uniform/const? boolean?)
(s/def :opts.uniform/use-seq-reentry? boolean?)

(s/fdef core/uniform-expr
  :args (s/cat :opts (s/keys :opt-un [:opts.uniform/legacy?
                                      :opts.uniform/branchname
                                      :opts.uniform/use-unmarked?
                                      :opts.uniform/unclear?
                                      :opts.uniform/const?
                                      :opts.uniform/use-seq-reentry?])
               :expr :formform.expr/expression)
  :ret  ::uniform-expr)
(defapi core/uniform-expr)


(def fns-with-specs (utils/list-fn-specs "formform.io"))


(comment
  (insta/failure? (core/read-expr "[:uncl []()]"))

  (core/uniform-expr {:unclear? false} [])

  )
