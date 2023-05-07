(ns formform.io
  "API for the `io` module of `formform`."
  (:require [formform.calc.specs :as calc-sp]
            [formform.expr.specs :as expr-sp]
            [formform.io.specs :as sp]
            [formform.io.core :as core]
            [formform.utils :as utils]
            [instaparse.core :as insta]
            [clojure.spec.alpha :as s]
            #_[clojure.spec.gen.alpha :as gen]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formula notation (extends commonly used parenthese notation)

(s/fdef read-expr
  :args (s/alt :ar1 (s/cat :s string?)
               :ar2 (s/cat :opts (s/keys :opt-un [::calc-sp/sort-code])
                           :s string?))
  :ret  (s/or :expr ::expr-sp/expression
              :fail insta/failure?))
(defn read-expr
  "Given a string in `formula` notation, returns the corresponding data structure that can be processed by `formform.expr`.

  Can be given a map with the following options:

  * `:sort-code` -> to specify a different `sort-code` for `formDNA` interpretation (see `formform.calc/sort-code?`)"
  ([s] (core/formula->expr {} s))
  ([opts s] (core/formula->expr opts s)))


(s/fdef print-expr
  :args (s/cat :expr ::expr-sp/expression)
  :ret  string?)
(defn print-expr
  "Given an expression, returns a string of its representation in `formula` notation."
  [expr] (core/expr->formula expr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniform expressions (JSON-like)

(s/def :opts.uniform/legacy? boolean?)
(s/def :opts.uniform/branchname string?)
(s/def :opts.uniform/use-unmarked? boolean?)
(s/def :opts.uniform/unclear? boolean?)
(s/def :opts.uniform/const? boolean?)
(s/def :opts.uniform/use-seq-reentry? boolean?)

(s/fdef uniform-expr
        :args (s/alt :ar1 (s/cat :expr ::expr-sp/expression)
                     :ar2 (s/cat :opts 
                                 (s/keys :opt-un 
                                         [:opts.uniform/legacy?
                                          :opts.uniform/branchname
                                          :opts.uniform/use-unmarked?
                                          :opts.uniform/unclear?
                                          :opts.uniform/const?
                                          :opts.uniform/use-seq-reentry?])
                                 :expr ::expr-sp/expression))
        :ret  ::sp/uniform-expr)
(defn uniform-expr
  "Given an expression, returns a `uniform` data structure that is a nested map with the following pattern:
  ```
  {:type <expr-type>
  …
  :children [<uniform> …]}
  ```

  Can be given an option map to support various customizations (see source), e.g. the `:legacy?` flag can be set to output a map that can be used as `formJSON` for backward compatibility with formform 1."
  ([expr] (core/uniform-expr {} expr))
  ([opts expr] (core/uniform-expr opts expr)))
  


(def ^:no-doc fns-with-specs (utils/list-fn-specs "formform.io"))


(comment)
  
