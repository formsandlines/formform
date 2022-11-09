(ns formform.formula
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [instaparse.core :as insta]))

;; ========================================================================
;;     formform formula module
;;     -- created 11/2022, (c) Peter Hofmann
;; ========================================================================

(insta/defparser formula->parsetree
  "src/formform/formula.ebnf" :auto-whitespace :standard)

(defn parse-dna-spec
  [sort-code varlist prefixed-s]
  (let [s       (subs prefixed-s 1)
        digits? (case (first s) (\N \U \I \M) false true)]
    (if (== 1 (count s))
      (if digits?
        (calc/digit->const s sort-code)
        (keyword s))
      (let [dna (if digits?
                  (calc/digits->dna s sort-code)
                  (if (= sort-code calc/nuim-code)
                    (keyword s)
                    (calc/consts->dna s sort-code)))]
        (if (nil? varlist)
          (expr/FDNA dna)
          (expr/FDNA varlist dna))))))

(defn parse-re-sign
  [s]
  (keyword
   (apply str "<" (map #(case % \@ "re" \~ "'" (str %)) s))))

(defn parse-re-opts
  [& opts]
  (into expr/seq-reentry-defaults
        (map #(case %
                "2r"   [:parity :even]
                "2r+1" [:parity :odd]
                "alt"  [:interpr :rec-ident]
                "open" [:open? true]
                (throw (ex-info "Invalid re-entry option."
                                {:opt %})))
             opts)))

(defn parse-seqre
  [& nodes]
  (let [[specs terms] (cond
                        (empty? nodes)          [{} '()]
                        (vector? (first nodes)) [{} nodes]
                        :else [(first nodes) (rest nodes)])]
    (apply expr/SEQ-REENTRY specs terms)))

(defn parse
  ([s] (parse {} s))
  ([{:keys [sort-code] :or {sort-code calc/nuim-code}} s]
   (insta/transform
    {:EXPR      expr/make-expr
     :FORM      expr/FORM
     :UNCLEAR   expr/UNCLEAR
     :TERM      vector
     :RE_SIGN   parse-re-sign
     :RE_OPTS   parse-re-opts
     :SEQRE     parse-seqre
     :VARLIST   vector
     :VAR       expr/VAR
     :VAR_QUOT  expr/VAR
     :FDNA      (partial parse-dna-spec sort-code nil)
     :FDNA_SPEC (partial parse-dna-spec sort-code)}
    (formula->parsetree s))))


