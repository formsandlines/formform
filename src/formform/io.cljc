;; ========================================================================
;;     formform io module
;;     -- created 11/2022, (c) Peter Hofmann
;; ========================================================================

(ns formform.io
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.grammar.formula :refer [parser]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            #?(:clj  [instaparse.core :as insta]
               :cljs [instaparse.core :as insta])))

;;-------------------------------------------------------------------------
;; read formula notation

(defn parse-symbol
  [sort-code s]
  (let [s (subs s 1)
        s (if (and (not= sort-code calc/nuim-code)
                   (some? (#{"0" "1" "2" "3"} s)))
            (-> s
                clojure.edn/read-string
                (calc/digit->const sort-code)
                (calc/const->digit)
                str)
            s)]
    (keyword s)))

(defn parse-operator
  [op-k & args]
  (if (expr/operator? [op-k])
    (apply expr/make-op op-k args)
    (apply vector op-k args)))

(defn parse-fdna
  [sort-code prefixed-s]
  (let [s       (subs prefixed-s 2)
        digits? (case (first s) (\N \U \I \M) false true)]
    (if digits?
      ;; ? allow numeric formDNA
      (calc/digits->dna (mapv (comp clojure.edn/read-string str) s) sort-code)
      (let [dna (apply calc/make-dna s)]
        (if (= sort-code calc/nuim-code)
          dna
          (calc/reorder-dna-seq dna sort-code calc/nuim-code))))))

(defn parse-re-sign
  [s]
  (keyword (apply str "<" (map #(case %
                                  \@ "r"
                                  \~ "'"
                                  (str %)) s))))

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
  [x & nodes]
  (let [[specs terms] (if (or (expr/seq-reentry-signature? x)
                              (expr/seq-reentry-opts? x))
                        [x nodes]
                        [{} (cons x nodes)])]
    (apply expr/seq-re specs terms)))

(defn parse-tree
  ([tree] (parse-tree {} tree))
  ([{:keys [sort-code] :or {sort-code calc/nuim-code}} tree]
   (insta/transform
    {:EXPR      expr/make
     :FORM      expr/form

     :VAR       expr/make
     :VAR_QUOT  expr/make

     :SYMBOL    (partial parse-symbol sort-code)
     :OPERATOR  parse-operator
     ; :UNPARSED  identity ;; EDN?

     :UNCLEAR   (partial expr/make :uncl)

     :SEQRE     parse-seqre
     :SEQRE_SYM (partial parse-symbol sort-code)
     :RE_SIGN   parse-re-sign
     :RE_OPTS   parse-re-opts

     :FDNA_LIT  (partial expr/make :fdna)
     :FDNA      (partial parse-fdna sort-code)
     :FDNA_SYM  (partial parse-symbol sort-code)
     :VARLIST   vector
     
     :MEMORY_SYM (partial parse-symbol sort-code)
     :REMLIST   vector
     :REM       vector}
    tree)))

(defn formula->expr
  ([s] (formula->expr {} s))
  ([opts s]
   (parse-tree opts (parser s))))

;; alias
(def read-expr formula->expr)


;;-------------------------------------------------------------------------
;; print formula notation

(declare expr->formula)

(def whitespace? #(or (nil? %) (= "" %)))

(defn ctx->formula
  [ctx]
  (str/join " " (remove whitespace? (map expr->formula ctx))))

(defn ctx-seq->formula
  [ctx-seq]
  (str/join ", " (map expr->formula ctx-seq)))

(defn form->formula
  [form]
  (let [form (apply expr/form form)]
    (str "(" (ctx->formula form) ")")))


(defn variable->formula
  [v]
  (if (string? v)
    (let [parts (str/split v #" ")]
      (if (> (count parts) 1)
        (str "'" v "'")
        v))
    (str v)))


(defn arrangement->formula
  [[_ & exprs]]
  (ctx->formula exprs))

(defn seq-reentry-sign->formula
  [sign]
  (-> (name sign)
      (subs 1)
      (str/replace #"r" "@")
      (str/replace #"'" "~")))

(defn seq-reentry->formula
  [[_ sign & nested-exprs]]
  (str "{" (seq-reentry-sign->formula sign) " "
       (ctx-seq->formula nested-exprs) "}"))

(defn formDNA->formula
  [[op-k varorder dna]]
  (str "[" op-k " [" (str/join ", " (map variable->formula varorder)) "] "
      (str "::" (str/join "" (map name dna))) "]"))

(defn memory->formula
  [[op-k rems expr]]
  (let [rems (str/join ", " (map (fn [[x y]]
                                   (str (expr->formula x) " = "
                                        (expr->formula y)))
                                 rems))]
    (str "[" op-k " " rems " | " (expr->formula expr) "]")))

(defn unclear->formula
  [[op-k & label]]
  (str "[" op-k " " (str/join "" label) "]"))

(defn operator->formula
  [[op-k & _ :as op]]
  (let [op (apply expr/make op)]
    ((case op-k
       :-      arrangement->formula
       :seq-re seq-reentry->formula
       :fdna   formDNA->formula
       :mem    memory->formula
       :uncl   unclear->formula
       str) op)))

(defn expr->formula
  [expr]
  (condp #(%1 %2) expr
    nil?           ""
    expr/variable? (variable->formula expr)
    keyword?       (str expr)
    expr/operator? (operator->formula expr)
    expr/form?     (form->formula expr)
    (throw (ex-info "Unknown expression type." {}))))

;; alias
(def print-expr expr->formula)


(comment
  )
