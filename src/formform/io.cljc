(ns formform.io
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [clojure.string :as str]
            [instaparse.core :as insta]))

;; ========================================================================
;;     formform io module
;;     -- created 11/2022, (c) Peter Hofmann
;; ========================================================================

;;-------------------------------------------------------------------------
;; read formula notation

(insta/defparser formula->parsetree
  "src/formform/formula.ebnf" :auto-whitespace :standard)

(defn parse-symbol
  [sort-code s]
  (let [s (subs s 1)
        s (if (and (not= sort-code calc/nuim-code)
                   (some? (#{"0" "1" "2" "3"} s)))
            (-> s
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
      (calc/digits->dna s sort-code)
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

(defn parse
  ([s] (parse {} s))
  ([opts s]
   (parse-tree opts (formula->parsetree s))))

;;-------------------------------------------------------------------------
;; print formula notation

(declare formula)

(def whitespace? #(or (nil? %) (= "" %)))

(defn ctx->formula
  [ctx]
  (str/join " " (remove whitespace? (map formula ctx))))

(defn ctx-seq->formula
  [ctx-seq]
  (str/join ", " (map formula ctx-seq)))

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
  [[op-k vars dna]]
  (str "[" op-k " [" (str/join ", " (map variable->formula vars)) "] "
      (str "::" (str/join "" (map name dna))) "]"))

(defn memory->formula
  [[op-k rems expr]]
  (let [rems (str/join ", " (map (fn [[x y]]
                                   (str (formula x) " = " (formula y)))
                                 rems))]
    (str "[" op-k " " rems " | " (formula expr) "]")))

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

(defn formula
  [expr]
  (condp #(%1 %2) expr
    nil?           ""
    expr/variable? (variable->formula expr)
    keyword?       (str expr)
    expr/operator? (operator->formula expr)
    expr/form?     (form->formula expr)
    (throw (ex-info "Unknown expression type." {}))))

;;-------------------------------------------------------------------------
;; uniform expressions

#_(comment

      (declare ctx->uniform)
      (declare cnt->uniform)

      (defn- FORM->uniform
        [form]
        {:type :form
         :ctx  (ctx->uniform form)})

      (def UFORM-uniform {:type :uform})
      (def IFORM-uniform {:type :iform})

      (defn- VAR->uniform
        [variable]
        {:type :var
         :label variable})

      (defn- UNCLEAR->uniform
        [uncl]
        {:type  :uncl
         :label (expr/UNCLEAR->label uncl)})

      (defn- SEQ-REENTRY->uniform
        [seq-re]
        {:type :seq-reentry
         :sign (expr/SEQ-REENTRY->sign seq-re)
         :ctx  (mapv (fn [nested-ctx]
                       {:type :nested-ctx
                        :ctx (ctx->uniform nested-ctx)})
                     (expr/SEQ-REENTRY->ctxs seq-re))})

      (defn- FDNA->uniform
        [fdna]
        {:type :fdna
         :dna (expr/FDNA->dna fdna)
         :varlist (expr/FDNA->varlist fdna)})

      (defn- MEMORY->uniform
        [mem]
        {:type :mem
         :rems (mapv (fn [[k v]] {:type :rem
                                  :ctx (vector (cnt->uniform k)
                                               (ctx->uniform v))})
                     (expr/MEMORY->rems mem))
         :ctx  (ctx->uniform (expr/MEMORY->ctx mem))})

      (defn- const->uniform
        [c]
        {:type  :const
         :value c})

      (defn- cnt->uniform
        [x]
        (cond
          (calc/const? x)       (const->uniform x)
          (expr/fdna? x)        (FDNA->uniform x)
          (expr/memory? x)      (MEMORY->uniform x)
          (expr/unclear? x)     (UNCLEAR->uniform x)
          (expr/seq-reentry? x) (SEQ-REENTRY->uniform x)
          (expr/pure-form? x)   (FORM->uniform x)
          (expr/uform? x)       UFORM-uniform
          (expr/iform? x)       IFORM-uniform
          (expr/variable? x)    (VAR->uniform x)
          :else x))

      (defn- ctx->uniform
        [ctx]
        (mapv cnt->uniform ctx))

      (defn uniform-expr [expr]
        {:type :expr
         :ctx  (ctx->uniform expr)})

      )


#_(comment

      (uniform-expr (expr/<-> (expr/<> 'a :U (expr/<fdna> ["x"] :NUIM)) 'b))

      )

(comment

  (parse {:sort-code calc/nmui-code} ":1 ()")

  (parse "::N")
  (parse "::0123")
  (parse "(a [:fdna [a, b]::0123012301230123] b)")
  (parse "[:x ((a) (b))]")
  (parse "[:seq-re ..@ a, b]")
  (parse-tree [:RE_SIGN "..@"])
  (parse ":seq-re")
  (parse "{a, b}")
  (parse "[:uncl hey]")
  ; (parse "[:mem [[a (x)] ['be os' /to/]] (a (b))]")
  (parse "[:mem a = (x), 'be os' = /to/ | (a (b))]")
  (parse "[:mem a = (x) | (a (b))]")
  (parse "[:mem | (a (b))]")

  (parse "a = (x), b = y | (a (b))")

  (keyword "dna")

  (parse "[:uncl []]")
  (parse "[:uncl \"foo bar\"]")
  
  (parse  "[:fdna [a]::0123]")

  (parse-tree [:VARLIST [:VAR "a"]])
  (parse-tree [:FDNA "::0123"])
  (parse-tree [:FDNA_SPEC [:VARLIST [:VAR "a"]] "::0123"])
  (parse-tree [:SYMBOL ":fdna"])

  (parse "((a (x)) b)")
  (parse "/unkfo/")
  (parse ":NUIM")

  (expr/=>* (parse "(((a b c) (x y)) (a c y)) ((b) (x) a c y)"))

  (formula [:fdna ['a] [:N :U :I :M]])
  (formula [:mem [['x :M] ['y :U]] ['x ['y]]])

  (condp #(%1 %2) "hi"
    expr/form? :form
    string? :t
    :f)

  ; #_:clj-kondo/ignore
  ; {:type :form
  ;  :ctx [{:type :var
  ;         :attr {:symbol "a"}}
  ;        {:type :fdna
  ;         :attr {:dna :NUIM
  ;                :varlist ["a"]}}
  ;        {:type :seq-re
  ;         :attr {:sign :<..re}
  ;         :ctx [{:type :nested-ctx
  ;                :ctx [{:type :form
  ;                       ...}
  ;                      {:type :var
  ;                       :attr {:symbol "b"}}]}
  ;               {:type :nested-ctx
  ;                :ctx [{:type :var
  ;                       :attr {:symbol "b"}}]}]}
  ;        {:type :const
  ;         :attr {:value :U
  ;                :symbol ":U"}}]}

  )
