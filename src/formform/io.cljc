(ns formform.io
  (:require [formform.calc :as calc]
            [formform.expr :as expr]
            [instaparse.core :as insta]))

;; ========================================================================
;;     formform io module
;;     -- created 11/2022, (c) Peter Hofmann
;; ========================================================================

;;-------------------------------------------------------------------------
;; formula notation


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
                  (let [dna (apply calc/make-dna s)]
                    (if (= sort-code calc/nuim-code)
                      dna
                      (calc/reorder-dna-seq dna sort-code calc/nuim-code))))]
        (if (nil? varlist)
          (expr/make :fdna dna)
          (expr/make :fdna varlist dna))))))

(defn parse-re-sign
  [s]
  {:re-specs (keyword (apply str "<" (map #(case %
                                             \@ "r"
                                             \~ "'"
                                             (str %)) s)))})

(defn parse-re-opts
  [& opts]
  {:re-specs (into expr/seq-reentry-defaults
                   (map #(case %
                           "2r"   [:parity :even]
                           "2r+1" [:parity :odd]
                           "alt"  [:interpr :rec-ident]
                           "open" [:open? true]
                           (throw (ex-info "Invalid re-entry option."
                                           {:opt %})))
                        opts))})

(defn parse-seqre
  [x & nodes]
  (let [[specs terms] (let [specs (:re-specs x)]
                        (if (nil? specs)
                          [{} (cons x nodes)]
                          [specs nodes]))]
    (apply expr/seq-re specs terms)))

(defn parse
  ([s] (parse {} s))
  ([{:keys [sort-code] :or {sort-code calc/nuim-code}} s]
   (insta/transform
    {:EXPR      expr/make ; apply?
     :FORM      expr/form ; apply?
     :UNCLEAR   (partial expr/make :uncl)
     :TERM      expr/make ; apply?
     :RE_SIGN   parse-re-sign
     :RE_OPTS   parse-re-opts
     :SEQRE     parse-seqre
     :VARLIST   vector
     :VAR       expr/make ; apply?
     :VAR_QUOT  expr/make ; apply?
     :FDNA      (partial parse-dna-spec sort-code nil)
     :FDNA_SPEC (partial parse-dna-spec sort-code)}
    (formula->parsetree s))))


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

  (parse "((a (x)) b)")
  (parse "/unkfo/")
  (parse ":NUIM")
  (expr/·mem [["a" :M] ["b" :N]] "a")
  (expr/·r ["a"] ["b"])

  '[(("a" ("x")) "b")]
  '[[:uncl "unkfo"]]
  '[[:fdna ["v__0"] :NUIM]]
  '[[:mem (["a" [:M]] ["b" [:N]]) "a"]]
  '[[:<re ["a"] ["b"]]]


  (expr/=>* (parse "(((a b c) (x y)) (a c y)) ((b) (x) a c y)"))

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
