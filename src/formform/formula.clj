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

(defn parse
  ([s] (parse {} s))
  ([{:keys [sort-code] :or {sort-code calc/nuim-code}} s]
   (insta/transform
    {:EXPR     expr/make-expr
     :FORM     expr/FORM
     :UNCLEAR  expr/UNCLEAR
     :TERM     vector
     :RE_SIGN  (fn [s]
                 (keyword
                  (apply str "<" (map #(case % \@ "re" \~ "'" (str %)) s))))
     :RE_OPTS  (fn [& opts]
                 (into expr/seq-reentry-defaults
                       (map #(case %
                               "2r"   [:parity :even]
                               "2r+1" [:parity :odd]
                               "alt"  [:interpr :rec-ident]
                               "open" [:open? true]
                               (throw (ex-info "Invalid re-entry option."
                                               {:opt %})))
                            opts)))
     :SEQRE    (fn [& nodes]
                 (let [[specs terms] (cond
                                       (empty? nodes)          [{} '()]
                                       (vector? (first nodes)) [{} nodes]
                                       :else [(first nodes) (rest nodes)])]
                   (apply expr/SEQ-REENTRY specs terms)))
     :VARLIST  vector
     :VAR      expr/VAR
     :VAR_QUOT expr/VAR
     :FDNA     (fn [varlist prefix & cs]
                 (let [consts (if (= sort-code calc/nuim-code)
                                cs
                                (calc/reorder-dna-seq cs
                                                      sort-code
                                                      calc/nuim-code))]
                   (expr/FDNA varlist (calc/consts->dna consts))))
     :CONST    keyword
     :CDIGIT   #(calc/int->const (clojure.edn/read-string %) sort-code)}
    (formula->parsetree s))))






(comment
  (parse "[]:N")
  (parse "(a {@}) ball")
  (parse {:sort-code calc/nmui-code} ":3")
  (parse "{@ a}")
  (parse "{..@ a}")
  (parse "{..@. a}")
  (parse "{..@~._ a, (a) b}")
  (parse "{a}")
  (parse "{}")
  (parse "{2r|}")
  (parse "{2r+1|}")
  (parse "{2r+1|alt| a}")
  (parse "{2r+1|open|alt| a}")
  (parse "((:1):2)")
  (parse "{f\"x_1\",\"…\",\"x_2|v|+1\"}")
  (expr/=> (parse "(:1)('6×2=13')") {"6×2=13" :N})
  (expr/=>* (parse "{L,R} {2r+1|L,E,R}"))
  (parse "((/green apple/)/red apple/)")
  (parse "{2r+1|/deeming/,/telling/,/understanding/}")
  (expr/=> (parse "{,,,,,}"))
  (parse "{alt|L,R}{alt|R,L}")
  (parse "(({L,E,R}{E,R,L}{L,R,E})(L E R))")
  (parse {:sort-code calc/nmui-code} "[a,b,c]:2310302310012021221111113232332212132133023103213021320233011023")

  formula->parsetree

  (def testp (insta/parser "src/formform/formula.ebnf"))
  testp

  (insta/parses formula->parsetree "a :U ua" :partial true)

  (formula->parsetree "{a}")
  (formula->parsetree "{}")
  (formula->parsetree "[a] :N")

  (parse "((a /unkfo total/) :M b {..@ x})")
  (parse "(a \"be os\")")
  (parse "(a 'be os')")
  (parse "(a be o2s)")
  (insta/parses formula->parsetree "[a,b]:NUIM")


  (insta/parses formula->parsetree ":U'U' :I u():U b")
  (insta/parses formula->parsetree "")

  (str "a" "b")

  (parse "{@~_ a {@ x, y}}")
  (parse "{2r|open| a 'bees'}")
  )

          ; | LITERAL NODE CTX
          ; | LITERAL <#'\s+'> LITERAL CTX
          ; | LITERAL Epsilon

; <CTX>     = NODE [CTX] 
;           | LITERAL [NODE [CTX] | (<#'\s+'> LITERAL [CTX])] 
;           | Epsilon;

