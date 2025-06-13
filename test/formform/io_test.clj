(ns formform.io-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.io.formula :refer [parser]]
            [formform.io :refer :all]
            [instaparse.core :as insta]
            [orchestra.spec.test :as stest]))

(doseq [fsym calc/fns-with-specs] (stest/instrument fsym))
(doseq [fsym expr/fns-with-specs] (stest/instrument fsym))
(doseq [fsym fns-with-specs]      (stest/instrument fsym))


(def tree parser)
(def trees (partial insta/parses parser))

(def fail? insta/failure?)

(deftest formula->parsetree-test
  (testing "Quoted labels"
    (testing "incomplete match"
      (is (fail? (tree "''")))
      (is (fail? (tree "\"\"")))
      (is (fail? (tree "//")))

      (is (fail? (tree "'")))
      (is (fail? (tree "\"")))
      (is (fail? (tree "/")))

      (is (fail? (tree "'\"")))
      (is (fail? (tree "\"'")))
      (is (fail? (tree "\"/")))
      (is (fail? (tree "/\"")))
      (is (fail? (tree "'/")))
      (is (fail? (tree "/'")))

      (is (fail? (tree "a'")))
      (is (fail? (tree "a\"")))
      (is (fail? (tree "a/")))
      (is (fail? (tree "'a")))
      (is (fail? (tree "\"a")))
      (is (fail? (tree "/a"))))

    (testing "interfering matches"
      (is (fail? (tree "'a''")))
      (is (fail? (tree "''a'")))
      (is (fail? (tree "\"a\"\"")))
      (is (fail? (tree "\"\"a\"")))
      (is (fail? (tree "/a//")))
      (is (fail? (tree "//a/")))

      ;; ? should this be allowed
      (is (fail? (tree "'\"a\"'")))
      (is (fail? (tree "\"'a'\"")))
      (is (fail? (tree "'/a/'")))
      (is (fail? (tree "/'a'/")))
      (is (fail? (tree "/\"a\"/")))

      (is (fail? (tree "'a\"'b\"")))
      (is (fail? (tree "'\"a'b\"")))
      (is (fail? (tree "'a\"b'\"")))
      (is (fail? (tree "'a/'b/")))
      (is (fail? (tree "'/a'b/")))
      (is (fail? (tree "'a/b'/")))
      (is (fail? (tree "\"a/\"b/")))
      (is (fail? (tree "\"/a\"b/")))
      (is (fail? (tree "\"a/b\"/"))))

    (testing "valid match"
      (is (= (tree "") [:EXPR]))
      (is (= (tree "'a'") [:EXPR [:VAR_QUOT "a"]]))
      (is (= (tree "\"a\"") [:EXPR [:VAR_QUOT "a"]]))
      (is (= (tree "/a/") [:EXPR [:UNCLEAR "a"]]))

      (is (= (tree "'a'\"a\"") [:EXPR [:VAR_QUOT "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "\"a\"'a'") [:EXPR [:VAR_QUOT "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "/a/'a'") [:EXPR [:UNCLEAR "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "'a'/a/") [:EXPR [:VAR_QUOT "a"] [:UNCLEAR "a"]]))
      (is (= (tree "/a/\"a\"") [:EXPR [:UNCLEAR "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "\"a\"/a/") [:EXPR [:VAR_QUOT "a"] [:UNCLEAR "a"]])))
    

    (testing "valid characters"
      (is (= (trees "'6×2=24÷2'") '([:EXPR [:VAR_QUOT "6×2=24÷2"]])))
      (is (= (trees "\"x_1\" \"…\" \"x_2|v|+1\"")
             '([:EXPR [:VAR_QUOT "x_1"] [:VAR_QUOT "…"]
                [:VAR_QUOT "x_2|v|+1"]])))))
  
  (testing "Parentheses"
    (testing "incomplete match"
      (is (fail? (tree "("))) (is (fail? (tree ")")))
      (is (fail? (tree "["))) (is (fail? (tree "]")))
      (is (fail? (tree "{"))) (is (fail? (tree "}")))

      (is (fail? (tree ")(")))
      (is (fail? (tree "][")))
      (is (fail? (tree "}{"))))

    (testing "invalid term"
      ;; ? maybe this has a use-case
      (is (fail? (tree "[]"))))
    
    (testing "interfering matches"
      (is (fail? (tree "([)]")))
      (is (fail? (tree "[(])")))
      (is (fail? (tree "({)}")))
      (is (fail? (tree "{(})")))
      (is (fail? (tree "[{]}")))
      (is (fail? (tree "[(])"))))

    (testing "valid empty pairs"
      (is (= (trees "()") [[:EXPR [:FORM]]]))
      (is (= (trees "{}") [[:EXPR [:SEQRE [:EXPR]]]]))

      (is (= (trees "(){}") [[:EXPR [:FORM] [:SEQRE [:EXPR]]]])))
    
    (testing "valid nested pairs"
      (is (= (trees "(())") [[:EXPR [:FORM [:FORM]]]]))
      (is (= (trees "(((()))()(()))()(()())")
             [[:EXPR [:FORM [:FORM [:FORM [:FORM]]] [:FORM] [:FORM [:FORM]]]
               [:FORM] [:FORM [:FORM] [:FORM]]]]))
      (is (= (trees "{{}}") [[:EXPR [:SEQRE [:EXPR [:SEQRE [:EXPR]]]]]]))
      (is (= (trees "{{{{}}}{}{{}}}{}{{}{}}")
             [[:EXPR [:SEQRE [:EXPR [:SEQRE [:EXPR [:SEQRE [:EXPR [:SEQRE [:EXPR]]]]]]
                              [:SEQRE [:EXPR]] [:SEQRE [:EXPR [:SEQRE [:EXPR]]]]]]
               [:SEQRE [:EXPR]] [:SEQRE [:EXPR [:SEQRE [:EXPR]] [:SEQRE [:EXPR]]]]]]))

      (is (= (trees "({})") [[:EXPR [:FORM [:SEQRE [:EXPR]]]]]))
      (is (= (trees "{()}") [[:EXPR [:SEQRE [:EXPR [:FORM]]]]]))
      (is (= (trees "({({})}(){()})(){(){}}")
             [[:EXPR [:FORM [:SEQRE [:EXPR [:FORM [:SEQRE [:EXPR]]]]] [:FORM]
                      [:SEQRE [:EXPR [:FORM]]]]
               [:FORM] [:SEQRE [:EXPR [:FORM] [:SEQRE [:EXPR]]]]]]))))
  
  (testing "Allowed spacing"
    (testing "between literals"
      (is (= (trees ":1") (trees " :1") (trees ":1 ")
             (trees " :1 ") '([:EXPR [:SYMBOL ":1"]])))
      (is (= (trees ":m") (trees " :m") (trees ":m ")
             (trees " :m ") '([:EXPR [:SYMBOL ":m"]])))
      (is (= (trees "a") (trees " a") (trees "a ")
             (trees " a ") '([:EXPR [:VAR "a"]])))

      (is (= (trees ":1a") '([:EXPR [:SYMBOL ":1a"]])))
      (is (= (trees ":na") '([:EXPR [:SYMBOL ":na"]])))
      (is (fail? (tree "a:1")))
      (is (fail? (tree "a:n")))
      ;; ? allow this
      (is (fail? (tree ":1:n")))
      (is (fail? (tree ":n:1")))
      (is (= (trees ":1 a") '([:EXPR [:SYMBOL ":1"] [:VAR "a"]])))
      (is (= (trees "a :1") '([:EXPR [:VAR "a"] [:SYMBOL ":1"]])))

      (is (= (trees "a :1b") '([:EXPR [:VAR "a"] [:SYMBOL ":1b"]])))
      (is (= (trees "a :nb") '([:EXPR [:VAR "a"] [:SYMBOL ":nb"]])))
      (is (fail? (tree "a:1 b")))
      (is (fail? (tree "a:n b")))

      (is (= (trees ":1b :1") '([:EXPR [:SYMBOL ":1b"] [:SYMBOL ":1"]])))
      (is (fail? (tree ":1 b:1")))
      (is (= (trees ":nb :n") '([:EXPR [:SYMBOL ":nb"] [:SYMBOL ":n"]])))
      (is (fail? (tree ":n b:n")))
      (is (fail? (tree ":1 :n:m")))
      (is (fail? (tree ":1:n :m")))
      (is (= (trees "a :1 b") '([:EXPR [:VAR "a"] [:SYMBOL ":1"] [:VAR "b"]])))
      (is (= (trees "a b :1") '([:EXPR [:VAR "a"] [:VAR "b"] [:SYMBOL ":1"]])))
      (is (= (trees ":1 a b") '([:EXPR [:SYMBOL ":1"] [:VAR "a"] [:VAR "b"]])))

      (is (fail? (trees "[]:n")))
      (is (fail? (trees "[]:mm")))
      (is (fail? (trees "[]:nuim")))
      ;; symbol =/= formdna
      (is (= (trees ":11") '([:EXPR [:SYMBOL ":11"]])))
      (is (= (trees ":mm") '([:EXPR [:SYMBOL ":mm"]])))
      (is (= (trees ":nuim") '([:EXPR [:SYMBOL ":nuim"]]))))
    
    (testing "between literals and forms"
      (is (= (trees "( a )") (trees "( a)") (trees "(a )") (trees "(a)")
             '([:EXPR [:FORM [:VAR "a"]]])))
      (is (= (trees "() a") (trees "()a") '([:EXPR [:FORM] [:VAR "a"]])))
      (is (= (trees "a ()") (trees "a()") '([:EXPR [:VAR "a"] [:FORM]])))
      (is (= (trees "() :1") (trees "():1") '([:EXPR [:FORM] [:SYMBOL ":1"]])))
      (is (= (trees ":1 ()") (trees ":1()") '([:EXPR [:SYMBOL ":1"] [:FORM]])))
      (is (= (trees "() :m") (trees "():m") '([:EXPR [:FORM] [:SYMBOL ":m"]])))
      (is (= (trees ":m ()") (trees ":m()") '([:EXPR [:SYMBOL ":m"] [:FORM]])))

      (is (= (trees "a () b") (trees "a() b") (trees "a ()b")
             (trees "a()b") '([:EXPR [:VAR "a"] [:FORM] [:VAR "b"]])))
      (is (= (trees ":1 () :m") (trees ":1() :m") (trees ":1 ():m")
             (trees ":1():m") '([:EXPR [:SYMBOL ":1"] [:FORM] [:SYMBOL ":m"]])))

      (is (= (trees "() a b")
             (trees "()a b") '([:EXPR [:FORM] [:VAR "a"] [:VAR "b"]])))
      (is (= (trees "() :1 :m")
             (trees "():1 :m") '([:EXPR
                                  [:FORM] [:SYMBOL ":1"] [:SYMBOL ":m"]])))
      (is (= (trees "a b ()")
             (trees "a b()") '([:EXPR [:VAR "a"] [:VAR "b"] [:FORM]])))
      (is (= (trees ":1 :m ()")
             (trees ":1 :m()") '([:EXPR
                                  [:SYMBOL ":1"] [:SYMBOL ":m"] [:FORM]])))

      (is (= (trees "( a ( b ) c )") (trees "(a(b)c)")
             '([:EXPR [:FORM [:VAR "a"] [:FORM [:VAR "b"]] [:VAR "c"]]])))

      (is (= (trees ":u'u' :i u():u b")
             '([:EXPR [:SYMBOL ":u"] [:VAR_QUOT "u"] [:SYMBOL ":i"]
                [:VAR "u"] [:FORM] [:SYMBOL ":u"] [:VAR "b"]])))))

  (testing "Symbols"
    (is (= (trees ":x") '([:EXPR [:SYMBOL ":x"]])))
    (is (= (trees "[:x]") '([:EXPR [:OPERATOR [:SYMBOL ":x"]]])))))


(def ->nmui {:sort-code calc/nmui-code})

(deftest formula->expr-test
  (testing "Correctness of single transformations"
    (is (= (read-expr "") nil))

    (is (= (read-expr "()") '()))

    (is (= (read-expr "{}")
           (read-expr "{@}") '[:seq-re :<r nil]))
    (is (= (read-expr "{..@}") '[:seq-re :<..r nil]))
    (is (= (read-expr "{..@.}") '[:seq-re :<..r. nil]))
    (is (= (read-expr "{@_}") '[:seq-re :<r_ nil]))
    (is (= (read-expr "{..@_}") '[:seq-re :<..r_ nil]))
    (is (= (read-expr "{..@._}") '[:seq-re :<..r._ nil]))
    (is (= (read-expr "{@~}") '[:seq-re :<r' nil]))
    (is (= (read-expr "{..@~}") '[:seq-re :<..r' nil]))
    (is (= (read-expr "{..@~.}") '[:seq-re :<..r'. nil]))
    (is (= (read-expr "{@~_}") '[:seq-re :<r'_ nil]))
    (is (= (read-expr "{..@~_}") '[:seq-re :<..r'_ nil]))
    (is (= (read-expr "{..@~._}") '[:seq-re :<..r'._ nil]))
    (is (= (read-expr "{2r|}") '[:seq-re :<..r nil]))
    (is (= (read-expr "{2r+1|}") '[:seq-re :<..r. nil]))
    (is (= (read-expr "{alt|}") '[:seq-re :<r' nil]))
    (is (= (read-expr "{open|}") '[:seq-re :<r_ nil]))

    (is (= (read-expr "{,}")
           (read-expr "{@ ,}") '[:seq-re :<r nil nil]))
    (is (= (read-expr "{,,}")
           (read-expr "{@ ,,}") '[:seq-re :<r nil nil nil]))
    (is (= (read-expr "{2r|,}") '[:seq-re :<..r nil nil]))
    (is (= (read-expr "{2r|,,}") '[:seq-re :<..r nil nil nil]))

    (is (= (read-expr "a") "a"))
    (is (= (read-expr "apple") "apple"))
    (is (= (read-expr "'apple juice'") "apple juice"))

    (is (= (read-expr "/some smell/") '[:uncl "some smell"]))

    (is (= (read-expr ":m") (read-expr ->nmui ":m") :m))
    (is (= (read-expr ":1") (read-expr ->nmui ":2") :1))
    (is (= (read-expr "::m") (read-expr ->nmui "::m") [:fdna [] [:m]]))
    (is (= (read-expr "::1") (read-expr ->nmui "::2") [:fdna [] [:u]]))
    (is (= (read-expr "::nuim")
           (read-expr "::0123") '[:fdna ["v__0"] [:n :u :i :m]]))
    (is (= (read-expr ->nmui "::nuim")
           (read-expr ->nmui "::0231") '[:fdna ["v__0"] [:n :i :m :u]]))
    (is (= (read-expr ->nmui "::2310302310012021221111113232332212132133023103213021320233011023")
           '[:fdna ["v__0" "v__1" "v__2"]
             [:u :m :n :i  :m :n :m :n  :u :u :m :n  :i :u :i :n 
              :m :m :i :u  :n :i :m :u  :n :u :m :i  :u :i :i :m 
              :i :u :m :n  :i :n :m :i  :m :u :i :n  :i :n :u :u 
              :u :m :m :u  :i :i :u :u  :i :u :u :i  :m :m :m :m]]))

    (is (= (read-expr "[:fdna []::m]")
           (read-expr "[:fdna []::3]") [:fdna [] [:m]]))
    (is (= (read-expr ->nmui "[:fdna []::i]")
           (read-expr ->nmui "[:fdna []::3]") [:fdna [] [:i]]))
    (is (= (read-expr "[:fdna [a]::nuim]")
           (read-expr "[:fdna [a]::0123]") '[:fdna ["a"] [:n :u :i :m]]))
    (is (= (read-expr ->nmui "[:fdna [a]::nuim]")
           (read-expr ->nmui "[:fdna [a]::0231]") '[:fdna ["a"] [:n :i :m :u]]))
    (is (= (read-expr "[:fdna [a,'z_3']::nuimmniiiunmmunu]")
           '[:fdna ["a" "z_3"] [:n :u :i :m
                                :m :n :i :i
                                :i :u :n :m
                                :m :u :n :u]]))
    (is (= (read-expr ->nmui "[:fdna [a,b,c]::2310302310012021221111113232332212132133023103213021320233011023]")
           '[:fdna ["a" "b" "c"]
             [:u :m :n :i  :m :n :m :n  :u :u :m :n  :i :u :i :n 
              :m :m :i :u  :n :i :m :u  :n :u :m :i  :u :i :i :m 
              :i :u :m :n  :i :n :m :i  :m :u :i :n  :i :n :u :u 
              :u :m :m :u  :i :i :u :u  :i :u :u :i  :m :m :m :m]])))

  (testing "Correctness of related transformations"
    (testing "of the same type"
      (is (= (read-expr "()()") '[:- () ()]))
      (is (= (read-expr "()()()()") '[:- () () () ()]))
      (is (= (read-expr "{}{}") '[:- [:seq-re :<r nil] [:seq-re :<r nil]]))
      (is (= (read-expr "a a") '[:- "a" "a"]))
      (is (= (read-expr "'a ball''a ball'") '[:- "a ball" "a ball"]))
      (is (= (read-expr "/b 4//b 4/") '[:- [:uncl "b 4"] [:uncl "b 4"]]))
      (is (= (read-expr ":m :m") '[:- :m :m]))
      (is (= (read-expr ":3 :3") '[:- :3 :3]))
      ;; ? read-expr as fdna instead
      (is (= (read-expr "[:fdna []::m] [:fdna []::m]")
             '[:- [:fdna [] [:m]] [:fdna [] [:m]]]))
      (is (= (read-expr "[:fdna [a]::nuim] [:fdna [a]::nuim]")
             '[:-
               [:fdna ["a"] [:n :u :i :m]]
               [:fdna ["a"] [:n :u :i :m]]]))))
  
  (testing "Correctness of nested transformations"
    (testing "of the same type"
      (is (= (read-expr "(())") '(())))
      (is (= (read-expr "(((()))(()())())") '(((())) (() ()) ())))
      (is (= (read-expr "{{}}") '[:seq-re :<r [:seq-re :<r nil]]))
      (is (= (read-expr "{{{{}}}{{}{}}{}}")
             '[:seq-re :<r
               [:-
                [:seq-re :<r [:seq-re :<r [:seq-re :<r nil]]]
                [:seq-re :<r [:- [:seq-re :<r nil] [:seq-re :<r nil]]]
                [:seq-re :<r nil]]]))
      (is (= (read-expr "{{,{},{,}},{{}},{{},,},,,}")
             '[:seq-re :<r
               [:seq-re :<r nil [:seq-re :<r nil] [:seq-re :<r nil nil]]
               [:seq-re :<r [:seq-re :<r nil]]
               [:seq-re :<r [:seq-re :<r nil] nil nil] nil nil nil])))
    
    (testing "of different types"
      (is (= (read-expr "(:u 'x_1' [:fdna ['x_1']::nmui] /はあ/ {alt|2r|} :2)")
             '(:u "x_1" [:fdna ["x_1"] [:n :m :u :i]] [:uncl "はあ"]
                  [:seq-re :<..r' nil] :2)))
      (is (= (read-expr "{:u 'x_1', [:fdna ['x_1']::nmui], /はあ/ {alt|2r|}, :2}")
             '[:seq-re :<r [:- :u "x_1"] [:fdna ["x_1"] [:n :m :u :i]]
               [:- [:uncl "はあ"] [:seq-re :<..r' nil]] :2]))
      (is (= (read-expr "(a(b(c)))") '("a" ("b" ("c")))))
      (is (= (read-expr "(((a)b)c)") '((("a") "b") "c")))
      (is (= (read-expr "((a (b :u))c d(e):2 f)g")
             '[:- (("a" ("b" :u)) "c" "d" ("e") :2 "f") "g"]))
      (is (= (read-expr "{a,b,c}") '[:seq-re :<r "a" "b" "c"]))
      (is (= (read-expr "{(a {b,(c),:u d}),{e,f :2}}g")
             '[:- [:seq-re :<r ["a" [:seq-re :<r "b" ["c"] [:- :u "d"]]]
                   [:seq-re :<r "e" [:- "f" :2]]] "g"]))

      (is (= (read-expr "{L,R} {2r+1|L,E,R}")
             '[:- [:seq-re :<r "L" "R"] [:seq-re :<..r. "L" "E" "R"]]))
      (is (= (read-expr "{alt|L,R}{alt|R,L}")
             '[:- [:seq-re :<r' "L" "R"] [:seq-re :<r' "R" "L"]]))
      (is (= (read-expr "(({L,E,R}{E,R,L}{L,R,E})(L E R))")
             '(([:seq-re :<r "L" "E" "R"]
                [:seq-re :<r "E" "R" "L"]
                [:seq-re :<r "L" "R" "E"]) ("L" "E" "R"))))
      (is (= (read-expr "((/green apple/)/red apple/)")
             '(([:uncl "green apple"]) [:uncl "red apple"])))
      (is (= (read-expr "{2r+1|/deeming/,/telling/,/understanding/}")
             '[:seq-re :<..r.
               [:uncl "deeming"]
               [:uncl "telling"]
               [:uncl "understanding"]]))))

  (testing "Operator parsing"
    (testing "predefined operators"
      (is (fail? (read-expr "[:uncl []()]"))) ;; ? should this work
      (is (= (read-expr "[:uncl foo bar]") '[:uncl "foo bar"]))

      (is (fail? (read-expr "[:fdna [] [:m]]"))) ;; ? should this work
      (is (= (read-expr "[:fdna [] ::m]") '[:fdna [] [:m]]))
      (is (= (read-expr "[:fdna [a] ::mnui]") '[:fdna ["a"] [:m :n :u :i]]))
      (is (fail?
           (read-expr "[:fdna [a b] ::mnuimmniiumnnini]"))) ;; ? should this work
      (is (= (read-expr "[:fdna [a, b] ::mnuimmniiumnnini]")
             '[:fdna ["a" "b"] [:m :n :u :i
                                :m :m :n :i
                                :i :u :m :n
                                :n :i :n :i]]))

      (is (= (read-expr "[:seq-re @]") '[:seq-re :<r nil]))
      (is (= (read-expr "[:seq-re @ ,]") '[:seq-re :<r nil nil]))
      (is (= (read-expr "[:seq-re ..@~._ a,b c,:u]")
             '[:seq-re :<..r'._ "a" [:- "b" "c"] :u]))

      (is (fail? (read-expr "[:mem ((a :m)) a]")))
      (is (fail? (read-expr "[:mem ]")))
      (is (= (read-expr "[:mem | ]") '[:mem [] nil]))
      (is (= (read-expr "[:mem a = (x) | (a (b))]")
             '[:mem [["a" ["x"]]] ["a" ["b"]]]))
      (is (= (read-expr "[:mem a = ((a) (b)), ((a) (b)) = :u | a]")
             '[:mem [["a" [["a"] ["b"]]] [[["a"] ["b"]] :u]] "a"]))

      (is (= (read-expr "[:tsds 011010 a,b,c]")
             '[:tsds [0 1 1 0 1 0] "a" "b" "c"]))
      (is (fail? (read-expr "[:tsds 011010 a,c]")))
      (is (fail? (read-expr "[:tsds 011010 a,b,c,d]")))
      (is (fail? (read-expr "[:tsds 01100 a,b,c]")))
      (is (fail? (read-expr "[:tsds 0110001 a,b,c]")))
      (is (fail? (read-expr "[:tsds 012100 a,b,c]")))
      (is (= (read-expr "[:tsds 000000 :u,b c,(() c)]")
             '[:tsds [0 0 0 0 0 0] :u [:- "b" "c"] [[] "c"]]))

      ;; Expression symbols and Operators need not be known
      (is (= (read-expr ":foo") :foo))
      (is (= (read-expr "[:foo x y]") '[:foo "x" "y"]))
      (is (= (read-expr "[:x a () [:y]]") '[:x "a" [] [:y]])))))

(deftest const->formula-test
  (testing "Correct formula output"
    (is (= (print-const :m) ":m"))
    (is (thrown? clojure.lang.ExceptionInfo (print-const :P)))))

(deftest dna->formula-test
  (testing "Correct formula output"
    (is (= (print-dna [:n :u :i :m]) "::nuim"))
    (is (thrown? clojure.lang.ExceptionInfo (print-dna [])))
    (is (thrown? clojure.lang.ExceptionInfo (print-dna [:n :i :m])))
    ;; Still valid according to spec:
    ;; ? should the spec be stricter
    (is (= (print-dna [:n :a :i :m]) "::naim"))
    ;; However, this throws, although still spec-valid:
    ;; ? should this be allowed
    (is (thrown? java.lang.ClassCastException (print-dna [0 1 2 3])))))

(deftest expr->formula-test
  (testing "Correct formula output"
    (testing "simple expressions"
      (is (= (print-expr []) "()"))
      (is (= (print-expr 'foo) "foo"))
      (is (= (print-expr "foo bar") "'foo bar'"))
      (is (= (print-expr :i) ":i"))
      (is (= (print-expr :3) ":3"))
      (is (= (print-expr [:-]) ""))
      (is (= (print-expr [:- 'a :i nil []]) "a :i ()"))
      (is (= (print-expr [:seq-re :<..r'._ nil]) "{..@~._ }"))
      (is (= (print-expr [:fdna [] [:u]]) "[:fdna [] ::u]")) ;; syntax shortcut?
      (is (= (print-expr [:uncl "!"]) "[:uncl !]"))
      (is (= (print-expr [:uncl "hello world"]) "[:uncl hello world]"))
      (is (= (print-expr [:mem [] nil]) "[:mem  | ]"))
      (is (= (print-expr [:mem [['x :m] ["why you?" :u]] ['x "why"]])
             "[:mem x = :m, 'why you?' = :u | (x why)]")))

    (testing "compound expressions"
      (is (= (print-expr [nil [[] nil [[] [[nil]]]]])
             "((() (() (()))))"))
      (is (= (print-expr [:- nil [:- [:- nil []] [:-] nil] nil])
             "()"))
      (is (= (print-expr [:seq-re :<r [:- 'a ['b]] [nil] nil [:- nil 'c]])
             "{@ a (b), (), , c}"))
      (is (= (print-expr [:fdna ['a "my var"]
                          [:m :i :u :n  :i :m :n :u  :u :n :m :i  :n :u :i :m]])
             "[:fdna [a, 'my var'] ::miunimnuunminuim]"))
      (is (= (print-expr [:mem [[['x ['y]] [:- nil [] [['x]]]]] [:- ['x ['y]]]])
             "[:mem (x (y)) = () ((x)) | (x (y))]"))
      (is (= (print-expr [ nil [[:seq-re :<..r' 'a [:seq-re :<r_ nil] 'b]]
                          [:seq-re :<..r. [:fdna [] [:u]]]])
             "(({..@~ a, {@_ }, b}) {..@. [:fdna [] ::u]})"))
      (is (= (print-expr [:- [[:mem [] [:mem [["foo" "bar"] ["bar" :m]] nil]]
                              [:mem [[[:seq-re :<r nil] nil]] [:uncl "hey x"]]]])
             "([:mem  | [:mem foo = bar, bar = :m | ]] [:mem {@ } =  | [:uncl hey x]])")))
    
    (testing "input validation"
      ;; Expression symbols and Operators need not be known
      (is (= (print-expr :foo) ":foo"))
      (is (= (print-expr [:foo]) "(:foo)"))
      (is (= (print-expr [:- [:- :foo]]) ":foo"))
      ;; Known operators need to be validated
      (is (thrown? clojure.lang.ExceptionInfo
                   (print-expr [:uncl ""])))
      (is (thrown? clojure.lang.ExceptionInfo
                   (print-expr [:mem [[]] nil])))))

  (testing "Equal data when parsing formula output"
    (is (let [expr [:seq-re :<..r'._ nil]]
          (= (read-expr (print-expr expr)) expr)))
    (is (let [fml "[:mem a = ((a) (b)), ((a) (b)) = :u | a]"]
          (= (print-expr (read-expr fml)) fml))))) 


