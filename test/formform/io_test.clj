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
      (is (= (trees ":M") (trees " :M") (trees ":M ")
             (trees " :M ") '([:EXPR [:SYMBOL ":M"]])))
      (is (= (trees "a") (trees " a") (trees "a ")
             (trees " a ") '([:EXPR [:VAR "a"]])))

      (is (= (trees ":1a") '([:EXPR [:SYMBOL ":1a"]])))
      (is (= (trees ":Na") '([:EXPR [:SYMBOL ":Na"]])))
      (is (fail? (tree "a:1")))
      (is (fail? (tree "a:N")))
      ;; ? allow this
      (is (fail? (tree ":1:N")))
      (is (fail? (tree ":N:1")))
      (is (= (trees ":1 a") '([:EXPR [:SYMBOL ":1"] [:VAR "a"]])))
      (is (= (trees "a :1") '([:EXPR [:VAR "a"] [:SYMBOL ":1"]])))

      (is (= (trees "a :1b") '([:EXPR [:VAR "a"] [:SYMBOL ":1b"]])))
      (is (= (trees "a :Nb") '([:EXPR [:VAR "a"] [:SYMBOL ":Nb"]])))
      (is (fail? (tree "a:1 b")))
      (is (fail? (tree "a:N b")))

      (is (= (trees ":1b :1") '([:EXPR [:SYMBOL ":1b"] [:SYMBOL ":1"]])))
      (is (fail? (tree ":1 b:1")))
      (is (= (trees ":Nb :N") '([:EXPR [:SYMBOL ":Nb"] [:SYMBOL ":N"]])))
      (is (fail? (tree ":N b:N")))
      (is (fail? (tree ":1 :N:M")))
      (is (fail? (tree ":1:N :M")))
      (is (= (trees "a :1 b") '([:EXPR [:VAR "a"] [:SYMBOL ":1"] [:VAR "b"]])))
      (is (= (trees "a b :1") '([:EXPR [:VAR "a"] [:VAR "b"] [:SYMBOL ":1"]])))
      (is (= (trees ":1 a b") '([:EXPR [:SYMBOL ":1"] [:VAR "a"] [:VAR "b"]])))

      (is (fail? (trees "[]:N")))
      (is (fail? (trees "[]:MM")))
      (is (fail? (trees "[]:NUIM")))
      ;; symbol =/= formDNA
      (is (= (trees ":11") '([:EXPR [:SYMBOL ":11"]])))
      (is (= (trees ":MM") '([:EXPR [:SYMBOL ":MM"]])))
      (is (= (trees ":NUIM") '([:EXPR [:SYMBOL ":NUIM"]]))))
      
    (testing "between literals and forms"
      (is (= (trees "( a )") (trees "( a)") (trees "(a )") (trees "(a)")
             '([:EXPR [:FORM [:VAR "a"]]])))
      (is (= (trees "() a") (trees "()a") '([:EXPR [:FORM] [:VAR "a"]])))
      (is (= (trees "a ()") (trees "a()") '([:EXPR [:VAR "a"] [:FORM]])))
      (is (= (trees "() :1") (trees "():1") '([:EXPR [:FORM] [:SYMBOL ":1"]])))
      (is (= (trees ":1 ()") (trees ":1()") '([:EXPR [:SYMBOL ":1"] [:FORM]])))
      (is (= (trees "() :M") (trees "():M") '([:EXPR [:FORM] [:SYMBOL ":M"]])))
      (is (= (trees ":M ()") (trees ":M()") '([:EXPR [:SYMBOL ":M"] [:FORM]])))

      (is (= (trees "a () b") (trees "a() b") (trees "a ()b")
             (trees "a()b") '([:EXPR [:VAR "a"] [:FORM] [:VAR "b"]])))
      (is (= (trees ":1 () :M") (trees ":1() :M") (trees ":1 ():M")
             (trees ":1():M") '([:EXPR [:SYMBOL ":1"] [:FORM] [:SYMBOL ":M"]])))

      (is (= (trees "() a b")
             (trees "()a b") '([:EXPR [:FORM] [:VAR "a"] [:VAR "b"]])))
      (is (= (trees "() :1 :M")
             (trees "():1 :M") '([:EXPR
                                  [:FORM] [:SYMBOL ":1"] [:SYMBOL ":M"]])))
      (is (= (trees "a b ()")
             (trees "a b()") '([:EXPR [:VAR "a"] [:VAR "b"] [:FORM]])))
      (is (= (trees ":1 :M ()")
             (trees ":1 :M()") '([:EXPR
                                  [:SYMBOL ":1"] [:SYMBOL ":M"] [:FORM]])))

      (is (= (trees "( a ( b ) c )") (trees "(a(b)c)")
             '([:EXPR [:FORM [:VAR "a"] [:FORM [:VAR "b"]] [:VAR "c"]]])))

      (is (= (trees ":U'U' :I u():U b")
             '([:EXPR [:SYMBOL ":U"] [:VAR_QUOT "U"] [:SYMBOL ":I"]
                [:VAR "u"] [:FORM] [:SYMBOL ":U"] [:VAR "b"]])))))

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

    (is (= (read-expr ":M") (read-expr ->nmui ":M") :M))
    (is (= (read-expr ":1") (read-expr ->nmui ":2") :1))
    (is (= (read-expr "::M") (read-expr ->nmui "::M") [:fdna [] [:M]]))
    (is (= (read-expr "::1") (read-expr ->nmui "::2") [:fdna [] [:U]]))
    (is (= (read-expr "::NUIM")
           (read-expr "::0123") '[:fdna ["v__0"] [:N :U :I :M]]))
    (is (= (read-expr ->nmui "::NUIM")
           (read-expr ->nmui "::0231") '[:fdna ["v__0"] [:I :N :U :M]]))
    (is (= (read-expr ->nmui "::2310302310012021221111113232332212132133023103213021320233011023")
           '[:fdna ["v__0" "v__1" "v__2"]
             [:I :N :U :M  :M :M :U :I  :I :U :M :I  :U :N :I :M
              :N :M :N :M  :M :U :I :N  :U :I :N :I  :U :U :N :M
              :I :I :U :U  :M :U :U :M  :M :M :M :M  :U :I :I :U
              :N :I :I :M  :U :I :N :M  :N :I :U :U  :U :M :N :I]]))

    (is (= (read-expr "[:fdna []::M]")
           (read-expr "[:fdna []::3]") [:fdna [] [:M]]))
    (is (= (read-expr ->nmui "[:fdna []::I]")
           (read-expr ->nmui "[:fdna []::3]") [:fdna [] [:I]]))
    (is (= (read-expr "[:fdna [a]::NUIM]")
           (read-expr "[:fdna [a]::0123]") '[:fdna ["a"] [:N :U :I :M]]))
    (is (= (read-expr ->nmui "[:fdna [a]::NUIM]")
           (read-expr ->nmui "[:fdna [a]::0231]") '[:fdna ["a"] [:I :N :U :M]]))
    (is (= (read-expr "[:fdna [a,'z_3']::NUIMMNIIIUNMMUNU]")
           '[:fdna ["a" "z_3"] [:N :U :I :M
                                :M :N :I :I
                                :I :U :N :M
                                :M :U :N :U]]))
    (is (= (read-expr ->nmui "[:fdna [a,b,c]::2310302310012021221111113232332212132133023103213021320233011023]")
           '[:fdna ["a" "b" "c"]
             [:I :N :U :M  :M :M :U :I  :I :U :M :I  :U :N :I :M
              :N :M :N :M  :M :U :I :N  :U :I :N :I  :U :U :N :M
              :I :I :U :U  :M :U :U :M  :M :M :M :M  :U :I :I :U
              :N :I :I :M  :U :I :N :M  :N :I :U :U  :U :M :N :I]])))

  (testing "Correctness of related transformations"
    (testing "of the same type"
      (is (= (read-expr "()()") '[:- () ()]))
      (is (= (read-expr "()()()()") '[:- () () () ()]))
      (is (= (read-expr "{}{}") '[:- [:seq-re :<r nil] [:seq-re :<r nil]]))
      (is (= (read-expr "a a") '[:- "a" "a"]))
      (is (= (read-expr "'a ball''a ball'") '[:- "a ball" "a ball"]))
      (is (= (read-expr "/b 4//b 4/") '[:- [:uncl "b 4"] [:uncl "b 4"]]))
      (is (= (read-expr ":M :M") '[:- :M :M]))
      (is (= (read-expr ":3 :3") '[:- :3 :3]))
      ;; ? read-expr as fdna instead
      (is (= (read-expr "[:fdna []::M] [:fdna []::M]")
             '[:- [:fdna [] [:M]] [:fdna [] [:M]]]))
      (is (= (read-expr "[:fdna [a]::NUIM] [:fdna [a]::NUIM]")
             '[:-
               [:fdna ["a"] [:N :U :I :M]]
               [:fdna ["a"] [:N :U :I :M]]]))))
      
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
      (is (= (read-expr "(:U 'x_1' [:fdna ['x_1']::NMUI] /はあ/ {alt|2r|} :2)")
             '(:U "x_1" [:fdna ["x_1"] [:N :M :U :I]] [:uncl "はあ"]
                  [:seq-re :<..r' nil] :2)))
      (is (= (read-expr "{:U 'x_1', [:fdna ['x_1']::NMUI], /はあ/ {alt|2r|}, :2}")
             '[:seq-re :<r [:- :U "x_1"] [:fdna ["x_1"] [:N :M :U :I]]
               [:- [:uncl "はあ"] [:seq-re :<..r' nil]] :2]))
      (is (= (read-expr "(a(b(c)))") '("a" ("b" ("c")))))
      (is (= (read-expr "(((a)b)c)") '((("a") "b") "c")))
      (is (= (read-expr "((a (b :U))c d(e):2 f)g")
             '[:- (("a" ("b" :U)) "c" "d" ("e") :2 "f") "g"]))
      (is (= (read-expr "{a,b,c}") '[:seq-re :<r "a" "b" "c"]))
      (is (= (read-expr "{(a {b,(c),:U d}),{e,f :2}}g")
             '[:- [:seq-re :<r ["a" [:seq-re :<r "b" ["c"] [:- :U "d"]]]
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

      (is (fail? (read-expr "[:fdna [] [:M]]"))) ;; ? should this work
      (is (= (read-expr "[:fdna [] ::M]") '[:fdna [] [:M]]))
      (is (= (read-expr "[:fdna [a] ::MNUI]") '[:fdna ["a"] [:M :N :U :I]]))
      (is (fail?
           (read-expr "[:fdna [a b] ::MNUIMMNIIUMNNINI]"))) ;; ? should this work
      (is (= (read-expr "[:fdna [a, b] ::MNUIMMNIIUMNNINI]")
             '[:fdna ["a" "b"] [:M :N :U :I
                                :M :M :N :I
                                :I :U :M :N
                                :N :I :N :I]]))

      (is (= (read-expr "[:seq-re @]") '[:seq-re :<r nil]))
      (is (= (read-expr "[:seq-re @ ,]") '[:seq-re :<r nil nil]))
      (is (= (read-expr "[:seq-re ..@~._ a,b c,:U]")
             '[:seq-re :<..r'._ "a" [:- "b" "c"] :U]))

      (is (fail? (read-expr "[:mem ((a :M)) a]")))
      (is (fail? (read-expr "[:mem ]")))
      (is (= (read-expr "[:mem | ]") '[:mem [] nil]))
      (is (= (read-expr "[:mem a = (x) | (a (b))]")
             '[:mem [["a" ["x"]]] ["a" ["b"]]]))
      (is (= (read-expr "[:mem a = ((a) (b)), ((a) (b)) = :U | a]")
             '[:mem [["a" [["a"] ["b"]]] [[["a"] ["b"]] :U]] "a"]))

      ;; Expression symbols and Operators need not be known
      (is (= (read-expr ":foo") :foo))
      (is (= (read-expr "[:foo x y]") '[:foo "x" "y"]))
      (is (= (read-expr "[:x a () [:y]]") '[:x "a" [] [:y]])))))
      
(deftest const->formula-test
  (testing "Correct formula output"
    (is (= (print-const :M) ":M"))
    (is (thrown? clojure.lang.ExceptionInfo (print-const :P)))))

(deftest dna->formula-test
  (testing "Correct formula output"
    (is (= (print-dna [:N :U :I :M]) "::NUIM"))
    (is (thrown? clojure.lang.ExceptionInfo (print-dna [])))
    (is (thrown? clojure.lang.ExceptionInfo (print-dna [:N :I :M])))
    ;; Still valid according to spec:
    ;; ? should the spec be stricter
    (is (= (print-dna [:N :A :I :M]) "::NAIM"))
    ;; However, this throws, although still spec-valid:
    ;; ? should this be allowed
    (is (thrown? java.lang.ClassCastException (print-dna [0 1 2 3])))))

(deftest expr->formula-test
  (testing "Correct formula output"
    (testing "simple expressions"
      (is (= (print-expr []) "()"))
      (is (= (print-expr 'foo) "foo"))
      (is (= (print-expr "foo bar") "'foo bar'"))
      (is (= (print-expr :I) ":I"))
      (is (= (print-expr :3) ":3"))
      (is (= (print-expr [:-]) ""))
      (is (= (print-expr [:- 'a :I nil []]) "a :I ()"))
      (is (= (print-expr [:seq-re :<..r'._ nil]) "{..@~._ }"))
      (is (= (print-expr [:fdna [] [:U]]) "[:fdna [] ::U]")) ;; syntax shortcut?
      (is (= (print-expr [:uncl "!"]) "[:uncl !]"))
      (is (= (print-expr [:uncl "hello world"]) "[:uncl hello world]"))
      (is (= (print-expr [:mem [] nil]) "[:mem  | ]"))
      (is (= (print-expr [:mem [['x :M] ["why you?" :U]] ['x "why"]])
             "[:mem x = :M, 'why you?' = :U | (x why)]")))

    (testing "compound expressions"
      (is (= (print-expr [nil [[] nil [[] [[nil]]]]])
             "((() (() (()))))"))
      (is (= (print-expr [:- nil [:- [:- nil []] [:-] nil] nil])
             "()"))
      (is (= (print-expr [:seq-re :<r [:- 'a ['b]] [nil] nil [:- nil 'c]])
             "{@ a (b), (), , c}"))
      (is (= (print-expr [:fdna ['a "my var"]
                          [:M :I :U :N  :I :M :N :U  :U :N :M :I  :N :U :I :M]])
             "[:fdna [a, 'my var'] ::MIUNIMNUUNMINUIM]"))
      (is (= (print-expr [:mem [[['x ['y]] [:- nil [] [['x]]]]] [:- ['x ['y]]]])
             "[:mem (x (y)) = () ((x)) | (x (y))]"))
      (is (= (print-expr [ nil [[:seq-re :<..r' 'a [:seq-re :<r_ nil] 'b]]
                          [:seq-re :<..r. [:fdna [] [:U]]]])
             "(({..@~ a, {@_ }, b}) {..@. [:fdna [] ::U]})"))
      (is (= (print-expr [:- [[:mem [] [:mem [["foo" "bar"] ["bar" :M]] nil]]
                              [:mem [[[:seq-re :<r nil] nil]] [:uncl "hey x"]]]])
             "([:mem  | [:mem foo = bar, bar = :M | ]] [:mem {@ } =  | [:uncl hey x]])")))
      
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
    (is (let [fml "[:mem a = ((a) (b)), ((a) (b)) = :U | a]"]
          (= (print-expr (read-expr fml)) fml))))) 


