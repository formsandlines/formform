(ns formform.formula-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            ; [formform.calc :as calc]
            [formform.formula :as fml :refer :all]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]
            [instaparse.core :as insta]))


(def tree formula->parsetree)
(def trees (partial insta/parses formula->parsetree))

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
      (is (= (tree "'a'") [:EXPR [:VAR_QUOT "a"]]))
      (is (= (tree "\"a\"") [:EXPR [:VAR_QUOT "a"]]))
      (is (= (tree "/a/") [:EXPR [:UNCLEAR "a"]]))

      (is (= (tree "'a'\"a\"") [:EXPR [:VAR_QUOT "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "\"a\"'a'") [:EXPR [:VAR_QUOT "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "/a/'a'") [:EXPR [:UNCLEAR "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "'a'/a/") [:EXPR [:VAR_QUOT "a"] [:UNCLEAR "a"]]))
      (is (= (tree "/a/\"a\"") [:EXPR [:UNCLEAR "a"] [:VAR_QUOT "a"]]))
      (is (= (tree "\"a\"/a/") [:EXPR [:VAR_QUOT "a"] [:UNCLEAR "a"]]))
      )

    (testing "valid characters"
      (is (= (trees "'6×2=24÷2'") '([:EXPR [:VAR_QUOT "6×2=24÷2"]])))
      (is (= (trees "\"x_1\" \"…\" \"x_2|v|+1\"")
             '([:EXPR [:VAR_QUOT "x_1"] [:VAR_QUOT "…"] 
                [:VAR_QUOT "x_2|v|+1"]])))
      )

    )

  (testing "Parentheses"
    (testing "incomplete match"
      (is (fail? (tree "("))) (is (fail? (tree ")")))
      (is (fail? (tree "["))) (is (fail? (tree "]")))
      (is (fail? (tree "{"))) (is (fail? (tree "}")))

      (is (fail? (tree ")(")))
      (is (fail? (tree "][")))
      (is (fail? (tree "}{")))
      )

    (testing "invalid term"
      ;; ? maybe this has a use-case
      (is (fail? (tree "[]")))
      )

    (testing "interfering matches"
      (is (fail? (tree "([)]")))
      (is (fail? (tree "[(])")))
      (is (fail? (tree "({)}")))
      (is (fail? (tree "{(})")))
      (is (fail? (tree "[{]}")))
      (is (fail? (tree "[(])")))

      )

    (testing "valid empty pairs"
      (is (= (trees "()") [[:EXPR [:FORM]]]))
      (is (= (trees "{}") [[:EXPR [:SEQRE [:TERM]]]]))

      (is (= (trees "(){}") [[:EXPR [:FORM] [:SEQRE [:TERM]]]]))
      )

    (testing "valid nested pairs"
      (is (= (trees "(())") [[:EXPR [:FORM [:FORM]]]]))
      (is (= (trees "(((()))()(()))()(()())")
             [[:EXPR [:FORM [:FORM [:FORM [:FORM]]] [:FORM] [:FORM [:FORM]]]
               [:FORM] [:FORM [:FORM] [:FORM]]]]))
      (is (= (trees "{{}}") [[:EXPR [:SEQRE [:TERM [:SEQRE [:TERM]]]]]]))
      (is (= (trees "{{{{}}}{}{{}}}{}{{}{}}")
             [[:EXPR [:SEQRE [:TERM [:SEQRE [:TERM [:SEQRE [:TERM [:SEQRE [:TERM]]]]]]
                              [:SEQRE [:TERM]] [:SEQRE [:TERM [:SEQRE [:TERM]]]]]]
               [:SEQRE [:TERM]] [:SEQRE [:TERM [:SEQRE [:TERM]] [:SEQRE [:TERM]]]]]]))

      (is (= (trees "({})") [[:EXPR [:FORM [:SEQRE [:TERM]]]]]))
      (is (= (trees "{()}") [[:EXPR [:SEQRE [:TERM [:FORM]]]]]))
      (is (= (trees "({({})}(){()})(){(){}}")
             [[:EXPR [:FORM [:SEQRE [:TERM [:FORM [:SEQRE [:TERM]]]]] [:FORM]
                      [:SEQRE [:TERM [:FORM]]]]
               [:FORM] [:SEQRE [:TERM [:FORM] [:SEQRE [:TERM]]]]]]))
      )

    )

  (testing "Allowed spacing"
    (testing "between literals"
      (is (= (trees ":1") (trees " :1") (trees ":1 ")
             (trees " :1 ") '([:EXPR [:FDNA ":1"]])))
      (is (= (trees ":M") (trees " :M") (trees ":M ")
             (trees " :M ") '([:EXPR [:FDNA ":M"]])))
      (is (= (trees "a") (trees " a") (trees "a ")
             (trees " a ") '([:EXPR [:VAR "a"]])))

      (is (fail? (tree ":1a"))) (is (fail? (tree "a:1")))
      (is (fail? (tree ":Na"))) (is (fail? (tree "a:N")))
      (is (fail? (tree ":1:N"))) (is (fail? (tree ":N:1")))
      (is (= (trees ":1 a") '([:EXPR [:FDNA ":1"] [:VAR "a"]])))
      (is (= (trees "a :1") '([:EXPR [:VAR "a"] [:FDNA ":1"]])))

      (is (fail? (tree "a :1b"))) (is (fail? (tree "a:1 b")))
      (is (fail? (tree "a :Nb"))) (is (fail? (tree "a:N b")))
      (is (fail? (tree ":1 b:1"))) (is (fail? (tree ":1b :1")))
      (is (fail? (tree ":N b:N"))) (is (fail? (tree ":Nb :N")))
      (is (fail? (tree ":1 :N:M"))) (is (fail? (tree ":1:N :M")))
      (is (= (trees "a :1 b") '([:EXPR [:VAR "a"] [:FDNA ":1"] [:VAR "b"]])))
      (is (= (trees "a b :1") '([:EXPR [:VAR "a"] [:VAR "b"] [:FDNA ":1"]])))
      (is (= (trees ":1 a b") '([:EXPR [:FDNA ":1"] [:VAR "a"] [:VAR "b"]])))

      (is (fail? (trees "[]:N")))
      (is (fail? (trees "[]:MM")))
      (is (fail? (trees "[]:NUIM")))
      (is (= (trees ":11") '([:EXPR [:FDNA ":11"]])))
      (is (= (trees ":MM") '([:EXPR [:FDNA ":MM"]])))
      (is (= (trees ":NUIM") '([:EXPR [:FDNA ":NUIM"]])))
      )

    (testing "between literals and forms"
      (is (= (trees "( a )") (trees "( a)") (trees "(a )") (trees "(a)")
             '([:EXPR [:FORM [:VAR "a"]]])))
      (is (= (trees "() a") (trees "()a") '([:EXPR [:FORM] [:VAR "a"]])))
      (is (= (trees "a ()") (trees "a()") '([:EXPR [:VAR "a"] [:FORM]])))
      (is (= (trees "() :1") (trees "():1") '([:EXPR [:FORM] [:FDNA ":1"]])))
      (is (= (trees ":1 ()") (trees ":1()") '([:EXPR [:FDNA ":1"] [:FORM]])))
      (is (= (trees "() :M") (trees "():M") '([:EXPR [:FORM] [:FDNA ":M"]])))
      (is (= (trees ":M ()") (trees ":M()") '([:EXPR [:FDNA ":M"] [:FORM]])))

      (is (= (trees "a () b") (trees "a() b") (trees "a ()b")
             (trees "a()b") '([:EXPR [:VAR "a"] [:FORM] [:VAR "b"]])))
      (is (= (trees ":1 () :M") (trees ":1() :M") (trees ":1 ():M")
             (trees ":1():M") '([:EXPR [:FDNA ":1"] [:FORM] [:FDNA ":M"]])))

      (is (= (trees "() a b")
             (trees "()a b") '([:EXPR [:FORM] [:VAR "a"] [:VAR "b"]])))
      (is (= (trees "() :1 :M")
             (trees "():1 :M") '([:EXPR [:FORM] [:FDNA ":1"] [:FDNA ":M"]])))
      (is (= (trees "a b ()")
             (trees "a b()") '([:EXPR [:VAR "a"] [:VAR "b"] [:FORM]])))
      (is (= (trees ":1 :M ()")
             (trees ":1 :M()") '([:EXPR [:FDNA ":1"] [:FDNA ":M"] [:FORM]])))

      (is (= (trees "( a ( b ) c )") (trees "(a(b)c)")
             '([:EXPR [:FORM [:VAR "a"] [:FORM [:VAR "b"]] [:VAR "c"]]])))

      (is (= (trees ":U'U' :I u():U b")
             '([:EXPR [:FDNA ":U"] [:VAR_QUOT "U"] [:FDNA ":I"] 
                [:VAR "u"] [:FORM] [:FDNA ":U"] [:VAR "b"]])))

      )
    )

  )

(def ->nmui {:sort-code calc/nmui-code})

(deftest parse-test
  (testing "Correctness of single transformations"
    (is (= (parse "") '[]))

    (is (= (parse "()") '[()]))

    (is (= (parse "{}")
           (parse "{@}") '[[:<re []]]))
    (is (= (parse "{..@}") '[[:<..re []]]))
    (is (= (parse "{..@.}") '[[:<..re. []]]))
    (is (= (parse "{@_}") '[[:<re_ []]]))
    (is (= (parse "{..@_}") '[[:<..re_ []]]))
    (is (= (parse "{..@._}") '[[:<..re._ []]]))
    (is (= (parse "{@~}") '[[:<re' []]]))
    (is (= (parse "{..@~}") '[[:<..re' []]]))
    (is (= (parse "{..@~.}") '[[:<..re'. []]]))
    (is (= (parse "{@~_}") '[[:<re'_ []]]))
    (is (= (parse "{..@~_}") '[[:<..re'_ []]]))
    (is (= (parse "{..@~._}") '[[:<..re'._ []]]))
    (is (= (parse "{2r|}") '[[:<..re []]]))
    (is (= (parse "{2r+1|}") '[[:<..re. []]]))
    (is (= (parse "{alt|}") '[[:<re' []]]))
    (is (= (parse "{open|}") '[[:<re_ []]]))

    (is (= (parse "{,}")
           (parse "{@ ,}") '[[:<re [] []]]))
    (is (= (parse "{,,}")
           (parse "{@ ,,}") '[[:<re [] [] []]]))
    (is (= (parse "{2r|,}") '[[:<..re [] []]]))
    (is (= (parse "{2r|,,}") '[[:<..re [] [] []]]))

    (is (= (parse "a") '["a"]))
    (is (= (parse "apple") '["apple"]))
    (is (= (parse "'apple juice'") '["apple juice"]))

    (is (= (parse "/some smell/") '[[:uncl "some smell"]]))

    (is (= (parse ":M") (parse ->nmui ":M") '[:M]))
    (is (= (parse ":1") (parse ->nmui ":2") '[:U]))
    (is (= (parse ":NUIM")
           (parse ":0123") '[[:fdna ["v__0"] :NUIM]]))
    (is (= (parse ->nmui ":NUIM")
           (parse ->nmui ":0231") '[[:fdna ["v__0"] :INUM]]))
    (is (= (parse ->nmui ":2310302310012021221111113232332212132133023103213021320233011023")
           '[[:fdna ["v__0" "v__1" "v__2"]
              :INUMMMUIIUMIUNIMNMNMMUINUINIUUNMIIUUMUUMMMMMUIIUNIIMUINMNIUUUMNI]]))

    (is (= (parse "[[]:M]")
           (parse "[[]:3]") '[:M]))
    (is (= (parse ->nmui "[[]:I]")
           (parse ->nmui "[[]:3]") '[:I]))
    (is (= (parse "[[a]:NUIM]")
           (parse "[[a]:0123]") '[[:fdna ["a"] :NUIM]]))
    (is (= (parse ->nmui "[[a]:NUIM]")
           (parse ->nmui "[[a]:0231]") '[[:fdna ["a"] :INUM]]))
    (is (= (parse "[[a,'z_3']:NUIMMNIIIUNMMUNU]")
           '[[:fdna ["a" "z_3"] :NUIMMNIIIUNMMUNU]]))
    (is (= (parse ->nmui "[[a,b,c]:2310302310012021221111113232332212132133023103213021320233011023]")
           '[[:fdna ["a" "b" "c"]
              :INUMMMUIIUMIUNIMNMNMMUINUINIUUNMIIUUMUUMMMMMUIIUNIIMUINMNIUUUMNI]]))

    )

  (testing "Correctness of related transformations"
    (testing "of the same type"
      (is (= (parse "()()") '[() ()]))
      (is (= (parse "()()()()") '[() () () ()]))
      (is (= (parse "{}{}") '[[:<re []] [:<re []]]))
      (is (= (parse "a a") '["a" "a"]))
      (is (= (parse "'a ball''a ball'") '["a ball" "a ball"]))
      (is (= (parse "/b 4//b 4/") '[[:uncl "b 4"] [:uncl "b 4"]]))
      (is (= (parse ":M :M") '[:M :M]))
      (is (= (parse ":3 :3") '[:M :M]))
      (is (= (parse "[[]:M] [[]:M]") '[:M :M]))
      (is (= (parse "[[a]:NUIM] [[a]:NUIM]") '[[:fdna ["a"] :NUIM] [:fdna ["a"] :NUIM]]))
      )
    )

  (testing "Correctness of nested transformations"
    (testing "of the same type"
      (is (= (parse "(())") '[(())]))
      (is (= (parse "(((()))(()())())") '[(((())) (() ()) ())]))
      (is (= (parse "{{}}") '[[:<re [[:<re []]]]]))
      (is (= (parse "{{{{}}}{{}{}}{}}")
             '[[:<re [[:<re [[:<re [[:<re []]]]]]
                      [:<re [[:<re []] [:<re []]]] [:<re []]]]]))
      (is (= (parse "{{,{},{,}},{{}},{{},,},,,}")
             '[[:<re [[:<re [] [[:<re []]] [[:<re [] []]]]]
                [[:<re [[:<re []]]]] [[:<re [[:<re []]] [] []]] [] [] []]]))
      )

    (testing "of different types"
      (is (= (parse "(:U 'x_1' [['x_1']:NMUI] /はあ/ {alt|2r|} :2)")
             '[(:U "x_1" [:fdna ["x_1"] :NMUI] [:uncl "はあ"]
                   [:<..re' []] :I)]))
      (is (= (parse "{:U 'x_1', [['x_1']:NMUI], /はあ/ {alt|2r|}, :2}")
             '[[:<re [:U "x_1"] [[:fdna ["x_1"] :NMUI]]
                [[:uncl "はあ"] [:<..re' []]] [:I]]]))
      (is (= (parse "(a(b(c)))") '[("a" ("b" ("c")))]))
      (is (= (parse "(((a)b)c)") '[((("a") "b") "c")]))
      (is (= (parse "((a (b :U))c d(e):2 f)g")
             '[(("a" ("b" :U)) "c" "d" ("e") :I "f") "g"]))
      (is (= (parse "{a,b,c}") '[[:<re ["a"] ["b"] ["c"]]]))
      (is (= (parse "{(a {b,(c),:U d}),{e,f :2}}g")
             '[[:<re [("a" [:<re ["b"] [("c")] [:U "d"]])]
                [[:<re ["e"] ["f" :I]]]] "g"]))

      (is (= (parse "{L,R} {2r+1|L,E,R}")
             '[[:<re ["L"] ["R"]] [:<..re. ["L"] ["E"] ["R"]]]))
      (is (= (parse "{alt|L,R}{alt|R,L}")
             '[[:<re' ["L"] ["R"]] [:<re' ["R"] ["L"]]]))
      (is (= (parse "(({L,E,R}{E,R,L}{L,R,E})(L E R))")
             '[(([:<re ["L"] ["E"] ["R"]]
                 [:<re ["E"] ["R"] ["L"]]
                 [:<re ["L"] ["R"] ["E"]]) ("L" "E" "R"))]))
      (is (= (parse "((/green apple/)/red apple/)")
             '[(([:uncl "green apple"]) [:uncl "red apple"])]))
      (is (= (parse "{2r+1|/deeming/,/telling/,/understanding/}")
             '[[:<..re. [[:uncl "deeming"]] [[:uncl "telling"]]
                [[:uncl "understanding"]]]]))

      )
    )

  ) 
