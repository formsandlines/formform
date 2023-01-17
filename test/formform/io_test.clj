(ns formform.io-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            ; [formform.calc :as calc]
            [formform.io :as io :refer :all]
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
      (is (= (trees ":NUIM") '([:EXPR [:SYMBOL ":NUIM"]])))
      )

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
                [:VAR "u"] [:FORM] [:SYMBOL ":U"] [:VAR "b"]])))

      )
    )

  )

(def ->nmui {:sort-code calc/nmui-code})

(deftest parse-test
  (testing "Correctness of single transformations"
    (is (= (parse "") nil))

    (is (= (parse "()") '()))

    (is (= (parse "{}")
           (parse "{@}") '[:seq-re :<r nil]))
    (is (= (parse "{..@}") '[:seq-re :<..r nil]))
    (is (= (parse "{..@.}") '[:seq-re :<..r. nil]))
    (is (= (parse "{@_}") '[:seq-re :<r_ nil]))
    (is (= (parse "{..@_}") '[:seq-re :<..r_ nil]))
    (is (= (parse "{..@._}") '[:seq-re :<..r._ nil]))
    (is (= (parse "{@~}") '[:seq-re :<r' nil]))
    (is (= (parse "{..@~}") '[:seq-re :<..r' nil]))
    (is (= (parse "{..@~.}") '[:seq-re :<..r'. nil]))
    (is (= (parse "{@~_}") '[:seq-re :<r'_ nil]))
    (is (= (parse "{..@~_}") '[:seq-re :<..r'_ nil]))
    (is (= (parse "{..@~._}") '[:seq-re :<..r'._ nil]))
    (is (= (parse "{2r|}") '[:seq-re :<..r nil]))
    (is (= (parse "{2r+1|}") '[:seq-re :<..r. nil]))
    (is (= (parse "{alt|}") '[:seq-re :<r' nil]))
    (is (= (parse "{open|}") '[:seq-re :<r_ nil]))

    (is (= (parse "{,}")
           (parse "{@ ,}") '[:seq-re :<r nil nil]))
    (is (= (parse "{,,}")
           (parse "{@ ,,}") '[:seq-re :<r nil nil nil]))
    (is (= (parse "{2r|,}") '[:seq-re :<..r nil nil]))
    (is (= (parse "{2r|,,}") '[:seq-re :<..r nil nil nil]))

    (is (= (parse "a") "a"))
    (is (= (parse "apple") "apple"))
    (is (= (parse "'apple juice'") "apple juice"))

    (is (= (parse "/some smell/") '[:uncl "some smell"]))

    (is (= (parse ":M") (parse ->nmui ":M") :M))
    (is (= (parse ":1") (parse ->nmui ":2") :1))
    (is (= (parse "::M") (parse ->nmui "::M") [:fdna [] [:M]]))
    (is (= (parse "::1") (parse ->nmui "::2") [:fdna [] [:U]]))
    (is (= (parse "::NUIM")
           (parse "::0123") '[:fdna ["v__0"] [:N :U :I :M]]))
    (is (= (parse ->nmui "::NUIM")
           (parse ->nmui "::0231") '[:fdna ["v__0"] [:I :N :U :M]]))
    (is (= (parse ->nmui "::2310302310012021221111113232332212132133023103213021320233011023")
           '[:fdna ["v__0" "v__1" "v__2"]
             [:I :N :U :M  :M :M :U :I  :I :U :M :I  :U :N :I :M
              :N :M :N :M  :M :U :I :N  :U :I :N :I  :U :U :N :M
              :I :I :U :U  :M :U :U :M  :M :M :M :M  :U :I :I :U
              :N :I :I :M  :U :I :N :M  :N :I :U :U  :U :M :N :I]]))

    (is (= (parse "[[]::M]")
           (parse "[[]::3]") [:fdna [] [:M]]))
    (is (= (parse ->nmui "[[]::I]")
           (parse ->nmui "[[]::3]") [:fdna [] [:I]]))
    (is (= (parse "[[a]::NUIM]")
           (parse "[[a]::0123]") '[:fdna ["a"] [:N :U :I :M]]))
    (is (= (parse ->nmui "[[a]::NUIM]")
           (parse ->nmui "[[a]::0231]") '[:fdna ["a"] [:I :N :U :M]]))
    (is (= (parse "[[a,'z_3']::NUIMMNIIIUNMMUNU]")
           '[:fdna ["a" "z_3"] [:N :U :I :M
                                :M :N :I :I
                                :I :U :N :M
                                :M :U :N :U]]))
    (is (= (parse ->nmui "[[a,b,c]::2310302310012021221111113232332212132133023103213021320233011023]")
           '[:fdna ["a" "b" "c"]
             [:I :N :U :M  :M :M :U :I  :I :U :M :I  :U :N :I :M
              :N :M :N :M  :M :U :I :N  :U :I :N :I  :U :U :N :M
              :I :I :U :U  :M :U :U :M  :M :M :M :M  :U :I :I :U
              :N :I :I :M  :U :I :N :M  :N :I :U :U  :U :M :N :I]]))

    )

  (testing "Correctness of related transformations"
    (testing "of the same type"
      (is (= (parse "()()") '[:- () ()]))
      (is (= (parse "()()()()") '[:- () () () ()]))
      (is (= (parse "{}{}") '[:- [:seq-re :<r nil] [:seq-re :<r nil]]))
      (is (= (parse "a a") '[:- "a" "a"]))
      (is (= (parse "'a ball''a ball'") '[:- "a ball" "a ball"]))
      (is (= (parse "/b 4//b 4/") '[:- [:uncl "b 4"] [:uncl "b 4"]]))
      (is (= (parse ":M :M") '[:- :M :M]))
      (is (= (parse ":3 :3") '[:- :3 :3]))
      ;; ? parse as fdna instead
      (is (= (parse "[[]::M] [[]::M]") '[:- [:fdna [] [:M]] [:fdna [] [:M]]]))
      (is (= (parse "[[a]::NUIM] [[a]::NUIM]") '[:-
                                                 [:fdna ["a"] [:N :U :I :M]]
                                                 [:fdna ["a"] [:N :U :I :M]]]))
      )
    )

  (testing "Correctness of nested transformations"
    (testing "of the same type"
      (is (= (parse "(())") '(())))
      (is (= (parse "(((()))(()())())") '(((())) (() ()) ())))
      (is (= (parse "{{}}") '[:seq-re :<r [:seq-re :<r nil]]))
      (is (= (parse "{{{{}}}{{}{}}{}}")
             '[:seq-re :<r
               [:-
                [:seq-re :<r [:seq-re :<r [:seq-re :<r nil]]]
                [:seq-re :<r [:- [:seq-re :<r nil] [:seq-re :<r nil]]]
                [:seq-re :<r nil]]]))
      (is (= (parse "{{,{},{,}},{{}},{{},,},,,}")
             '[:seq-re :<r
               [:seq-re :<r nil [:seq-re :<r nil] [:seq-re :<r nil nil]]
               [:seq-re :<r [:seq-re :<r nil]]
               [:seq-re :<r [:seq-re :<r nil] nil nil] nil nil nil]))
      )

    (testing "of different types"
      (is (= (parse "(:U 'x_1' [['x_1']::NMUI] /はあ/ {alt|2r|} :2)")
             '(:U "x_1" [:fdna ["x_1"] [:N :M :U :I]] [:uncl "はあ"]
                  [:seq-re :<..r' nil] :2)))
      (is (= (parse "{:U 'x_1', [['x_1']::NMUI], /はあ/ {alt|2r|}, :2}")
             '[:seq-re :<r [:- :U "x_1"] [:fdna ["x_1"] [:N :M :U :I]]
               [:- [:uncl "はあ"] [:seq-re :<..r' nil]] :2]))
      (is (= (parse "(a(b(c)))") '("a" ("b" ("c")))))
      (is (= (parse "(((a)b)c)") '((("a") "b") "c")))
      (is (= (parse "((a (b :U))c d(e):2 f)g")
             '[:- (("a" ("b" :U)) "c" "d" ("e") :2 "f") "g"]))
      (is (= (parse "{a,b,c}") '[:seq-re :<r "a" "b" "c"]))
      (is (= (parse "{(a {b,(c),:U d}),{e,f :2}}g")
             '[:- [:seq-re :<r ["a" [:seq-re :<r "b" ["c"] [:- :U "d"]]]
                   [:seq-re :<r "e" [:- "f" :2]]] "g"]))

      (is (= (parse "{L,R} {2r+1|L,E,R}")
             '[:- [:seq-re :<r "L" "R"] [:seq-re :<..r. "L" "E" "R"]]))
      (is (= (parse "{alt|L,R}{alt|R,L}")
             '[:- [:seq-re :<r' "L" "R"] [:seq-re :<r' "R" "L"]]))
      (is (= (parse "(({L,E,R}{E,R,L}{L,R,E})(L E R))")
             '(([:seq-re :<r "L" "E" "R"]
                [:seq-re :<r "E" "R" "L"]
                [:seq-re :<r "L" "R" "E"]) ("L" "E" "R"))))
      (is (= (parse "((/green apple/)/red apple/)")
             '(([:uncl "green apple"]) [:uncl "red apple"])))
      (is (= (parse "{2r+1|/deeming/,/telling/,/understanding/}")
             '[:seq-re :<..r.
               [:uncl "deeming"]
               [:uncl "telling"]
               [:uncl "understanding"]]))

      )
    )

  ) 
