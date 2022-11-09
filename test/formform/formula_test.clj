(ns formform.formula-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            ; [formform.calc :as calc]
            [formform.formula :as fml :refer :all]
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
      ))

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
      (is (= (trees "[]:N") [[:EXPR [:FDNA [:VARLIST] ":" [:CONST "N"]]]]))

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
             (trees " :1 ") '([:EXPR [:CDIGIT "1"]])))
      (is (= (trees ":M") (trees " :M") (trees ":M ")
             (trees " :M ") '([:EXPR [:CONST "M"]])))
      (is (= (trees "a") (trees " a") (trees "a ")
             (trees " a ") '([:EXPR [:VAR "a"]])))

      (is (fail? (tree ":1a"))) (is (fail? (tree "a:1")))
      (is (fail? (tree ":Na"))) (is (fail? (tree "a:N")))
      (is (fail? (tree ":1:N"))) (is (fail? (tree ":N:1")))
      (is (= (trees ":1 a") '([:EXPR [:CDIGIT "1"] [:VAR "a"]])))
      (is (= (trees "a :1") '([:EXPR [:VAR "a"] [:CDIGIT "1"]])))

      (is (fail? (tree "a :1b"))) (is (fail? (tree "a:1 b")))
      (is (fail? (tree "a :Nb"))) (is (fail? (tree "a:N b")))
      (is (fail? (tree ":1 b:1"))) (is (fail? (tree ":1b :1")))
      (is (fail? (tree ":N b:N"))) (is (fail? (tree ":Nb :N")))
      (is (fail? (tree ":1 :N:M"))) (is (fail? (tree ":1:N :M")))
      (is (= (trees "a :1 b") '([:EXPR [:VAR "a"] [:CDIGIT "1"] [:VAR "b"]])))
      (is (= (trees "a b :1") '([:EXPR [:VAR "a"] [:VAR "b"] [:CDIGIT "1"]])))
      (is (= (trees ":1 a b") '([:EXPR [:CDIGIT "1"] [:VAR "a"] [:VAR "b"]])))

      (is (fail? (tree ":11"))) (is (fail? (tree ":UU")))
      (is (fail? (tree ":NUIM")))
      )

    (testing "between literals and forms"
      (is (= (trees "( a )") (trees "( a)") (trees "(a )") (trees "(a)")
             '([:EXPR [:FORM [:VAR "a"]]])))
      (is (= (trees "() a") (trees "()a") '([:EXPR [:FORM] [:VAR "a"]])))
      (is (= (trees "a ()") (trees "a()") '([:EXPR [:VAR "a"] [:FORM]])))
      (is (= (trees "() :1") (trees "():1") '([:EXPR [:FORM] [:CDIGIT "1"]])))
      (is (= (trees ":1 ()") (trees ":1()") '([:EXPR [:CDIGIT "1"] [:FORM]])))
      (is (= (trees "() :M") (trees "():M") '([:EXPR [:FORM] [:CONST "M"]])))
      (is (= (trees ":M ()") (trees ":M()") '([:EXPR [:CONST "M"] [:FORM]])))

      (is (= (trees "a () b") (trees "a() b") (trees "a ()b")
             (trees "a()b") '([:EXPR [:VAR "a"] [:FORM] [:VAR "b"]])))
      (is (= (trees ":1 () :M") (trees ":1() :M") (trees ":1 ():M")
             (trees ":1():M") '([:EXPR [:CDIGIT "1"] [:FORM] [:CONST "M"]])))

      (is (= (trees "() a b")
             (trees "()a b") '([:EXPR [:FORM] [:VAR "a"] [:VAR "b"]])))
      (is (= (trees "() :1 :M")
             (trees "():1 :M") '([:EXPR [:FORM] [:CDIGIT "1"] [:CONST "M"]])))
      (is (= (trees "a b ()")
             (trees "a b()") '([:EXPR [:VAR "a"] [:VAR "b"] [:FORM]])))
      (is (= (trees ":1 :M ()")
             (trees ":1 :M()") '([:EXPR [:CDIGIT "1"] [:CONST "M"] [:FORM]])))

      (is (= (trees "( a ( b ) c )") (trees "(a(b)c)")
             '([:EXPR [:FORM [:VAR "a"] [:FORM [:VAR "b"]] [:VAR "c"]]])))

      )
    )

  )


; (deftest parse-test
;   (testing "Context of the test assertions"
;     (is (= assertion-values)))) 
