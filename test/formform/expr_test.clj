(ns formform.expr-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.expr :as fe]))

(deftest cnt>-test
  (testing "Simple expressions"
    (testing "FORM"
      (testing "equal"
        (are [x y] (= x y)
          (fe/cnt> nil)    nil
          (fe/cnt> '())    '()
          (fe/cnt> :mn)    :mn
          (fe/cnt> '(:mn)) '(:mn)))
      (testing "marked"
        (are [x y] (= x y)
          (fe/cnt> '(nil))   '()
          (fe/cnt> '(()))    nil
          (fe/cnt> '(:mn))   '(:mn)
          (fe/cnt> '((:mn))) :mn)))

    (testing "Constant"
      (testing "equal"
        (are [x y] (= x y)
          (fe/cnt> :N) nil
          (fe/cnt> :M) '()
          (fe/cnt> :U) :mn
          (fe/cnt> :I) '(:mn)))
      (testing "marked"
        (are [x y] (= x y)
          (fe/cnt> '(:N)) '()
          (fe/cnt> '(:M)) nil
          (fe/cnt> '(:U)) '(:mn)
          (fe/cnt> '(:I)) :mn)))

    (testing "variable"
      (testing "without env"
        (is (= (fe/cnt> 'a) 'a))
        (is (= (fe/cnt> "a") "a")))

      (testing "with env"
        (is (= (fe/cnt> 'a {'a [ :M ]})
               (fe/cnt> '(a) {'a [ :N ]})
               (fe/cnt> '((a (b))) {'a [ :I ] 'b [ :I ]})
               '()))
        (is (= (fe/cnt> "a" {"a" [ :U ]}) :mn)))))

  (testing "Nested expressions"
    (testing "reduced by calling"
      (is (= (fe/cnt> '(nil () nil () nil))
             (fe/cnt> '(() :M (:N)))
             nil))
      (is (= (fe/cnt> '(:U nil :U :mn))
             '(:mn)))
      (is (= (fe/cnt> '(:I nil :I (:mn)))
             ':mn))
      (is (= (fe/cnt> '("foo" "foo" "bar" "foo"))
             '("foo" "bar")))
      (is (= (fe/cnt> '(a b b a b a))
             '(a b)))
      (is (= (fe/cnt> '((a) (a) (a)))
             '((a))))
      (is (= (fe/cnt> '((a (b)) (a (b))))
             '((a (b))))))

    (testing "reduced by crossing"
      (is (= (fe/cnt> '(()))
             (fe/cnt> '((nil)))
             (fe/cnt> '(((:M))))
             (fe/cnt> '((((nil) nil) nil) nil))
             nil))
      (is (= (fe/cnt> '((())))
             (fe/cnt> '(((nil))))
             (fe/cnt> '(((((:N))))))
             (fe/cnt> '(((((nil) nil) nil) nil) nil))
             '()))
      (is (= (fe/cnt> '(((()) (()))))
             (fe/cnt> '((((nil)) (:M))))
             (fe/cnt> '(((:U)) ((:I))))
             nil))
      (is (= (fe/cnt> '(((a))))
             '(a))))

    (testing "irreducable"
      (is (= (fe/cnt> '(a b))
             '(a b)))
      (is (= (fe/cnt> '((a)))
             '((a))))
      ;; should these be reducable? pattern-matching?
      ;; -> might get too complex for eval
      (is (= (fe/cnt> '((a (b)) ((b) a)))
             '((a (b)) ((b) a)))))))


(deftest ctx>-test
  (testing "Relations of simple expressions"
    (testing "FORMs"
      (is (= (fe/ctx> [ nil nil ]) [ ]))
      (is (= (fe/ctx> [ '() '() ]) [ '() ]))
      (is (= (fe/ctx> [ :mn :mn ]) [ :mn ]))
      (is (= (fe/ctx> [ '(:mn) '(:mn) ]) [ '(:mn) ])))

    (testing "Constants"
      (is (= (fe/ctx> [ :N :N ]) [ ]))
      (is (= (fe/ctx> [ :M :M ]) [ '() ]))
      (is (= (fe/ctx> [ :U :U ]) [ :mn ]))
      (is (= (fe/ctx> [ :I :I ]) [ '(:mn) ]))
      (is (= (fe/ctx> [ :M :N ]) [ '() ]))
      (is (= (fe/ctx> [ :U :N ]) [ :mn ]))
      (is (= (fe/ctx> [ :I :N ]) [ '(:mn) ]))
      (is (= (fe/ctx> [ :U :I ]) [ '() ]))
      (is (= (fe/ctx> [ :U :M ]) [ '() ]))
      (is (= (fe/ctx> [ :I :M ]) [ '() ])))

    (testing "Variables"
      (testing "without env"
        (is (= (fe/ctx> [ 'a 'b ])
               [ 'a 'b ]))
        (is (= (fe/ctx> [ 'a 'a ])
               [ 'a ]))
        (is (= (fe/ctx> [ 'a "a" ]) ;; should be equal?
               [ 'a "a" ])))

      (testing "with env"
        (is (= (fe/ctx> [ 'a 'a ] {'a [ :U ]})
               [ :mn ]))
        (is (= (fe/ctx> [ 'a 'b ] {'a [ :U ] 'b [ :I ]})
               [ '() ]))
        (is (= (fe/ctx> [ 'a "a" ] {'a [ :U ] "a" [ :I ]}) ;; should be equal?
               [ '() ]))
        (is (= (fe/ctx> [ 'a 'b ] {'a [ :U ]})
               [ :mn 'b ]))))

    (testing "mixed"
      (is (= (fe/ctx> [ '(()) nil :N '(((()))) '(() () ()) ])
             [ ]))
      (is (= (fe/ctx> [ '((() ())) :M '() ])
             [ '() ]))
      (is (= (fe/ctx> [ :mn '((:mn)) :U '((:U) (:U)) ])
             [ :mn ]))
      (is (= (fe/ctx> [ '(:mn) '(((:mn))) :I '((:I) (:I)) ])
             [ '(:mn) ]))
      (is (= (fe/ctx> [ :U '(:I) ])
             [ :mn ]))
      (is (= (fe/ctx> [ '(:I) '(:U) ])
             [ '() ]))
      (is (= (fe/ctx> [ '((:mn)) '(:mn) ])
             [ '() ]))
      (is (= (fe/ctx> [ :mn '(:I) ])
             [ :mn ]))
      (is (= (fe/ctx> [ '(:U) '((:mn)) ])
             [ '() ]))
      (is (= (fe/ctx> [ '((:I)) :mn ])
             [ '() ]))))

  (testing "Relations of complex expressions"
    (testing "pure FORMs"
      (is (= (fe/ctx> [ '(()) ])
             [ ]))
      (is (= (fe/ctx> [ '(() ()) ])
             [ ]))
      (is (= (fe/ctx> [ '() '(() ()) ])
             [ '() ]))
      (is (= (fe/ctx> [ '(((() (() ()))) ((() (())) (() ()) (()))) ])
             [ ])))

    (testing "nested variables (reflexion rule)"
      (is (= (fe/ctx> '[ ((a)) ])
             [ 'a ]))
      (is (= (fe/ctx> '[ ((a b c)) ])
             [ 'a 'b 'c ]))
      (is (= (fe/ctx> '[ ((a ((b c)) d)) ((e ((f)))) ])
             [ 'a 'b 'c 'd 'e 'f ]))
      (is (= (fe/ctx> '[ (a ((b c)) d) ((e (f))) ])
             [ '(a b c d) 'e '(f) ])))

    (testing "nested U/I relations (generation rule)"
      (is (= (fe/ctx> '[ :U (:I) ])
             [ :mn ]))
      (is (= (fe/ctx> '[ :U (:N (:I)) ])
             [ '() ]))
      (is (= (fe/ctx> '[ :U (:N (:N (:I))) ])
             [ :mn ]))
      (is (= (fe/ctx> '[ :U (:N (:U (:N (:I)))) ])
             [ '() ]))
      (is (= (fe/ctx> '[ :N (:U (:N (:N (:I)))) ])
             [ '(:mn) ]))
      (is (= (fe/ctx> '[ :I (:U) ])
             [ '(:mn) ]))
      (is (= (fe/ctx> '[ :I (:N (:U)) ])
             [ '() ]))
      (is (= (fe/ctx> '[ :I (:N (:N (:U))) ])
             [ '(:mn) ]))
      (is (= (fe/ctx> '[ :I (:N (:I (:N (:U)))) ])
             [ '() ]))
      (is (= (fe/ctx> '[ :N (:I (:N (:N (:U)))) ])
             [ :mn ]))
      (is (= (fe/ctx> '[ :U (:I a) ])
             [ :mn ]))
      (is (= (fe/ctx> '[ :U ((a :U) :I) ])
             [ :mn ]))
      (is (= (fe/ctx> '[ :U ((a :I) :U) ])
             [ '() ]))
      (is (= (fe/ctx> '[ :I ((a (b :U (c))) c) ])
             [ '(:mn) '((a) c) ]))
      ))

  (testing "Long relations"
    (is (= (fe/ctx> (repeat 1000 nil)) [ ]))
    (is (= (fe/ctx> (repeat 1000 '())) [ '() ]))
    (is (= (fe/ctx> (repeat 1000 :mn)) [ :mn ]))
    (is (= (fe/ctx> (repeat 1000 '(:mn))) [ '(:mn) ]))
    (is (= (fe/ctx> (repeat 1000 :N)) [ ]))
    (is (= (fe/ctx> (repeat 1000 :M)) [ '() ]))
    (is (= (fe/ctx> (repeat 1000 :U)) [ :mn ]))
    (is (= (fe/ctx> (repeat 1000 :I)) [ '(:mn) ])))) 


(deftest =>-test
  (testing "Reducable expressions"
    (testing "FORMs"
      (are [x y] (= x y)
        (fe/=> (fe/make-expr nil))    [ :N ]
        (fe/=> (fe/make-expr '()))    [ :M ]
        (fe/=> (fe/make-expr :mn))    [ :U ]
        (fe/=> (fe/make-expr '(:mn))) [ :I ]))

    (testing "Constants"
      (are [x y] (= x y)
        (fe/=> (fe/make-expr :N)) [ :N ]
        (fe/=> (fe/make-expr :M)) [ :M ]
        (fe/=> (fe/make-expr :U)) [ :U ]
        (fe/=> (fe/make-expr :I)) [ :I ])))

  (testing "irreducable expressions"
    (is (= (fe/=> (fe/make-expr 'a))
           [ :_ ]))
    (is (= (fe/=> (fe/make-expr '("x" ("y"))))
           [ :_ ])))

  (testing "metadata"
    (is (fe/expr-tag? (fe/=> (fe/make-expr 'a))))
    (is (= (meta (fe/=> (fe/make-expr 'a)))
           {:expr ['a], :env {}}))
    (is (= (meta (fe/=> (fe/make-expr 'a) {'a [ :M ]}))
           {:expr [()], :env {'a [:M]}}))))


(deftest =>*-test
  (testing "Correctness of returned combinatorial space"
    (is (= (fe/=>* (fe/make-expr nil))
           [[ '() :N ]]))
           ; [[ '() '(:N) ]]))
    (is (= (fe/=>* (fe/make-expr nil) false)
           [[:N]]))

    (is (= (fe/=>* (fe/make-expr 'a))
           [[ '(a) :MIUN ]]))
           ; [[ '(a) '(:M :I :U :N) ]]))
    (is (= (fe/=>* (fe/make-expr 'a) false)
           [[:N] [:U] [:I] [:M]]))

    (is (= (fe/=>* (fe/make-expr 'a 'b))
           [[ '(a b) :MMMMMIMIMMUUMIUN ]]))
           ; [[ '(a b) '(:M :M :M :M :M :I :M :I :M :M :U :U :M :I :U :N) ]]))
    (is (= (fe/=>* (fe/make-expr 'a 'b) false)
           [[:N] [:U] [:I] [:M]
            [:U] [:U] [:M] [:M]
            [:I] [:M] [:I] [:M]
            [:M] [:M] [:M] [:M]]))

    (is (= (fe/=>* (fe/make-expr 'a 'b 'c))
           [[ '(a b c) :MMMMMMMMMMMMMMMMMMMMMIMIMMMMMIMIMMMMMMMMMMUUMMUUMMMMMIMIMMUUMIUN ]]))
           ; [[ '(a b c) '(:M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :I :M :I :M :M :M :M :M :I :M :I :M :M :M :M :M :M :M :M :M :M :U :U :M :M :U :U :M :M :M :M :M :I :M :I :M :M :U :U :M :I :U :N) ]]))
    (is (= (fe/=>* (fe/make-expr 'a 'b 'c) false)
           [[:N] [:U] [:I] [:M]
            [:U] [:U] [:M] [:M]
            [:I] [:M] [:I] [:M]
            [:M] [:M] [:M] [:M]

            [:U] [:U] [:M] [:M]
            [:U] [:U] [:M] [:M]
            [:M] [:M] [:M] [:M]
            [:M] [:M] [:M] [:M]

            [:I] [:M] [:I] [:M]
            [:M] [:M] [:M] [:M]
            [:I] [:M] [:I] [:M]
            [:M] [:M] [:M] [:M]

            [:M] [:M] [:M] [:M]
            [:M] [:M] [:M] [:M]
            [:M] [:M] [:M] [:M]
            [:M] [:M] [:M] [:M]])))) 


(deftest dna-expr?-test
  (testing "Validity of dna-expr"
    (is (fe/dna-expr? ^:expr [['a 'b] :IUMNIUMNIUMNIUMN])))) 


(deftest vars-test
  (testing "At root level"
    (is (= (fe/vars [ 'a 'b ] {})
           '(a b)))
    (is (= (fe/vars [ "a" "a" ] {})
           '("a")))
    (is (= (fe/vars [ "a" 'a ] {}) ;; should this be equal?
           '("a" a))))

  (testing "Empty"
    (is (= (fe/vars [ '(() ()) '() ] {})
           '())))

  (testing "Nested"
    (is (= (fe/vars [ '(a) ] {})
           '(a)))
    (is (= (fe/vars [ '((a) b) 'c ] {})
           '(a b c))))

  (testing "Differentiated from other elements"
    (is (= (fe/vars [ :N 'a nil ] {})
           '(a))))

  (testing "Variable ordering"
    (is (= (fe/vars [ 'a "b" 'c "d" ] {:ordered? false})
           (fe/vars [ 'a "b" 'c "d" ] {:ordered? true})
           '(a "b" c "d")))
    (is (= (fe/vars [ "b" 'c 'a "d" ] {:ordered? false})
           (fe/vars [ "b" 'c 'a "d" ] {})
           '("b" c a "d")))
    (is (= (fe/vars [ "b" 'c 'a "d" ] {:ordered? true})
           '(a "b" c "d"))))

  (testing "Exotic variable names"
    ;; incorrect ordering due to symbols
    ; (is (= (fe/vars '[ "apple tree" ("something" else) ])
    ;        '("apple tree" else "something")))
    )) 


(def seqre-opts
  (vec (for [interpr [:rec-instr :rec-ident]
             open?   [false true]
             parity  [:any :even :odd]]
         {:parity parity, :open? open?, :interpr interpr})))

(deftest seq-reentry-sign-test
  (testing "All possible inputs"
    (are [x y] (= x y)
      (fe/seq-reentry-opts->sign (seqre-opts 0))  :<re
      (fe/seq-reentry-opts->sign (seqre-opts 1))  :<..re
      (fe/seq-reentry-opts->sign (seqre-opts 2))  :<..re.
      (fe/seq-reentry-opts->sign (seqre-opts 3))  :<re_
      (fe/seq-reentry-opts->sign (seqre-opts 4))  :<..re_
      (fe/seq-reentry-opts->sign (seqre-opts 5))  :<..re._

      (fe/seq-reentry-opts->sign (seqre-opts 6))  :<re'
      (fe/seq-reentry-opts->sign (seqre-opts 7))  :<..re'
      (fe/seq-reentry-opts->sign (seqre-opts 8))  :<..re'.
      (fe/seq-reentry-opts->sign (seqre-opts 9))  :<re'_
      (fe/seq-reentry-opts->sign (seqre-opts 10)) :<..re'_
      (fe/seq-reentry-opts->sign (seqre-opts 11)) :<..re'._)))

(deftest seq-reentry-sign->opts-test
  (testing "Basic functionality"
    (is (= (fe/seq-reentry-sign->opts :<re) (seqre-opts 0)))
    (is (= (fe/seq-reentry-sign->opts :<..re'._) (seqre-opts 11))))) 

(deftest seq-reentry-expr-test
  (testing "Empty expressions"
    (are [x y] (= x y)
      (fe/seq-reentry-expr (seqre-opts 0))  [[:<re]]
      (fe/seq-reentry-expr (seqre-opts 1))  [[:<..re]]
      (fe/seq-reentry-expr (seqre-opts 2))  [[:<..re.]]
      (fe/seq-reentry-expr (seqre-opts 3))  [[:<re_]]
      (fe/seq-reentry-expr (seqre-opts 4))  [[:<..re_]]
      (fe/seq-reentry-expr (seqre-opts 5))  [[:<..re._]]

      (fe/seq-reentry-expr (seqre-opts 6))  [[:<re']]
      (fe/seq-reentry-expr (seqre-opts 7))  [[:<..re']]
      (fe/seq-reentry-expr (seqre-opts 8))  [[:<..re'.]]
      (fe/seq-reentry-expr (seqre-opts 9))  [[:<re'_]]
      (fe/seq-reentry-expr (seqre-opts 10)) [[:<..re'_]]
      (fe/seq-reentry-expr (seqre-opts 11)) [[:<..re'._]]))

  (testing "Default type"
    (is (= (fe/seq-reentry-expr {}) '[[:<re]])))

  (testing "Content number and type"
    (is (= (fe/seq-reentry-expr {} 'x) '[[:<re x]]))
    (is (= (fe/seq-reentry-expr {} 'x 'y) '[[:<re x y]]))
    (is (fe/expr? (fe/seq-reentry-expr {})))
    (is (fe/expr?
          (second (first (fe/seq-reentry-expr {} (fe/·· 'x 'y) 'z)))))))

(def f (fn [opts & ctx] (fe/reduce-seq-reentry
                          (first (apply fe/seq-reentry-expr opts ctx)))))

;; ! check more thoroughly if these are correct
(deftest reduce-seq-re-test
  (testing "Shape of empty expression"
    (are [x y] (= x y)
         ;; ? should nil be removed or valuable to retain context?
      (f (seqre-opts 0))  '[{:f* [((:f* nil) nil)], :f1 [(:f* nil)]} :f1]
      (f (seqre-opts 1))  '[{:f* [((:f* nil) nil)]} :f*]
      (f (seqre-opts 2))  '[{:f* [((:f* nil) nil)], :f1 [(:f* nil)]} :f1]
      (f (seqre-opts 3))  '[{:f* [((:f* nil) nil)], :f2 [(:f* nil)], 
                             :f1 [:f2 nil]} :f1]
      (f (seqre-opts 4))  '[{:f* [((:f* nil) nil)], :f1 [:f* nil]} :f1]
      (f (seqre-opts 5))  '[{:f* [((:f* nil) nil)], :f2 [(:f* nil)], 
                             :f1 [:f2 nil]} :f1]

      (f (seqre-opts 6))  '[{:f* [((:f* nil) nil)], :f1 [(:f* nil)]} :f1]
      (f (seqre-opts 7))  '[{:f* [((:f* nil) nil)]} :f*]
      (f (seqre-opts 8))  '[{:f* [((:f* nil) nil)], :f1 [(:f* nil)]} :f1]
      (f (seqre-opts 9))  '[{:f* [((:f* nil) nil)], :f2 [(:f* nil)], 
                             :f1 [:f2 nil]} :f1]
      (f (seqre-opts 10)) '[{:f* [((:f* nil) nil)], :f1 [:f* nil]} :f1]
      (f (seqre-opts 11)) '[{:f* [((:f* nil) nil)], :f2 [(:f* nil)], 
                             :f1 [:f2 nil]} :f1]))

  (testing "Shape of expressions with resolution 1"
    (are [x y] (= x y)
         ;; ? should redundant content (inner 'a) be reduced?
      (f (seqre-opts 0) 'a)  '[{:f* [((:f* a) a)], :f1 [(:f* a)]} :f1]
      (f (seqre-opts 1) 'a)  '[{:f* [((:f* a) a)]} :f*]
      (f (seqre-opts 2) 'a)  '[{:f* [((:f* a) a)], :f1 [(:f* a)]} :f1]
      (f (seqre-opts 3) 'a)  '[{:f* [((:f* a) a)], :f2 [(:f* a)], 
                                :f1 [:f2 a]} :f1]
      (f (seqre-opts 4) 'a)  '[{:f* [((:f* a) a)], :f1 [:f* a]} :f1]
      (f (seqre-opts 5) 'a)  '[{:f* [((:f* a) a)], :f2 [(:f* a)], 
                                :f1 [:f2 a]} :f1]

      (f (seqre-opts 6) 'a)  '[{:f* [((:f* a) a)], :f1 [(:f* a)]} :f1]
      (f (seqre-opts 7) 'a)  '[{:f* [((:f* a) a)]} :f*]
      (f (seqre-opts 8) 'a)  '[{:f* [((:f* a) a)], :f1 [(:f* a)]} :f1]
      (f (seqre-opts 9) 'a)  '[{:f* [((:f* a) a)], :f2 [(:f* a)], 
                                :f1 [:f2 a]} :f1]
      (f (seqre-opts 10) 'a) '[{:f* [((:f* a) a)], :f1 [:f* a]} :f1]
      (f (seqre-opts 11) 'a) '[{:f* [((:f* a) a)], :f2 [(:f* a)], 
                                :f1 [:f2 a]} :f1]))

  (testing "Shape of expressions with even resolution"
    (are [x y] (= x y)
         ;; ? should even/odd construct redundant re-entries?
      (f (seqre-opts 0) 'a 'b)  '[{:f* [((:f* a) b)]} :f*]
      (f (seqre-opts 1) 'a 'b)  '[{:f* [((:f* a) b)]} :f*]
      (f (seqre-opts 2) 'a 'b)  '[{:f* [((:f* a) b)]} :f*]
      (f (seqre-opts 3) 'a 'b)  '[{:f* [((:f* a) b)], :f1 [(:f* a) b]} :f1]
      (f (seqre-opts 4) 'a 'b)  '[{:f* [((:f* a) b)], :f1 [(:f* a) b]} :f1]
      (f (seqre-opts 5) 'a 'b)  '[{:f* [((:f* a) b)], :f1 [(:f* a) b]} :f1]

      (f (seqre-opts 6) 'a 'b)  '[{:f* [((:f* a) b)]} :f*]
      (f (seqre-opts 7) 'a 'b)  '[{:f* [((:f* a) b)]} :f*]
      (f (seqre-opts 8) 'a 'b)  '[{:f* [((:f* a) b)]} :f*]
      (f (seqre-opts 9) 'a 'b)  '[{:f* [((:f* a) b)], :f1 [(:f* a) b]} :f1]
      (f (seqre-opts 10) 'a 'b) '[{:f* [((:f* a) b)], :f1 [(:f* a) b]} :f1]
      (f (seqre-opts 11) 'a 'b) '[{:f* [((:f* a) b)], :f1 [(:f* a) b]} :f1]))
  
  (testing "Shape of expressions with odd resolution"
    (are [x y] (= x y)
      (f (seqre-opts 0) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f1 [(((:f* a) b) c)]} :f1]
      (f (seqre-opts 1) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)]} :f*]
      (f (seqre-opts 2) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f1 [(((:f* a) b) c)]} :f1]
      (f (seqre-opts 3) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f2 [(((:f* a) b) c)], 
                                      :f1 [((:f2 a) b) c]} :f1]
      (f (seqre-opts 4) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f1 [((:f* a) b) c]} :f1]
      (f (seqre-opts 5) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f2 [(((:f* a) b) c)], 
                                      :f1 [((:f2 a) b) c]} :f1]

      (f (seqre-opts 6) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f1 [(((:f* a) b) c)]} :f1]
      (f (seqre-opts 7) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)]} :f*]
      (f (seqre-opts 8) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f1 [(((:f* a) b) c)]} :f1]
      (f (seqre-opts 9) 'a 'b 'c)  '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f2 [(((:f* a) b) c)], 
                                      :f1 [((:f2 a) b) c]} :f1]
      (f (seqre-opts 10) 'a 'b 'c) '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f1 [((:f* a) b) c]} :f1]
      (f (seqre-opts 11) 'a 'b 'c) '[{:f* [((((((:f* a) b) c) a) b) c)], 
                                      :f2 [(((:f* a) b) c)], 
                                      :f1 [((:f2 a) b) c]} :f1]))) 


