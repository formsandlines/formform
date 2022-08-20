(ns formform.expr-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [formform.expr :as fe]))

(deftest cnt>-test
  (testing "Simple expressions"
    (testing "FORM"
      (testing "equal"
        (is (= (fe/cnt> nil) nil))
        (is (= (fe/cnt> '()) '()))
        (is (= (fe/cnt> :mn) :mn))
        (is (= (fe/cnt> '(:mn)) '(:mn))))
      (testing "marked"
        (is (= (fe/cnt> '(nil)) '()))
        (is (= (fe/cnt> '(())) nil))
        (is (= (fe/cnt> '(:mn)) '(:mn)))
        (is (= (fe/cnt> '((:mn))) :mn))))

    (testing "Constant"
      (testing "equal"
        (is (= (fe/cnt> :N) nil))
        (is (= (fe/cnt> :M) '()))
        (is (= (fe/cnt> :U) :mn))
        (is (= (fe/cnt> :I) '(:mn))))
      (testing "marked"
        (is (= (fe/cnt> '(:N)) '()))
        (is (= (fe/cnt> '(:M)) nil))
        (is (= (fe/cnt> '(:U)) '(:mn)))
        (is (= (fe/cnt> '(:I)) :mn))))

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
      ; (is (= (fe/cnt> '((a)))
      ;        'a)) ;; fails!
      ; (is (= (fe/cnt> '(((a))))
      ;        '(a))) ;; fails!
      )

    (testing "irreducable"
      (is (= (fe/cnt> '(a b))
             '(a b)))
      ;; should these be reducable? pattern-matching?
      (is (= (fe/cnt> '((a (b)) ((b) a)))
             '((a (b)) ((b) a))))
      (is (= (fe/cnt> '((a)))
             '((a))))
      (is (= (fe/cnt> '(((a))))
             '(((a))))))))


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
             [ ]))))

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
      (is (= (fe/=> [ nil ])
             [ :N ]))
      (is (= (fe/=> [ '() ])
             [ :M ]))
      (is (= (fe/=> [ :mn ])
             [ :U ]))
      (is (= (fe/=> [ '(:mn) ])
             [ :I ])))

    (testing "Constants"
      (is (= (fe/=> [ :N ])
             [ :N ]))
      (is (= (fe/=> [ :M ])
             [ :M ]))
      (is (= (fe/=> [ :U ])
             [ :U ]))
      (is (= (fe/=> [ :I ])
             [ :I ]))))

  (testing "irreducable expressions"
    (is (= (fe/=> [ 'a ])
           [ :_ ]))
    (is (= (fe/=> [ '("x" ("y")) ])
           [ :_ ])))

  (testing "metadata"
    (is (= (meta (fe/=> [ 'a ]))
           {:expr ['a], :env {}}))
    (is (= (meta (fe/=> [ 'a ] {'a [ :M ]}))
           {:expr [()], :env {'a [:M]}}))))


(deftest =>*-test
  (testing "Correctness of returned combinatorial space"
    (is (= (fe/=>* [ nil ])
           [[ '() '(:N) ]]))
    (is (= (fe/=>* [ nil ] false)
           '([:N])))

    (is (= (fe/=>* [ 'a ])
           [[ '(a) '(:M :I :U :N) ]]))
    (is (= (fe/=>* [ 'a ] false)
           '([:N] [:U] [:I] [:M])))

    (is (= (fe/=>* [ 'a 'b ])
           [[ '(a b) '(:M :M :M :M :M :I :M :I :M :M :U :U :M :I :U :N) ]]))
    (is (= (fe/=>* [ 'a 'b ] false)
           '([:N] [:U] [:I] [:M] 
             [:U] [:U] [:M] [:M] 
             [:I] [:M] [:I] [:M] 
             [:M] [:M] [:M] [:M])))

    (is (= (fe/=>* [ 'a 'b 'c ])
           [[ '(a b c) '(:M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :M :I :M :I :M :M :M :M :M :I :M :I :M :M :M :M :M :M :M :M :M :M :U :U :M :M :U :U :M :M :M :M :M :I :M :I :M :M :U :U :M :I :U :N) ]]))
    (is (= (fe/=>* [ 'a 'b 'c ] false)
           '([:N] [:U] [:I] [:M] 
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
             [:M] [:M] [:M] [:M]))))) 


(deftest dna-expr?-test
  (testing "Validity of dna-expr"
    (is (fe/dna-expr? [['a 'b] :IUMNIUMNIUMNIUMN])))) 


(deftest vars-test
  (testing "At root level"
    (is (= (fe/vars [ 'a 'b ])
           '(a b)))
    (is (= (fe/vars [ "a" "a" ])
           '("a")))
    (is (= (fe/vars [ "a" 'a ]) ;; should this be equal?
           '("a" a))))

  (testing "Empty"
    (is (= (fe/vars [ '(() ()) '() ])
           '())))

  (testing "Nested"
    (is (= (fe/vars [ '(a) ])
           '(a)))
    (is (= (fe/vars [ '((a) b) 'c ])
           '(a b c))))

  (testing "Differentiated from other elements"
    (is (= (fe/vars [ :N 'a nil ])
           '(a))))) 

