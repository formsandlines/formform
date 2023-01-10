(ns formform.expr-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]))

(deftest make-op-test
  (testing "Missing operator keyword"
    (is (thrown? java.lang.AssertionError
                 (make-op 'a 'b))))

  (testing "Unknown operator keyword"
    (is (thrown-with-msg? Exception #"Unknown operator"
                          (make-op :x 'a))))

  (testing "Known operators"
    (is (= '[:- a b] (make-op :- 'a 'b)))
    (is (= '[:* a b] (make-op :* 'a 'b)))
    (is (= '[:| a b] (make-op :| 'a 'b)))
    (is (= '[:uncl "hello"] (make-op :uncl "hello")))
    (is (= '[:mem [[x :U]] x] (make-op :mem [['x :U]] 'x)))
    (is (= '[:seq-re :<r a b] (make-op :seq-re :<r 'a 'b))))

  ;; ! needs validation and tests
  (testing "Invalid arguments to known operators"
    (is (= true true))))

(deftest op-data-test
  (testing "Missing operator keyword"
    (is (thrown? java.lang.AssertionError
                 (op-data ['a 'b]))))

  (testing "Unknown operator keyword"
    (is (thrown-with-msg? Exception #"Unknown operator"
                          (op-data [:x 'a]))))

  (testing "Correctness of output"
    (is (= '{:ctx [a b]}
           (op-data (make-op :- 'a 'b))))
    (is (= '{:label "hello"}
           (op-data (make-op :uncl "hello"))))
    (is (= '{:rems [[x :U]], :ctx [x]}
           (op-data (make-op :mem [['x :U]] 'x))))
    (is (= '{:sign :<r, :nested-exprs [a b]}
           (op-data (make-op :seq-re :<r 'a 'b)))))) 

(deftest op-get-test
  (testing "Missing operator keyword"
    (is (thrown? java.lang.AssertionError
                 (op-get ['a 'b] :foo))))

  (testing "Unknown operator keyword"
    (is (thrown-with-msg? Exception #"Unknown operator"
                          (op-get [:x 'a] :foo))))

  (testing "Correctness of output"
    (is (= '[a b]
           (op-get (make-op :- 'a 'b) :ctx)))
    (is (= "hello"
           (op-get (make-op :uncl "hello") :label)))
    (let [op (make-op :mem [['x :U]] 'x)]
      (is (= '[[x :U]] (op-get op :rems)))
      (is (= '[x] (op-get op :ctx))))
    (let [op (make-op :seq-re :<r 'a 'b)]
      (is (= :<r (op-get op :sign)))
      (is (= '[a b] (op-get op :nested-exprs)))))) 

(deftest defoperator-test
  (testing "Defining a new operator"
    (do (defoperator :and [a b] [[a] [b]])
        (let [op (make-op :and 'x 'y)]
          (is (= '[:and x y] op))
          (is (= '[[x] [y]] (interpret-op op)))))))


(deftest find-vars-test
  (testing "At root level"
    (is (= '(a b)
           (find-vars (make 'a 'b) {})))
    (is (= '("a")
           (find-vars (make "a" "a") {})))
    (is (= '("a" a)
           (find-vars (make "a" 'a) {}) ;; should this be equal?
           )))

  (testing "Empty"
    (is (= '()
           (find-vars (make) {})
           (find-vars (form) {'a :M})
           (find-vars (make '(() ()) '()) {}))))

  (testing "Nested"
    (is (= '(a)
           (find-vars (make '(a)) {})
           (find-vars (form 'a) {})))
    (is (= '(a b c)
           (find-vars (make '((a) b) 'c) {}))))

  (testing "In different sequentials"
    (is (= '(x y)
           (find-vars '(x y) {})
           (find-vars '((x (y x))) {})))
    (is (= '(x "y")
           (find-vars (make :fdna ['x "y"] [:N :U :I :M
                                            :N :U :I :M
                                            :N :U :I :M
                                            :N :U :I :M]) {})))
    (is (= '(a x y "b" "z")
           (find-vars (make :mem [['a '(x y)] ["b" "z"]] 'a) {})))
    (is (= '("foo")
           (find-vars (make :uncl "foo") {})))
    (is (= '(a b c)
           (find-vars (make :<r [:- 'a 'b] 'c) {}))))

  (testing "In nested special FORMs"
    (is (= '(a b c)
           (find-vars (make :<r [:- 'a :M] [:- '(b) (make :<..r 'c)]) {})))
    (is (= '(a b c d)
           (find-vars (make :mem [['a (make :mem [['b :U]] 'c)]] 'd) {}))))

  (testing "Differentiated from other elements"
    (is (= '(a)
           (find-vars (make :N 'a nil) {}))))

  (testing "Variable ordering"
    (is (= '(a "b" c "d")
           (find-vars (make 'a "b" 'c "d") {:ordered? false})
           (find-vars (make 'a "b" 'c "d") {:ordered? true})))
    (is (= '("b" c a "d")
           (find-vars (make "b" 'c 'a "d") {:ordered? false})
           (find-vars (make "b" 'c 'a "d") {})))
    (is (= '(a "b" c "d")
           (find-vars (make "b" 'c 'a "d") {:ordered? true}))))

  (testing "Specific variables"
    (is (= '(x)
           (find-vars (make 'a [:x ['x]] "z" ["x" 'y]) {:vars #{'x "y" 'z}}))))

  (testing "Exotic variable names"
    ;; incorrect ordering due to symbols
    ; (is (= (find-vars '[ "apple tree" ("something" else) ])
    ;        '("apple tree" else "something")))
    ))


(defn- simplify-expr-chain-reversed [r->l? exprs env]
  (let [rev-exprs (fn [xs] (reverse (map #(if (arrangement? %)
                                            (cons (first %) (reverse (rest %)))
                                            %) xs)))]
    (rev-exprs (simplify-expr-chain {:rtl? r->l?} (rev-exprs exprs) {}))))

(def simplify-expr-chain->rtl (partial simplify-expr-chain-reversed true))
(def simplify-expr-chain->ltr (partial simplify-expr-chain-reversed false))

(comment
  ;; test
  (let [xc '[ a nil b ]]
    {:normal (simplify-expr-chain xc {})
     :rtl (simplify-expr-chain->rtl xc {})
     :reverse (reverse (simplify-expr-chain (reverse xc) {}))}))

;; ! add more tests with marks ()
(deftest simplify-expr-chain-test
  (testing "Correctness of reduction with one form per nesting-level"
    (are [x y] (= y (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))
      '( nil ) '[nil] ;; () => ()
      '( () )  '[()]  ;; (()) => (())
      '( a )   '[a]   ;; (a) => (a)

      '( nil nil ) '[()] ;; (())
      '( a nil )   '[()] ;; (a ()) => (())
      '( nil a )   '[nil a] ;; ((a))
      '( () () )   '[()] ;; (() (())) => (())
      '( a () )    '[a] ;; (a (())) => (a)
      '( () a )    '[()] ;; (() (a)) => (())
      '( a b )     '[a b] ;; (a (b))
      '( a a )     '[()] ;; (a (a)) => (a ()) => (())

      '( nil nil nil ) '[nil] ;; ((())) => ()
      '( a nil nil )   '[a] ;; (a)
      '( a nil () )    '[()] ;; (a ((()))) => (a ()) => (())
      '( nil a nil )   '[nil] ;; ((a ())) => ((())) => ()
      '( nil a () )    '[nil a] ;; ((a (()))) => ((a))
      '( nil nil a )   '[a] ;; (((a))) => (a)
      '( nil nil () )  '[()] ;; (((()))) => (())
      '( nil a b )     '[nil a b] ;; ((a (b)))
      '( a nil b )     '[[:- a b]] ;; (a ((b))) => (a b)
      '( a b nil )     '[a] ;; (a (b ())) => (a (())) => (a)
      '( a b c )       '[a b c] ;; (a (b (c)))

      '( nil nil nil nil ) '[()] ;; (((()))) => (())
      '( a b nil nil )     '[a b] ;; (a (b (()))) => (a (b))
      '( a nil nil b )     '[a b] ;; (a (((b)))) => (a (b))
      '( nil nil a b )     '[a b] ;; (((a (b)))) => (a (b))
      '( a nil b c )       '[[:- a b] c] ;; (a ((b (c)))) => (a b (c))
      '( a b nil c )       '[a [:- b c]] ;; (a (b ((c)))) => (a (b c))

      '( a nil b nil c )   '[[:- a b c]] ;; (a ((b ((c))))) => (a b c)
      '( a nil nil b c )   '[a b c] ;; (a (((b (c))))) => (a (b (c)))
      '( a b nil nil c )   '[a b c] ;; (a (b (((c))))) => (a (b (c)))
      '( a nil nil nil b ) '[[:- a b]] ;; (a ((((b))))) => (a b)
      '( nil a nil b nil ) '[nil] ;; ((a ((b ())))) => ((a b ())) => … => ()
      '( nil nil nil a b ) '[nil a b] ;; ((((a (b))))) => ((a (b)))
      '( a b nil nil nil ) '[a])) ;; (a (b ((())))) => (a (b ())) => … => (a)

  (testing "Equal reduction behavior with forms wrapped in arrangements"
    (are [x y] (= y (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))
      '( [:- nil] ) '[nil]
      '( [:- ()] )  '[()]
      '( [:- a] )   '[a]

      '( [:- nil] [:- nil] ) '[()]
      '( [:- nil] nil )      '[()]
      '( nil [:- nil] )      '[()]
      '( [:- a] [:- nil] )   '[()]
      '( [:- a] nil )        '[()]
      '( a [:- nil] )        '[()]
      '( [:- nil] [:- a] )   '[nil a]
      '( [:- nil] a )        '[nil a]
      '( nil [:- a] )        '[nil a]
      '( [:- ()] [:- ()] )   '[()]
      '( [:- ()] () )        '[()]
      '( () [:- ()] )        '[()]
      '( [:- a] [:- ()] )    '[a]
      '( [:- a] () )         '[a]
      '( a [:- ()] )         '[a]
      '( [:- ()] [:- a] )    '[()]
      '( [:- ()] a )         '[()]
      '( () [:- a] )         '[()]
      '( [:- a] [:- b] )     '[a b]
      '( [:- a] b )          '[a b]
      '( a [:- b] )          '[a b]
      '( [:- a] [:- a] )     '[()]
      '( [:- a] a )          '[()]
      '( a [:- a] )          '[()]))

  (testing "Correctness of reduction with multiple forms per nesting-level"
    (are [x y] (= y (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))
      '( [:- nil nil] ) '[nil]
      '( [:- () ()] )   '[()]
      '( [:- a a] )     '[a]

      '( [:- nil a] [:- nil ()] ) '[a] ;; (a (())) => (a)
      '( [:- a ()] [:- nil b] )   '[()] ;; (a () (b)) => (())
      ))

  (testing "Correctness of reduction via (de)generation rule across chain"
    (let [x [[:- 'a 'b] [:- 'a 'c]] ]
      (is (= '[[:- a b] c]
             (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))))
    ;; (a b (a c)) => (a b (c))
    (let [x ['x 'a nil 'b 'x] ]
      (is (= '[x]
             (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))))
    ;; (x (a ( (b (x))))) => (x (a b ())) => (x)
    (let [x [[:- 'a 'b] [:- 'a 'c] [:- 'c 'a 'd]] ]
      (is (= '[[:- a b] c d]
             (simplify-expr-chain x {})
             (simplify-expr-chain->rtl x {}))))
    ;; (a b (a c (c a d))) => (a b (c (d)))
    (let [x [[:- :U 'b] [:- 'a :I]] ]
      (is (= '[[:- :U b]]
             (simplify-expr-chain x {})
             (simplify-expr-chain->rtl x {}))))
    ;; (:U b (a :I)) => (:U b (a :U :I)) => (:U b (())) => (:U b)
    (let [x [[:- :U 'b] [:- 'a :I] 'c] ]
      (is (= '[[:- :U b]]
             (simplify-expr-chain x {})
             (simplify-expr-chain->rtl x {}))))
    ;; (:U b (a :I (c))) => (:U b (a () (c))) => (:U b (())) => (:U b)

    (let [x [:f* [:- :U 'a] [:- :U 'b]] ]
      (is (= '[:f* a [:- :U b]]
             (simplify-expr-chain {:rtl? true} x {})
             (simplify-expr-chain->ltr x {}))))
    ;; (((:f*) :U a) :U b) => (((:f*) a) :U b)
    (let [x [:f* :U :U] ]
      (is (= '[[:- :f* :U]]
             (simplify-expr-chain {:rtl? true} x {})
             (simplify-expr-chain->ltr x {}))))
    ;; (((:f*) :U) :U) => (((:f*)) :U) => (:f* :U)
    ))


(defn f
  ([x] (f x {}))
  ([x env] (simplify-seq-reentry x env)))

(deftest reduce-seq-reentry-test
  (testing "Reduction of primitive seq-re types"
    (are [x1 x2 y] (= y (f x1) (f x2))
      (seq-re :<r nil)     (seq-re :<r' nil)     [:U]
      (seq-re :<..r nil)   (seq-re :<..r' nil)   :U
      (seq-re :<..r. nil)  (seq-re :<..r'. nil)  [:U]
      (seq-re :<r_ nil)    (seq-re :<r'_ nil)    [:U]
      (seq-re :<..r_ nil)  (seq-re :<..r'_ nil)  :U
      (seq-re :<..r._ nil) (seq-re :<..r'._ nil) [:U])

    (are [x1 x2 y] (= y (f x1) (f x2))
      (seq-re :<r nil nil)     (seq-re :<r' nil nil)     :U
      (seq-re :<..r nil nil)   (seq-re :<..r' nil nil)   :U
      (seq-re :<..r. nil nil)  (seq-re :<..r'. nil nil)  :U
      (seq-re :<r_ nil nil)    (seq-re :<r'_ nil nil)    [:U]
      (seq-re :<..r_ nil nil)  (seq-re :<..r'_ nil nil)  [:U]
      (seq-re :<..r._ nil nil) (seq-re :<..r'._ nil nil) [:U]))

  (testing "Reduction from all possible ambiguous cases"
    (is (= [:U]
           (f (seq-re :<r :U nil))     (f (seq-re :<r :I nil))
           (f (seq-re :<..r :U nil))   (f (seq-re :<..r :I nil))
           (f (seq-re :<..r. :U nil))  (f (seq-re :<..r. :I nil))
           (f (seq-re :<r_ :U nil))    (f (seq-re :<r_ :I nil))
           (f (seq-re :<..r_ :U nil))  (f (seq-re :<..r_ :I nil))
           (f (seq-re :<..r._ :U nil)) (f (seq-re :<..r._ :I nil))))
    (is (= [:U]
           (f (seq-re :<r'_ :U nil))
           (f (seq-re :<..r'_ :U nil)) (f (seq-re :<..r'._ :U nil))
           (f (seq-re :<r'_ :I nil))
           (f (seq-re :<..r'_ :I nil)) (f (seq-re :<..r'._ :I nil))))
      ;; Exceptions in alternative interpretation
    (is (= :U
           (f (seq-re :<r' :U nil))
           (f (seq-re :<..r' :U nil)) (f (seq-re :<..r'. :U nil))))
    (is (= []
           (f (seq-re :<r' :I nil))
           (f (seq-re :<..r' :I nil)) (f (seq-re :<..r'. :I nil)))))

  (testing "Non-reduction of uninterpreted expressions"
    (are [x y] (= y (f x))
      (seq-re :<r 'a nil)     '[:seq-re :<r a nil]
      (seq-re :<..r 'a nil)   '[:seq-re :<..r a nil]
      (seq-re :<..r. 'a nil)  '[:seq-re :<..r. a nil]
      (seq-re :<r_ 'a nil)    '[:seq-re :<r_ a nil]
      (seq-re :<..r_ 'a nil)  '[:seq-re :<..r_ a nil]
      (seq-re :<..r._ 'a nil) '[:seq-re :<..r._ a nil]

      (seq-re :<r' 'a nil)     '((:U a))
      (seq-re :<..r' 'a nil)   '((:U a))
      (seq-re :<..r'. 'a nil)  '((:U a))
      (seq-re :<r'_ 'a nil)    '[:seq-re :<r'_ a nil]
      (seq-re :<..r'_ 'a nil)  '[:seq-re :<..r'_ a nil]
      (seq-re :<..r'._ 'a nil) '[:seq-re :<..r'._ a nil])

    (are [x y] (= y (f x))
      (seq-re :<r 'a)     '[:seq-re :<r a]
      (seq-re :<..r 'a)   '[:seq-re :<..r a]
      (seq-re :<..r. 'a)  '[:seq-re :<..r. a]
      (seq-re :<r_ 'a)    '[:seq-re :<r_ a]
      (seq-re :<..r_ 'a)  '[:seq-re :<..r_ a]
      (seq-re :<..r._ 'a) '[:seq-re :<..r._ a]

      (seq-re :<r' 'a)     '[:seq-re :<r' a]
      (seq-re :<..r' 'a)   '[:seq-re :<..r' a]
      (seq-re :<..r'. 'a)  '[:seq-re :<..r'. a]
      (seq-re :<r'_ 'a)    '(([:U] a))
      (seq-re :<..r'_ 'a)  '((:U a))
      (seq-re :<..r'._ 'a) '(([:U] a)))

    (are [x y] (= y (f x))
      (seq-re :<r nil 'a)     '[:seq-re :<r nil a]
      (seq-re :<..r nil 'a)   '[:seq-re :<..r nil a]
      (seq-re :<..r. nil 'a)  '[:seq-re :<..r. nil a]
      (seq-re :<r_ nil 'a)    '[:seq-re :<r_ nil a]
      (seq-re :<..r_ nil 'a)  '[:seq-re :<..r_ nil a]
      (seq-re :<..r._ nil 'a) '[:seq-re :<..r._ nil a]

      (seq-re :<r' nil 'a)     '[:seq-re :<r' nil a]
      (seq-re :<..r' nil 'a)   '[:seq-re :<..r' nil a]
      (seq-re :<..r'. nil 'a)  '[:seq-re :<..r'. nil a]
      (seq-re :<r'_ nil 'a)    '(([:U] a))
      (seq-re :<..r'_ nil 'a)  '(([:U] a))
      (seq-re :<..r'._ nil 'a) '(([:U] a)))

    (are [x y] (= y (f x))
      (seq-re :<r nil 'a nil)     '[:seq-re :<r nil a nil]
      (seq-re :<..r nil 'a nil)   '[:seq-re :<..r nil a nil]
      (seq-re :<..r. nil 'a nil)  '[:seq-re :<..r. nil a nil]
      (seq-re :<r_ nil 'a nil)    '[:seq-re :<r_ nil a nil]
      (seq-re :<..r_ nil 'a nil)  '[:seq-re :<..r_ nil a nil]
      (seq-re :<..r._ nil 'a nil) '[:seq-re :<..r._ nil a nil]

      (seq-re :<r' nil 'a nil)     '(([:U] a))
      (seq-re :<..r' nil 'a nil)   '((:U a))
      (seq-re :<..r'. nil 'a nil)  '(([:U] a))
      (seq-re :<r'_ nil 'a nil)    '[:seq-re :<r'_ nil a nil]
      (seq-re :<..r'_ nil 'a nil)  '[:seq-re :<..r'_ nil a nil]
      (seq-re :<..r'._ nil 'a nil) '[:seq-re :<..r'._ nil a nil]))

  (testing "Reduction to binary FORMs (in case of mark)"
    (is (= '() (f (seq-re :<r 'a nil) {'a :M})))
    (is (= '(a) (f (seq-re :<r [:- [] 'a] 'a))))
    (is (= '(a) (f (seq-re :<r 'b [:- [] 'a] 'a))))
    (is (= '((a) :U) (f (seq-re :<r [:- 'b :I] 'a :U)))))

  (testing "Irreducable cases"
    (is (= '[:seq-re :<r b [:U] a] (f (seq-re :<r 'b [:- :I 'a] 'a)))) )

  )

