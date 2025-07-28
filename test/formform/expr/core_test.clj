(ns formform.expr.core-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.expr.specs :as sp :refer [fns-with-specs]]
            [formform.expr.core :refer :all]
            [clojure.spec.alpha :refer [valid?]]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))


(def arrangement? (partial valid? ::sp/arrangement))
(def chain>> simplify-nesting-chain)

(defn- simplify-nesting-chain-reversed
  "Reverses expr chain before simplification and reverses again (= restores original order) afterwards. Assuming we simplified the expr chain before with the opposite value of `r->l?`, this should yield an identical result for our test."
  [r->l? exprs env]
  (let [rev-exprs (fn [xs] (reverse (map #(if (arrangement? %)
                                           (cons (first %) (reverse (rest %)))
                                           %) xs)))]
    (rev-exprs (chain>> {:rtl? r->l?} (rev-exprs exprs) env))))

(def chain>>-rtl (partial simplify-nesting-chain-reversed true))
(def chain>>-ltr (partial simplify-nesting-chain-reversed false))

(comment
  ;; test
  (let [xc '[ a nil b]]
    {:normal (chain>> xc {})
     :rtl (chain>>-rtl xc {})
     :reverse (reverse (chain>> (reverse xc) {}))})

  (chain>> '( nil ) {}))


;; ! add more tests with marks ()

;; Here we test not only the match of the simplification with the expected
;; result, but also if it matches the simplification with the reversed nesting
;; direction, where the input sequence is also reversed (such that the operation
;; has the same effect) and the result then restored to the original order. This
;; ensures operational consistency between both nesting directions.
(deftest simplify-nesting-chain-test
  (testing "Correctness of reduction with one form per nesting-level"
    (are [x y] (= y (chain>> x {}) (chain>>-rtl x {}))
      '( )     '[nil] ;; () => () ; ? is this interpretation unproblematic
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
    (are [x y] (= y (chain>> x {}) (chain>>-rtl x {}))
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
    (are [x y] (= y (chain>> x {}) (chain>>-rtl x {}))
      '( [:- nil nil] ) '[nil]
      '( [:- () ()] )   '[()]
      '( [:- a a] )     '[a]

      '( [:- nil a] [:- nil ()] ) '[a] ;; (a (())) => (a)
      '( [:- a ()] [:- nil b] )   '[()])) ;; (a () (b)) => (())

  (testing "Correctness of reduction via (de)generation rule across chain"
    (let [x [[:- 'a 'b] [:- 'a 'c]]]
      (is (= '[[:- a b] c]
             (chain>> x {}) (chain>>-rtl x {}))))
    ;; (a b (a c)) => (a b (c))
    (let [x ['x 'a nil 'b 'x]]
      (is (= '[x]
             (chain>> x {}) (chain>>-rtl x {}))))
    ;; (x (a ( (b (x))))) => (x (a b ())) => (x)
    (let [x [[:- 'a 'b] [:- 'a 'c] [:- 'c 'a 'd]]]
      (is (= '[[:- a b] c d]
             (chain>> x {})
             (chain>>-rtl x {}))))
    ;; (a b (a c (c a d))) => (a b (c (d)))
    (let [x [[:- :u 'b] [:- 'a :i]]]
      (is (= '[[:- :u b]]
             (chain>> x {})
             (chain>>-rtl x {}))))
    ;; (:u b (a :i)) => (:u b (a :u :i)) => (:u b (())) => (:u b)
    (let [x [[:- :u 'b] [:- 'a :i] 'c]]
      (is (= '[[:- :u b]]
             (chain>> x {})
             (chain>>-rtl x {}))))
    ;; (:u b (a :i (c))) => (:u b (a () (c))) => (:u b (())) => (:u b)

    (let [x [:f* [:- :u 'a] [:- :u 'b]]]
      (is (= '[:f* a [:- :u b]]
             (chain>> {:rtl? true} x {})
             (chain>>-ltr x {}))))
    ;; (((:f*) :u a) :u b) => (((:f*) a) :u b)
    (let [x [:f* :u :u]]
      (is (= '[[:- :f* :u]]
             (chain>> {:rtl? true} x {})
             (chain>>-ltr x {}))))))
;; (((:f*) :u) :u) => (((:f*)) :u) => (:f* :u)
