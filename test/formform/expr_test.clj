(ns formform.expr-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]
            [instaparse.core :as insta]))

(deftest find-vars-test
  (testing "At root level"
    (is (= (find-vars [ 'a 'b ] {})
           '(a b)))
    (is (= (find-vars [ "a" "a" ] {})
           '("a")))
    (is (= (find-vars [ "a" 'a ] {}) ;; should this be equal?
           '("a" a))))

  (testing "Empty"
    (is (= (find-vars [] {})
           (find-vars [] {'a :M})
           (find-vars [ '(() ()) '() ] {})
           '())))

  (testing "In different sequentials"
    (is (= (find-vars '(x y) {})
           (find-vars '((x (y x))) {})
           '(x y)))
    (is (= (find-vars (·dna ['x "y"] :NUIMNUIMNUIMNUIM) {})
           (find-vars (FDNA ['x "y"] :NUIMNUIMNUIMNUIM) {})
           '(x "y")))
    (is (= (find-vars (·mem [['a '(x y)] ["b" "z"]] 'a) {})
           (find-vars (MEMORY [['a '(x y)] ["b" "z"]] 'a) {})
           '(a x y "b" "z")))
    (is (= (find-vars (·uncl "foo") {})
           (find-vars (UNCLEAR "foo") {})
           '("foo")))
    (is (= (find-vars (·r ['a 'b] ['c]) {})
           (find-vars (SEQ-REENTRY {} ['a 'b] ['c]) {})
           '(a b c))))

  (testing "In nested special FORMs"
    (is (= (find-vars (·r ['a :M] ['(b) (·2r ['c])]) {})
           '(a b c)))
    (is (= (find-vars (MEMORY [['a (MEMORY [['b :U]] 'c)]] 'd) {})
           '(a b c d))))

  (testing "Nested"
    (is (= (find-vars [ '(a) ] {})
           '(a)))
    (is (= (find-vars [ '((a) b) 'c ] {})
           '(a b c))))

  (testing "Differentiated from other elements"
    (is (= (find-vars [ :N 'a nil ] {})
           '(a))))

  (testing "Variable ordering"
    (is (= (find-vars [ 'a "b" 'c "d" ] {:ordered? false})
           (find-vars [ 'a "b" 'c "d" ] {:ordered? true})
           '(a "b" c "d")))
    (is (= (find-vars [ "b" 'c 'a "d" ] {:ordered? false})
           (find-vars [ "b" 'c 'a "d" ] {})
           '("b" c a "d")))
    (is (= (find-vars [ "b" 'c 'a "d" ] {:ordered? true})
           '(a "b" c "d"))))

  (testing "Specific variables"
    (is (= (find-vars '[ a (:x (x)) "z" ("x" y) ] {:vars #{'x "y" 'z}})
           '(x))))

  (testing "Exotic variable names"
    ;; incorrect ordering due to symbols
    ; (is (= (find-vars '[ "apple tree" ("something" else) ])
    ;        '("apple tree" else "something")))
    ))

(deftest cnt>-test
  (testing "Simple content"
    (testing "FORM"
      (testing "equal"
        (are [x y] (= x y)
          (cnt> nil)    nil
          (cnt> '())    '()
          (cnt> :mn)    :mn
          (cnt> '(:mn)) '(:mn)))
      (testing "marked"
        (are [x y] (= x y)
          (cnt> '(nil))   '()
          (cnt> '(()))    nil
          (cnt> '(:mn))   '(:mn)
          (cnt> '((:mn))) :mn)))

    (testing "Constant"
      (testing "equal"
        (are [x y] (= x y)
          (cnt> :N) nil
          (cnt> :M) '()
          (cnt> :U) :mn
          (cnt> :I) '(:mn)))
      (testing "marked"
        (are [x y] (= x y)
          (cnt> '(:N)) '()
          (cnt> '(:M)) nil
          (cnt> '(:U)) '(:mn)
          (cnt> '(:I)) :mn)))

    (testing "variable"
      (testing "without env"
        (is (= (cnt> 'a) 'a))
        (is (= (cnt> "a") "a")))

      (testing "with env"
        (is (= (cnt> 'a {'a ·M})
               (cnt> '(a) {'a ·N})
               (cnt> '((a (b))) {'a ·I 'b ·I})
               '()))
        (is (= (cnt> "a" {"a" ·U}) :mn)))))

  (testing "Nested content"
    (testing "reduced by calling"
      (is (= (cnt> '(nil () nil () nil))
             (cnt> '(() :M (:N)))
             nil))
      (is (= (cnt> '(:U nil :U :mn))
             '(:mn)))
      (is (= (cnt> '(:I nil :I (:mn)))
             ':mn))
      (is (= (cnt> '("foo" "foo" "bar" "foo"))
             '("foo" "bar")))
      (is (= (cnt> '(a b b a b a))
             '(a b)))
      (is (= (cnt> '((a) (a) (a)))
             'a))
      (is (= (cnt> '((a (b)) (a (b))))
             '((a (b))))))

    (testing "reduced by crossing"
      (is (= (cnt> '(()))
             (cnt> '((nil)))
             (cnt> '(((:M))))
             (cnt> '((((nil) nil) nil) nil))
             nil))
      (is (= (cnt> '((())))
             (cnt> '(((nil))))
             (cnt> '(((((:N))))))
             (cnt> '(((((nil) nil) nil) nil) nil))
             '()))
      (is (= (cnt> '(((()) (()))))
             (cnt> '((((nil)) (:M))))
             (cnt> '(((:U)) ((:I))))
             nil))
      (is (= (cnt> '(((a))))
             '(a)))
      (is (= (cnt> '((a)))
             'a))
      (is (= (cnt> '(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) {'x :U})
             '(:mn))))

    (testing "irreducable"
      (is (= (cnt> '(a b))
             '(a b)))
      ;; should these be reducable? pattern-matching?
      ;; -> might get too complex for eval
      (is (= (cnt> '((a (b)) ((b) a)))
             '((a (b)) ((b) a)))))))


(deftest ctx>-test
  (testing "Relations of simple expressions"
    (testing "FORMs"
      (is (= (ctx> [ nil nil ]) [ ]))
      (is (= (ctx> [ '() '() ]) [ '() ]))
      (is (= (ctx> [ :mn :mn ]) [ :mn ]))
      (is (= (ctx> [ '(:mn) '(:mn) ]) [ '(:mn) ])))

    (testing "Constants"
      (is (= (ctx> [ :N :N ]) [ ]))
      (is (= (ctx> [ :M :M ]) [ '() ]))
      (is (= (ctx> [ :U :U ]) [ :mn ]))
      (is (= (ctx> [ :I :I ]) [ '(:mn) ]))
      (is (= (ctx> [ :M :N ]) [ '() ]))
      (is (= (ctx> [ :U :N ]) [ :mn ]))
      (is (= (ctx> [ :I :N ]) [ '(:mn) ]))
      (is (= (ctx> [ :U :I ]) [ '() ]))
      (is (= (ctx> [ :U :M ]) [ '() ]))
      (is (= (ctx> [ :I :M ]) [ '() ])))

    (testing "Variables"
      (testing "without env"
        (is (= (ctx> [ 'a 'b ])
               [ 'a 'b ]))
        (is (= (ctx> [ 'a 'a ])
               [ 'a ]))
        (is (= (ctx> [ 'a "a" ]) ;; should be equal?
               [ 'a "a" ])))

      (testing "with env"
        (is (= (ctx> [ 'a 'a ] {'a :U})
               [ :mn ]))
        (is (= (ctx> [ 'a 'b ] {'a :U 'b :I})
               [ '() ]))
        (is (= (ctx> [ 'a "a" ] {'a ·U "a" ·I}) ;; should be equal?
               [ '() ]))
        (is (= (ctx> [ 'a 'b ] {'a ·U})
               [ :mn 'b ]))
        (is (= (ctx> (·· 'x) {'x 'y})
               [ 'y ])))

      (testing "with recursive env"
        ;; ? infinite recursion or dissoc from env on first interpretation
        (is (= (ctx> (·· 'x) {'x 'x})
               (ctx> (·· 'x) {'x (·· 'x)})
               '[x]))))

    (testing "mixed"
      (is (= (ctx> [ '(()) nil :N '(((()))) '(() () ()) ])
             [ ]))
      (is (= (ctx> [ '((() ())) :M '() ])
             [ '() ]))
      (is (= (ctx> [ :mn '((:mn)) :U '((:U) (:U)) ])
             [ :mn ]))
      (is (= (ctx> [ '(:mn) '(((:mn))) :I '((:I) (:I)) ])
             [ '(:mn) ]))
      (is (= (ctx> [ :U '(:I) ])
             [ :mn ]))
      (is (= (ctx> [ '(:I) '(:U) ])
             [ '() ]))
      (is (= (ctx> [ '((:mn)) '(:mn) ])
             [ '() ]))
      (is (= (ctx> [ :mn '(:I) ])
             [ :mn ]))
      (is (= (ctx> [ '(:U) '((:mn)) ])
             [ '() ]))
      (is (= (ctx> [ '((:I)) :mn ])
             [ '() ]))))

  (testing "Relations of complex expressions"
    (testing "pure FORMs"
      (is (= (ctx> [ '(()) ])
             [ ]))
      (is (= (ctx> [ '(() ()) ])
             [ ]))
      (is (= (ctx> [ '() '(() ()) ])
             [ '() ]))
      (is (= (ctx> [ '(((() (() ()))) ((() (())) (() ()) (()))) ])
             [ ])))

    (testing "nested variables (reflexion rule)"
      (is (= (ctx> '[ ((a)) ])
             [ 'a ]))
      (is (= (ctx> '[ ((a b c)) ])
             [ 'a 'b 'c ]))
      (is (= (ctx> '[ ((a ((b c)) d)) ((e ((f)))) ])
             [ 'a 'b 'c 'd 'e 'f ]))
      (is (= (ctx> '[ (a ((b c)) d) ((e (f))) ])
             [ '(a b c d) 'e '(f) ])))

    (testing "nested U/I relations (generation rule)"
      (is (= (ctx> '[ :U (:I) ])
             [ :mn ]))
      (is (= (ctx> '[ :U (:N (:I)) ])
             [ '() ]))
      (is (= (ctx> '[ :U (:N (:N (:I))) ])
             [ :mn ]))
      (is (= (ctx> '[ :U (:N (:U (:N (:I)))) ])
             [ '() ]))
      (is (= (ctx> '[ :N (:U (:N (:N (:I)))) ])
             [ '(:mn) ]))
      (is (= (ctx> '[ :I (:U) ])
             [ '(:mn) ]))
      (is (= (ctx> '[ :I (:N (:U)) ])
             [ '() ]))
      (is (= (ctx> '[ :I (:N (:N (:U))) ])
             [ '(:mn) ]))
      (is (= (ctx> '[ :I (:N (:I (:N (:U)))) ])
             [ '() ]))
      (is (= (ctx> '[ :N (:I (:N (:N (:U)))) ])
             [ :mn ]))
      (is (= (ctx> '[ :U (:I a) ])
             [ :mn ]))
      (is (= (ctx> '[ :U ((a :U) :I) ])
             [ :mn ]))
      (is (= (ctx> '[ :U ((a :I) :U) ])
             [ '() ]))
      (is (= (ctx> '[ :I ((a (b :U (c))) c) ])
             [ '(:mn) '((a) c) ]))))

  (testing "Degeneration in nested expressions"
    (is (= (ctx> '[ a b (a c (c a d)) ] {})
           '[ a b (c (d)) ]))
    (is (= (ctx> '[ (a (b a c) (a c (b c d a))) ] {})
           '[ (a (b c) (c (b d))) ]))
    (is (= (ctx> '[ x (a (b x)) (y (a x (b x))) ])
           '[ x (a (b)) (y) ])))

  (testing "Long relations"
    (is (= (ctx> (repeat 100 nil)) [ ]))
    (is (= (ctx> (repeat 100 '())) [ '() ]))
    (is (= (ctx> (repeat 100 :mn)) [ :mn ]))
    (is (= (ctx> (repeat 100 '(:mn))) [ '(:mn) ]))
    (is (= (ctx> (repeat 100 :N)) [ ]))
    (is (= (ctx> (repeat 100 :M)) [ '() ]))
    (is (= (ctx> (repeat 100 :U)) [ :mn ]))
    (is (= (ctx> (repeat 100 :I)) [ '(:mn) ]))))


(deftest =>-test
  (testing "Reducable expressions"
    (testing "FORMs"
      (are [x y] (= x y)
        (=> (make-expr nil))    [ :N ]
        (=> (make-expr '()))    [ :M ]
        (=> (make-expr :mn))    [ :U ]
        (=> (make-expr '(:mn))) [ :I ]))

    (testing "Constants"
      (are [x y] (= x y)
        (=> (make-expr :N)) [ :N ]
        (=> (make-expr :M)) [ :M ]
        (=> (make-expr :U)) [ :U ]
        (=> (make-expr :I)) [ :I ])))

  (testing "irreducable expressions"
    (is (= (=> (make-expr 'a))
           [ :_ ]))
    (is (= (=> (make-expr '("x" ("y"))))
           [ :_ ])))

  (testing "metadata"
    (is (expr-tag? (=> (make-expr 'a))))
    (is (= (meta (=> (make-expr 'a)))
           {:expr ['a], :env {}}))
    (is (= (meta (=> (make-expr 'a) {'a :M}))
           {:expr [()], :env {'a :M}}))))


(let [->nmui (fn [fdna-expr]
               (apply
                str
                (FDNA->varlist (first fdna-expr)) "::"
                (calc/dna->digits (FDNA->dna (first fdna-expr))
                                  calc/nmui-code)))]
  (deftest =>*-test
    (testing "Correctness of returned combinatorial space"
      (is (= (=>* (make-expr nil))
             [[:fdna '() :N ]]))

      (is (= (=>* (make-expr nil) {:to-fdna? false})
             [[:N]]))

      (is (= (=>* (make-expr 'a))
             [[:fdna '(a) :MIUN ]]))

      (is (= (=>* (make-expr 'a) {:to-fdna? false})
             [[:N] [:U] [:I] [:M]]))

      (is (= (=>* (make-expr 'a 'b))
             [[:fdna '(a b) :MMMMMIMIMMUUMIUN ]]))

      (is (= (=>* (make-expr 'a 'b) {:to-fdna? false})
             [[:N] [:U] [:I] [:M]
              [:U] [:U] [:M] [:M]
              [:I] [:M] [:I] [:M]
              [:M] [:M] [:M] [:M]]))

      (is (= (=>* (make-expr 'a 'b 'c))
             [[:fdna '(a b c) :MMMMMMMMMMMMMMMMMMMMMIMIMMMMMIMIMMMMMMMMMMUUMMUUMMMMMIMIMMUUMIUN ]]))

      (is (= (=>* (make-expr 'a 'b 'c) {:to-fdna? false})
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
              [:M] [:M] [:M] [:M]])))

    (testing "Correctness of evaluation for simple seq-re FORMs"
      (is (= (=>* (·r ['a])) (=>* (·2r+1 ['a]))
             '[[:fdna [a] :NIII]]))
      (is (= (=>* (·2r ['a]))
             '[[:fdna [a] :NUUU]]))

      (is (= (=>* (··r ['a])) (=>* (··2r+1 ['a]))
             '[[:fdna [a] :MUUI]]))
      (is (= (=>* (··2r ['a]))
             '[[:fdna [a] :MIIU]]))

      (is (= (=>* (·r' ['a])) (=>* (·2r'+1 ['a]))
             '[[:fdna [a] :NIII]]))
      (is (= (=>* (·2r' ['a]))
             '[[:fdna [a] :NUUU]]))

      (is (= (=>* (··r' ['a])) (=>* (··2r'+1 ['a]))
             '[[:fdna [a] :MIMI]]))
      (is (= (=>* (··2r' ['a]))
             '[[:fdna [a] :MMUU]])))

    (testing "Correctness of evaluation in complex seq-re FORMs"
      (is (= (=>* (·r ['b] [IFORM 'a] ['a]))
             '[[:fdna [a b] :NNNNNINIIIIIIUIU]])))

    (testing "Congruence of evaluated formDNA with formform 1 results"
    ;; SelFi Collection (see https://observablehq.com/@formsandlines/1d-ca-for-4-valued-form-logic-selfis)

    ;; Mark1
      (is (= (->nmui (=>* (·· (·r ['l] ['e] ['r])
                              (·r ['l] ['r] ['e])) {:vars ['l 'e 'r]}))
             "[l e r]::3121103223011213012313312301311301231032230132103121133123011113"))

    ;; StripesD100000
      (is (= (->nmui (=>* (·· (·r ['l] ['e] ['r])) {:vars ['l 'e 'r]}))
             "[l e r]::3302200223013003030323022301030303032002230100003302230223013303"))

    ;; StripesL000100
      (is (= (->nmui (=>* (·· (·r ['l] ['r] ['e])) {:vars ['l 'e 'r]}))
             "[l e r]::3223303000002213022033330000321302203030000032103223333300002213"))

    ;; Mono000101
      (is (= (->nmui (=>* (·· (·r ['l] ['r] ['e])
                              (·r ['e] ['l] ['r])) {:vars ['l 'e 'r]}))
             "[l e r]::3121333303031111222213312002111121211331230111113223333300001113"))

    ;; Rhythm101101
      (is (= (->nmui (=>* (·· (·r ['l] ['e] ['r])
                              (·r ['e] ['r] ['l])
                              (·r ['l] ['r] ['e])
                              (·r ['e] ['l] ['r])) {:vars ['l 'e 'r]}))
             "[l e r]::3121111121211111111113311331111121211331230111111111111111111113"))

    ;; NewSense
      (is (= (->nmui (=>* (·· (·r ['l] ['e] ['r])
                              (·r ['r] ['e] ['l])
                              (·r ['l] ['r] ['e])) {:vars ['l 'e 'r]}))
             "[l e r]::3121121221211213311313311331311301231032230132101111111111111113"))

    ;; Slit / xor4vRnd
      (is (= (->nmui (=>* (·· (· (· 'l) 'r) (· (· 'r) 'l))))
             "[l r]::0123103223013210"))

    ;; or4v
      (is (= (->nmui (=>* (·· 'l 'r)))
             "[l r]::3113121211113210"))

    ;; xorReId / xorReIdRnd
      (is (= (->nmui (=>* (·· (·r' ['l] ['r]) (·r' ['r] ['l]))))
             "[l r]::2121123223011212"))

    ;; Rule4v30
      (is (= (->nmui (=>* (·· (· (· 'l) 'e 'r)
                              (· (· 'e) 'l) (· (· 'r) 'l)) {:vars ['l 'e 'r]}))
             "[l e r]::0220212122220123133130303333103220020303000023013113121211113210"))

    ;; Rule4v111
      (is (= (->nmui (=>* (·· (· (· (· 'l) 'e) 'r)
                              (· (· (· 'l) 'r) 'e)
                              (· (· (· 'e) 'r) 'l)) {:vars ['l 'e 'r]}))
             "[l e r]::2121121221211212311313311331311301231032230132101111111111111111"))

    ;; Structure111Re / Co(mprehend)OneAnother (identical to “NewSense”)
      (is (= (->nmui (=>* (·· (·r ['l] ['e] ['r])
                              (·r ['e] ['r] ['l])
                              (·r ['l] ['r] ['e])) {:vars ['l 'e 'r]}))
             "[l e r]::3121121221211213311313311331311301231032230132101111111111111113"))

    ;; Rule4v110
      (is (= (->nmui (=>* (·· (· (· 'e) 'r)
                              (· (· 'r) 'e)
                              (· (· 'r) 'l)) {:vars ['l 'e 'r]}))
             "[l e r]::0123121221213210311310321331321001231032230132103113121211113210"))

    ;; uniTuringReRnd
      (is (= (->nmui (=>* (· (· (·r ['l] ['e] ['r])
                                (·r ['e] ['r] ['l])
                                (·r ['l] ['r] ['e]))
                             (· 'l 'e 'r)) {:vars ['l 'e 'r]}))
             "[l e r]::3123121221213213311310321331321001231032230132103113121211113210"))

      ))) 


(deftest expand-expr-test
  (testing "Correctness of transformation"
    (is (= (expand-expr (·· 'x 'y))
           '((x y))))
    (is (= (expand-expr (·mem [['a :M] ['b :U]] (·· 'x 'y)))
           '[:mem [[a [:M]] [b [:U]]] x y]))))


(deftest dna-expr?-test
  (testing "Validity of dna-expr"
    (is (fdna? ^:expr [:fdna ['a 'b] :IUMNIUMNIUMNIUMN])))) 


(deftest reduce-dna-test
  (testing "Basic functionality"
    (is (= (reduce-fdna (FDNA ['a] :NUIM) {'a :U}) :I))
    (is (= (reduce-fdna
            (FDNA ['a 'b] (calc/consts->dna [:N :U :I :M
                                             :U :I :M :I
                                             :I :M :I :U
                                             :M :I :U :N])) {'a :M})
           '[:fdna [b] :NUIM])))

  (testing "Partial matches in dictionary"
    (is (= (reduce-fdna (FDNA ['a] :NUIM) {'x :M})
           '[:fdna [a] :NUIM]))
    (is (= (reduce-fdna (FDNA ['a] :NUIM) {'x :M 'a :U})
           :I)))

  (testing "Correctness of transformations"
    (are [x y] (= (reduce-fdna (FDNA ['a] :NUIM) {'a x}) y)
      :N :M
      :U :I
      :I :U
      :M :N)

    (is (= (reduce-fdna
            (FDNA ['a 'b] (calc/consts->dna [:N :U :I :M
                                             :U :I :M :I
                                             :I :M :I :U
                                             :M :I :U :N])) {'a :U})
           '[:fdna [b] :IMIU])))

  )

(deftest expand-fdna-test
  ;; ! unchecked
  (testing "Correctness of transformation"
    (is (= (expand-fdna (FDNA)) '[:N]))
    (is (= (expand-fdna (FDNA :U)) '[:U]))
    (is (= (expand-fdna (FDNA [] :M)) '[:M]))
    (is (= (expand-fdna (FDNA ['a] :NUIM))
           '[((:M) (([:<re [(a)]] [:<..re [(a)]])))
             ((:I) ((([:<re [(a)]] a) ([:<..re [a]] (a)))))
             ((:U) ((([:<re [a]] (a)) ([:<..re [(a)]] a))))
             ((:N) (([:<re [a]] [:<..re [a]])))]))
    (is (= (expand-fdna (FDNA ['a 'b] :MINIUMINUMNIMMIM))
           '[((:I) (([:<re [a]] [:<..re [a]]))
                   (([:<re [(b)]] [:<..re [(b)]])))
             ((:I) (([:<re [a]] [:<..re [a]]))
                   ((([:<re [b]] (b)) ([:<..re [(b)]] b))))
             ((:U) ((([:<re [(a)]] a) ([:<..re [a]] (a))))
                   (([:<re [b]] [:<..re [b]])))
             ((:M) (([:<re [a]] [:<..re [a]]))
                   (([:<re [b]] [:<..re [b]])))
             ((:M) (([:<re [(a)]] [:<..re [(a)]]))
                   (([:<re [b]] [:<..re [b]])))
             ((:M) (([:<re [(a)]] [:<..re [(a)]]))
                   (([:<re [(b)]] [:<..re [(b)]])))
             ((:M) ((([:<re [(a)]] a) ([:<..re [a]] (a))))
                   ((([:<re [b]] (b)) ([:<..re [(b)]] b))))
             ((:U) ((([:<re [a]] (a)) ([:<..re [(a)]] a)))
                   (([:<re [b]] [:<..re [b]])))
             ((:N) ((([:<re [(a)]] a) ([:<..re [a]] (a))))
                   ((([:<re [(b)]] b) ([:<..re [b]] (b)))))
             ((:M) (([:<re [(a)]] [:<..re [(a)]]))
                   ((([:<re [b]] (b)) ([:<..re [(b)]] b))))
             ((:M) ((([:<re [a]] (a)) ([:<..re [(a)]] a)))
                   ((([:<re [b]] (b)) ([:<..re [(b)]] b))))
             ((:N) ((([:<re [a]] (a)) ([:<..re [(a)]] a)))
                   (([:<re [(b)]] [:<..re [(b)]])))
             ((:N) (([:<re [a]] [:<..re [a]]))
                   ((([:<re [(b)]] b) ([:<..re [b]] (b)))))
             ((:I) ((([:<re [(a)]] a) ([:<..re [a]] (a))))
                   (([:<re [(b)]] [:<..re [(b)]])))
             ((:I) ((([:<re [a]] (a)) ([:<..re [(a)]] a)))
                   ((([:<re [(b)]] b) ([:<..re [b]] (b)))))
             ((:I) (([:<re [(a)]] [:<..re [(a)]]))
                   ((([:<re [(b)]] b) ([:<..re [b]] (b)))))]))))


(deftest reduce-unclear-test
  (testing "Basic functionality"
    (is (= (reduce-unclear (UNCLEAR "hey") {})
           '[:fdna ["hey"] :NUUU]))

    (are [x y] (= (reduce-unclear (UNCLEAR "unkFo") {"unkFo" x}) y)
      :N :U
      :U :U
      :I :U
      :M :N)

    (is (= (ctx> (·uncl "hey") {"hey" :M})
           '[])))) 

(deftest expand-unclear-test
  (testing "Correctness of transformation"
    (is (= (expand-unclear (UNCLEAR "foo"))
           [[:<re ["foo"] ["foo"]]]))
    (is (= (expand-unclear (UNCLEAR #{:X :Y} #".+"))
           [[:<re ["#{:Y :X}.+"] ["#{:Y :X}.+"]]])))) 


(deftest filter-rems-test
  (testing "Removal of unreferenced shadowed rems"
    (is (= (#'expr/filter-rems [['a '(x)] ['x 'a] ['x :M]] (·· 'x))
           '[[x :M]]))
    (is (= (#'expr/filter-rems [['a :N] ['x :M] ['a '(x)]] (·· 'a))
           '[[a (x)] [x :M]])))) 

(deftest reduce-memory-test
  (testing "Correctness of reduction"
    (is (= (reduce-memory (MEMORY [['a :M]] 'a) {})
           (reduce-memory (MEMORY [['a MARK]] 'a) {})
           (reduce-memory (MEMORY [['a ·mark]] 'a) {})
           (reduce-memory (MEMORY [['a (·· MARK)]] 'a) {})
           '()))
    (is (= (reduce-memory (MEMORY [['a :U]] (· 'b) 'a) {})
           '(((b) :mn))))
    (is (= (reduce-memory (MEMORY [['x :N]] 'y) {})
           'y))
    )

  (testing "In expression context"
    (is (= (ctx> (·mem [['a :U]] (· 'a) 'b))
           (ctx> (·· (·mem [['a :U]] (· 'a)) 'b))
           '[(:mn) b]))
    (is (= (ctx> (·· (·mem [['a :U]] 'a) 'b))
           '[:mn b]))
    ;; env substitution has priority over degeneration from observed values
    (is (= (ctx> (·· 'y (·mem [['y 'z]] 'y)))
           '[y z]))
    ;; rems can shadow previous rems or given reduction env
    (is (= (ctx> (·· 'x (·mem [['y 'z]] 'x)) {'x 'y})
           (ctx> (·· 'x (·mem [['x 'z]] 'x)) {'x 'y})
           '[y z]))
    (is (= (ctx> (·mem [['y 'x]] 'x) {'x 'y})
           (ctx> (·mem [['y (·· 'x)]] 'x) {'x 'y})
           '[y]))
    (is (= (ctx> (·mem [['x 'y] ['y 'z]] 'w) {'w 'x})
           (ctx> (·mem [['y 'z] ['x 'y]] 'w) {'w 'x})
           '[z]))
    (is (= (ctx> (·mem [['x (·· 'x 'z)]] 'x) {'x 'y})
           '[y z]))
    (is (= (ctx> (·· 'x (·mem [['y 'x]] 'x)) {'x 'y})
           '[y]))
    )

  (testing "Substitution of values from recursive rems"
    ;; (= ctx' ctx) because reduce-by-calling:
    (is (= (reduce-memory (MEMORY [[:x (·· :x)]] :x) {})
           :x))
    ;; ? merge context during repeated substitution or only once afterwards?
    (is (= (reduce-memory (MEMORY [[:x (·· 'a (·· :x))]] :x) {})
           '[:mem [[:x [a :x]]] a :x])))

  (testing "Exception in infinite reduction (stack overflow)"
    ;; infinite recursion because outer expr env nullifies previous dissoc:
    (is (thrown-with-msg? Exception #"Context too deeply nested"
                          (reduce-memory (MEMORY [[:x (· :x)]] :x) {})))
    (is (thrown-with-msg? Exception #"Context too deeply nested"
                          (reduce-memory (MEMORY [[:x (·· (· :x))]] :x) {})))
    (is (thrown-with-msg? Exception #"Context too deeply nested"
                          (reduce-memory (MEMORY [[:x (· (·· :x))]] :x) {})))
    )

  (testing "Combined with outer env"
    (= (reduce-memory (MEMORY [['a :N]] 'a 'b) {'b :U})
       :mn))

  (testing "Reduction of shadowed rems"
    (is (= (reduce-memory (MEMORY [['a '(x)] ['b (·· 'a :U)]] (·· 'b)) {})
           '(((x) :mn)))))) 

(deftest expand-memory-test
  (testing "Correctness of transformation"
    (is (= (expand-memory (first (·mem [['a :M] ['b :U]] 'x 'y)))
           '(((a :M) ((a) (:M))) ((b :U) ((b) (:U))) (x y))))))


(def seqre-opts
  (vec (for [interpr [:rec-instr :rec-ident]
             open?   [false true]
             parity  [:any :even :odd]]
         {:parity parity, :open? open?, :interpr interpr})))

(deftest seq-reentry-sign-test
  (testing "All possible inputs"
    (are [x y] (= x y)
      (seq-reentry-opts->sign (seqre-opts 0))  :<re
      (seq-reentry-opts->sign (seqre-opts 1))  :<..re
      (seq-reentry-opts->sign (seqre-opts 2))  :<..re.
      (seq-reentry-opts->sign (seqre-opts 3))  :<re_
      (seq-reentry-opts->sign (seqre-opts 4))  :<..re_
      (seq-reentry-opts->sign (seqre-opts 5))  :<..re._

      (seq-reentry-opts->sign (seqre-opts 6))  :<re'
      (seq-reentry-opts->sign (seqre-opts 7))  :<..re'
      (seq-reentry-opts->sign (seqre-opts 8))  :<..re'.
      (seq-reentry-opts->sign (seqre-opts 9))  :<re'_
      (seq-reentry-opts->sign (seqre-opts 10)) :<..re'_
      (seq-reentry-opts->sign (seqre-opts 11)) :<..re'._)))

(deftest seq-reentry-sign->opts-test
  (testing "Basic functionality"
    (is (= (seq-reentry-sign->opts :<re) (seqre-opts 0)))
    (is (= (seq-reentry-sign->opts :<..re'._) (seqre-opts 11))))) 

(deftest seq-reentry-expr-test
  (testing "Empty expressions"
    (are [x y] (= x y)
      (·seq-re (seqre-opts 0))  [[:<re]]
      (·seq-re (seqre-opts 1))  [[:<..re]]
      (·seq-re (seqre-opts 2))  [[:<..re.]]
      (·seq-re (seqre-opts 3))  [[:<re_]]
      (·seq-re (seqre-opts 4))  [[:<..re_]]
      (·seq-re (seqre-opts 5))  [[:<..re._]]

      (·seq-re (seqre-opts 6))  [[:<re']]
      (·seq-re (seqre-opts 7))  [[:<..re']]
      (·seq-re (seqre-opts 8))  [[:<..re'.]]
      (·seq-re (seqre-opts 9))  [[:<re'_]]
      (·seq-re (seqre-opts 10)) [[:<..re'_]]
      (·seq-re (seqre-opts 11)) [[:<..re'._]]))

  (testing "Default type"
    (is (= (·seq-re {}) '[[:<re]])))

  (testing "Content number and type"
    (is (= (·seq-re {} ['x]) '[[:<re [x]]]))
    (is (= (·seq-re {} ['x] ['y]) '[[:<re [x] [y]]]))
    (is (expr? (·seq-re {})))
    (is (vector? (second (first (·seq-re {} (·· 'x 'y) ['z])))))))


(deftest reduce-context-chain-test
  (testing "Correctness of rightward nesting-reduction"
    (are [x y] (= (reduce-context-chain x {}) y)
      '( [] [] )   '[[] []] ;; (())
      '( [a] [] )  '[[a] []] ;; (a ())
      '( [] [a] )  '[[] [a]] ;; ((a))
      '( [a] [b] ) '[[a] [b]] ;; (a (b))

      '( [] [] [] )    '[[]] ;; ((())) => ()
      '( [a] [] [] )   '[[a]] ;; (a)
      '( [] [a] [] )   '[[] [a] []] ;; ((a ()))
      '( [] [] [a] )   '[[a]] ;; (a)
      '( [] [a] [b] )  '[[] [a] [b]] ;; ((a (b)))
      '( [a] [] [b] )  '[[a b]] ;; (a b)
      '( [a] [b] [] )  '[[a] [b] []] ;; (a (b ()))
      '( [a] [b] [c] ) '[[a] [b] [c]] ;; (a (b (c)))

      '( [] [] [] [] )    '[[] []] ;; (((()))) => (())
      '( [a] [b] [] [] )  '[[a] [b]] ;; (a (b))
      '( [a] [] [] [b] )  '[[a] [b]] ;; (a (b))
      '( [] [] [a] [b] )  '[[a] [b]] ;; (a (b))
      '( [a] [] [b] [c] ) '[[a b] [c]] ;; (a b (c))
      '( [a] [b] [] [c] ) '[[a] [b c]] ;; (a (b c))

      '( [a] [] [b] [] [c] ) '[[a b c]] ;; (a b c)
      '( [a] [] [] [b] [c] ) '[[a] [b] [c]] ;; (a (b (c)))
      '( [a] [b] [] [] [c] ) '[[a] [b] [c]] ;; (a (b (c)))
      '( [a] [] [] [] [b] )  '[[a b]] ;; (a ((((b))))) => (a b)
      '( [] [a] [] [b] [] )  '[[] [a b] []] ;; ((a b ()))
      '( [] [] [] [a] [b] )  '[[] [a] [b]] ;; ((a (b)))
      '( [a] [b] [] [] [] )  '[[a] [b] []])) ;; (a (b ()))

  (testing "Correctness of leftward nesting-reduction"
    (are [x y] (= (reduce-context-chain x {:rtl true}) y)
      '( [] [] )   '[[] []] ;; (())
      '( [a] [] )  '[[a] []] ;; ((a))
      '( [] [a] )  '[[] [a]] ;; (() a)
      '( [a] [b] ) '[[a] [b]] ;; ((a) b)

      '( [] [] [] )    '[[]] ;; ((())) => ()
      '( [a] [] [] )   '[[a]] ;; (a)
      '( [] [a] [] )   '[[] [a] []] ;; ((() a))
      '( [] [] [a] )   '[[a]] ;; ((()) a) => (a)
      '( [] [a] [b] )  '[[] [a] [b]] ;; ((() a) b)
      '( [a] [] [b] )  '[[a b]] ;; (((a)) b) => (a b)
      '( [a] [b] [] )  '[[a] [b] []] ;; (((a) b))
      '( [a] [b] [c] ) '[[a] [b] [c]] ;; (((a) b) c)

      '( [] [] [] [] )    '[[] []] ;; (((()))) => (())
      '( [a] [b] [] [] )  '[[a] [b]] ;; ((((a) b))) => ((a) b)
      '( [a] [] [] [b] )  '[[a] [b]] ;; ((((a))) b) => ((a) b)
      '( [] [] [a] [b] )  '[[a] [b]] ;; (((()) a) b) => ((a) b)
      '( [a] [] [b] [c] ) '[[a b] [c]] ;; ((((a)) b) c) => ((a b) c)
      '( [a] [b] [] [c] ) '[[a] [b c]] ;; ((((a) b)) c) => ((a) b c)

      '( [a] [] [b] [] [c] ) '[[a b c]] ;; (((((a)) b)) c) => (a b c)
      '( [a] [] [] [b] [c] ) '[[a] [b] [c]] ;; (((a) b) c)
      '( [a] [b] [] [] [c] ) '[[a] [b] [c]] ;; (((a) b) c)
      '( [a] [] [] [] [b] )  '[[a b]] ;; (((((a)))) b) => (a b)
      '( [] [a] [] [b] [] )  '[[] [a b] []] ;; ((((() a)) b)) => ((() a b))
      '( [] [] [] [a] [b] )  '[[] [a] [b]] ;; ((((())) a) b) => ((() a) b)
      '( [a] [b] [] [] [] )  '[[a] [b] []])) ;; (((((a) b)))) => (((a) b))

  (testing "Correctness of context reduction"
    (is (= (reduce-context-chain [['a 'b] ['a 'c]] {})
           '[[a b] [c]]))
    (is (= (reduce-context-chain [['a 'b] ['a 'c] ['c 'a 'd]] {})
           '[[a b] [c] [d]]))
    (is (= (reduce-context-chain [[UFORM 'b] ['a IFORM]] {})
           '[[:mn b] [()]]))
    (is (= (reduce-context-chain [[UFORM 'b] ['a IFORM] ['c]] {})
           '[[:mn b] [()]]))

    (is (= (reduce-context-chain {:rtl? true}
                                    [['c] ['a IFORM] [UFORM 'b]] {})
           '[[()] [:mn b]]))
    (is (= (reduce-context-chain {:rtl? true}
                                    [[:f*] [UFORM 'a] [UFORM 'b]] {})
           '[[:f*] [a] [:mn b]]))
    (is (= (reduce-context-chain {:rtl? true}
                                    [[:f*] [UFORM] [UFORM]] {})
           '[[:f* :mn]])))
  )

(deftest reduce-seq-reentry-test
  (testing "Reduction of primitive seq-re types"
    (are [x1 x2 y] (= (reduce-seq-reentry (first x1) {})
                      (reduce-seq-reentry (first x2) {}) y)
      (·r [])     (·r' [])     IFORM
      (·2r [])    (·2r' [])    UFORM
      (·2r+1 [])  (·2r'+1 [])  IFORM
      (··r [])    (··r' [])    IFORM
      (··2r [])   (··2r' [])   UFORM
      (··2r+1 []) (··2r'+1 []) IFORM)

    (are [x1 x2 y] (= (reduce-seq-reentry (first x1) {})
                      (reduce-seq-reentry (first x2) {}) y)
      (·r [] [])     (·r' [] [])     UFORM
      (·2r [] [])    (·2r' [] [])    UFORM
      (·2r+1 [] [])  (·2r'+1 [] [])  UFORM
      (··r [] [])    (··r' [] [])    IFORM
      (··2r [] [])   (··2r' [] [])   IFORM
      (··2r+1 [] []) (··2r'+1 [] []) IFORM))

  (testing "Reduction from all possible ambiguous cases"
    (let [f (fn [x] (reduce-seq-reentry (first x) {}))]
      (is (= (f (·r [UFORM] []))     (f (·r [IFORM] []))
             (f (·2r [UFORM] []))    (f (·2r [IFORM] []))
             (f (·2r+1 [UFORM] []))  (f (·2r+1 [IFORM] []))
             (f (··r [UFORM] []))    (f (··r [IFORM] []))
             (f (··2r [UFORM] []))   (f (··2r [IFORM] []))
             (f (··2r+1 [UFORM] [])) (f (··2r+1 [IFORM] []))
             IFORM))
      (is (= (f (··r' [UFORM] []))
             (f (··2r' [UFORM] [])) (f (··2r'+1 [UFORM] []))
             (f (··r' [IFORM] []))
             (f (··2r' [IFORM] [])) (f (··2r'+1 [IFORM] []))
             IFORM))
      ;; Exceptions in alternative interpretation
      (is (= (f (·r' [UFORM] []))
             (f (·2r' [UFORM] [])) (f (·2r'+1 [UFORM] []))
             UFORM))
      (is (= (f (·r' [IFORM] []))
             (f (·2r' [IFORM] [])) (f (·2r'+1 [IFORM] []))
             MARK))
      ))

  (testing "Non-reduction of uninterpreted expressions"
    (are [x y] (= (reduce-seq-reentry (first x) {}) y)
      (·r ['a] [])     '[:<re [a] []]
      (·2r ['a] [])    '[:<..re [a] []]
      (·2r+1 ['a] [])  '[:<..re. [a] []]
      (··r ['a] [])    '[:<re_ [a] []]
      (··2r ['a] [])   '[:<..re_ [a] []]
      (··2r+1 ['a] []) '[:<..re._ [a] []]

      (·r' ['a] [])     '((:mn a))
      (·2r' ['a] [])    '((:mn a))
      (·2r'+1 ['a] [])  '((:mn a))
      (··r' ['a] [])    '[:<re'_ [a] []]
      (··2r' ['a] [])   '[:<..re'_ [a] []]
      (··2r'+1 ['a] []) '[:<..re'._ [a] []])

    (are [x y] (= (reduce-seq-reentry (first x) {}) y)
      (·r ['a])     '[:<re [a]]
      (·2r ['a])    '[:<..re [a]]
      (·2r+1 ['a])  '[:<..re. [a]]
      (··r ['a])    '[:<re_ [a]]
      (··2r ['a])   '[:<..re_ [a]]
      (··2r+1 ['a]) '[:<..re._ [a]]

      (·r' ['a])     '[:<re' [a]]
      (·2r' ['a])    '[:<..re' [a]]
      (·2r'+1 ['a])  '[:<..re'. [a]]
      (··r' ['a])    '(((:mn) a))
      (··2r' ['a])   '((:mn a))
      (··2r'+1 ['a]) '(((:mn) a)))

    (are [x y] (= (reduce-seq-reentry (first x) {}) y)
      (·r [] ['a])     '[:<re [] [a]]
      (·2r [] ['a])    '[:<..re [] [a]]
      (·2r+1 [] ['a])  '[:<..re. [] [a]]
      (··r [] ['a])    '[:<re_ [] [a]]
      (··2r [] ['a])   '[:<..re_ [] [a]]
      (··2r+1 [] ['a]) '[:<..re._ [] [a]]

      (·r' [] ['a])     '[:<re' [] [a]]
      (·2r' [] ['a])    '[:<..re' [] [a]]
      (·2r'+1 [] ['a])  '[:<..re'. [] [a]]
      (··r' [] ['a])    '(((:mn) a))
      (··2r' [] ['a])   '(((:mn) a))
      (··2r'+1 [] ['a]) '(((:mn) a)))

    (are [x y] (= (reduce-seq-reentry (first x) {}) y)
      (·r [] ['a] [])     '[:<re [] [a] []]
      (·2r [] ['a] [])    '[:<..re [] [a] []]
      (·2r+1 [] ['a] [])  '[:<..re. [] [a] []]
      (··r [] ['a] [])    '[:<re_ [] [a] []]
      (··2r [] ['a] [])   '[:<..re_ [] [a] []]
      (··2r+1 [] ['a] []) '[:<..re._ [] [a] []]

      (·r' [] ['a] [])     '(((:mn) a))
      (·2r' [] ['a] [])    '((:mn a))
      (·2r'+1 [] ['a] [])  '(((:mn) a))
      (··r' [] ['a] [])    '[:<re'_ [] [a] []]
      (··2r' [] ['a] [])   '[:<..re'_ [] [a] []]
      (··2r'+1 [] ['a] []) '[:<..re'._ [] [a] []]))

  (testing "Reduction to binary FORMs (in case of mark)"
    (is (= (reduce-seq-reentry (first (·r ['a] [])) {'a :M})
           '()))
    (is (= (reduce-seq-reentry (first (·r [MARK 'a] ['a])) {})
           '(a)))
    (is (= (reduce-seq-reentry (first (·r ['b] [MARK 'a] ['a])) {})
           '(a)))
    (is (= (reduce-seq-reentry (first (·r ['b IFORM] ['a] [UFORM])) {})
           '((a) :mn))))

  (testing "Irreducable cases"
    (is (= (reduce-seq-reentry (first (·r ['b] [IFORM 'a] ['a])) {})
           '[:<re [b] [(:mn)] [a]])) )

  )

(deftest nested-expr-test
  (testing "Shape of simple cases"
    (is (= (nested-expr {} [])
           '[()]))
    (is (= (nested-expr {} [] [])
           '[(())]))
    (is (= (nested-expr {} ['a])
           '[(a)]))
    (is (= (nested-expr {} ['a] ['b])
           '[((a) b)]))
    (is (= (nested-expr {} ['a] ['b] ['c])
           '[(((a) b) c)]))
    (is (= (nested-expr {:unmarked? true} [])
           '[]))
    (is (= (nested-expr {:unmarked? true} [] [])
           '[()]))
    (is (= (nested-expr {:unmarked? true} ['a])
           '[a]))
    (is (= (nested-expr {:unmarked? true} ['a] ['b])
           '[(a) b]))
    (is (= (nested-expr {:unmarked? true} ['a] ['b] ['c])
           '[((a) b) c]))

    (is (= (nested-expr {:rightwards? true} [])
           '[()]))
    (is (= (nested-expr {:rightwards? true} [] [])
           '[(())]))
    (is (= (nested-expr {:rightwards? true} ['a])
           '[(a)]))
    (is (= (nested-expr {:rightwards? true} ['a] ['b])
           '[(a (b))]))
    (is (= (nested-expr {:rightwards? true} ['a] ['b] ['c])
           '[(a (b (c)))]))
    (is (= (nested-expr {:rightwards? true :unmarked? true} [])
           '[]))
    (is (= (nested-expr {:rightwards? true :unmarked? true} [] [])
           '[()]))
    (is (= (nested-expr {:rightwards? true :unmarked? true} ['a])
           '[a]))
    (is (= (nested-expr {:rightwards? true :unmarked? true} ['a] ['b])
           '[a (b)]))
    (is (= (nested-expr {:rightwards? true :unmarked? true} ['a] ['b] ['c])
           '[a (b (c))])))

  (testing "Shape of output expression"
    (is (= (nested-expr {} [:f 'a] [] ['b] [])
           '[((((:f a)) b))])))) 

;; ! check more thoroughly if these are correct
(let [f (fn f [opts & ctx] (expand-seq-reentry
                            (first (apply ·seq-re opts ctx))))]
  (deftest expand-seq-re-test
    (testing "Shape of empty expression"
      (are [x y] (= x y)
         ;; ? should nil be removed or valuable to retain context?
        (f (seqre-opts 0))  '[[:mem [[:f* [((:f*))]] [:f1 [(:f*)]]] :f1]]
        (f (seqre-opts 1))  '[[:mem [[:f* [((:f*))]]] :f*]]
        (f (seqre-opts 2))  '[[:mem [[:f* [((:f*))]] [:f1 [(:f*)]]] :f1]]
        (f (seqre-opts 3))  '[[:mem [[:f* [((:f*))]] [:f2 [(:f*)]] [:f1 [:f2]]] :f1]]
        (f (seqre-opts 4))  '[[:mem [[:f* [((:f*))]] [:f1 [:f*]]] :f1]]
        (f (seqre-opts 5))  '[[:mem [[:f* [((:f*))]] [:f2 [(:f*)]] [:f1 [:f2]]] :f1]]

        (f (seqre-opts 6))  '[[:mem [[:f* [((:f*))]] [:f1 [(:f*)]]] :f1]]
        (f (seqre-opts 7))  '[[:mem [[:f* [((:f*))]]] :f*]]
        (f (seqre-opts 8))  '[[:mem [[:f* [((:f*))]] [:f1 [(:f*)]]] :f1]]
        (f (seqre-opts 9))  '[[:mem [[:f* [((:f*))]] [:f2 [(:f*)]] [:f1 [:f2]]] :f1]]
        (f (seqre-opts 10)) '[[:mem [[:f* [((:f*))]] [:f1 [:f*]]] :f1]]
        (f (seqre-opts 11)) '[[:mem [[:f* [((:f*))]] [:f2 [(:f*)]] [:f1 [:f2]]] :f1]]))

    (testing "Shape of expressions with resolution 1"
      (are [x y] (= x y)
         ;; ? should redundant content (inner 'a) be reduced?
        (f (seqre-opts 0) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f1 [(:f* a)]]] :f1]]
        (f (seqre-opts 1) ['a])  '[[:mem [[:f* [((:f* a) a)]]] :f*]]
        (f (seqre-opts 2) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f1 [(:f* a)]]] :f1]]
        (f (seqre-opts 3) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f2 [(:f* a)]] [:f1 [:f2 a]]] :f1]]
        (f (seqre-opts 4) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f1 [:f* a]]] :f1]]
        (f (seqre-opts 5) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f2 [(:f* a)]] [:f1 [:f2 a]]] :f1]]

        (f (seqre-opts 6) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f1 [(:f* a)]]] :f1]]
        (f (seqre-opts 7) ['a])  '[[:mem [[:f* [((:f* a) a)]]] :f*]]
        (f (seqre-opts 8) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f1 [(:f* a)]]] :f1]]
        (f (seqre-opts 9) ['a])  '[[:mem [[:f* [((:f* a) a)]] [:f2 [(:f* a)]] [:f1 [:f2 a]]] :f1]]
        (f (seqre-opts 10) ['a]) '[[:mem [[:f* [((:f* a) a)]] [:f1 [:f* a]]] :f1]]
        (f (seqre-opts 11) ['a]) '[[:mem [[:f* [((:f* a) a)]] [:f2 [(:f* a)]] [:f1 [:f2 a]]] :f1]]))

    (testing "Shape of expressions with even resolution"
      (are [x y] (= x y)
         ;; ? should even/odd construct redundant re-entries?
        (f (seqre-opts 0) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]]] :f*]]
        (f (seqre-opts 1) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]]] :f*]]
        (f (seqre-opts 2) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]]] :f*]]
        (f (seqre-opts 3) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]] [:f1 [(:f* a) b]]] :f1]]
        (f (seqre-opts 4) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]] [:f1 [(:f* a) b]]] :f1]]
        (f (seqre-opts 5) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]] [:f1 [(:f* a) b]]] :f1]]

        (f (seqre-opts 6) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]]] :f*]]
        (f (seqre-opts 7) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]]] :f*]]
        (f (seqre-opts 8) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]]] :f*]]
        (f (seqre-opts 9) ['a] ['b])  '[[:mem [[:f* [((:f* a) b)]] [:f1 [(:f* a) b]]] :f1]]
        (f (seqre-opts 10) ['a] ['b]) '[[:mem [[:f* [((:f* a) b)]] [:f1 [(:f* a) b]]] :f1]]
        (f (seqre-opts 11) ['a] ['b]) '[[:mem [[:f* [((:f* a) b)]] [:f1 [(:f* a) b]]] :f1]]))

    (testing "Shape of expressions with odd resolution"
      (are [x y] (= x y)
        (f (seqre-opts 0) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f1 [(((:f* a) b) c)]]] :f1]]
        (f (seqre-opts 1) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]]] :f*]]
        (f (seqre-opts 2) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f1 [(((:f* a) b) c)]]] :f1]]
        (f (seqre-opts 3) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f2 [(((:f* a) b) c)]] [:f1 [((:f2 a) b) c]]] :f1]]
        (f (seqre-opts 4) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f1 [((:f* a) b) c]]] :f1]]
        (f (seqre-opts 5) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f2 [(((:f* a) b) c)]] [:f1 [((:f2 a) b) c]]] :f1]]

        (f (seqre-opts 6) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f1 [(((:f* a) b) c)]]] :f1]]
        (f (seqre-opts 7) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]]] :f*]]
        (f (seqre-opts 8) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f1 [(((:f* a) b) c)]]] :f1]]
        (f (seqre-opts 9) ['a] ['b] ['c])  '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f2 [(((:f* a) b) c)]] [:f1 [((:f2 a) b) c]]] :f1]]
        (f (seqre-opts 10) ['a] ['b] ['c]) '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f1 [((:f* a) b) c]]] :f1]]
        (f (seqre-opts 11) ['a] ['b] ['c]) '[[:mem [[:f* [((((((:f* a) b) c) a) b) c)]] [:f2 [(((:f* a) b) c)]] [:f1 [((:f2 a) b) c]]] :f1]]))

    (testing "Arbitrary expressions"
      (is (= (expand-seq-reentry (first (··2r [] ['x] [])))
             '[[:mem [[:f* [((((((:f*) x))) x))]] [:f1 [((:f*) x)]]] :f1]])))
    )) 


(deftest ·N->M-test
  (is (= (·N->M 'a) '[([:<re [(a)]] [:<..re [(a)]])]))) 

(deftest ·M->M-test
  (is (= (·M->M 'a) '[([:<re [a]] [:<..re [a]])])))

(deftest ·U->M-test
  (is (= (·U->M 'a) '[(([:<re [(a)]] a) ([:<..re [a]] (a)))])))

(deftest ·I->M-test
  (is (= (·I->M 'a) '[(([:<re [a]] (a)) ([:<..re [(a)]] a))])))

(deftest ·sel-test
  (testing "Correct expression"
    (is (= (·sel {})
           '[()]))
    (is (= (·sel {'a :M})
           '[((a))]))
    (is (= (·sel {'a :I})
           '[((a) ([:<..re])) (a ([:<re]))]))
    (is (= (·sel {'a :M 'b :N 'c :M})
           '[((a) b (c))]))
    (is (= (·sel {'a :M, 'b :N, 'c :U})
           '[(a (b) c ([:<..re])) (a (b) (c) ([:<re]))])))

  (testing "Without simplification"
    (is (= (·sel {} false)
           '[(([:<..re])) (([:<re]))]))
    (is (= (·sel {'a :M} false)
           '[(a ([:<..re])) (a ([:<re]))])))) 

(def tree formula->parsetree)
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
      (is (= (tree "'a'") [:S [:VAR "a"]]))
      (is (= (tree "\"a\"") [:S [:VAR "a"]]))
      (is (= (tree "/a/") [:S [:UNCLEAR "a"]]))

      (is (= (tree "'a'\"a\"") [:S [:VAR "a"] [:VAR "a"]]))
      (is (= (tree "\"a\"'a'") [:S [:VAR "a"] [:VAR "a"]]))
      (is (= (tree "/a/'a'") [:S [:UNCLEAR "a"] [:VAR "a"]]))
      (is (= (tree "'a'/a/") [:S [:VAR "a"] [:UNCLEAR "a"]]))
      (is (= (tree "/a/\"a\"") [:S [:UNCLEAR "a"] [:VAR "a"]]))
      (is (= (tree "\"a\"/a/") [:S [:VAR "a"] [:UNCLEAR "a"]]))
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
      (is (= (tree "()") [:S [:FORM]]))
      (is (= (tree "{}") [:S [:SEQRE]]))
      (is (= (tree "[[]:N]") [:S [:FDNA [:VARLIST] ":" [:CONST "N"]]]))

      (is (= (tree "(){}") [:S [:FORM] [:SEQRE]]))
      )

    (testing "valid nested pairs"
      (is (= (tree "(())") [:S [:FORM [:FORM]]]))
      (is (= (tree "(((()))()(()))()(()())")
             [:S [:FORM [:FORM [:FORM [:FORM]]] [:FORM] [:FORM [:FORM]]]
              [:FORM] [:FORM [:FORM] [:FORM]]]))
      (is (= (tree "{{}}") [:S [:SEQRE [:TERM [:SEQRE]]]]))
      (is (= (tree "{{{{}}}{}{{}}}{}{{}{}}")
             [:S [:SEQRE [:TERM [:SEQRE [:TERM [:SEQRE [:TERM [:SEQRE]]]]]
                          [:SEQRE] [:SEQRE [:TERM [:SEQRE]]]]]
              [:SEQRE] [:SEQRE [:TERM [:SEQRE] [:SEQRE]]]]))

      (is (= (tree "({})") [:S [:FORM [:SEQRE]]]))
      (is (= (tree "{()}") [:S [:SEQRE [:TERM [:FORM]]]]))
      (is (= (tree "({({})}(){()})(){(){}}")
             [:S [:FORM [:SEQRE [:TERM [:FORM [:SEQRE]]]] [:FORM]
                  [:SEQRE [:TERM [:FORM]]]]
              [:FORM] [:SEQRE [:TERM [:FORM] [:SEQRE]]]]))
      )

    )

  ; (testing "Allowed characters"
  ;   (testing "in unquoted variables"
  ;     (is (fail? (tree "1a")))
  ;     (is (fail? (tree ":Na")))
  ;     (is (fail? (tree "1:N")))
  ;     (is (fail? (tree ":NUIM")))
  ;     )
  ;   )

  ) 

; (deftest parse-test
;   (testing "Context of the test assertions"
;     (is (= assertion-values)))) 
