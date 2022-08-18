(ns formform.expr
  (:require [formform.calc :as calc]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive expressions

(def n '[ nil ])
(def m '[ () ])
(def u '[ :mn ])
(def i '[ (:mn) ])

(def const->expr {:N n :U u :I i :M m})
(def expr->const {n :N u :U i :I m :M})

(defn dna-expr?
  [x]
  (if-let [[vars dna] x]
    (and 
      (vector? vars)
      (calc/dna? dna)
      (== (count vars) (calc/dna-dim dna)))
    false))

(declare reduce-context)

(defn- reduce-content
  ([x] (reduce-content x {}))
  ([x env]
   (case x
     (() :M (nil) (:N) ((())) ((:M)) (((nil))) (((:N)))
      ((:U :I)) ((:I :U)))
     '()
     (nil :N (()) (:M) ((nil)) ((:N))
      (:U :I) (:I :U))
     nil
     (:mn :U (:I) ((:mn)) ((:U)) (((:I))))
     :mn
     ((:mn) :I (:U) (((:mn))) ((:I)) (((:U))))
     '(:mn)
     (cond
       ; (dna-expr? x) (let [[vars dna] x]
       ;                 )
       ; (calc/dna? x)
       (coll? x) (reduce-context x env)
       :else (if-let [interpr (env x)]
               (let [r (reduce-context interpr env)]
                 (if (<= (count r) 1)
                   (first r)
                   (list r)))
               x)))))

(defn- reduce-context
  ([ctx] (reduce-context ctx {}))
  ([ctx env]
   (loop [ctx ctx]
     (cond
       (some #{'() :M '(nil) '(:N) '((())) '((:M)) '(((nil))) '(((:N)))} ctx)
       '( () )
       (and
         (some #{:mn :U '(:I) '((:mn)) '((:U)) '(((:I)))} ctx)
         (some #{'(:mn) :I '(:U) '(((:mn))) '((:I)) '(((:U)))} ctx))
       '( () )
       :else (let [ctx' (->> ctx
                             distinct
                             (map #(reduce-content % env))
                             (remove nil?))]
               (if (= ctx' ctx)
                 ctx'
                 (recur ctx')))))))

(defn- eval-expr
  ([expr] (eval-expr expr {}))
  ([expr env]
   (let [[r & ctx] (reduce-context expr env)
         v (when (empty? ctx)
             (case r
               (( )   :M) :M
               (nil   :N) :N
               (:mn   :U) :U
               ((:mn) :I) :I
               nil))]
     (if (some? v)
       v
       {:expr (into [r] ctx) :env env}))))

(defn cnt>
  "Reduces a FORM content recursively until it cannot be further reduced.
  All reductions are justified by the axioms of FORM logic.
  - if `x` is a complex FORM, calls `reduce-context` on `x`
  - if no reduction applies, tries to retrieve the value from given `env`
  - if retrieval was unsuccessful, returns `x` as is"
  ([x]     (cnt> x {}))
  ([x env] (reduce-content x env)))

(defn ctx>
  "Reduces a FORM context recursively until it cannot be further reduced.
  All reductions are justified by the axioms of FORM logic.
  - for complex expressions, calls `reduce-content` on every unique element"
  ([ctx]     (ctx> ctx {}))
  ([ctx env] (vec (reduce-context ctx env))))

(defn =>
  "Evaluates a FORM expression and either returns a formDNA or a map with the maximally reduced expression and its environment `env`.
  - `env` must be a map with a content/variable in `expr` as a key"
  ([expr]     (=> expr {}))
  ([expr env] (eval-expr expr env)))


(comment
  (=> [ '() ])
  (=> [ :M ])
  (=> [ '(:M) ])
  (=> [ '(:M) :M ])
  (=> [ '() '() ])
  (=> [ '(()) ])
  (=> [ '(() ()) ])
  (=> [ '(() ()) '() ])
  (=> [ '(((() nil) nil) nil) ])
  (=> [ '((((() nil) nil) nil) nil) ])

  (=> [ 'a ]) ;=> {:expr [a], :env {}}
  (=> [ 'a ] {'a [ :N ]}) ;=> :N
  (=> [ 'a 'b ]) ;=> {:expr [a b], :env {}}
  (=> [ 'a 'b ] {'a [ :M ]}) ;=> :M
  (=> [ 'a 'b ] {'a [ :N ]}) ;=> {:expr [b], :env {a [:N]}}

  (=> [ '(a) ]) ;=> {:expr [(a)], :env {}}
  (=> [ '(a) ] {'a [ :M ]}) ;=> :N
  (=> [ '(a b) ]) ;=> {:expr [(a b)], :env {}}
  (=> [ '(a b) ] {'a [ :N ]}) ;=> {:expr [(b)], :env {a [:N]}}
  (=> [ '(a b) ] {'a [ :N ] 'b [ :M ]}) ;=> :N
  (=> [ '((a) b) ]) ;=> {:expr [((a) b)], :env {}}
  (=> [ '((a) b) ] {'a [ :M ]}) ;=> {:expr [(b)], :env {a [:M]}}
  (=> [ '((a) b) 'c ]) ;=> {:expr [((a) b) c], :env {}}
  (=> [ '((a) b :M) 'c ]) ;=> {:expr [c], :env {}}

  (=> [ :U ]) ;=> :U
  (=> [ :I ]) ;=> :I
  (=> [ :U :I ]) ;=> :M
  (=> [ :I nil :U '(()) :N :I ]) ;=> :M
  (=> [ '(:U) ]) ;=> :I
  (=> [ '(:I) ]) ;=> :U
  (=> [ '((:I)) ]) ;=> :I
  (=> [ '((:U)) ]) ;=> :U
  (=> [ '(((:U))) ]) ;=> :I
  (=> [ '(((:U)) (:I)) ]) ;=> :I
  (=> [ '(:U :M) ]) ;=> :N
  (=> [ '(:U) :M ]) ;=> :M
  (=> [ '(:U) :I ]) ;=> :I
  (=> [ :U '(:I) ]) ;=> :U
  (=> [ :U '(:U) ]) ;=> :M


  (ctx> [ '((a) b :M) ]) ;=> [] 
  (ctx> [ '((a) b) :M ]) ;=> [()] 
  (ctx> [ '((a) b) ]) ;=> [((a) b)] 
  (ctx> [ '((a) b) ] {'a [ :M ]}) ;=> [(b)] 
  (ctx> [ :U ]) ;=> [:mn]
  (ctx> [ '(:U) ]) ;=> [(:mn)]
  (ctx> [ '(:U) :I ]) ;=> [(:mn)]
  (ctx> [ '(:U) :U ]) ;=> [()]
  (ctx> [ :I ]) ;=> [(:mn)]
  (ctx> [ '(:I) ]) ;=> [:mn]
  (ctx> [ '(:I) :U ]) ;=> [:mn]
  (ctx> [ '(:I) :I ]) ;=> [()]
  (ctx> [ :mn ]) ;=> [:mn]
  (ctx> [ '(:mn) ]) ;=> [(:mn)]
  (ctx> [ '(:mn) :mn ]) ;=> [()]

  (cnt> '((a) b)) ;=> ((a) b)
  (cnt> '((a) b) {'a [ :M ]}) ;=> (b)
  (cnt> '((a) b :M) ) ;=> (()) 
  (cnt> '((a) b) ) ;=> ((a) b) 
  (cnt> :U ) ;=> :mn
  (cnt> '(:U) ) ;=> (:mn)
  (cnt> :I ) ;=> (:mn)
  (cnt> '(:I) ) ;=> :mn
  (cnt> :mn ) ;=> :mn
  (cnt> '(:mn) ) ;=> (:mn)


  (let [expr `[ (~@m ~@n) ~@m ]]
    expr) ;=> [(() nil) ()]

  (dna-expr? [['a 'b] :IUMNIUMNIUMNIUMN])

  )
