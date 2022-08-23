(ns formform.expr
  (:require [clojure.math.combinatorics :as combo]
            [formform.calc :as calc]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive expressions

(def n '[ nil ])
(def m '[ () ])
(def u '[ :mn ])
(def i '[ (:mn) ])

(def const->expr {:N n :U u :I i :M m})
(def expr->const {n :N u :U i :I m :M})

(def expr? vector?)
(def form? seq?)
(def mem-form? map?)
(defn variable? [x] (or (string? x) (symbol? x)))


(defn dna-expr?
  [x]
  (if-let [[vars dna] x]
    (and 
      (vector? vars)
      (calc/dna? dna)
      (== (count vars) (calc/dna-dim dna)))
    false))

(defn filter-dna-seq-by-env
  [dna-seq env]
  (let [vpoint (mapv second (sort-by first (seq env)))]
    (calc/filter-dna-seq dna-seq vpoint)))


(defn vars
  [expr]
  (let [children (fn [x] (filter #(or (form? %) (variable? %)) x))]
    (->> (seq expr)
         (tree-seq sequential? children)
         rest
         (filter (complement sequential?))
         distinct)))

; (defn all-envs
;   [vars]
;   (let [vspc (calc/vspace (count vars))]
;     (map (comp (partial apply hash-map) (partial interleave vars))
;       vspc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduction

(declare reduce-context)

; (defn- reduce-dna
;   [dna env]
;   (let [vars (keys env)]
;     nil))

(defn- reduce-matching-content
  ([x] (reduce-matching-content x x))
  ([x default]
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
     default)))

(defn- reduce-content
  [x env]
  (let [r (reduce-matching-content x :failed)]
    (if (not= :failed r)
      r
      (cond
       ; (dna-expr? x) (let [[vars dna] x]
       ;                 )
       ; (calc/dna? x)
        (form? x)     (let [r (reduce-context x env)]
                        (reduce-matching-content r))
        (mem-form? x) nil  ;; TODO
        :else (if-let [interpr (env x)]
                (cond
                  (keyword? interpr) interpr
                  :else (let [r (reduce-context interpr env)]
                          (if (<= (count r) 1)
                            (first r)
                            (list r))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation

(defn =>
  "Evaluates a FORM expression with an optional `env` and returns a Constant expression with attached metadata including the maximally reduced expression in `:expr` and the environment in `:env`.
  - `env` must be a map with a content/variable in `expr` as a key"
  ([expr] (=> expr {}))
  ([expr env]
   (let [[r & more :as ctx] (ctx> expr env)
         v (when (empty? more)
             (case r
               (( )   :M) :M
               (nil   :N) :N
               (:mn   :U) :U
               ((:mn) :I) :I
               nil))
         m {:expr ctx :env env}]
     (if (some? v)
       (with-meta [ v ] m)
       (with-meta [ :_ ] m)))))

(defn =>*
  "Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.
  - if `as-dna-expr?` is false, returns a seq of results as returned by `=>` in the order of the corresponding `vspace` ordering"
  ([expr] (=>* expr true))
  ([expr as-dna-expr?]
   (let [vars (vars expr)
         vspc (calc/vspace (count vars))
         all-envs (fn [vars]
                    (map (comp
                           (partial apply hash-map)
                           (partial interleave vars)) vspc))
         envs (all-envs vars)]
     (if as-dna-expr?
       (let [consts (mapv (comp first (partial => expr)) envs)]
         [ [vars (rseq consts)] ])
       (let [results (map (partial => expr) envs)]
         (with-meta results {:vars vars :vspc vspc}))))))


(defn eval-expr
  "Calls `=>` but instead of an expression returns the `const` value or nil."
  ([expr]     (eval-expr expr {}))
  ([expr env] (let [[x] (=> expr env)]
                (if (calc/const? x)
                  x
                  nil))))

(defn eval-all
  "Calls `=>*` but instead of an expression returns a `vdict`"
  [expr {:keys [sorted?] :or {sorted? true}}]
  (let [rs (=>* expr false)
        {:keys [vars vspc]} (meta rs)]
    (with-meta
      (->> rs
           (map first)
           (map vector vspc)
           (into (if sorted? (sorted-map-by calc/compare-dna) (hash-map))))
      {:vars vars})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nested sequences

(defmacro <·
  "nest-to-left macro: (((…)a)b)
  use brackets [x y …] to group expressions on the same level"
  [& exprs]
  (if (empty? exprs)
    '()
    (let [[x & r] exprs]
      (loop [x (if (vector? x) `(list ~@x) `(list ~x))
             r r]
        (if (empty? r)
          x
          (let [y (first r)]
            (recur (if (vector? y) `(list ~x ~@y) `(list ~x ~y))
              (rest r))))))))

(defmacro ·>
  "nest-to-right macro: (a(b(…)))
  use brackets [x y …] to group expressions on the same level"
  [& exprs]
  (if (empty? exprs)
    '()
    (let [[x & r] exprs]
      (if (empty? r)
        (if (vector? x) `(list ~@x)          `(list ~x))
        (if (vector? x) `(list ~@x (·> ~@r)) `(list ~x (·> ~@r)))))))



(comment

  [[ '... {:f '((:f a) b)} '... ] {}]
  [[ '... :f0 '... ] {:f '((:f0 a) b)}]

  [[ '... {:f '((:f a) {:f '((:f :N) :U)} )} '... ] {}]
  [[ '... :f0 '... ] {:f0 '((:f0 a) {:f '((:f :N) :U)} )}]
  [[ '... :f0 '... ] {:f0 '((:f0 a) :f1 ), :f1 '((:f1 :N) :U)}]


  (cnt> '( (( ((f a) b) ) f )  (( f ) ((f a) b) ) ))

  (defrecord SeqRE [parity unmarked? interpr fseq])

  [ `(->SeqRE :any true :rec-ident (<· ~'a ~'b)) ]

  (->SeqRE :any true :rec-ident (<· 'a 'b))

  (seq ['a 'b])

  (·> 'a ['b :M] 'c ['d :mn])
  (<· 'a ['b :M] 'c ['d :mn])

  [ {:par :even :f (<· 'a 'b)} ]

  (=> [ '((a) b) ] {'a :N, 'b :U})
  (eval-expr [ '((a) b) ])
  (=> [ '((a) b) ] {'a [:N], 'b [:U]})
  (meta (=> [ '((a) b) ] ))
  (=>* [ '((a) b) ])
  (eval-all [ '((a) b) ] {})

  (let [expr `[ (~@m ~@n) ~@m ]]
    expr) ;=> [(() nil) ()]
  
  )


(comment
  ;; experimental self-equivalent re-entry macros

  (defmacro <re
    [& exprs]
    (if (or (empty? exprs) (odd? (count exprs)))
      `(..<re. ~@exprs)
      (let [[x & r] exprs]
        `{:f* (<· [:f* ~x] ~@r) })))

  (defmacro ..<re
    [& exprs]
    (let [n (count exprs)]
      (if (and (> n 0) (even? n))
        `(<re ~@exprs)
        (if (empty? exprs)
          '{:f* (list (list :f*)) }
          (let [[x & r] exprs]
            `{:f* (<· [(<· [:f* ~x] ~@r) ~x] ~@r) })))))

  (defmacro ..<re.
    [& exprs]
    (let [n (count exprs)]
      (if (and (> n 0) (even? n))
        `(<re ~@exprs)
        (if (empty? exprs)
          '(into {:f' (list :f*) } (..<re))
          (let [[x & r] exprs]
            `(into {:f' (<· [:f* ~x] ~@r) } (..<re ~@exprs))
          ; `[ (<· [(..<re ~@exprs) ~x] ~@r) ]
            )))))

  (defmacro <re_
    [& exprs]
    (let [n (count exprs)]
      (if (and (> n 0) (even? n))
        (let [[x & r] exprs]
          `(into {:f_ (cons (<· [:f* ~x] ~@r) '())} (<re ~@exprs)))
        `(..<re._ ~@exprs))))

  (defmacro ..<re_
    [& exprs]
    (let [n (count exprs)]
      (if (and (> n 0) (even? n))
        `(<re_ ~@exprs)
        (if (empty? exprs)
          `(into {:f_ '((:f*))} (..<re ~@exprs))
          (let [[x & r] exprs]
            `(into {:f_ (cons (<· [:f* ~x] ~@r) '())} (..<re ~@exprs)))))))

  (defmacro ..<re._
    [& exprs]
    (let [n (count exprs)]
      (if (and (> n 0) (even? n))
        `(<re_ ~@exprs)
        (if (empty? exprs)
          `(into {:f_ '((:f'))} (..<re. ~@exprs))
          (let [[x & r] exprs]
            `(into {:f_ (cons (<· [:f' ~x] ~@r) '())} (..<re. ~@exprs)))))))

  (<re 'a 'b 'c) ;== (..<re. a b c)
  (<re 'a 'b) ;=> {:f* ((:f* a) b)}
  (<re 'a) ;== (..<re. a)
  (<re) ;== (..<re.)

  (..<re 'a 'b 'c) ;=> {:f* ((((((:f* a) b) c) a) b) c)}
  (..<re 'a 'b) ;== (<re a b)
  (..<re 'a) ;=> {:f* ((:f* a) a)}
  (..<re) ;=> {:f* ((:f*))}

  (..<re. 'a 'b 'c) ;=> {:f' (((:f* a) b) c), :f* ((((((:f* a) b) c) a) b) c)}
  (..<re. 'a 'b) ;== (<re a b)
  (..<re. 'a) ;=> {:f' (:f* a), :f* ((:f* a) a)}
  (..<re.) ;=> {:f' (:f*), :f* ((:f*))}

  (<re_ 'a 'b 'c) ;== (..<re._ a b c)
  (<re_ 'a 'b) ;=> {:f_ (((:f* a) b)), :f* ((:f* a) b)}
  (<re_ 'a) ;== (..<re._ a)
  (<re_) ;== (..<re._)

  (..<re_ 'a 'b 'c) ;=> {:f_ ((((:f* a) b) c)), :f* ((((((:f* a) b) c) a) b) c)}
  (..<re_ 'a 'b) ;== (<re_ a b)
  (..<re_ 'a) ;=> {:f_ ((:f* a)), :f* ((:f* a) a)}
  (..<re_) ;=> {:f_ ((:f*)), :f* ((:f*))}

  (..<re._ 'a 'b 'c) ;=> {:f_ ((((:f' a) b) c)), 
                     ;    :f' (((:f* a) b) c), :f* ((((((:f* a) b) c) a) b) c)}
  (..<re._ 'a 'b) ;== (<re_ a b)
  (..<re._ 'a) ;=> {:f_ ((:f' a)), :f' (:f* a), :f* ((:f* a) a)}
  (..<re._) ;=> {:f_ ((:f')), :f' (:f*), :f* ((:f*))}

  )

