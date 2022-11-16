(ns formform.alg
  (:require [formform.expr expr]))






(comment

  #_:clj-kondo/ignore
  (comment

    (defexp ·nor [ !x ... ]
      [ (!x ...) ])

    (defexp ·and [ !x ... ]
      [ ((!x) ...) ])

    (defexp ·equiv [ !x ... ]
      [ (·nor !x ...) (·and !x ...) ])

    (defexp ·xor [ !x ... . !y ... ] 
      [ (·dec !x . !y ...) (·dec !y . !x ...) ])

    (defexp ·mem [ x . [sym exp] ... ] 
      [ (·and (·equiv sym exp) ... x) ])


    `[ ~@(·nor :M :N) ] ;=> [ (:M) (:N) ]
    )

  )

(comment

  ; (defn ·and [a b] `[ ((~a) (~b)) ])
  ; (defn ·or [a b] `[ ~a ~b ])
  ; (defn ·and [& fs] `[ (~@(map (fn [f] `(~f)) fs)) ])

;   (defn ·or [& fs] `[ ~@fs ])
;   (defn ·nor [& fs] `[ (~@(apply ·or fs)) ])

;   (defn ·and [& fs] `[ ~@(apply ·nor (apply concat (map ·nor fs))) ])
;   (defn ·equiv [& fs] `[ ~@(apply ·nor fs) ~@(apply ·and fs) ])


  (defn ·· [& fs] (comp vec (partial apply concat) (apply juxt fs)))
  (defn · [& fs] (comp vec list (partial apply concat) (apply juxt fs)))

  (defn ·or [ctx] ctx)
  (defn ·nor [ctx] `[ (~@ctx) ])
  (defn ·nand [ctx] `[ ~@(map list ctx) ])
  (def ·and (comp ·nor ·nand))
  (defn ·impl [ctx] `[ (~(first ctx)) ~@(rest ctx) ])
  (def ·dec (comp ·nor ·impl))
  (def ·equiv (·· ·nor ·and))
  (def ·xor (·· #(·dec `[ ~(first %) ~@(rest %) ])
                #(·dec `[ (~(rest %)) ~(first %) ]) ))


  ; (def ·and (comp ·nand ·nor))

  (expr/ctx> (·or [ :M '() 'a ]))
  (expr/ctx> (·nor [ :M '() 'a ]))
  (expr/ctx> (·nand [ :M '() 'a ]))
  (expr/ctx> (·and [ :M '() 'a ]))
  (expr/ctx> (·impl [ :M '() 'a ]))
  (expr/ctx> (·dec [ :M '() 'a ]))
  (expr/ctx> (·equiv [ :M '() 'a ]))
  (expr/ctx> (·xor [ :M '() 'a ]))

  ((·· ·nor ·and) [ :M :N ])

  ; (ctx> `[ ~@(·or :M '() 'a) ])
  ; (ctx> `[ ~@(·nor :M '() 'a) ])
  ; (ctx> `[ ~@(·and :M '() 'a) ])
  ; (ctx> `[ ~@(·equiv :M '() 'a) ])

  ; (defmacro ·and [a b]
  ;   `(list (list ~a) (list ~b)))

  ; (defn ·and [a b] `((~a) (~b)))

  ; (=> [ (·and :M '(())) ])
  ; (=> [ (·and :M '()) ])
  ; (=> [ `(:U ~(·and :M '()) ~@(repeat 2 '())) ])

  )
