(ns formform.expr-notes
  (:require [nextjournal.clerk :as clerk] 
            [formform.calc :as calc]
            [formform.expr :as expr
             :refer [MARK UFORM IFORM fvar unclear n m u i fdna
                     cnt> ctx> => =>*
                     · ··]]))

;; # Expr module

;; ## FORMs

(·)

(· 'a 'b)

(· (· 'a 'b) (· 'c 'd))

;; ### Primitives

[nil MARK UFORM IFORM]

;; ### unclear FORM

(unclear "sharp")


;; ---
;; ## Expressions

(··)

(meta (··))

(·· (· 'a) (· 'b))

;; ### …of simple values

[n m u i]

;; ### …of complex values (formDNA)

(fdna ['a] (calc/make-dna :M :U :I :N))

;; ### …with local env

(·· {'a :M, 'b :N} (· (· 'a) 'b))

;; ### combined

(·· (·· (··)) (··))

(·· (·· 'a (·· 'b 'c)) 'd (·· {} 'e))

(·· (·· {'a :U} 'a) 'b)

;; ### abstraction

(defn ·and [x y] (·· (· (· x) (· y))))
(defn ·nor [x y] (·· (· x y)))
(defn ·equiv [x y] (·· (·and x y) (·nor x y)))

(·equiv (fvar 'a) (fvar 'b))


;; ---
;; ## Reduction

;; ### …of content

(= MARK (cnt> (·)) (cnt> (· (· (·)))))
(= nil (cnt> (· MARK)))
(= IFORM (cnt> (· UFORM)))
(= UFORM (cnt> (· IFORM)))

;; ### …of context

(= m (ctx> (·· UFORM IFORM)))

;; ### irreducable FORMs/expressions

(cnt> (· 'a))

(ctx> (·· (· 'a) 'b))


;; ---
;; ## Evaluation

[(=> m) (=> n) (=> u) (=> i)]

(=> (·· u i))

;; ### uninterpreted expressions (“holes”)

(=> (·· 'x))

;; ### interpretation with envs

(=> (·· 'x) {'x :U})

(=>* (·· 'x))

