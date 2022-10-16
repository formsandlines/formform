(ns formform.expr-notes
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr
             :refer [MARK UFORM IFORM FORM UNCLEAR
                     cnt> ctx> => =>*
                     ·n ·m ·u ·i ·var ·uncl ·dna · ··]]))

;; # Expr module

;; ### Primitives

[nil MARK UFORM IFORM]

(FORM MARK)

(FORM (FORM MARK MARK) (FORM MARK MARK))

(FORM UFORM IFORM)

(UNCLEAR "sound of silence")


;; ---
;; ## Expressions

(··)

(meta (··))

(·· (·· (··)) (··))

(·· 'a 'b)

(·· (·· 'a (·· 'b 'c)) 'd (·· 'e))


;; ### FORM expressions

(·)

(meta (·))

(· (· (·)) (·))

(· 'a 'b)

(· (· 'a (· 'b 'c)) 'd (· 'e))


;; ### unclear FORM expressions

(def ux (·uncl ")" '(unfug ")")))

(expr/unclear-expr->label ux)


;; ### express simple values

(·· ·n ·m ·u ·i)


;; ### express complex values (formDNA)

(·dna)

(·dna (calc/rand-dna 2))

(def dx (·dna ['a] :M :U :I :N))

(expr/dna-expr->dna-seq dx)

(expr/dna-expr->varlist dx)


;; ### combine

(·· (· 'a) (· 'b))


;; ### abstract

(defn ·and [x y] (·· (· (· x) (· y))))
(defn ·nor [x y] (·· (· x y)))
(defn ·equiv [x y] (·· (·and x y) (·nor x y)))

(·equiv (·var 'a) (·var 'b))


;; ### expressions can have local environments

(·· {'a :M, 'b :N} (· (· 'a) 'b))

(·· (·· {'a :U} 'a) 'b)


;; ---
;; ## Reduction

;; ### …of content

(= MARK (cnt> (·)) (cnt> (· (· (·)))))
(= nil (cnt> (· MARK)))
(= IFORM (cnt> (· UFORM)))
(= UFORM (cnt> (· IFORM)))

;; ### …of context

(= ·m (ctx> (·· UFORM IFORM)))

;; ### irreducable FORMs/expressions

(cnt> (· 'a))

(ctx> (·· (· 'a) 'b))


;; ---
;; ## Evaluation

[(=> ·m) (=> ·n) (=> ·u) (=> ·i)]

(=> (·· ·u ·i))

;; ### uninterpreted expressions (“holes”)

(=> (·· 'x))

;; ### interpretation with envs

(=> (·· 'x) {'x :U})

(=>* (·· 'x))

