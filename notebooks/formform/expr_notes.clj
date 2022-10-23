(ns formform.expr-notes
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]))

;; # Expr module

;; ## Content

[NONE MARK UFORM IFORM]

(FORM MARK)

(FORM (FORM MARK MARK) (FORM MARK MARK))

(FORM UFORM IFORM)

(UNCLEAR "sound of silence")

(SEQ-REENTRY :<re 'a 'b)

(FDNA)

(FDNA :M)

(FDNA (calc/make-dna :N :U :I :M))


;; ---
;; ## Context

(··)

(meta (··))

(·· (·· (··)) (··))

(·· 'a 'b)

(·· (·· 'a (·· 'b 'c)) 'd (·· 'e))


;; ### Expression wrapper…

(·· ·none ·mark ·uform ·iform)

;; #### …of FORMs

(·)

(meta (·))

(· (· ·mark) ·mark)

(· ·uform ·iform)

(· (· 'a (· 'b 'c)) 'd (· 'e))

;; #### …of unclear FORMs

;; The constructor takes any type of argument and just returns a single
;; string:

(def ux (·uncl ")" '(unfug ")")))

(UNCLEAR->label (first ux))


;; ### Self-equivalent re-entry (“seq-re”) expressions

;; A seq-re expression can be built using the shorthand constructors:

;; - `·` for marked, `··` for unmarked re-entry FORMs
;; - `2r` for even, `2r+1` for odd re-entry numbers
;; - `r` alone for any re-entry number (will be determined logically)
;; - `'` for the alternative interpretation (“recursive identity”)

;; Or à la carte with the general constructor and an option map:

;; - `:parity` (of re-entry number) → `:even`, `:odd` or `:any` (default)
;; - `:open?` (unmarked FORM) → `true` or `false` (default)
;; - `interpr` (interpretation of `mn`) → `:rec-instr` (default) or `:rec-ident`

(defn =return [a b] (if (= a b) b false))

(=return  (·r 'a 'b)         (·seq-re {} 'a 'b))
(=return  (·r 'a 'b 'c)      (·seq-re {} 'a 'b 'c))
(=return  (·2r 'a 'b)        (·seq-re {:parity :even} 'a 'b))
(=return  (·2r+1 'a 'b 'c)   (·seq-re {:parity :odd} 'a 'b 'c))

(=return  (··r 'a 'b)        (·seq-re {:open? true} 'a 'b))
(=return  (··r 'a 'b 'c)     (·seq-re {:open? true} 'a 'b 'c))
(=return  (··2r 'a 'b)       (·seq-re {:open? true :parity :even} 'a 'b))
(=return  (··2r+1 'a 'b 'c)  (·seq-re {:open? true :parity :odd} 'a 'b 'c))


;; ### Value expressions

;; #### …of constants

(·· ·N ·M ·U ·I)


;; #### …of formDNA

(·dna)

(·dna :U)

(·dna (calc/rand-dna 2))

(def dx (·dna ['a] :M :U :I :N))

(FDNA->dna (first dx))

(FDNA->varlist (first dx))

(= ·N (expand-fdna (FDNA)))
(= ·U (expand-fdna (FDNA :U)))


;; ### Combination

(·· (· 'a) (· 'b))

(·r ['a (·2r 'a ['b 'c])] ['b (·2r+1 ['a 'b] 'c)])


;; ### Abstraction

(defn ·and [x y] (·· (· (· x) (· y))))
(defn ·nor [x y] (·· (· x y)))
(defn ·equiv [x y] (·· (·and x y) (·nor x y)))

(·equiv (·? 'a) (·? 'b))

;; Quoting lets us keep an abstraction for clarity or deferred application:

(·equiv `(·and ~@(·? 'a) ~@(·? 'b)) (·? 'c))

;; Quoting the whole form requires an outer expression for further combination:

(·· `(·equiv (·? a) (·? b)))

;; Always use syntax quotes (backticks) to include the correct namespace.

;; Keep in mind that these expressions are only complete if all symbols
;; have been properly defined, otherwise reduction will throw an error
;; because symbols prefixed by `·` are always interpreted as expressions,
;; not as unknowns (logic variables).

;; This approach is not recommended for use outside of your programming
;; environment, since it mixes program specificities with portable
;; program-independent data that should be translateable 1:1 into
;; FORM logic notation.


;; ### Expressions with local environments

(·· {'a :M, 'b :N} (· (· 'a) 'b))

(·· (·· {'a :U} 'a) 'b)


;; ---
;; ## Reduction

;; ### …of content

(= MARK (cnt> (FORM)) (cnt> (FORM (FORM MARK))))
(= NONE (cnt> (FORM MARK)))
(= IFORM (cnt> (FORM UFORM)))
(= UFORM (cnt> (FORM IFORM)))

;; ### …of context

(= ·mark (ctx> (·· UFORM IFORM)))


;; ### irreducable FORMs/expressions

(cnt> (· 'a))

(ctx> (·· (· 'a) 'b))


;; ---
;; ## Evaluation

[(=> ·M) (=> ·N) (=> ·U) (=> ·I)]

(=> (·· ·U ·I))

;; ### uninterpreted expressions (“holes”)

(=> (·· 'x))

;; ### interpretation with envs

(=> (·· 'x) {'x :U})

(=>* (·· 'x))

