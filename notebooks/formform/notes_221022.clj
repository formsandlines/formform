(ns formform.notes-221022
  (:require [nextjournal.clerk :as clerk]
            [formform.calc :as calc]
            [formform.expr :as expr :refer :all]))


;; An expression like this:
(·r 'a 'b 'c 'd 'e)

;; Is currently reduced to this:
'[{:f* [((((((((((:f* a) b) c) d) e) a) b) c) d) e)], :f1 [(((((:f* a) b) c) d) e)]} :f1]

;; But what if we’d rather reduce it to a simpler intermediate expression:
'[{:f* [:< [:f* a] b c d e a b c d e] :f1 [:< [:f* a] b c d e]} :f1]

;; Here the nesting instructions are represented by a special context type,
;; that can be reduced to a simple nesting expression.

;; So this:

'[:< a b c]

;; on its own will reduce to that:

'[(((a) b) c)]

;; Or this:

'[:<_ a b c]

;; to that:

'[((a) b) c]

;; Those are not really new content types but rather types of context.
;; So they have to be defined as expressions.

;; We already have expression constructors for these things:

(·< 'a 'b 'c)

;; But they are immediate transformations, not represented in data.

;; However, what about:

(SEQ-REENTRY :<re 'a 'b 'c)
