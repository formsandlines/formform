^{::clerk/visibility {:code :hide :result :hide}}
(ns formform.expr-types
  (:require [nextjournal.clerk :as clerk]
            [formform.expr :as expr :refer :all]))

^{::clerk/visibility {:code :hide :result :hide}}
(defn table-hz [header row-fn col-fn xs]
  (clerk/table
   (clerk/use-headers
    (cons header
          (map (comp row-fn col-fn) xs)))))

^{::clerk/visibility {:code :hide :result :hide}}
(defn table-vt [header col-fn row-fn xs]
  (clerk/table #_{:nextjournal.clerk/width :wide}
   (clerk/use-headers
    (cons (cons "" (map #(str "Example " (inc %)) (range (count xs))))
      (apply map vector
           (cons (map #(clerk/html [:span.font-medium %]) header)
                 (map (comp col-fn row-fn) xs)))))))

;; # Expression types

;; ---
;; ## Symbols

^{::clerk/visibility {:code :hide :result :hide}}
(def sym-table (partial table-hz
                        ["Symbol" "Interpretation" "Simplification"]
                        (juxt make interpret simplify)))

;; ### Value symbols

^{::clerk/visibility {:code :fold :result :show}}
(sym-table identity [:N :M :U :I])

;; ---
;; ## Syntactic operators

^{::clerk/visibility {:code :hide :result :hide}}
(def op-table-hz (partial table-hz
                          ["Operator" "Interpretation" "Simplification"]
                          (juxt (partial apply make) interpret simplify)))

;; ### Spaces

^{::clerk/visibility {:code :fold :result :show}}
(op-table-hz (fn [op-sym] [op-sym 'a 'b 'c])
             [:- :* :|])

;; ### Sequences

^{::clerk/visibility {:code :fold :result :show}}
(op-table-hz (fn [op-sym] [op-sym 'a 'b 'c])
             [:<- :-> :< :>])

;; ### Combination

^{::clerk/visibility {:code :hide :result :show}}
(clerk/html [:em.text-sm.text-orange-600 "Redundant!"])

^{::clerk/visibility {:code :fold :result :show}}
(op-table-hz (fn [op-sym] [op-sym 'a 'b 'c])
             [:<-> :<>])

;; ---
;; ## Semantic operators

^{::clerk/visibility {:code :hide :result :hide}}
(def op-table-vt (partial table-vt
                          ["Operator" "Interpretation" "Simplification"]
                          (juxt (partial apply make) interpret simplify)))

;; ### Unclear FORMs

^{::clerk/visibility {:code :fold :result :show}}
(op-table-vt identity
             [[:uncl "foobar"]])

;; Unclear FORMs are variable values:

(first (=> (make :uncl "foobar")))

;; If we believe the value to be marked, the unclear FORM is determined:

(simplify (make :uncl "foobar") {"foobar" :M})

;; Otherwise, it remains undetermined:

(let [expr (make :uncl "foobar")]
  (= :U
     (simplify expr {"foobar" :N})
     (simplify expr {"foobar" :U})
     (simplify expr {"foobar" :I})))


;; ### Memory FORMs

^{::clerk/visibility {:code :fold :result :show}}
(op-table-vt identity
             [[:mem [['a :N] ['b :U]] (form 'a 'b)]])


;; While the original expression is uninterpreted:

(simplify (form 'a 'b) {})

(first (=> (form 'a 'b)))

;; The memory FORM interprets its contents by association:

(simplify (make :mem [['a :N] ['b :U]]
                (form 'a 'b)) {})

(first (=> (make :mem [['a :N] ['b :U]]
                 (form 'a 'b))))

;; Which yields the same result as interpretation through an environment:

(first (=> (form 'a 'b) {'a :N, 'b :U}))


;; ### Self-equivalent re-entry FORMs

^{::clerk/visibility {:code :fold :result :show}}
(op-table-vt identity
             [[:seq-re :<r nil nil] [:seq-re :<r nil]])

;; Re-entry can be cut off in some cases:

(=>* (seq-re :<r 'a 'b []))

;; Some configurations are equivalent:

(= '[:fdna [a] [:N :I :I :I]]
   (=>* (seq-re :<..r. 'a))
   (=>* (seq-re :<..r'. 'a))
   (=>* (seq-re :<r 'a))
   (=>* (seq-re :<r' 'a)))

(= '[:fdna [a] [:N :U :U :U]]
   (=>* (seq-re :<..r 'a))
   (=>* (seq-re :<..r' 'a))
   (=>* (seq-re :<r 'a 'a))
   (=>* (seq-re :<r' 'a 'a)))

;; With unmarked seq re-entry FORMs, evaluation is different between interpretations:

(=>* (seq-re :<..r_ 'a))
(=>* (seq-re :<..r'_ 'a))

(= '[:fdna [a] [:M :U :U :I]]
   (=>* (seq-re :<..r._ 'a))
   (=>* (seq-re :<r_ 'a 'a)))

(= '[:fdna [a] [:M :I :M :I]]
   (=>* (seq-re :<..r'._ 'a))
   (=>* (seq-re :<r'_ 'a 'a)))


;; ### formDNA FORMs

^{::clerk/visibility {:code :fold :result :show}}
(op-table-vt identity
             [[:fdna [] [:N]] [:fdna ['a] [:N :U :I :M]]])

;; Interpretation to isolator terms:

(take 4 (iterate interpret (make :fdna ['a] [:N :U :I :M])))

;; Evaluates back to itself:

(let [fdna (make :fdna ['a] [:N :U :I :M])]
  (= fdna
     (=>* fdna)
     (=>* (interpret fdna))))

;; ### FORM classes

;; #### Isolator class

^{::clerk/visibility {:code :fold :result :show}}
(table-hz ["Operator" "Interpretation"]
          (juxt make interpret)
          (fn [op-sym] [op-sym 'a])
          [:N->M :U->M :I->M :M->M])

;; In evaluation, isolator FORMs “isolate” a specific interpretation by leaving all other interpretations unmarked:

^{::clerk/visibility {:code :fold :result :show}}
(table-hz ["Operator" "Evaluation" "Simplification"]
          (juxt make =>* simplify)
          (fn [op-sym] [op-sym 'a])
          [:N->M :U->M :I->M :M->M])

