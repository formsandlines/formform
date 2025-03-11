;; ========================================================================
;;     formform emulation module
;;     -- created 02/2025, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.emul.core
  (:require [formform.calc :as calc]
            ;; [formform.utils :as utils]
            ))

;; uncomment to redefine multimethods
#_
(do (def sys-ini nil)
    (def get-umwelt nil)
    (def apply-rule nil))

(defn- val-or-rand [v]
  (case v
    :rand (calc/rand-const)
    v))

(defn get-resolution
  [gen]
  (loop [xs  gen
         res [(count gen)]]
    (let [x (first xs)]
      (if (vector? x)
        (recur x (conj res (count x)))
        res))))


(def dimensions
  (-> (make-hierarchy)
      (derive :1d :dim)
      (derive :2d :dim)))

(defmulti sys-ini
  (fn [res ini-spec]
    (case (count res)
      1 [:1d (first ini-spec)]
      2 [:2d (first ini-spec)]
      (throw (ex-info "Unsupported CA dimension" {:dim (count res)}))))
  :hierarchy #'dimensions)

(defmethod sys-ini [:1d :fill-all]
  [[w] [_ bg]]
  (vec (repeatedly w (partial val-or-rand bg))))

(defmethod sys-ini [:2d :fill-all]
  [[w h] [_ bg]]
  (vec (repeatedly h #(vec (repeatedly w (partial val-or-rand bg))))))

;; ? if `fg` is vector, `cw` shouldn’t have to be specified
(defmethod sys-ini [:1d :fill-center]
  [[w] [_ area bg]]
  (let [explicit? (vector? area)
        cw (if explicit? (count area) (:res area))
        cx (quot w 2)
        cpoints (into {} (for [x (range cw)
                               :let [v (if explicit?
                                         (get area x)
                                         (val-or-rand (:val area)))
                                     coords (- (+ x cx) (quot cw 2))]]
                           [coords v]))]
    (mapv (fn [x]
            (if-let [v (cpoints x)]
              v
              (val-or-rand bg)))
          (range w))))

(defmethod sys-ini [:2d :fill-center]
  [[w h] [_ area bg]]
  (let [explicit? (vector? area)
        [cw ch] (if explicit?
                  [(count (first area)) (count area)]
                  (:res area))
        cx (quot w 2)
        cy (quot h 2)
        cpoints (into {} (for [y (range ch)
                               x (range cw)
                               :let [v (if explicit?
                                         (get-in area [y x])
                                         (val-or-rand (:val area)))
                                     coords [(- (+ x cx) (quot cw 2))
                                             (- (+ y cy) (quot ch 2))]]]
                           [coords v]))]
    (mapv (fn [y]
            (mapv (fn [x]
                    (if-let [v (cpoints [x y])]
                      v
                      (val-or-rand bg)))
                  (range w)))
          (range h))))

(defmethod sys-ini [:dim :random]
  [res _]
  (sys-ini res [:fill-all :rand]))

(defmethod sys-ini [:1d :rand-center]
  [res [_ size]]
  (sys-ini res [:fill-center {:res size :val :rand} :N]))

(defmethod sys-ini [:1d :ball]
  [res [_]]
  (sys-ini res [:fill-center [:I :U :M :U :I] :N]))

(defmethod sys-ini [:2d :ball]
  [res [_]]
  (sys-ini res [:fill-center [[:N :N :I :N :N]
                              [:N :I :U :I :N]
                              [:I :U :M :U :I]
                              [:N :I :U :I :N]
                              [:N :N :I :N :N]] :N]))

(defmethod sys-ini [:2d :rand-center]
  [res [_ size]]
  (sys-ini res [:fill-center {:res [size size] :val :rand} :N]))

(defmethod sys-ini :default
  [res _]
  (sys-ini res [:random]))


(defn- wrap-bounds
  [n lower-bound upper-bound]
  (cond
    (> n upper-bound) lower-bound
    (< n lower-bound) upper-bound
    :else n))

(defmulti get-umwelt
  (fn
    ([_ _ _] :default)
    ([_ _ _ umwelt-spec] (first umwelt-spec))))

(defmethod get-umwelt :default
  [gen cell umwelt-spec]
  (let [res (get-resolution gen)]
    (get-umwelt res gen cell umwelt-spec)))

(defmethod get-umwelt :select-ltr
  [[w] gen [[x] _] [_ size]]
  (let [g (fn [dx]
            (get gen (wrap-bounds (+ x dx) 0 (dec w))))]
    (case size
      0 []
      1 [(g  0)]
      2 [(g -1) (g  1)]
      3 [(g -1) (g  0) (g  1)]
      4 [(g -2) (g -1) (g  1) (g  2)]
      5 [(g -2) (g -1) (g  0) (g  1) (g  2)]
      (throw (ex-info "Unsupported neighbourhood size" {:size size})))))

(defmethod get-umwelt :self-select-ltr
  [[w h] gen [[x y] self-v] [_ size]]
  (let [g (fn [dx dy]
            (get-in gen
                    [(wrap-bounds (+ y dy) 0 (dec h))
                     (wrap-bounds (+ x dx) 0 (dec w))]))]
    (case size
      0 []
      1 (case self-v
          :N [(g -1  0)] ;; ←
          :U [(g  0  1)] ;; ↓
          :I [(g  0 -1)] ;; ↑
          :M [(g  1  0)] ;; →
          )
      2 (case self-v
          :N [(g -1  1) (g -1 -1)] ;; ←
          :U [(g  1  1) (g -1  1)] ;; ↓
          :I [(g -1 -1) (g  1 -1)] ;; ↑
          :M [(g  1 -1) (g  1  1)] ;; →
          )
      3 (case self-v
          :N [(g -1  1) (g -1  0) (g -1 -1)] ;; ← (bottom -> top)
          :U [(g  1  1) (g  0  1) (g -1  1)] ;; ↓ (right -> left)
          :I [(g -1 -1) (g  0 -1) (g  1 -1)] ;; ↑ (left -> right)
          :M [(g  1 -1) (g  1  0) (g  1  1)] ;; → (top -> bottom)
          )
      4 (case self-v
          :N [(g -1  2) (g -1  1) (g -1 -1) (g -1 -2)] ;; ←
          :U [(g  2  1) (g  1  1) (g -1  1) (g -2  1)] ;; ↓
          :I [(g -2 -1) (g -1 -1) (g  1 -1) (g  2 -1)] ;; ↑
          :M [(g  1 -2) (g  1 -1) (g  1  1) (g  1  2)] ;; →
          )
      5 (case self-v
          :N [(g -1  2) (g -1  1) (g -1  0) (g -1 -1) (g -1 -2)] ;; ←
          :U [(g  2  1) (g  1  1) (g  0  1) (g -1  1) (g -2  1)] ;; ↓
          :I [(g -2 -1) (g -1 -1) (g  0 -1) (g  1 -1) (g  2 -1)] ;; ↑
          :M [(g  1 -2) (g  1 -1) (g  1  0) (g  1  1) (g  1  2)] ;; →
          )
      (throw (ex-info "Unsupported neighbourhood size" {:size size})))))

(defmethod get-umwelt :moore
  [[w h] gen [[x y] _] [_ self?]]
  (let [g (fn [dx dy]
            (get-in gen
                    [(wrap-bounds (+ y dy) 0 (dec h))
                     (wrap-bounds (+ x dx) 0 (dec w))]))
        self (if self? (g 0 0) nil)]
    ;; `self` can be nil! (remove when counting)
    [(g -1 -1) (g  0 -1) (g  1 -1)
     (g -1  0)   self    (g  1  0)
     (g -1  1) (g  0  1) (g  1  1)]))

(defmethod get-umwelt :vneumann
  [[w h] gen [[x y] _] [_ self?]]
  (let [g (fn [dx dy]
            (get-in gen
                    [(wrap-bounds (+ y dy) 0 (dec h))
                     (wrap-bounds (+ x dx) 0 (dec w))]))
        self (if self? (g 0 0) nil)]
    ;; `self` can be nil! (remove when counting)
    [          (g  0 -1)          
     (g -1  0)   self    (g  1  0)
     ,         (g  0  1)          ]))


(defmulti apply-rule (fn [_ _ rule-spec] (first rule-spec)))

(defmethod apply-rule :match
  [umwelt _ [_ dna]]
  (calc/dna-get dna umwelt))

(defmethod apply-rule :life
  [umwelt self-v [_ dna]]
  (let [alive (vec (remove #(or (= :N %) (nil? %)) umwelt))]
    (case (count alive)
      2 self-v
      3 (calc/dna-get dna alive)
      :N)))


(defn sys-next
  [[w h :as res] rule-spec umwelt-spec gen]
  (if h
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [[_ v :as cell] [[x y] (get-in gen [y x])]
                          env (get-umwelt res gen cell umwelt-spec)
                          next-v (apply-rule env v rule-spec)]
                      next-v))
                  (range w)))
          (range h))
    (mapv (fn [x]
            (let [[_ v :as cell] [[x] (get gen x)]
                  env (get-umwelt res gen cell umwelt-spec)
                  next-v (apply-rule env v rule-spec)]
              next-v))
          (range w))))

(defn make-selfi
  [res dna ini-spec]
  (let [rule-spec   [:match dna]
        umwelt-size (calc/dna-dimension dna)
        umwelt-spec [:select-ltr umwelt-size]
        gen1 (sys-ini res ini-spec)]
    (iterate (partial sys-next res rule-spec umwelt-spec) gen1)))

(defn make-mindform
  [res dna ini-spec]
  (let [rule-spec   [:match dna]
        umwelt-size (calc/dna-dimension dna)
        umwelt-spec [:self-select-ltr umwelt-size]
        gen1 (sys-ini res ini-spec)]
    (iterate (partial sys-next res rule-spec umwelt-spec) gen1)))

(defn make-lifeform
  [res dna]
  (let [rule-spec   [:life dna]
        umwelt-spec [:moore false]
        ini-spec    [:random]
        gen1 (sys-ini res ini-spec)]
    (iterate (partial sys-next res rule-spec umwelt-spec) gen1)))

(defn make-decisionform
  [res dna init-size]
  (let [rule-spec   [:life dna]
        umwelt-spec [:moore false]
        ini-spec    [:rand-center init-size]
        gen1 (sys-ini res ini-spec)]
    (iterate (partial sys-next res rule-spec umwelt-spec) gen1)))


(comment
  (get-resolution [[[1 1 1] [1 1 1]]])

  ,)
