;; ========================================================================
;;     formform emulation module
;;     -- created 02/2025, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.emul.core
  (:require [formform.calc.core :as calc-core]
            ;; [formform.expr :as expr]
            #?(:clj  [formform.emul.interfaces :as i
                      :refer [defini defumwelt defrule defspecies
                              UmweltOptimized RuleOptimized]]
               :cljs [formform.emul.interfaces :as i
                      :refer [UmweltOptimized RuleOptimized]
                      :refer-macros [defini defumwelt defrule defspecies]])
            [formform.utils :as utils])
  ;; #?(:clj (:import [formform.emul.interfaces UmweltOptimized RuleOptimized]))
  )

(defn- val-or-rand [v]
  (case v
    :rand (rand-nth calc-core/nuim-code)
    v))

(defn get-resolution-from-generation
  [gen]
  (loop [xs  gen
         res [(count gen)]]
    (let [x (first xs)]
      (if (vector? x)
        (recur x (conj res (count x)))
        res))))

(defini :fill-all [pattern]
  "Fills a generation with a given `pattern`, which can be one of:
- a constant value (`:N`/`:M`/`:U`/`:I`) or `:rand` to fill random values
- an explicit n-dimensional vector of constants
- a function that takes an index and returns a constant"
  (make-gen
   [_ w]
   (cond
     (keyword? pattern) (vec (repeatedly w (partial val-or-rand pattern)))
     (sequential? pattern) (vec pattern)
     :else (mapv pattern (range w))))

  (make-gen
   [_ w h]
   (cond
     (keyword? pattern) (vec (repeatedly
                              h #(vec (repeatedly
                                       w (partial val-or-rand pattern)))))
     (sequential? pattern) (mapv vec pattern)
     :else (mapv (fn [y] (mapv (fn [x] (pattern x y))
                              (range w)))
                 (range h)))))

(defini :fill-center [area bg]
  "Fills the center of given `area` within a generation otherwise filled with given background value `bg`.
- `area`: either a concrete value vector or a map `{:res [<x> <?y>] :val <const|:rand>}
- `bg`: <const|:rand>"
  (make-gen [_ w]
    (let [explicit? (vector? area)
          cw (if explicit? (count area) (first (:res area)))
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

  (make-gen [_ w h]
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
            (range h)))))

(defini :random []
  "Fills a generation with random values."
  (make-gen
   [_ w]
   (i/make-gen (->Ini-FillAll :rand) w))

  (make-gen
   [_ w h]
   (i/make-gen (->Ini-FillAll :rand) w h)))

(defini :rand-center [size]
  "Fills the center of given `size` within a generation with random values."
  (make-gen
   [_ w]
   (i/make-gen (->Ini-FillCenter {:res size :val :rand} :N) w))

  (make-gen
   [_ w h]
   (i/make-gen (->Ini-FillCenter {:res [size size] :val :rand} :N) w h)))

(defini :ball []
  "Fills the center of a generation with the pattern `…iumui…`."
  (make-gen
   [_ w]
   (i/make-gen (->Ini-FillCenter [:I :U :M :U :I] :N) w))

  (make-gen
   [_ w h]
   (i/make-gen (->Ini-FillCenter [[:N :N :I :N :N]
                                  [:N :I :U :I :N]
                                  [:I :U :M :U :I]
                                  [:N :I :U :I :N]
                                  [:N :N :I :N :N]] :N) w h)))


(defn- wrap-bounds
  [n lower-bound upper-bound]
  (cond
    (> n upper-bound) lower-bound
    (< n lower-bound) upper-bound
    :else n))


(defumwelt :select-ltr [size]
  "In a 1d environment, observes the cell itself and its direct neighborhood of given `size`."
  (observe-umwelt
   [_ gen1d [[x] _] w]
   (let [g (fn [dx]
             (get gen1d (wrap-bounds (+ x dx) 0 (dec w))))]
     (case size
       0 []
       1 [(g  0)]
       2 [(g -1) (g  1)]
       3 [(g -1) (g  0) (g  1)]
       4 [(g -2) (g -1) (g  1) (g  2)]
       5 [(g -2) (g -1) (g  0) (g  1) (g  2)]
       (throw (ex-info "Unsupported neighbourhood size" {:size size})))))

  UmweltOptimized
  (observe-umwelt--fast
   [_ gen1d-arr [[x] _] w]
   (let [g (fn [dx]
             (calc-core/const->digit
              (aget gen1d-arr (wrap-bounds (+ x dx) 0 (dec w)))))]
     (case size
       0 ""
       1 (str (g  0))
       2 (str (g -1) (g  1))
       3 (str (g -1) (g  0) (g  1))
       4 (str (g -2) (g -1) (g  1) (g  2))
       5 (str (g -2) (g -1) (g  0) (g  1) (g  2))
       (throw (ex-info "Unsupported neighbourhood size" {:size size}))))))

(defumwelt :self-select-ltr [size]
  "In a 2d environment, the cell “chooses” the direction in which it will observe its neighborhood of given `size` (like in `:select-ltr`). The cell’s choice is determined by its own value:
- `:N`: ← (left)
- `:M`: → (right)
- `:U`: ↓ (down)
- `:I`: ↑ (up)"
  (observe-umwelt
   [_ gen2d [[x y] self-v] w h]
   (let [g (fn [dx dy]
             (get-in gen2d ;; -2 fps
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

  UmweltOptimized
  (observe-umwelt--fast
   [_ gen2d-arr [[x y] self-v] w h]
   (let [g (fn [dx dy]
             (calc-core/const->digit
              (aget gen2d-arr
                    (wrap-bounds (+ y dy) 0 (dec h))
                    (wrap-bounds (+ x dx) 0 (dec w)))))]
     (case size
       0 ""
       1 (case self-v
           :N (str (g -1  0)) ;; ←
           :U (str (g  0  1)) ;; ↓
           :I (str (g  0 -1)) ;; ↑
           :M (str (g  1  0)) ;; →
           )
       2 (case self-v
           :N (str (g -1  1) (g -1 -1)) ;; ←
           :U (str (g  1  1) (g -1  1)) ;; ↓
           :I (str (g -1 -1) (g  1 -1)) ;; ↑
           :M (str (g  1 -1) (g  1  1)) ;; →
           )
       3 (case self-v
           :N (str (g -1  1) (g -1  0) (g -1 -1)) ;; ← (bottom -> top)
           :U (str (g  1  1) (g  0  1) (g -1  1)) ;; ↓ (right -> left)
           :I (str (g -1 -1) (g  0 -1) (g  1 -1)) ;; ↑ (left -> right)
           :M (str (g  1 -1) (g  1  0) (g  1  1)) ;; → (top -> bottom)
           )
       4 (case self-v
           :N (str (g -1  2) (g -1  1) (g -1 -1) (g -1 -2)) ;; ←
           :U (str (g  2  1) (g  1  1) (g -1  1) (g -2  1)) ;; ↓
           :I (str (g -2 -1) (g -1 -1) (g  1 -1) (g  2 -1)) ;; ↑
           :M (str (g  1 -2) (g  1 -1) (g  1  1) (g  1  2)) ;; →
           )
       5 (case self-v
           :N (str (g -1  2) (g -1  1) (g -1  0) (g -1 -1) (g -1 -2)) ;; ←
           :U (str (g  2  1) (g  1  1) (g  0  1) (g -1  1) (g -2  1)) ;; ↓
           :I (str (g -2 -1) (g -1 -1) (g  0 -1) (g  1 -1) (g  2 -1)) ;; ↑
           :M (str (g  1 -2) (g  1 -1) (g  1  0) (g  1  1) (g  1  2)) ;; →
           )
       (throw (ex-info "Unsupported neighbourhood size" {:size size}))))))

(defumwelt :moore [order self?]
  "In a 2d environment, observes the cell’s direct neighborhood, made up of 8 or 9 (given `self?` is true) cells (corner cells included)."
  (observe-umwelt
   [_ gen2d [[x y] _] w h]
   (let [g (fn [dx dy]
             (get-in gen2d
                     [(wrap-bounds (+ y dy) 0 (dec h))
                      (wrap-bounds (+ x dx) 0 (dec w))]))
         self (if self? (g 0 0) nil)]
     ;; `self` can be nil! (remove when counting)
     (case order
       :column-first
       ;; 0 3 5   0 3 6
       ;; 1 . 6   1 4 7
       ;; 2 4 7   2 5 8
       [(g -1 -1) (g -1  0) (g -1  1)
        (g  0 -1)   self    (g  0  1)
        (g  1 -1) (g  1  0) (g  1  1)]
       :row-first
       ;; 0 1 2   0 1 2
       ;; 3 . 4   3 4 5
       ;; 5 6 7   6 7 8
       [(g -1 -1) (g  0 -1) (g  1 -1)
        (g -1  0)   self    (g  1  0)
        (g -1  1) (g  0  1) (g  1  1)]
       (throw (ex-info "Invalid order for umwelt `:moore`."
                       {:order order})))))

  UmweltOptimized
  (observe-umwelt--fast
   [_ gen2d-arr [[x y] _] w h]
   (let [g (fn [dx dy]
             (let [c (calc-core/const->digit
                      (aget gen2d-arr
                            (wrap-bounds (+ y dy) 0 (dec h))
                            (wrap-bounds (+ x dx) 0 (dec w))))]
               (case c 0 nil c)))
         self (if self? (g 0 0) nil)]
     (case order
       :column-first
       (str (g -1 -1) (g -1  0) (g -1  1)
            (g  0 -1)   self    (g  0  1)
            (g  1 -1) (g  1  0) (g  1  1))
       :row-first
       (str (g -1 -1) (g  0 -1) (g  1 -1)
            (g -1  0)   self    (g  1  0)
            (g -1  1) (g  0  1) (g  1  1))
       (throw (ex-info "Invalid order for umwelt `:moore`."
                       {:order order}))))))

(defumwelt :von-neumann [order self?]
  "In a 2d environment, observes the cell’s direct neighborhood, made up of 4 or 5 (given `self?` is true) cells (corner cells not included)."
  (observe-umwelt
   [_ gen2d [[x y] _] w h]
   (let [g (fn [dx dy]
             (get-in gen2d
                     [(wrap-bounds (+ y dy) 0 (dec h))
                      (wrap-bounds (+ x dx) 0 (dec w))]))
         self (if self? (g 0 0) nil)]
     ;; `self` can be nil! (remove when counting)
     (case order
       :column-first
       ;;   1       1
       ;; 0 . 3   0 2 4
       ;;   2       3
       [,         (g -1  0)
        (g  0 -1)   self    (g  0  1)
        ,         (g  1  0)]
       :row-first
       ;;   0       0
       ;; 1 . 2   1 2 3
       ;;   3       4
       [          (g  0 -1)          
        (g -1  0)   self    (g  1  0)
        ,         (g  0  1)          ]
       (throw (ex-info "Invalid order for umwelt `:von-neumann`."
                       {:order order})))))

  UmweltOptimized
  (observe-umwelt--fast
   [_ gen2d-arr [[x y] _] w h]
   (let [g (fn [dx dy]
             (let [c (calc-core/const->digit
                      (aget gen2d-arr
                            (wrap-bounds (+ y dy) 0 (dec h))
                            (wrap-bounds (+ x dx) 0 (dec w))))]
               (case c 0 nil c)))
         self (if self? (g 0 0) nil)]
     (case order
       :column-first
       (str
        ,         (g -1  0)
        (g  0 -1)   self    (g  0  1)
        ,         (g  1  0))
       :row-first
       (str
        ,         (g  0 -1)          
        (g -1  0)   self    (g  1  0)
        ,         (g  0  1))
       (throw (ex-info "Invalid order for umwelt `:von-neumann`."
                       {:order order}))))))

#_
(def match-dna (comp first calc-core/filter-dna))

(defn match-dna
  [dna umwelt]
  (if (== (count dna) 1)
    (dna 0)
    (let [qtn (apply str (mapv calc-core/const?->digit umwelt))
          idx (- (count dna) 1
                 (utils/parse-int qtn 4))]
      (dna idx))))

(defn match-dna--fast
  [dna umwelt-qtn]
  (if (== (count dna) 1)
    (dna 0)
    (dna (- (count dna) 1 (utils/parse-int umwelt-qtn 4)))))

(defrule :match [dna]
  "Matches an `umwelt` (of cell values) directly against the given `dna`, which is equivalent to interpreting and calculating a corresponding expression."
  (apply-rule
   [_ umwelt _]
   (match-dna dna umwelt))

  RuleOptimized
  (apply-rule--fast
   [_ umwelt-qtn _]
   (match-dna--fast dna umwelt-qtn)))

(defrule :life [dna]
  "Modeled after the rules for the “Game of Life”:
- a cell is “alive” when its value is not `:N`
- if the cell has 2 neighbors, it keeps its own value
- if the cell has 3 neighbors, it matches their values against the given `dna` (see `:match` rule)
- in every other case, the cell “dies” (turns to `:N`)"
  (apply-rule
   [_ umwelt self-v]
   (let [alive (vec (remove #(or (= :N %) (nil? %)) umwelt))]
     (case (count alive)
       2 self-v
       3 (match-dna dna alive)
       :N)))

  RuleOptimized
  (apply-rule--fast
   [_ umwelt-qtn self-v]
   (case (count umwelt-qtn)
     2 self-v
     3 (match-dna--fast dna umwelt-qtn)
     :N)))





(defrecord CASpec [resolution rule-spec umwelt-spec ini-spec])

(defspecies :selfi [dna ini]
  "1D cellular automaton. Takes a `dna` for its rule function (type `:match`) and an `ini` type (via `make-ini`). Its ‘umwelt’ is of type `:select-ltr`."
  (specify-ca
   [this options w]
   (let [{:keys [overwrites]} options
         umwelt-size (calc-core/dna-dimension dna)]
     (with-meta
       (map->CASpec (merge
                     {:label       "SelFi"
                      :resolution  [w]
                      :rule-spec   (->Rule-Match dna)
                      :umwelt-spec (->Umwelt-SelectLtr umwelt-size)
                      :ini-spec    ini}
                     overwrites))
       {:constructor this}))))


(defspecies :mindform [dna ini]
  "2D cellular automaton. Takes a `dna` for its rule function (type `:match`) and an `ini` type (via `make-ini`). Its ‘umwelt’ is of type `:self-select-ltr`."
  (specify-ca
   [this options w h]
   (let [{:keys [overwrites]} options
         umwelt-size (calc-core/dna-dimension dna)]
     (with-meta
       (map->CASpec (merge
                     {:label       "MindFORM"
                      :resolution  [w h]
                      :rule-spec   (->Rule-Match dna)
                      :umwelt-spec (->Umwelt-SelfSelectLtr umwelt-size)
                      :ini-spec    ini}
                     overwrites))
       {:constructor this}))))

(defspecies :lifeform [dna]
  "2D cellular automaton. Takes a `dna` as part of its rule function, which is of type `:life`. Its ‘umwelt’ is of type `:moore`."
  (specify-ca
   [this options w h]
   (let [{:keys [overwrites]} options]
     (with-meta
       (map->CASpec (merge
                     {:label       "LifeFORM"
                      :resolution  [w h]
                      :rule-spec   (->Rule-Life dna)
                      :umwelt-spec (->Umwelt-Moore :column-first false)
                      :ini-spec    (->Ini-Random)}
                     overwrites))
       {:constructor this}))))

(defspecies :decisionform [dna init-size]
  "2D cellular automaton. Takes a `dna` as part of its rule function, which is of type `:life`, and an initial size for its `:rand-center` type ini. Its ‘umwelt’ is of type `:moore`."
  (specify-ca
   [this options w h]
   (let [{:keys [overwrites]} options]
     (with-meta
       (map->CASpec (merge
                     {:label       "DecisionFORM"
                      :resolution  [w h]
                      :rule-spec   (->Rule-Life dna)
                      :umwelt-spec (->Umwelt-Moore :column-first false)
                      :ini-spec    (->Ini-RandCenter init-size)}
                     overwrites))
       {:constructor this}))))


(def sys-ini i/make-gen)

(defn sys-next
  [res rng-w rng-h rule-spec umwelt-spec gen]
  (if rng-h
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [[_ v :as cell] [[x y] (get-in gen [y x])]
                          umwelt (apply i/observe-umwelt
                                        umwelt-spec gen cell res)
                          next-v (i/apply-rule
                                  rule-spec umwelt v)]
                      next-v))
                  rng-w))
          rng-h)
    (mapv (fn [x]
            (let [[_ v :as cell] [[x] (get gen x)]
                  umwelt (apply i/observe-umwelt umwelt-spec gen cell res)
                  next-v (i/apply-rule rule-spec umwelt v)]
              next-v))
          rng-w)))


(defn sys-next--fast
  "More performant version of `sys-next` that uses native platform arrays instead of vectors for the generations and calls special `--fast` methods that operate on them. Note that these methods must be implemented for the provided `rule-spec` and `umwelt-spec`.

  Always prefer `sys-next` for better compatibility across library functions and maximal flexibility."
  ([w h rule-spec umwelt-spec gen]
   (let [compute (fn [x y]
                   (let [[_ v :as cell] [[x y] (aget gen y x)]
                         qtn (i/observe-umwelt--fast umwelt-spec gen cell w h)]
                     (i/apply-rule--fast rule-spec qtn v)))]
     #?(:clj (let [^"[[Lclojure.lang.Keyword;"
                   next-gen (make-array clojure.lang.Keyword h w)]
               (loop [y 0]
                 (when (< y h)
                   (loop [x 0]
                     (when (< x w)
                       (aset next-gen y x (compute x y))
                       (recur (inc x))))
                   (recur (inc y))))
               next-gen)

        :cljs (let [next-gen #js []]
                (loop [y 0]
                  (when (< y h)
                    (let [js-row #js []]
                      (.push next-gen js-row)
                      (loop [x 0]
                        (when (< x w)
                          (.push js-row (compute x y))
                          (recur (inc x))))
                      (recur (inc y)))))
                next-gen))))

  ([w rule-spec umwelt-spec gen]
   (let [compute (fn [x]
                   (let [[_ v :as cell] [[x] (aget gen x)]
                         qtn (i/observe-umwelt--fast umwelt-spec gen cell w)]
                     (i/apply-rule--fast rule-spec qtn v)))]
     #?(:clj (let [^"[[Lclojure.lang.Keyword;"
                   next-gen (make-array clojure.lang.Keyword w)]
               (loop [x 0]
                 (when (< x w)
                   (aset next-gen x (compute x))
                   (recur (inc x))))
               next-gen)

        :cljs (let [next-gen #js []]
                (loop [x 0]
                  (when (< x w)
                    (.push next-gen (compute x))
                    (recur (inc x))))
                next-gen)))))


(deftype CellularAutomaton
    #?(:cljs [res init-evolution calc-next history-cache-limit
              ^:mutable history
              ^:mutable curr-idx
              ^:mutable curr-gen]
       :clj  [res init-evolution calc-next history-cache-limit
              ^:unsynchronized-mutable history
              ^:unsynchronized-mutable curr-idx
              ^:unsynchronized-mutable curr-gen])
  i/CASystem
  (step [_]
    (let [next-idx (inc curr-idx)
          next-gen (if-let [cached (when (< next-idx (count history))
                                     (nth history next-idx))]
                     cached
                     (calc-next curr-gen))]
      (set! curr-idx next-idx)
      (set! curr-gen next-gen)
      (when (< (dec (count history)) next-idx history-cache-limit)
        (set! history (conj! history next-gen)))))
  (restart [_]
    (set! curr-idx 0)
    (set! curr-gen (init-evolution 0)))
  (get-resolution [_]
    res)
  (get-current-generation [_ optimized?]
    (if optimized?
      curr-gen
      (utils/array-to-vector curr-gen)))
  (get-cached-history [_ optimized?]
    (let [^clojure.lang.PersistentVector v (persistent! history)]
      (set! history (transient v))
      (if optimized?
        v
        (mapv utils/array-to-vector v))))
  (get-system-time [_]
    curr-idx)
  (get-history-cache-limit [_]
    history-cache-limit))

;; because direct method access for deftype JS objects is weird
;; remember to turn dashes into underscores for method names!
#?(:cljs (extend-type CellularAutomaton
           Object
           (step [_this] (i/step _this))
           (restart [_this] (i/restart _this))
           (get_resolution [_this] (i/get-resolution _this))
           (get_current_generation [_this optimized?]
             (i/get-current-generation _this optimized?))
           (get_cached_history [_this optimized?]
             (i/get-cached-history _this optimized?))
           (get_system_time [_this] (i/get-system-time _this))
           (get_history_cache_limit [_this] (i/get-history-cache-limit _this))))

;; ? use bigint
(defn calc-generation-cache-limit
  [resolution cell-limit]
  (let [cells-per-gen (apply * resolution)]
    (int (/ cell-limit cells-per-gen))))

(defn create-ca
  [{:keys [resolution rule-spec umwelt-spec ini-spec]} hist-cache-limit]
  (let [optimized? (and (satisfies? UmweltOptimized umwelt-spec)
                        (satisfies? RuleOptimized rule-spec))
        [w h] resolution
        gen1 (let [v (apply sys-ini ini-spec resolution)]
               (if optimized?
                 (if h
                   (utils/keywords-to-array-2d v)
                   (utils/keywords-to-array v))
                 v))
        calc-next (if optimized?
                    (if h
                      (partial sys-next--fast w h rule-spec umwelt-spec)
                      (partial sys-next--fast w rule-spec umwelt-spec))
                    (partial sys-next
                             resolution (range w) (when h (range h))
                             rule-spec umwelt-spec))
        evolution [gen1]
        hist-cache-limit (or hist-cache-limit
                             (calc-generation-cache-limit resolution 8000000))]
    (->CellularAutomaton resolution evolution calc-next hist-cache-limit
                         (transient evolution) 0 gen1)))

(defn ca-iterator
  [{:keys [resolution rule-spec umwelt-spec ini-spec]}]
  (let [gen1 (apply sys-ini ini-spec resolution)
        [w h] resolution]
    (iterate (partial sys-next
                      resolution (range w) (when h (range h))
                      rule-spec umwelt-spec) gen1)))


(comment
  (def dna [:N :U :I :M :U :U :M :M :I :M :I :M :M :M :M :M :N :U :I :M :U :I :M :M :I :M :I :M :M :M :M :M :N :U :I :M :U :U :M :M :I :M :I :M :M :M :M :M :N :U :I :M :U :I :M :M :I :M :I :M :M :M :M :I])

  (def rule (->Rule-Life dna))

  (i/apply-rule rule (i/observe-umwelt (->Umwelt-Moore :column-first false)
                                       [[:N :N :U]
                                        [:M :_ :N]
                                        [:N :N :I]]
                                       [[1 1] :N]
                                       3 3)
                :N)
  ;;=> 

  (require '[clojure.math.combinatorics :as combo])

  (calc-core/filter-dna dna [:U :M :I])
  
  (filter
   (fn [[p [v]]] (= v :M))
   (map
    (fn [p] [p (calc-core/filter-dna dna (vec p))])
    (combo/permutations [:U :M :I])))

  ,)

(comment
[[:N :N :U]
 [:M :_ :N] ;; self: :N
 [:N :N :I]]
;;=> :U (should be :M)
;; solutions:
;; [:M :U :I] ← ↗ ↘
;; [:M :I :U] ← ↘ ↗

[[:N :U :N]
 [:I :_ :N] ;; self: :M
 [:N :U :N]]
;;=> :M (should be :I)
;; solutions:
;; [:I :U :U] ← ↓ ↑ / ← ↑ ↓

[[:N :I :N]
 [:N :_ :N] ;; self: :N
 [:M :M :N]]
;;=> :N (should be :U)
;; solutions:
;; [:M :I :M] ↙ ↑ ↓ / ↓ ↑ ↙
;; [:M :M :I] ↙ ↓ ↑ / ↓ ↙ ↑

[[:N :N :N]
 [:N :_ :I] ;; self: :N
 [:U :N :M]]
;;=> :I (should be :U)
;; solutions:
;; [:U :I :M] ↙ → ↘
;; [:U :M :I] ↙ ↘ →

[[:N :N :N]
 [:N :_ :U] ;; self: :M
 [:I :I :N]]
;;=> :U (should be :M)
;; solutions:
;; [:I :U :I] ↙ → ↓ / ↓ → ↙
;; [:I :I :U] ↙ ↓ → / ↓ ↙ →

[[:N :N :N]
 [:N :_ :M] ;; self: :N
 [:I :M :N]]
;;=> :U (should be :N)
;; solutions:
;; [:I :M :M] ↙ ↓ → / ↙ → ↓

[[:N :N :U]
 [:I :_ :M] ;; self: :N
 [:N :N :N]]
;;=> :U (should be :I)
;; solutions:
;; [:I :U :M] ← ↗ →
;; [:I :M :U] ← → ↗

  ,)
