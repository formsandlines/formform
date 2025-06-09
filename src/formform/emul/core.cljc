;; ========================================================================
;;     formform emulation module
;;     -- created 02/2025, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.emul.core
  (:require [formform.calc.core :as calc-core]
            [clojure.test.check.random :as random]
            ;; [formform.expr :as expr]
            #?(:clj  [formform.emul.interfaces :as i
                      :refer [defini defumwelt defrule
                              UmweltOptimized RuleOptimized]]
               :cljs [formform.emul.interfaces :as i
                      :refer [UmweltOptimized RuleOptimized]
                      :refer-macros [defini defumwelt defrule]])
            #?(:cljs [goog.math.Long :as glong])
            [formform.utils :as utils])
  ;; #?(:clj (:import [formform.emul.interfaces UmweltOptimized RuleOptimized]))
  )

(defn get-resolution-from-generation
  [gen]
  (loop [xs  gen
         res [(count gen)]]
    (let [x (first xs)]
      (if (vector? x)
        (recur x (conj res (count x)))
        res))))

(defn make-rng
  ([] (make-rng nil))
  ([seed] (if seed
            (random/make-random seed)
            (random/make-random))))

(defn rng-select
  [rng coll]
  (let [n #?(:clj  (mod (random/rand-long rng) 4)
             ;; goog.math.Long `.modulo` behaves more like `rem` in Clojure
             ;; so we need to do some math to make it behave like `mod`:
             :cljs (let [^goog.math.Long gl
                         (.modulo ^goog.math.Long (random/rand-long rng)
                                  ^goog.math.Long (glong/fromNumber 4))
                         ^js/Number nrem (.toInt gl)]
                     (if (< nrem 0) (+ nrem 4) nrem)))]
    (coll n)))

(defn transduce-ini
  ([{:keys [no-rng? seed]} xform w]
   (into [] (comp (map-indexed (fn [i rng']
                                 {:rng rng'
                                  :i i
                                  :w w
                                  :x i}))
                  xform
                  (map :v))
         (if no-rng?
           (repeat w nil)
           (random/split-n (make-rng seed) w))))

  ([{:keys [no-rng? seed]} xform w h]
   (into [] (comp (map-indexed (fn [i rng']
                                 {:rng rng'
                                  :w w
                                  :h h
                                  :i i
                                  :x (mod i w)
                                  :y (quot i w)}))
                  xform
                  (map :v)
                  (partition-all w))
         (if no-rng?
           (repeat (* w h) nil)
           (random/split-n (make-rng seed) (* w h))))))

(defn- ini-transducer?
  [ini]
  (and (record? ini) (satisfies? i/IniTransducer ini)))

(defn- validate-ini-transducer
  [ini]
  (if (ini-transducer? ini)
    ini
    (throw (ex-info "Ini must be a record and satisfy the `formform.emul.interfaces.IniTransducer` protocol." {:ini ini}))))

(defini :constant [-opts const]
  "Fills a generation with a given constant (`:N`/`:M`/`:U`/`:I`)."
  (make-gen [this w] (transduce-ini -opts (i/ini-xform1d this) w))
  (make-gen [this w h] (transduce-ini -opts (i/ini-xform2d this) w h))

  i/IniTransducer
  (ini-xform1d [_] (map #(assoc % :v const)))
  (ini-xform2d [this] (i/ini-xform1d this)))


#_
(defn val-random
  [{:keys [rng]}]
  (rng-select rng calc-core/nuim-code))

(defn val-random
  [{:keys [rng]}]
  (rng-select rng calc-core/nuim-code))

(defini :random [-opts distribution]
  "Fills a generation with random values. Takes a `distribution`, which is either a map of `:N`/`:U`/`:I`/`:M` keys to ratios from 0.0 to 1.0 (representing the proportion of the respective constant in relation to all random values) or a single decimal number that defines an equal ratio for `:N`/`:U`/`:I` against `:N` (e.g. `0.0` → all `:N`, `1.0` → no `:N`, `0.5` → all evenly distributed).

The (first) `-opts` argument is a map that can take a `:seed` entry with an integer number to provide a seed for reproducable random generations."
  (make-gen [this w] (transduce-ini -opts (i/ini-xform1d this) w))
  (make-gen [this w h] (transduce-ini -opts (i/ini-xform2d this) w h))

  i/IniTransducer
  (ini-xform1d [_] (map #(assoc % :v (val-random %))))
  (ini-xform2d [this] (i/ini-xform1d this)))


(defn make-val-cycle [pattern]
  (fn [{:keys [y x]}]
    (pattern (mod (+ x (or y 0)) (count pattern)))))

(defini :cycle [-opts pattern]
  "Fills a generation with repeatedly with the same value sequence."
  (make-gen [this w] (transduce-ini -opts (i/ini-xform1d this) w))
  (make-gen [this w h] (transduce-ini -opts (i/ini-xform2d this) w h))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [val-cycle (make-val-cycle pattern)]
     (map #(assoc % :v (val-cycle %)))))
  (ini-xform2d [this] (i/ini-xform1d this)))


(defn- segment-bounds
  "Calculates the start and end index of a subsequence (“segment”) given its position within a sequence and its alignment at that position. The sequence wraps around itself, so when its subsequence indices would be out of bounds, their count continues from the beginning/end.
  - `segm-pos` can be an index or an alignment keyword (see below)
  - `segm-align` must be one of `:start`, `:center` or `:end`"
  [total-len segm-len segm-pos segm-align segm-offset]
  (let [wrap (fn [x] (cond (< x 0) (+ total-len x)
                          (>= x total-len) (- x total-len)
                          :else x))
        anchor (case segm-pos
                 :start  0
                 :center (quot total-len 2)
                 :end    total-len
                 segm-pos)
        shift (case segm-align
                :start  0
                :center (- (quot segm-len 2))
                :end    (- segm-len))
        start (wrap (+ anchor segm-offset shift))
        end   (wrap (+ start (dec segm-len)))]
    [start end]))

(defn- within-segment-bounds?
  "Checks if a given index `x` is within the boundaries of a segment (see `segment-bounds`) in the context of the given total length of the wrapping sequence."
  [[start end] total-len x]
  (if (> start end)
    (or (<= start x (dec total-len))
        (<= 0 x end))
    (<= start x end)))

(def align->normalized
  {:left [:start :center]
   :right [:end :center]
   :center [:center :center]
   :top [:center :start]
   :bottom [:center :end]

   :topleft [:start :start]
   :topcenter [:center :start]
   :topright [:end :start]
   :centerleft [:start :center]
   :centerright [:end :center]
   :bottomleft [:start :end]
   :bottomcenter [:center :end]
   :bottomright [:end :end]})

(def align-tuples
  (set (vals align->normalized)))

(defn- normalize-position [pos]
  (let [v (vec (if-let [norm-align (or (align-tuples pos)
                                       (align->normalized pos))]
                 norm-align
                 (cond
                   (sequential? pos) pos
                   (int? pos) [pos pos]
                   (nil? pos) []
                   :else (throw (ex-info "Invalid position!" {:pos pos})))))]
    {:pos-x (get v 0 0)
     :pos-y (get v 1 0)}))

(defn- normalize-alignment [align]
  (let [v (vec (if-let [norm-align (or (align-tuples align)
                                       (align->normalized align))]
                 norm-align
                 (if (nil? align)
                   []
                   (throw (ex-info "Invalid alignment!" {:align align})))))]
    {:align-x (get v 0 :start)
     :align-y (get v 1 :start)}))

(defn- normalize-offset [offset]
  (let [v (vec (cond
                 (sequential? offset) offset
                 (int? offset) [offset offset]
                 (nil? offset) []
                 :else (throw (ex-info "Invalid offset!" {:offset offset}))))]
    {:offset-x (get v 0 0)
     :offset-y (get v 1 0)}))

;; (defrecord Pattern [gen1d gen2d])
;; (defrecord PatternSpec [w h f])

(defn- normalize-anchor [{:keys [pos align offset] :as anchor}]
  (if (and (:pos-x anchor)
           (:align-x anchor)
           (:offset-x anchor))
    anchor ;; already normalized
    (merge (normalize-position pos)
           (normalize-alignment align)
           (normalize-offset offset))))

(defn- normalize-pattern
  [pattern]
  (cond
    (and (map? pattern)
         (or (:gen1d pattern) (:gen2d pattern)
             (:f pattern))) pattern
    (vector? pattern) (let [dim (if (sequential? (first pattern))
                                  2 1)]
                        ;; ? defaults for missing dimensions
                        {:gen1d (if (== dim 1) pattern (first pattern))
                         :gen2d (if (== dim 2) pattern [pattern])})
    :else (throw (ex-info "Invalid pattern!" {:pattern pattern}))))

(defn- get-pattern-type
  [normal-pattern]
  (cond
    (:f normal-pattern) :recipe
    (or (:gen1d normal-pattern)
        (:gen2d normal-pattern)) :explicit
    :else (throw (ex-info "Unknown pattern type." {:pattern normal-pattern}))))

(defn- get-pattern-dimensions
  [{:keys [gen1d gen2d w h] :as pattern}]
  (case (get-pattern-type pattern)
    :explicit {:1d (count gen1d)
               :2d [(count (first gen2d)) (count gen2d)]}
    :recipe   {:1d w
               :2d [w h]}))

(defn- make-pattern-fn
  [normal-pattern]
  (case (get-pattern-type normal-pattern)
    :explicit (fn [{:keys [i j v] :as env}]
                (let [v' (if j
                           (get-in (:gen2d normal-pattern) [i j])
                           (get (:gen1d normal-pattern) i))]
                  (case v'
                    :_ v
                    :? (val-random env)
                    v')))
    :recipe (:f normal-pattern)))

(defn make-val-figure1d
  [normal-pattern normal-anchor]
  (let [{:keys [pos-x align-x offset-x]} normal-anchor
        {ptn-w :1d} (get-pattern-dimensions normal-pattern)
        get-val (make-pattern-fn normal-pattern)]
    (fn [{:keys [w x v] :as env}]
      (let [[s0 s1 :as xb] (segment-bounds w ptn-w pos-x align-x offset-x)
            at-ptn? (within-segment-bounds? xb w x)]
        (if at-ptn?
          (get-val (-> env (assoc :i (if (and (> s0 s1) (< x s0))
                                       (+ x (- ptn-w (inc s1)))
                                       (- x s0)))))
          v)))))

(defn make-val-figure2d
  [normal-pattern normal-anchor]
  (let [{:keys [pos-x    pos-y
                align-x  align-y
                offset-x offset-y]} normal-anchor
        {[ptn-w ptn-h] :2d} (get-pattern-dimensions normal-pattern)
        get-val (make-pattern-fn normal-pattern)]
    (fn [{:keys [w h x y v] :as env}]
      (let [[sx0 sx1 :as xb] (segment-bounds w ptn-w pos-x align-x offset-x)
            [sy0 sy1 :as yb] (segment-bounds h ptn-h pos-y align-y offset-y)
            at-ptn?
            (and (within-segment-bounds? xb w x)
                 (within-segment-bounds? yb h y))]
        (if at-ptn?
          (get-val (-> env (assoc :i (if (and (> sy0 sy1) (< y sy0))
                                       (+ y (- ptn-h (inc sy1)))
                                       (- y sy0))
                                  :j (if (and (> sx0 sx1) (< x sx0))
                                       (+ x (- ptn-w (inc sx1)))
                                       (- x sx0)))))
          v)))))

(defn- parse-bg [-opts bg]
  (cond
    ((set calc-core/nuim-code) bg) (->Ini-Constant -opts bg)
    (= :? bg) (->Ini-Random -opts)
    (ini-transducer? bg) bg
    :else (throw (ex-info "Invalid background ini." {:bg-ini bg}))))

(defini :figure [-opts bg pattern anchor]
  "Places a given `pattern` at the position specified by `anchor` before a given background ini.
- `bg`: a constant or ini that defines the background pattern of this ini
- `pattern`: either a (1d/2d) vector of figure values (constants, `:_` to fall back to `bg-ini` or `:?` for a random value) or a map that specifies the pattern implicitly with the following keys:
  - `w`/`h`: size of the pattern
  - `f`: function that takes a map of the current `:x`, `:y` coordinates and the background value `:v` (among other parameters) and must return the value at that coordinate
- `anchor`: map with the following (all optional) keys:
  - `pos`: can be an index (for x/y), a vector of indices `[x y]` or an alignment keyword (see `align`)
  - `align`: a keyword, e.g. `:left`, `:center` `:right`, `topleft`, …
  - `offset`: an integer of the pattern offset (number of cells from `pos`)"
  (make-gen [this w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts (comp (i/ini-xform2d bg-ini)
                                         (i/ini-xform2d this)) w h)))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [val-figure (make-val-figure1d (normalize-pattern pattern)
                                       (normalize-anchor anchor))]
     (map #(assoc % :v (val-figure %)))))
  (ini-xform2d
   [_]
   (let [val-figure (make-val-figure2d (normalize-pattern pattern)
                                       (normalize-anchor anchor))]
     (map #(assoc % :v (val-figure %))))))


(defini :rand-figure [-opts bg size anchor]
  "Generates a figure of given `size` with random constants at the position specified by `anchor` before a given background ini (see docs of `:figure` ini for further explanation). "
  (make-gen [this w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts (comp (i/ini-xform2d bg-ini)
                                         (i/ini-xform2d this)) w h)))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [w (cond
             (vector? size) (first size)
             (int? size) size
             :else 1)
         pattern {:w w :f val-random}]
     (i/ini-xform1d (->Ini-Figure -opts nil pattern anchor))))
  (ini-xform2d
   [_]
   (let [[w h] (cond
                 (vector? size) size
                 (int? size) [size size]
                 :else [1 1])
         pattern {:w w :h h :f val-random}]
     (i/ini-xform2d (->Ini-Figure -opts nil pattern anchor)))))


(defini :ball [-opts bg style anchor]
  "Generates a “ball” figure on top of a given background ini (see docs of `:figure` ini for further explanation)."
  (make-gen [this w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts (comp (i/ini-xform2d bg-ini)
                                         (i/ini-xform2d this)) w h)))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [pattern (case style
                   (:inverted :moore-inverted)
                   [:U :I :N :I :U]
                   [:I :U :M :U :I])]
     (i/ini-xform1d (->Ini-Figure -opts nil pattern anchor))))
  (ini-xform2d
   [_]
   (let [pattern (case style
                   :moore
                   [[:I :I :I :I :I]
                    [:I :U :U :U :I]
                    [:I :U :M :U :I]
                    [:I :U :U :U :I]
                    [:I :I :I :I :I]]
                   :moore-inverted
                   [[:U :U :U :U :U]
                    [:U :I :I :I :U]
                    [:U :I :N :I :U]
                    [:U :I :I :I :U]
                    [:U :U :U :U :U]]
                   :inverted
                   [[:_ :_ :U :_ :_]
                    [:_ :U :I :U :_]
                    [:U :I :N :I :U]
                    [:_ :U :I :U :_]
                    [:_ :_ :U :_ :_]]
                   [[:_ :_ :I :_ :_]
                    [:_ :I :U :I :_]
                    [:I :U :M :U :I]
                    [:_ :I :U :I :_]
                    [:_ :_ :I :_ :_]])]
     (i/ini-xform2d (->Ini-Figure -opts nil pattern anchor)))))

(defini :comp-figures [-opts bg figure-inis]
  "Takes a background ini and a sequence of `figure-inis` and composes them all together. Inis that appear later in the sequence may overwrite earlier ones when they overlap.
- `bg-ini`: another ini that defines the background pattern of this ini"
  (make-gen [this w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts (comp (i/ini-xform2d bg-ini)
                                         (i/ini-xform2d this)) w h)))

  i/IniTransducer
  (ini-xform1d [_] (apply comp
                          (map (comp i/ini-xform1d validate-ini-transducer)
                               figure-inis)))
  (ini-xform2d [_] (apply comp
                          (map (comp i/ini-xform2d validate-ini-transducer)
                               figure-inis))))

(defn calc-nth-pattern-offset
  [n w total-w gap align global-offset]
  (let [offset (+ (* n w) (* n gap))]
    (+ (case align
         :start offset
         :center (- offset (- (quot total-w 2)
                              (quot w 2)))
         :end (- offset))
       global-offset)))

(defini :figure-repeat [-opts bg pattern anchor copies spacing]
  "Repeats a given `pattern` n (`copies`) times over a given background ini with a specified `spacing` between each instance. The instances are positioned as specified by `anchor` (see docs of `:figure` ini for further explanation)."
  (make-gen [this w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini -opts (comp (i/ini-xform2d bg-ini)
                                         (i/ini-xform2d this)) w h)))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [{:keys [align-x offset-x] :as normal-anchor} (normalize-anchor anchor)
         nx (if (vector? copies) (first copies) copies)
         dx (if (vector? spacing) (first spacing) spacing)
         normal-pattern (normalize-pattern pattern)
         {ptn-w :1d} (get-pattern-dimensions normal-pattern)
         total-ptn-w (+ (* nx ptn-w) (* (dec nx) dx))
         fig-xforms
         (for [x (range nx)
               :let [offset-x' (calc-nth-pattern-offset
                                x ptn-w total-ptn-w dx align-x offset-x)]]
           (i/ini-xform1d
            (->Ini-Figure -opts nil normal-pattern
                          (assoc normal-anchor :offset-x offset-x'))))]
     (apply comp fig-xforms)))

  (ini-xform2d
   [_]
   (let [{:keys [align-x align-y
                 offset-x offset-y]
          :as normal-anchor} (normalize-anchor anchor)
         [nx ny] (if (vector? copies) copies [copies copies])
         [dx dy] (if (vector? spacing) spacing [spacing spacing])
         normal-pattern (normalize-pattern pattern)
         {[ptn-w ptn-h] :2d} (get-pattern-dimensions normal-pattern)
         total-ptn-w (+ (* nx ptn-w) (* (dec nx) dx))
         total-ptn-h (+ (* ny ptn-h) (* (dec ny) dy))
         fig-xforms
         (for [y (range ny)
               x (range nx)
               :let [offset-x' (calc-nth-pattern-offset
                                x ptn-w total-ptn-w dx align-x offset-x)
                     offset-y' (calc-nth-pattern-offset
                                y ptn-h total-ptn-h dy align-y offset-y)]]
           (i/ini-xform2d
            (->Ini-Figure -opts nil normal-pattern
                          (assoc normal-anchor
                                 :offset-x offset-x'
                                 :offset-y offset-y'))))]
     (apply comp fig-xforms))))

#_
(defini :figure-distribute [bg-ini pattern anchor quantity distribution]
  "Distributes a given `pattern` over a given background ini with the specified distribution parameters and quantity. The instances are positioned from the given `origin` coordinates with a given alignment."
  (make-gen [this w] (transduce-ini (comp (i/ini-xform bg-ini)
                                          (i/ini-xform this)) w))
  (make-gen [this w h] (transduce-ini (comp (i/ini-xform bg-ini)
                                            (i/ini-xform this)) w h))

  i/IniTransducer
  (ini-xform
   [_]
   (if (== (pattern-dimension pattern) 2)
     (let [{:keys [pos align] :or {pos [0 0] align [:top :left]}} anchor
           {:keys [density clustering]} distribution
           {:keys [min max]} quantity
           [orig-x orig-y] pos
           [w h] [(count (first pattern)) (count pattern)]
           fig-xforms (for [y (range ny)
                            x (range nx)
                            :let [pos [(+ orig-x (* x w) (* x dx))
                                       (+ orig-y (* y h) (* y dy))]]]
                        (i/ini-xform
                         (->Ini-Figure nil pattern pos [0 0])))]
       (apply comp fig-xforms))

     (let [{:keys [pos align] :or {pos 0 align :left}} anchor
           {:keys [density clustering]} distribution
           orig-x pos
           dx gap
           nx num
           w (count pattern)
           fig-xforms (for [x (range nx)
                            :let [pos (+ orig-x (* x w) (* x dx))]]
                        (i/ini-xform
                         (->Ini-Figure nil pattern pos 0)))]
       (apply comp fig-xforms)))))


(defn- wrap-bounds
  [n lower-bound upper-bound]
  (cond
    (> n upper-bound) lower-bound
    (< n lower-bound) upper-bound
    :else n))

(defumwelt :select-ltr [-opts size]
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

(defumwelt :self-select-ltr [-opts size]
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

(defumwelt :moore [-opts order self?]
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

(defumwelt :von-neumann [-opts order self?]
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

(defrule :match [-opts dna]
  "Matches an `umwelt` (of cell values) directly against the given `dna`, which is equivalent to interpreting and calculating a corresponding expression."
  (apply-rule
   [_ umwelt _]
   (match-dna dna umwelt))

  RuleOptimized
  (apply-rule--fast
   [_ umwelt-qtn _]
   (match-dna--fast dna umwelt-qtn)))

(defrule :life [-opts dna]
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


(defrecord CASpec [rule-spec umwelt-spec ini-spec])

#_
(comment
  (defspecies :selfi [-opts dna ini]
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


  (defspecies :mindform [-opts dna ini]
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

  (defspecies :lifeform [-opts dna]
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
                        :ini-spec    (->Ini-Random -opts)}
                       overwrites))
         {:constructor this}))))

  (defspecies :decisionform [-opts dna init-size]
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
                        :ini-spec    (->Ini-RandFigure -opts
                                                       (->Ini-Constant -opts :N)
                                                       init-size
                                                       {:pos :center
                                                        :align :center})}
                       overwrites))
         {:constructor this}))))

  ,)


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


;; ! check if multi-arity aset is slower than `(aset (aget arr i) j val)`
;;   -> see https://ask.clojure.org/index.php/729/aset-aget-perform-poorly-multi-dimensional-arrays-even-hints
;; ! check type hinting to avoid reflection and boxing
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
  [{:keys [rule-spec umwelt-spec ini-spec]} hist-cache-limit resolution]
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
  [{:keys [rule-spec umwelt-spec ini-spec]} resolution]
  (let [gen1 (apply sys-ini ini-spec resolution)
        [w h] resolution]
    (iterate (partial sys-next
                      resolution (range w) (when h (range h))
                      rule-spec umwelt-spec) gen1)))




(comment
  (def dna [:N :U :I :M :U :U :M :M :I :M :I :M :M :M :M :M :N :U :I :M :U :I :M :M :I :M :I :M :M :M :M :M :N :U :I :M :U :U :M :M :I :M :I :M :M :M :M :M :N :U :I :M :U :I :M :M :I :M :I :M :M :M :M :I])

  (def rule (->Rule-Life {} dna))

  (i/apply-rule rule (i/observe-umwelt (->Umwelt-Moore {} :column-first false)
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

(comment

  (require '[clojure.test.check.random :as random])

  (def rng (random/make-random 40))
  (random/rand-long ((random/split-n rng 3) 2))

  (random/rand-long rng)
  (random/rand-long (random/make-random (random/rand-long rng)))

  (let [[init-rng next-rng] (random/split rng)]
    [(random/rand-long init-rng)
     next-rng])

  (defn random-nums
    [rng]
    (lazy-seq
     (let [[rng1 rng2] (random/split rng)
           x (random/rand-long rng1)]
       (cons x (random-nums rng2)))))

  (take 10 (random-nums rng))

  (rng-select rng calc-core/nuim-code)

  ,)



(comment

  (i/make-gen (->Ini-Constant {} :U) 6)
  (i/make-gen (->Ini-Constant {} :I) 6 3)

  (i/make-gen (->Ini-Random {:seed nil}) 6)
  (i/make-gen (->Ini-Random {:seed 10}) 6 3)

  (i/make-gen (->Ini-Cycle {} [:M :U :I]) 6)
  (i/make-gen (->Ini-Cycle {} [:M :U :I]) 6 3)

  (let [f (make-val-cycle [:M :U :I])]
    (for [x (range 10)
          y (range 10)]
      (f {:x x :y y})))

  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :N)
                            [:M :_ :I] {:pos :center :align :center}) 6)
  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            {:w 2 :h 5 :f (fn [_] :I)}
                            {:offset [1 2]}) 6 6)
  (i/make-gen (->Ini-Figure {:seed 12}
                            (->Ini-Constant {} :_)
                            {:w 3 :h 3 :f val-random}
                            {:pos [1 1]}) 6 6)
  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            {:w 6 :f (make-val-cycle [:U :I :M])}
                            {:pos 3}) 12)
  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            {:w 3 :f val-random}
                            {}) 5)
  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            [[:M :U :I]
                             [:N :I :U]] {:pos :center :align :center}) 6 4)
  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            [:N :M :U]
                            {:pos :center :align :center})
              20)

  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            [:a :b :c :d :e :f :g]
                            {:pos 7})
              10)

  (i/make-gen (->Ini-Ball {} :N nil {:pos :center :align :center})
              7)
  ;; [:N :I :U :M :U :I :N]

  (i/make-gen (->Ini-Ball {}
                          (->Ini-Constant {} :M)
                          :inverted
                          {:pos :center :align :center})
              7)
  ;; [:M :U :I :N :I :U :M]
  
  (i/make-gen (->Ini-Ball {}
                          (->Ini-Constant {} :N)
                          nil
                          {:pos :center :align :center})
              7 7)
  ;; [[:N :N :N :N :N :N :N]
  ;;  [:N :N :N :I :N :N :N]
  ;;  [:N :N :I :U :I :N :N]
  ;;  [:N :I :U :M :U :I :N]
  ;;  [:N :N :I :U :I :N :N]
  ;;  [:N :N :N :I :N :N :N]
  ;;  [:N :N :N :N :N :N :N]]

  (i/make-gen (->Ini-Ball {}
                          (->Ini-Constant {} :N)
                          :moore
                          {:pos :center :align :center})
              7 7)
  ;; [[:N :N :N :N :N :N :N]
  ;;  [:N :I :I :I :I :I :N]
  ;;  [:N :I :U :U :U :I :N]
  ;;  [:N :I :U :M :U :I :N]
  ;;  [:N :I :U :U :U :I :N]
  ;;  [:N :I :I :I :I :I :N]
  ;;  [:N :N :N :N :N :N :N]]
  
  (i/make-gen (->Ini-RandFigure {}
                                (->Ini-Constant {} :_)
                                [3 2]
                                {:pos :center
                                 :align :center})
              9 5)
  ;; [[:_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :_ :_ :M :U :M :_ :_ :_]
  ;;  [:_ :_ :_ :U :N :U :_ :_ :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_]]

  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            {:w 3 :h 3 :f val-random}
                            {:pos :center
                             :align :center})
              9)
  ;; [:_ :_ :_ :I :N :I :_ :_ :_]

  (i/make-gen (->Ini-CompFigures {}
                                 (->Ini-Constant {} :_)
                                 [(->Ini-Figure {} nil [:U :I] {:pos 1})
                                  (->Ini-Ball {} nil nil {:pos 4})
                                  (->Ini-Figure {} nil [:I :N :U]
                                                {:pos :right :align :right})])
              15)
  ;; [:_ :U :I :_ :I :U :M :U :I :_ :_ :_ :I :N :U]

  (i/make-gen (->Ini-FigureRepeat {}
                                  (->Ini-Constant {} :_)
                                  [:M :U]
                                  {:pos 0 :align :left}
                                  3
                                  1)
              15)
  [:M :U :I :_ :M :U :I :_ :M :U]

  (i/make-gen (->Ini-FigureRepeat {}
                                  (->Ini-Constant {} :_)
                                  {:w 3 :h 3
                                   :f val-random}
                                  {:pos :center
                                   :align :center}
                                  3
                                  1)
              13 13)
  ;; [[:_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :U :I :U :_ :M :M :M :_ :N :N :N :_]
  ;;  [:_ :U :I :N :_ :U :M :M :_ :I :U :U :_]
  ;;  [:_ :I :N :M :_ :N :M :I :_ :N :I :M :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :M :N :N :_ :U :U :I :_ :I :I :U :_]
  ;;  [:_ :N :I :M :_ :I :N :U :_ :I :M :I :_]
  ;;  [:_ :M :U :N :_ :U :U :M :_ :I :N :U :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :M :N :N :_ :N :I :M :_ :I :M :N :_]
  ;;  [:_ :N :N :N :_ :I :U :U :_ :M :U :I :_]
  ;;  [:_ :U :M :N :_ :N :I :M :_ :M :I :I :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_]]
  
  
  (i/make-gen (->Ini-FigureRepeat {}
                                  (->Ini-Constant {} :_)
                                  [[:M :U]
                                   [:N :I]]
                                  {:pos [1 1] ;; or just 1 or :center, …
                                   :align :topleft}
                                  [3 3] ;; or just 3
                                  [1 1] ;; or just 1
                                  )
              10 10)
  ;; =>
  ;; [[:_ :_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :M :U :_ :M :U :_ :M :U :_]
  ;;  [:_ :N :I :_ :N :I :_ :N :I :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :M :U :_ :M :U :_ :M :U :_]
  ;;  [:_ :N :I :_ :N :I :_ :N :I :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_ :_]
  ;;  [:_ :M :U :_ :M :U :_ :M :U :_]
  ;;  [:_ :N :I :_ :N :I :_ :N :I :_]
  ;;  [:_ :_ :_ :_ :_ :_ :_ :_ :_ :_]]


  #_
  (i/make-gen (->Ini-FigureDistribute (->Ini-Constant :_)
                                      [[:M :U]
                                       [:N :I]]
                                      {:pos [1 1] :align :topleft}
                                      {:min 2 :max 8}
                                      {:density 0.7
                                       :clustering 0.3})
              10 10)

  ,)


(comment
  (let [opts [:start :center :end]]
    (for [align opts
          self-align opts]
      [[align self-align] (segment-bounds 9 4 align self-align 0)]))
  [[[:start :start] [0 3]]
   [[:start :center] [7 1]]
   [[:start :end] [5 8]]
   [[:center :start] [4 7]]
   [[:center :center] [2 5]]
   [[:center :end] [0 3]]
   [[:end :start] [0 3]]
   [[:end :center] [7 1]]
   [[:end :end] [5 8]]]
  
  ;; [0 1 2 3 4 5 6 7 8]
  ;;  | anchor (start)
  ;;                    | anchor (end)
  ;; [0 1 2 3]           ;; start/end start
  ;;  2 3]         [0 1  ;; start/end center
  ;;           [0 1 2 3] ;; start/end end
  
  ;; [0 1 2 3 4 5 6 7 8]
  ;;          | anchor
  ;;         [0 1 2 3]   ;; center start
  ;;     [0 1 2 3]       ;; center center
  ;; [0 1 2 3]           ;; center end

  
  (let [opts [:start :center :end]]
    (for [align opts
          self-align opts]
      [[align self-align] (segment-bounds 9 5 align self-align 0)]))
  [[[:start :start] [0 4]]
   [[:start :center] [7 2]]
   [[:start :end] [4 8]]
   [[:center :start] [4 8]]
   [[:center :center] [2 6]]
   [[:center :end] [8 3]]
   [[:end :start] [0 4]]
   [[:end :center] [7 2]]
   [[:end :end] [4 8]]]
  
  ;; [0 1 2 3 4 5 6 7 8]
  ;;  | anchor (start)
  ;;                    | anchor (end)
  ;; [0 1 2 3 4]         ;; start/end start
  ;;  2 3 4]       [0 1  ;; start/end center
  ;;         [0 1 2 3 4] ;; start/end end
  
  ;; [0 1 2 3 4 5 6 7 8]
  ;;          | anchor
  ;;         [0 1 2 3 4]   ;; center start
  ;;     [0 1 2 3 4]       ;; center center
  ;;  1 2 3 4]       [0    ;; center end

  
  (let [opts [:start :center :end]]
    (for [align opts
          self-align opts]
      [[align self-align] (segment-bounds 10 4 align self-align 0)]))
  [[[:start :start] [0 3]]
   [[:start :center] [8 1]]
   [[:start :end] [6 9]]
   [[:center :start] [5 8]]
   [[:center :center] [3 6]]
   [[:center :end] [1 4]]
   [[:end :start] [0 3]]
   [[:end :center] [8 1]]
   [[:end :end] [6 9]]]
  
  ;; [0 1 2 3 4 5 6 7 8 9]
  ;;  | anchor (start)
  ;;            | anchor (end)
  ;; [0 1 2 3]             ;; start/end start
  ;;  2 3]           [0 1  ;; start/end center
  ;;             [0 1 2 3] ;; start/end end

  ;; [0 1 2 3 4 5 6 7 8 9]
  ;;            | anchor
  ;;           [0 1 2 3]   ;; center start
  ;;       [0 1 2 3]       ;; center center
  ;;   [0 1 2 3]           ;; center end

  
  (let [opts [:start :center :end]]
    (for [align opts
          self-align opts]
      [[align self-align] (segment-bounds 3 2 align self-align 0)]))
  [[[:start :start] [0 1]]
   [[:start :center] [2 0]]
   [[:start :end] [1 2]]
   [[:center :start] [1 2]]
   [[:center :center] [0 1]]
   [[:center :end] [2 0]]
   [[:end :start] [0 1]]
   [[:end :center] [2 0]]
   [[:end :end] [1 2]]]

  ;; [0 1 2]
  ;;  | anchor (start)
  ;;        | anchor (end)
  ;; [0 1]   ;; start/end start
  ;;  1] [0  ;; start/end center
  ;;   [0 1] ;; start/end end

  ;; [0 1 2]
  ;;    | anchor
  ;;   [0 1] ;; center start
  ;; [0 1]   ;; center center
  ;;  1] [0  ;; center end

  
  (let [opts [:start :center :end]]
    (for [align opts
          self-align opts]
      [[align self-align] (segment-bounds 2 2 align self-align 0)]))
  [[[:start :start] [0 1]]
   [[:start :center] [1 0]]
   [[:start :end] [0 1]]
   [[:center :start] [1 0]]
   [[:center :center] [0 1]]
   [[:center :end] [1 0]]
   [[:end :start] [0 1]]
   [[:end :center] [1 0]]
   [[:end :end] [0 1]]]
  
  ;; [0 1]
  ;;  | anchor (start)
  ;;      | anchor (end)
  ;; [0 1] ;; start/end start
  ;;  1|0  ;; start/end center
  ;; [0 1] ;; start/end end

  ;; [0 1]
  ;;    | anchor
  ;;  1|0  ;; center start
  ;; [0 1] ;; center center
  ;;  1|0  ;; center end

  
  (let [opts [:start :center :end]]
    (for [align opts
          self-align opts]
      [[align self-align] (segment-bounds 2 1 align self-align 0)]))
  [[[:start :start] [0 0]]
   [[:start :center] [0 0]]
   [[:start :end] [1 1]]
   [[:center :start] [1 1]]
   [[:center :center] [1 1]]
   [[:center :end] [0 0]]
   [[:end :start] [0 0]]
   [[:end :center] [0 0]]
   [[:end :end] [1 1]]]
  
  ;; [0 1]
  ;;  | anchor (start)
  ;;      | anchor (end)
  ;; [0]   ;; start/end start
  ;; [0]   ;; start/end center
  ;;   [0] ;; start/end end

  ;; [0 1]
  ;;    | anchor
  ;;   [0] ;; center start
  ;;   [0] ;; center center
  ;; [0]   ;; center end

  
  (let [opts [:start :center :end]]
    (for [align opts
          self-align opts]
      (segment-bounds 1 1 align self-align 0)))
  [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]
  
  ;; [0 1 2 3 4 5 6 7 8]
  ;;          | anchor
  ;;  5]     [0 1 2 3 4  ;; center start
  ;;   [0 1 2 3 4 5]     ;; center center
  ;;  2 3 4 5]     [0 1  ;; center end

  

  ,)

(comment
  #_
  (defn- get-pattern-specs
    [pattern]
    (cond
      (vector? pattern) (let [dim (if (sequential? (first pattern)) 2 1)]
                          {:type :explicit
                           ;; :dim dim
                           :ptn-w (if (== dim 2)
                                    (count (first pattern))
                                    (count pattern))
                           :ptn-h (when (== dim 2) (count pattern))})
      (map? pattern) {:type :recipe
                      ;; :dim (if (:h pattern) 2 1)
                      :ptn-w (or (:w pattern) (:h pattern) 1)
                      :ptn-h (or (:h pattern) (:w pattern) 1)}
      :else (throw (ex-info "Unknown pattern type." {:pattern pattern}))))

  #_
  (defn- get-pattern-specs
    [pattern]
    (let [type (get-pattern-type pattern)]
      (case type
        :explicit (let [pattern (normalize-pattern pattern)]
                    {:type type
                     :pattern pattern
                     ;; ? pattern w/h might not always be same between 1d/2d
                     :ptn-w (count (:gen1d pattern))
                     :ptn-h (count (:gen2d pattern))})
        :recipe {:type type
                 :pattern (:f pattern)
                 :ptn-w (or (:w pattern) (:h pattern) 1)
                 :ptn-h (or (:h pattern) (:w pattern) 1)}
        (throw (ex-info "Unknown pattern type." {:pattern pattern})))))
  
  #_
  (defn make-val-figure [pattern pos align offset]
    (let [{:keys [type dim ptn-w ptn-h]} (get-pattern-specs pattern)
          get-val (case type
                    :explicit (if (== dim 1)
                                (fn [{i :i}] (get pattern i))
                                (fn [{i :i j :j}] (get-in pattern [i j])))
                    :recipe (:f pattern))
          pos (normalize-position pos)
          align (normalize-alignment align)]
      (case dim
        1 (let [pos-x (or (and (vector? pos) (first pos)) pos 0)
                align-x (or (first align) :start)
                offset-x (or offset 0)]
            (fn [{:keys [w x v] :as env}]
              (let [[s0 s1 :as bounds] (segment-bounds
                                        w ptn-w pos-x align-x offset-x)
                    at-ptn? (within-segment-bounds? bounds w x)]
                (if at-ptn?
                  (get-val (-> env (assoc :i (if (and (> s0 s1) (< x s0))
                                               (+ x (- ptn-w (inc s1)))
                                               (- x s0)))))
                  v))))

        2 (let [[pos-x pos-y] (or pos [0 0])
                [align-x align-y] (or align [:start :start])
                [offset-x offset-y] (or offset [0 0])]
            (fn [{:keys [w h x y v] :as env}]
              (let [[sx0 sx1 :as xbounds] (segment-bounds
                                           w ptn-w pos-x align-x offset-x)
                    [sy0 sy1 :as ybounds] (segment-bounds
                                           h ptn-h pos-y align-y offset-y)
                    at-ptn?
                    (and (within-segment-bounds? xbounds w x)
                         (within-segment-bounds? ybounds h y))]
                (if at-ptn?
                  (get-val (-> env (assoc :i (if (and (> sy0 sy1) (< y sy0))
                                               (+ y (- ptn-h (inc sy1)))
                                               (- y sy0))
                                          :j (if (and (> sx0 sx1) (< x sx0))
                                               (+ x (- ptn-w (inc sx1)))
                                               (- x sx0)))))
                  v)))))))


  ,)

