;; ========================================================================
;;     formform emulation module
;;     -- created 02/2025, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.emul.core
  (:require [formform.calc.core :as calc-core]
            #?(:clj  [formform.emul.interfaces :as i
                      :refer [defini defumwelt defrule
                              UmweltOptimized RuleOptimized]]
               :cljs [formform.emul.interfaces :as i
                      :refer [UmweltOptimized RuleOptimized]
                      :refer-macros [defini defumwelt defrule]])
            [formform.utils :as utils])
  ;; #?(:clj (:import [formform.emul.interfaces UmweltOptimized RuleOptimized]))
  )

#?(:clj (set! *warn-on-reflection* true))

(defn get-resolution-from-generation
  [gen]
  (loop [xs  gen
         res [(count gen)]]
    (let [x (first xs)]
      (if (vector? x)
        (recur x (conj res (count x)))
        res))))

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
           (utils/rng-split-n (utils/make-rng seed) w))))

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
           (utils/rng-split-n (utils/make-rng seed) (* w h))))))

(defn- ini-transducer?
  [ini]
  (and (record? ini) (satisfies? i/IniTransducer ini)))

(defn- validate-ini-transducer
  [ini]
  (if (ini-transducer? ini)
    ini
    (throw (ex-info "Ini must be a record and satisfy the `formform.emul.interfaces.IniTransducer` protocol." {:ini ini}))))

(defn merge-ini-opts
  [spec-opts gen-opts]
  ;; :seed should only be defined in gen-opts!
  (merge (dissoc spec-opts :seed) gen-opts))


(defini :constant [-opts const]
  "Fills a generation with a given constant (`:n`/`:m`/`:u`/`:i`)."
  (make-gen [this opts w]
            (transduce-ini (merge-ini-opts -opts opts) (i/ini-xform1d this) w))
  (make-gen [this opts w h]
            (transduce-ini (merge-ini-opts -opts opts) (i/ini-xform2d this) w h))

  i/IniTransducer
  (ini-xform1d [_] (map #(assoc % :v const)))
  (ini-xform2d [this] (i/ini-xform1d this)))


(defn val-random
  [rng weights]
  (calc-core/rand-const rng weights))

(defn val-decay
  [rng v decay]
  (if (< (utils/rng-rand-double rng) decay)
    :_
    v))

(defini :random [-opts]
  "Fills a generation with random values. The (first) `-opts` argument is a map where you can set the following optional parameter:
- `:weights` → specifies the relative probability of each of the four constants to be randomly chosen. Can be provided either as:
  - a sequence of 4 non-negative numbers (e.g. `[1 0 2 5]`) in n-u-i-m order
  - a map (e.g. `{:i 1 :u 2}`), where missing weights are 0
  - a single number in the interval [0.0, 1.0] that represents the ratio of `:u`/`:i`/`m` against `:n` (whose weight is 1 - x)

Note: a random seed is not part of the ini spec, but it can be set when calling `sys-ini`."
  (make-gen [this opts w]
            (transduce-ini (merge-ini-opts -opts opts) (i/ini-xform1d this) w))
  (make-gen [this opts w h]
            (transduce-ini (merge-ini-opts -opts opts) (i/ini-xform2d this) w h))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [normal-weights (when-let [w (:weights -opts)]
                          (calc-core/conform-nuim-weights w))]
     (map #(assoc % :v (val-random (:rng %) normal-weights)))))
  (ini-xform2d [this] (i/ini-xform1d this)))


(defn make-val-cycle [pattern]
  (fn [{:keys [y x]}]
    (pattern (mod (+ x (or y 0)) (count pattern)))))

(defini :cycle [-opts pattern]
  "Fills a generation repeatedly with the same value sequence."
  (make-gen [this opts w]
            (transduce-ini (merge-ini-opts -opts opts) (i/ini-xform1d this) w))
  (make-gen [this opts w h]
            (transduce-ini (merge-ini-opts -opts opts) (i/ini-xform2d this) w h))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [val-cycle (make-val-cycle pattern)]
     (map #(assoc % :v (val-cycle %)))))
  (ini-xform2d [this] (i/ini-xform1d this)))


(defn segment-bounds
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

(def align-keywords
  (set (keys align->normalized)))


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

(defn- normalize-anchor [anchor]
  (cond
    ;; absolute position/index in generation
    (or (int? anchor)
        (sequential? anchor))
    (merge (normalize-position  anchor)
           (normalize-alignment nil)
           (normalize-offset    nil))
    
    ;; relative gen. alignment = self-alignment
    (align-keywords anchor)
    (merge (normalize-position  anchor)
           (normalize-alignment anchor)
           (normalize-offset    nil))

    ;; specific position, alignment and offset
    (map? anchor)
    (if (and (:pos-x anchor)
             (:align-x anchor)
             (:offset-x anchor))
      anchor ;; already normalized
      (merge (normalize-position  (:pos anchor))
             (normalize-alignment (:align anchor))
             (normalize-offset    (:offset anchor))))

    :else
    (throw (ex-info "Invalid anchor type!" {:anchor anchor}))))

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
  [normal-pattern {:keys [weights decay]}]
  (let [normal-weights (when weights (calc-core/conform-nuim-weights weights))
        ptn-fn (case (get-pattern-type normal-pattern)
                 :explicit (fn [{:keys [i j] :as env}]
                             (assoc env :v
                                    (if j
                                      (get-in (:gen2d normal-pattern) [i j])
                                      (get (:gen1d normal-pattern) i))))
                 :recipe (:f normal-pattern))]
    ;; pattern function gets wrapped to resolve holes and random values
    (comp (fn [{:keys [v v-prev rng] :as env}]
            (case v
              :? (assoc env :v (val-random rng normal-weights))
              :_ (assoc env :v v-prev)
              env))
          (if (and decay (> decay 0.0))
            (fn [{:keys [v rng] :as env}]
              (let [rng' (-> rng utils/rng-split first)]
                (assoc env :v (val-decay rng' v decay))))
            identity)
          ptn-fn
          #(assoc % :v-prev (:v %)))))

(defn make-figure1d
  [normal-pattern normal-anchor opts]
  (let [{:keys [pos-x align-x offset-x]} normal-anchor
        {ptn-w :1d} (get-pattern-dimensions normal-pattern)
        update-val (make-pattern-fn normal-pattern opts)]
    (fn [{:keys [w x v] :as env}]
      (let [[s0 s1 :as xb] (segment-bounds w ptn-w pos-x align-x offset-x)
            at-ptn? (within-segment-bounds? xb w x)]
        (if at-ptn?
          (-> env
              (assoc :i (if (and (> s0 s1) (< x s0))
                          (+ x (- ptn-w (inc s1)))
                          (- x s0)))
              update-val)
          env)))))

(defn make-figure2d
  [normal-pattern normal-anchor opts]
  (let [{:keys [pos-x    pos-y
                align-x  align-y
                offset-x offset-y]} normal-anchor
        {[ptn-w ptn-h] :2d} (get-pattern-dimensions normal-pattern)
        update-val (make-pattern-fn normal-pattern opts)]
    (fn [{:keys [w h x y v] :as env}]
      (let [[sx0 sx1 :as xb] (segment-bounds w ptn-w pos-x align-x offset-x)
            [sy0 sy1 :as yb] (segment-bounds h ptn-h pos-y align-y offset-y)
            at-ptn?
            (and (within-segment-bounds? xb w x)
                 (within-segment-bounds? yb h y))]
        (if at-ptn?
          (-> env
              (assoc :i (if (and (> sy0 sy1) (< y sy0))
                          (+ y (- ptn-h (inc sy1)))
                          (- y sy0))
                     :j (if (and (> sx0 sx1) (< x sx0))
                          (+ x (- ptn-w (inc sx1)))
                          (- x sx0)))
              update-val)
          env)))))

(defn- parse-bg [-opts bg]
  (cond
    ((set calc-core/nuim-code) bg) (i/->rec ->Ini-Constant -opts bg)
    (= :? bg) (i/->rec ->Ini-Random -opts)
    (ini-transducer? bg) bg
    :else (throw (ex-info "Invalid background ini." {:bg-ini bg}))))

(defini :figure [-opts bg pattern anchor]
  "Places a given `pattern` at the position specified by `anchor` before a given background ini.
- `bg`: a constant or ini that defines the background pattern of this ini
- `pattern`: either a (1D/2D) vector of figure values (constants, `:_` to fall back to `bg-ini` or `:?` for a random value) or a map that specifies the pattern implicitly with the following keys:
  - `w`/`h`: size of the pattern
  - `f`: function that takes a map of the current `:x`, `:y` coordinates and the background value `:v` (among other parameters) and returns the same map, usually with a new/updated `:v` value (which can also be `:?` for a random value).
- `anchor`: one of the following:
  - absolute position: can be an index (x=y) or a vector of indices `[x y]`
  - relative position: a keyword, e.g. `:left`, `:center` `:right`, `topleft`, …
  - a map with (all optional) keys:
    - `pos`: same as absolute or relative position
    - `align`: alignment at `pos`, same keywords as in “relative position”
    - `offset`: integer (x=y) or int vector `[x y]` as number of cells from `pos`

Note: you can find some predefined patterns in `ini-patterns`."
  (make-gen [this opts w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this opts w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform2d bg-ini)
                                   (i/ini-xform2d this)) w h)))

  i/IniTransducer
  (ini-xform1d
   [_]
   (map (make-figure1d (normalize-pattern pattern)
                       (normalize-anchor anchor)
                       -opts)))
  (ini-xform2d
   [_]
   (map (make-figure2d (normalize-pattern pattern)
                       (normalize-anchor anchor)
                       -opts))))


;; ? add `:rounded-2d?` option for elliptical 2D patterns
(defini :rand-figure [-opts bg size anchor] ; [-opts bg size shape anchor]
  "Generates a figure of given `size` with random constants at the position specified by `anchor` before a given background ini (see docs of `:figure` ini for further explanation).
- `size` can be either a single integer (for 1D or square-sized 2D patterns) or a vector `[w h]` for 2D patterns.

The (first) `-opts` argument is a map where you can set the following optional parameters:
- `:decay` → specifies the “disintegration” of the pattern, i.e. the probability of “holes” (`:_`) to occur randomly in it (number in the interval `[0.0, 1.0]`, default: 0.0)
- `:weights` → specifies the relative probability of each of the four constants to be randomly chosen. Default is `0.5`. Can be provided either as:
  - a sequence of 4 non-negative numbers (e.g. `[1 0 2 5]`) in n-u-i-m order
  - a map (e.g. `{:i 1 :u 2}`), where missing weights are 0
  - a single number in the interval [0.0, 1.0] that represents the ratio of `:u`/`:i`/`m` against `:n` (whose weight is 1 - x)

Note: a random seed is not part of the ini spec, but it can be set when calling `sys-ini`."
  (make-gen [this opts w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this opts w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform2d bg-ini)
                                   (i/ini-xform2d this)) w h)))

  i/IniTransducer
  (ini-xform1d
   [_]
   (let [w (cond (vector? size) (first size)
                 (int? size) size
                 :else 1)
         pattern {:w w
                  :f #(assoc % :v :?)}]
     (i/ini-xform1d (i/->rec ->Ini-Figure -opts nil pattern anchor))))
  (ini-xform2d
   [_]
   (let [[w h] (cond (vector? size) size
                     (int? size) [size size]
                     :else [1 1])
         pattern {:w w :h h
                  :f #(assoc % :v :?)}]
     (i/ini-xform2d (i/->rec ->Ini-Figure -opts nil pattern anchor)))))


(defini :comp-figures [-opts bg figure-inis]
  "Takes a background ini and a sequence of `figure-inis` and composes them all together. Inis that appear later in the sequence may overwrite earlier ones when they overlap.
- `bg-ini`: another ini that defines the background pattern of this ini"
  (make-gen [this opts w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this opts w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform2d bg-ini)
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
  (make-gen [this opts w]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform1d bg-ini)
                                   (i/ini-xform1d this)) w)))
  (make-gen [this opts w h]
            (let [bg-ini (parse-bg -opts bg)]
              (transduce-ini (merge-ini-opts -opts opts)
                             (comp (i/ini-xform2d bg-ini)
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
            (i/->rec ->Ini-Figure -opts nil normal-pattern
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
            (i/->rec ->Ini-Figure -opts nil normal-pattern
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

(defumwelt :select-ltr [-opts ^long size]
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
   (let [g (^long fn [^long dx]
            (calc-core/const->digit
             (aget ^"[Lclojure.lang.Keyword;" gen1d-arr
                   (wrap-bounds (+ x dx) 0 (dec w)))))]
     (case size
       0 ""
       1 (str (g  0))
       2 (str (g -1) (g  1))
       3 (str (g -1) (g  0) (g  1))
       4 (str (g -2) (g -1) (g  1) (g  2))
       5 (str (g -2) (g -1) (g  0) (g  1) (g  2))
       (throw (ex-info "Unsupported neighbourhood size" {:size size}))))))

(defumwelt :self-select-ltr [-opts ^long size]
  "In a 2d environment, the cell “chooses” the direction in which it will observe its neighborhood of given `size` (like in `:select-ltr`). The cell’s choice is determined by its own value:
- `:n`: ← (left)
- `:m`: → (right)
- `:u`: ↓ (down)
- `:i`: ↑ (up)"
  (observe-umwelt
   [_ gen2d [[x y] self-v] w h]
   (let [g (fn [dx dy]
             (get-in gen2d ;; -2 fps
                     [(wrap-bounds (+ y dy) 0 (dec h))
                      (wrap-bounds (+ x dx) 0 (dec w))]))]
     (case size
       0 []
       1 (case self-v
           :n [(g -1  0)] ;; ←
           :u [(g  0  1)] ;; ↓
           :i [(g  0 -1)] ;; ↑
           :m [(g  1  0)] ;; →
           )
       2 (case self-v
           :n [(g -1  1) (g -1 -1)] ;; ←
           :u [(g  1  1) (g -1  1)] ;; ↓
           :i [(g -1 -1) (g  1 -1)] ;; ↑
           :m [(g  1 -1) (g  1  1)] ;; →
           )
       3 (case self-v
           :n [(g -1  1) (g -1  0) (g -1 -1)] ;; ← (bottom -> top)
           :u [(g  1  1) (g  0  1) (g -1  1)] ;; ↓ (right -> left)
           :i [(g -1 -1) (g  0 -1) (g  1 -1)] ;; ↑ (left -> right)
           :m [(g  1 -1) (g  1  0) (g  1  1)] ;; → (top -> bottom)
           )
       4 (case self-v
           :n [(g -1  2) (g -1  1) (g -1 -1) (g -1 -2)] ;; ←
           :u [(g  2  1) (g  1  1) (g -1  1) (g -2  1)] ;; ↓
           :i [(g -2 -1) (g -1 -1) (g  1 -1) (g  2 -1)] ;; ↑
           :m [(g  1 -2) (g  1 -1) (g  1  1) (g  1  2)] ;; →
           )
       5 (case self-v
           :n [(g -1  2) (g -1  1) (g -1  0) (g -1 -1) (g -1 -2)] ;; ←
           :u [(g  2  1) (g  1  1) (g  0  1) (g -1  1) (g -2  1)] ;; ↓
           :i [(g -2 -1) (g -1 -1) (g  0 -1) (g  1 -1) (g  2 -1)] ;; ↑
           :m [(g  1 -2) (g  1 -1) (g  1  0) (g  1  1) (g  1  2)] ;; →
           )
       (throw (ex-info "Unsupported neighbourhood size" {:size size})))))

  UmweltOptimized
  (observe-umwelt--fast
   [_ gen2d-arr [[x y] self-v] w h]
   (let [g (^long fn [^long dx ^long dy]
            (calc-core/const->digit
             (aget ^"[Lclojure.lang.Keyword;"
                   (aget ^"[[Lclojure.lang.Keyword;" gen2d-arr
                         (wrap-bounds (+ y dy) 0 (dec h)))
                   (wrap-bounds (+ x dx) 0 (dec w)))))]
     (case size
       0 ""
       1 (case self-v
           :n (str (g -1  0)) ;; ←
           :u (str (g  0  1)) ;; ↓
           :i (str (g  0 -1)) ;; ↑
           :m (str (g  1  0)) ;; →
           )
       2 (case self-v
           :n (str (g -1  1) (g -1 -1)) ;; ←
           :u (str (g  1  1) (g -1  1)) ;; ↓
           :i (str (g -1 -1) (g  1 -1)) ;; ↑
           :m (str (g  1 -1) (g  1  1)) ;; →
           )
       3 (case self-v
           :n (str (g -1  1) (g -1  0) (g -1 -1)) ;; ← (bottom -> top)
           :u (str (g  1  1) (g  0  1) (g -1  1)) ;; ↓ (right -> left)
           :i (str (g -1 -1) (g  0 -1) (g  1 -1)) ;; ↑ (left -> right)
           :m (str (g  1 -1) (g  1  0) (g  1  1)) ;; → (top -> bottom)
           )
       4 (case self-v
           :n (str (g -1  2) (g -1  1) (g -1 -1) (g -1 -2)) ;; ←
           :u (str (g  2  1) (g  1  1) (g -1  1) (g -2  1)) ;; ↓
           :i (str (g -2 -1) (g -1 -1) (g  1 -1) (g  2 -1)) ;; ↑
           :m (str (g  1 -2) (g  1 -1) (g  1  1) (g  1  2)) ;; →
           )
       5 (case self-v
           :n (str (g -1  2) (g -1  1) (g -1  0) (g -1 -1) (g -1 -2)) ;; ←
           :u (str (g  2  1) (g  1  1) (g  0  1) (g -1  1) (g -2  1)) ;; ↓
           :i (str (g -2 -1) (g -1 -1) (g  0 -1) (g  1 -1) (g  2 -1)) ;; ↑
           :m (str (g  1 -2) (g  1 -1) (g  1  0) (g  1  1) (g  1  2)) ;; →
           )
       (throw (ex-info "Unsupported neighbourhood size" {:size size}))))))

(defumwelt :moore [-opts order self?]
  "In a 2d environment, observes the cell’s direct neighborhood, made up of 8 or 9 (given `self?` is true) cells (corner cells included)."
  (observe-umwelt
   [_ gen2d [[x y] _] w h]
   (let [g (fn [dx dy]
             (get-in gen2d
                     [(wrap-bounds (+ y dy) 0 (dec h))
                      (wrap-bounds (+ x dx) 0 (dec w))]))]
     ;; `self` can be nil! (remove when counting)
     (case order
       :column-first
       ;; 0 3 5   0 3 6
       ;; 1 . 6   1 4 7
       ;; 2 4 7   2 5 8
       (if self?
         [(g -1 -1) (g -1  0) (g -1  1)
          (g  0 -1) (g  0  0) (g  0  1)
          (g  1 -1) (g  1  0) (g  1  1)]

         [(g -1 -1) (g -1  0) (g -1  1)
          (g  0 -1)           (g  0  1)
          (g  1 -1) (g  1  0) (g  1  1)])
       :row-first
       ;; 0 1 2   0 1 2
       ;; 3 . 4   3 4 5
       ;; 5 6 7   6 7 8
       (if self?
         [(g -1 -1) (g  0 -1) (g  1 -1)
          (g -1  0) (g  0  0) (g  1  0)
          (g -1  1) (g  0  1) (g  1  1)]

         [(g -1 -1) (g  0 -1) (g  1 -1)
          (g -1  0)           (g  1  0)
          (g -1  1) (g  0  1) (g  1  1)])
       (throw (ex-info "Invalid order for umwelt `:moore`."
                       {:order order})))))

  UmweltOptimized
  (observe-umwelt--fast
   [_ gen2d-arr [[x y] _] w h]
   (let [g (^long fn [^long dx ^long dy]
            (let [^long c (calc-core/const->digit
                           (aget ^"[Lclojure.lang.Keyword;"
                                 (aget ^"[[Lclojure.lang.Keyword;" gen2d-arr
                                       (wrap-bounds (+ y dy) 0 (dec h)))
                                 (wrap-bounds (+ x dx) 0 (dec w))))]
              (if (== c 0) nil c)))
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
                      (wrap-bounds (+ x dx) 0 (dec w))]))]
     ;; `self` can be nil! (remove when counting)
     (case order
       :column-first
       ;;   1       1
       ;; 0 . 3   0 2 4
       ;;   2       3
       (if self?
         [,         (g -1  0)
          (g  0 -1) (g  0  0) (g  0  1)
          ,         (g  1  0)]

         [,         (g -1  0)
          (g  0 -1)           (g  0  1)
          ,         (g  1  0)])
       :row-first
       ;;   0       0
       ;; 1 . 2   1 2 3
       ;;   3       4
       (if self?
         [          (g  0 -1)          
          (g -1  0) (g  0  0) (g  1  0)
          ,         (g  0  1)          ]

         [          (g  0 -1)          
          (g -1  0)           (g  1  0)
          ,         (g  0  1)          ])
       (throw (ex-info "Invalid order for umwelt `:von-neumann`."
                       {:order order})))))

  UmweltOptimized
  (observe-umwelt--fast
   [_ gen2d-arr [[x y] _] w h]
   (let [g (^long fn [^long dx ^long dy]
            (let [^long c (calc-core/const->digit
                           (aget ^"[Lclojure.lang.Keyword;"
                                 (aget ^"[[Lclojure.lang.Keyword;" gen2d-arr
                                       (wrap-bounds (+ y dy) 0 (dec h)))
                                 (wrap-bounds (+ x dx) 0 (dec w))))]
              (if (== c 0) nil c)))
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
    (let [qtn (apply str (mapv calc-core/const_->digit umwelt))
          idx (utils/parse-int qtn 4)]
      (dna idx))))

(defn match-dna--fast
  [dna umwelt-qtn]
  (if (== (count dna) 1)
    (dna 0)
    (dna (utils/parse-int umwelt-qtn 4))))

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
- a cell is “alive” when its value is not `:n`
- if the cell has 2 neighbors, it keeps its own value
- if the cell has 3 neighbors, it matches their values against the given `dna` (see `:match` rule)
- in every other case, the cell “dies” (turns to `:n`)"
  (apply-rule
   [_ umwelt self-v]
   (let [alive (vec (remove #(or (= :n %) (nil? %)) umwelt))]
     (case (count alive)
       2 self-v
       3 (match-dna dna alive)
       :n)))

  RuleOptimized
  (apply-rule--fast
   [_ umwelt-qtn self-v]
   (case (count umwelt-qtn)
     2 self-v
     3 (match-dna--fast dna umwelt-qtn)
     :n)))


(defrecord CASpec [rule-spec umwelt-spec ini-spec])

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


;; multi-arity `aset` might be slower than `(aset (aget arr i) j val)`
;; -> see https://ask.clojure.org/index.php/729/aset-aget-perform-poorly-multi-dimensional-arrays-even-hints
(defn sys-next--fast
  "More performant version of `sys-next` that uses native platform arrays instead of vectors for the generations and calls special `--fast` methods that operate on them. Note that these methods must be implemented for the provided `rule-spec` and `umwelt-spec`.

  Always prefer `sys-next` for better compatibility across library functions and maximal flexibility."
  ([w h rule-spec umwelt-spec gen]
   (let [^clojure.lang.Keyword
         compute (fn [^long x ^long y]
                   (let [[_ v :as cell]
                         [[x y] (aget ^"[Lclojure.lang.Keyword;"
                                      (aget ^"[[Lclojure.lang.Keyword;" gen y)
                                      x)]
                         qtn (i/observe-umwelt--fast umwelt-spec gen cell w h)]
                     (i/apply-rule--fast rule-spec qtn v)))]
     #?(:clj (let [^"[[Lclojure.lang.Keyword;"
                   next-gen (make-array clojure.lang.Keyword h w)]
               (loop [y 0]
                 (when (< y h)
                   (loop [x 0]
                     (when (< x w)
                       (aset ^"[Lclojure.lang.Keyword;"
                             (aget next-gen y) x (compute x y))
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
   (let [^clojure.lang.Keyword
         compute (fn [^long x]
                   (let [[_ v :as cell]
                         [[x] (aget ^"[Lclojure.lang.Keyword;" gen x)]
                         qtn (i/observe-umwelt--fast umwelt-spec gen cell w)]
                     (i/apply-rule--fast rule-spec qtn v)))]
     #?(:clj (let [^"[Lclojure.lang.Keyword;"
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
  ;; (get-seed [_]
  ;;   seed)
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
  [{:keys [rule-spec umwelt-spec ini-spec]} opts resolution]
  (let [hist-cache-limit (:hist-cache-limit opts)
        optimized? (and (satisfies? UmweltOptimized umwelt-spec)
                        (satisfies? RuleOptimized rule-spec))
        [w h] resolution
        gen1 (let [v (apply i/make-gen ini-spec opts resolution)]
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
  [{:keys [rule-spec umwelt-spec ini-spec]} opts resolution]
  (let [gen1 (apply i/make-gen ini-spec opts resolution)
        [w h] resolution]
    (iterate (partial sys-next
                      resolution (range w) (when h (range h))
                      rule-spec umwelt-spec) gen1)))




(comment
  (def dna [:n :u :i :m :u :u :m :m :i :m :i :m :m :m :m :m :n :u :i :m :u :i :m :m :i :m :i :m :m :m :m :m :n :u :i :m :u :u :m :m :i :m :i :m :m :m :m :m :n :u :i :m :u :i :m :m :i :m :i :m :m :m :m :i])

  (def rule (i/->rec ->Rule-Life {} dna))


  (require '[clojure.math.combinatorics :as combo])

  (calc-core/filter-dna dna [:u :m :i])
  
  (filter
   (fn [[p [v]]] (= v :m))
   (map
    (fn [p] [p (calc-core/filter-dna dna (vec p))])
    (combo/permutations [:u :m :i])))

  ,)

(comment
  [[:n :n :u]
   [:m :_ :n] ;; self: :n
   [:n :n :i]]
  ;;=> :u (should be :m)
  ;; solutions:
  ;; [:m :u :i] ← ↗ ↘
  ;; [:m :i :u] ← ↘ ↗

  [[:n :u :n]
   [:i :_ :n] ;; self: :m
   [:n :u :n]]
  ;;=> :m (should be :i)
  ;; solutions:
  ;; [:i :u :u] ← ↓ ↑ / ← ↑ ↓

  [[:n :i :n]
   [:n :_ :n] ;; self: :n
   [:m :m :n]]
  ;;=> :n (should be :u)
  ;; solutions:
  ;; [:m :i :m] ↙ ↑ ↓ / ↓ ↑ ↙
  ;; [:m :m :i] ↙ ↓ ↑ / ↓ ↙ ↑

  [[:n :n :n]
   [:n :_ :i] ;; self: :n
   [:u :n :m]]
  ;;=> :i (should be :u)
  ;; solutions:
  ;; [:u :i :m] ↙ → ↘
  ;; [:u :m :i] ↙ ↘ →

  [[:n :n :n]
   [:n :_ :u] ;; self: :m
   [:i :i :n]]
  ;;=> :u (should be :m)
  ;; solutions:
  ;; [:i :u :i] ↙ → ↓ / ↓ → ↙
  ;; [:i :i :u] ↙ ↓ → / ↓ ↙ →

  [[:n :n :n]
   [:n :_ :m] ;; self: :n
   [:i :m :n]]
  ;;=> :u (should be :n)
  ;; solutions:
  ;; [:i :m :m] ↙ ↓ → / ↙ → ↓

  [[:n :n :u]
   [:i :_ :m] ;; self: :n
   [:n :n :n]]
  ;;=> :u (should be :i)
  ;; solutions:
  ;; [:i :u :m] ← ↗ →
  ;; [:i :m :u] ← → ↗

  ,)


(comment

  (i/make-gen (->Ini-Figure {} (->Ini-Constant {} :_)
                            {:w 2 :h 5 :f #(assoc % :v :i)}
                            {:offset [1 2]}) {} 6 6)
  (i/make-gen (->Ini-Figure {} :n {:w 2 :f #(assoc % :v :?)}
                            :center) {} 10)
  (i/make-gen (->Ini-Figure {:seed 12}
                            (->Ini-Constant {} :_)
                            {:w 3 :h 3 :f val-random}
                            {:pos [1 1]}) {} 6 6)
  (i/make-gen (->Ini-Figure {}
                            (->Ini-Constant {} :_)
                            {:w 6 :f (make-val-cycle [:u :i :m])}
                            {:pos 3}) {} 12)

  
  #_
  (i/make-gen (->Ini-FigureDistribute (->Ini-Constant :_)
                                      [[:m :u]
                                       [:n :i]]
                                      {:pos [1 1] :align :topleft}
                                      {:min 2 :max 8}
                                      {:density    0.7
                                       :clustering 0.3})
              10 10)

  ,)


