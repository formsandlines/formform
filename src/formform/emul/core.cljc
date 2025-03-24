;; ========================================================================
;;     formform emulation module
;;     -- created 02/2025, (c) Peter Hofmann
;; ========================================================================

(ns ^:no-doc formform.emul.core
  (:require [formform.calc :as calc]
            ;; [formform.expr :as expr]
            #?(:clj  [formform.emul.interfaces :as i
                      :refer [defini defumwelt defrule defspecies]]
               :cljs [formform.emul.interfaces :as i
                      :refer-macros [defini defumwelt defrule defspecies]])
            ;; [formform.utils :as utils]
            ))

(defn- val-or-rand [v]
  (case v
    :rand (calc/rand-const)
    v))

(defn get-resolution-from-generation
  [gen]
  (loop [xs  gen
         res [(count gen)]]
    (let [x (first xs)]
      (if (vector? x)
        (recur x (conj res (count x)))
        res))))

(defini :fill-all [bg]
  "Fills a generation with a given `bg` value.
- `bg`: <const|:rand>"
  (make-gen
   [_ w]
   (vec (repeatedly w (partial val-or-rand bg))))

  (make-gen
   [_ w h]
   (vec (repeatedly h #(vec (repeatedly w (partial val-or-rand bg)))))))

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
             (get-in gen2d
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
       (throw (ex-info "Unsupported neighbourhood size" {:size size}))))))

(defumwelt :moore [self?]
  "In a 2d environment, observes the cell’s direct neighborhood, made up of 8 or 9 (given `self?` is true) cells (corner cells included)."
  (observe-umwelt
   [_ gen2d [[x y] _] w h]
   (let [g (fn [dx dy]
             (get-in gen2d
                     [(wrap-bounds (+ y dy) 0 (dec h))
                      (wrap-bounds (+ x dx) 0 (dec w))]))
         self (if self? (g 0 0) nil)]
     ;; `self` can be nil! (remove when counting)
     [(g -1 -1) (g  0 -1) (g  1 -1)
      (g -1  0)   self    (g  1  0)
      (g -1  1) (g  0  1) (g  1  1)])))

(defumwelt :von-neumann [self?]
  "In a 2d environment, observes the cell’s direct neighborhood, made up of 4 or 5 (given `self?` is true) cells (corner cells not included)."
  (observe-umwelt
   [_ gen2d [[x y] _] w h]
   (let [g (fn [dx dy]
             (get-in gen2d
                     [(wrap-bounds (+ y dy) 0 (dec h))
                      (wrap-bounds (+ x dx) 0 (dec w))]))
         self (if self? (g 0 0) nil)]
     ;; `self` can be nil! (remove when counting)
     [          (g  0 -1)          
      (g -1  0)   self    (g  1  0)
      ,         (g  0  1)          ])))


(defrule :match [dna]
  "Matches an `umwelt` (of cell values) directly against the given `dna`, which is equivalent to interpreting and calculating a corresponding expression."
  (apply-rule
   [_ umwelt _]
   (calc/dna-get dna umwelt)))

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
       3 (calc/dna-get dna alive)
       :N))))


(def sys-ini i/make-gen)

(defn sys-next
  [[w h :as res] rule-spec umwelt-spec gen]
  (if h
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [[_ v :as cell] [[x y] (get-in gen [y x])]
                          env (apply i/observe-umwelt umwelt-spec gen cell res)
                          next-v (i/apply-rule rule-spec env v)]
                      next-v))
                  (range w)))
          (range h))
    (mapv (fn [x]
            (let [[_ v :as cell] [[x] (get gen x)]
                  env (apply i/observe-umwelt umwelt-spec gen cell res)
                  next-v (i/apply-rule rule-spec env v)]
              next-v))
          (range w))))


(defrecord CASpec [resolution rule-spec umwelt-spec ini-spec])

(defspecies :selfi [dna ini]
  "1D cellular automaton. Takes a `dna` for its rule function (type `:match`) and an `ini` type (via `make-ini`). Its ‘umwelt’ is of type `:select-ltr`."
  (specify-ca
   [this w]
   (let [umwelt-size (calc/dna-dimension dna)]
     (with-meta
       (map->CASpec {:label       "SelFi"
                     :resolution  [w]
                     :rule-spec   (->Rule-Match dna)
                     :umwelt-spec (->Umwelt-SelectLtr umwelt-size)
                     :ini-spec    ini})
       {:constructor this}))))


(defspecies :mindform [dna ini]
  "2D cellular automaton. Takes a `dna` for its rule function (type `:match`) and an `ini` type (via `make-ini`). Its ‘umwelt’ is of type `:self-select-ltr`."
  (specify-ca
   [this w h]
   (let [umwelt-size (calc/dna-dimension dna)]
     (with-meta
       (map->CASpec {:label       "MindFORM"
                     :resolution  [w h]
                     :rule-spec   (->Rule-Match dna)
                     :umwelt-spec (->Umwelt-SelfSelectLtr umwelt-size)
                     :ini-spec    ini})
       {:constructor this}))))

(defspecies :lifeform [dna]
  "2D cellular automaton. Takes a `dna` as part of its rule function, which is of type `:life`. Its ‘umwelt’ is of type `:moore`."
  (specify-ca
   [this w h]
   (with-meta
     (map->CASpec {:label       "LifeFORM"
                   :resolution  [w h]
                   :rule-spec   (->Rule-Life dna)
                   :umwelt-spec (->Umwelt-Moore false)
                   :ini-spec    (->Ini-Random)})
     {:constructor this})))

(defspecies :decisionform [dna init-size]
  "2D cellular automaton. Takes a `dna` as part of its rule function, which is of type `:life`, and an initial size for its `:rand-center` type ini. Its ‘umwelt’ is of type `:moore`."
  (specify-ca
   [this w h]
   (with-meta
     (map->CASpec {:label       "DecisionFORM"
                   :resolution  [w h]
                   :rule-spec   (->Rule-Life dna)
                   :umwelt-spec (->Umwelt-Moore false)
                   :ini-spec    (->Ini-RandCenter init-size)})
     {:constructor this})))


(defn ca-iterator
  [{:keys [resolution rule-spec umwelt-spec ini-spec]}]
  (let [gen1 (apply sys-ini ini-spec resolution)]
    (iterate (partial sys-next resolution rule-spec umwelt-spec) gen1)))


(deftype CellularAutomaton
    #?(:cljs [res init-evolution next-gen ^:mutable evolution]
       :clj  [res init-evolution next-gen ^:unsynchronized-mutable evolution])
  i/CASystem
  (step [_]
    (set! evolution
          (conj! evolution (next-gen
                            (nth evolution (dec (count evolution)))))))
  (restart [_]
    (set! evolution (transient init-evolution)))
  (get-resolution [_]
    res)
  (get-evolution [_]
    (let [^clojure.lang.PersistentVector v (persistent! evolution)]
      (set! evolution (transient v))
      v)))

;; because direct method access for deftype JS objects is weird
;; remember to turn dashes into underscores for method names!
#?(:cljs (extend-type CellularAutomaton
           Object
           (step [_this] (i/step _this))
           (restart [_this] (i/restart _this))
           (get_resolution [_this] (i/get-resolution _this))
           (get_evolution [_this] (i/get-evolution _this))))

(defn create-ca
  [{:keys [resolution rule-spec umwelt-spec ini-spec]}]
  (let [gen1 (apply sys-ini ini-spec resolution)
        next-gen (partial sys-next resolution rule-spec umwelt-spec)
        evolution [gen1]]
    (->CellularAutomaton resolution evolution next-gen (transient evolution))))



#_
(comment
  (defn make-automaton
    [ca]
    (let [res (get-resolution-from-generation (first ca))
          ca-state (atom (rest ca))
          evolution (atom [(first ca)])
          step! (fn []
                  (swap! ca-state rest)
                  (swap! evolution conj (first @ca-state)))
          reset! (fn []
                   (reset! ca-state (rest ca))
                   (reset! evolution [(first ca)]))
          get-resolution res
          get-evolution (fn [] @evolution)]
      (defrecord )))

  (defn automaton
    [ca res]
    (let [ca-state (atom (rest ca))
          evolution (atom [(first ca)])
          ;; _ (reset! ca-state (rest ca))
          ;; _ (reset! evolution [(first ca)])
          step! (fn []
                  (swap! ca-state rest)
                  (swap! evolution conj (first @ca-state)))
          reset! (fn []
                   (reset! ca-state (rest ca))
                   (reset! evolution [(first ca)]))
          get-evolution (fn [] @evolution)
          get-generation (fn [] (first @evolution))]
      {:step! step!
       :reset! reset!
       :get-resolution res
       :get-evolution get-evolution
       :get-generation get-generation})))

#_
(comment
  (defrecord CellularAutomaton [res init !iterator !evolution]
    ICellularAutomaton
    (step! [this]
      (swap! !iterator rest)
      (swap! !evolution conj (first @!iterator)))
    (restart! [this]
      (reset! !iterator (rest init))
      (reset! !evolution [(first init)]))
    (get-resolution [this]
      res)
    (get-evolution [this]
      @!evolution)
    (toString [this]
      "test"))

  ;; (defmethod print-method CellularAutomaton [record ^java.io.Writer writer]
  ;;   (.write writer "#MyRecord{...}"))

  ;; Platform-specific print method implementations
  (let [ca->str (fn [ca]
                  (let [res (str ":res " (:res ca))
                        init (str ":init #clojure.lang.Iterate(" (first (:init ca)) " …)")]
                    (str "#CellularAutomaton{" res ", " init "}")))]
    #?(:clj
       (defmethod print-method CellularAutomaton [record ^java.io.Writer writer]
         (.write writer (ca->str record))))
    #?(:cljs
       (extend-protocol IPrintWithWriter
         CellularAutomaton
         (-pr-writer [record writer _opts]
           (write-all writer (ca->str record))))))

  (defn automaton!
    [generator]
    (let [gen1 (first generator)
          res (get-resolution-from-generation gen1)
          !iterator (atom (rest generator))
          !evolution (atom [gen1])]
      (->CellularAutomaton res generator !iterator !evolution))))


#_
(comment
  (defprotocol ICellularAutomaton
    (step! [this] "Advances the automaton by one generation.")
    (restart! [this] "Resets the automaton to its initial state.")
    (get-resolution [this] "Returns the resolution of the automaton.")
    (get-evolution [this] "Returns an immutable copy of the current evolution."))

  (defn create-ca
    [initial-state]
    (let [gen1 (first initial-state)
          res (get-resolution-from-generation gen1)
          !iterator (atom (rest initial-state))
          !evolution (atom [gen1])]
      (reify ICellularAutomaton
        (step! [_]
          (swap! !iterator rest)
          (swap! !evolution conj (first @!iterator)))
        (restart! [_]
          (reset! !iterator (rest initial-state))
          (reset! !evolution [(first initial-state)]))
        (get-resolution [_]
          res)
        (get-evolution [_]
          @!evolution)))))

#_
(comment

  (defprotocol ICellularAutomaton
    (step! [this] "Advances the automaton by one generation.")
    (restart! [this] "Resets the automaton to its initial state.")
    (get-resolution [this] "Returns the resolution of the automaton.")
    (get-evolution [this] "Returns an immutable copy of the current evolution."))

  (defrecord CellularAutomaton [res initial-state !iterator !evolution]
    ICellularAutomaton
    (step! [_]
      (swap! !iterator rest)
      (swap! !evolution conj (first @!iterator)))
    (restart! [_]
      (reset! !iterator (rest initial-state))
      (reset! !evolution [(first initial-state)]))
    (get-resolution [_]
      res)
    (get-evolution [_]
      @!evolution))

  (defmulti execute-command
    "Execute a command on a CellularAutomaton."
    (fn [ca command & args] command))

  (defmethod execute-command :step
    [ca _]
    (step! ca))

  (defmethod execute-command :restart
    [ca _]
    (restart! ca))

  (defmethod execute-command :get-resolution
    [ca _]
    (get-resolution ca))

  (defmethod execute-command :get-evolution
    [ca _]
    (get-evolution ca))

  #?(:clj (extend-type CellularAutomaton
            clojure.lang.IFn
            (invoke [this command & args]
              (apply execute-command this command args)))
     :cljs (extend-type CellularAutomaton
             cljs.core.IFn
             (invoke [this command & args]
               (apply execute-command this command args))))

  )

#_
(comment
  (defprotocol PAutomaton
    (make-rule [this args])
    (make-umwelt [this args])
    (make-ini [this args]))

  (defrecord Automaton [type rule umwelt ini]
    PAutomaton
    (make-rule [_ [dna]]
      [rule dna])
    (make-umwelt [_ [dna]]
      [umwelt (calc/dna-dimension dna)])
    (make-ini [_ ini-spec]
      ini-spec))

  (def selfi (->Automaton :1d :match :select-ltr nil))
  (def mindform (->Automaton :2d :match :self-select-ltr nil))
  (def lifeform (->Automaton :2d :life :moore :random))
  (def decisionform (->Automaton :2d :life :moore :rand-center))


  ,)


#_
(comment
  (defprotocol PCellularAutomaton
    (step [this] "Advances the automaton by one generation.")
    (restart [this] "Resets the automaton to its initial state.")
    (get-resolution [this] "Returns the resolution of the automaton.")
    (get-evolution [this] "Returns an immutable copy of the current evolution."))

  (deftype CellularAutomaton
      #?(:cljs [res initial-state ^:mutable evolution ^:mutable iterator]
         :clj  [res initial-state ^:unsynchronized-mutable evolution
                ^:unsynchronized-mutable iterator])
    PCellularAutomaton
    (step [_]
      (set! iterator (rest iterator))
      (conj! evolution (first iterator)))
    (restart [_]
      (set! iterator (rest initial-state))
      (set! evolution (transient [(first initial-state)])))
    (get-resolution [_]
      res)
    (get-evolution [_]
      (let [^clojure.lang.PersistentVector v (persistent! evolution)]
        (set! evolution (transient v))
        v)))

  ;; because direct method access for deftype JS objects is weird
  ;; remember to turn dashes into underscores for method names!
  #?(:cljs (extend-type CellularAutomaton
             Object
             (step [_this] (step _this))
             (restart [_this] (restart _this))
             (get_resolution [_this] (get-resolution _this))
             (get_evolution [_this] (get-evolution _this))))



  ,)


#_
(comment

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


  (defprotocol PCellularAutomaton
    (step [this] "Advances the automaton by one generation.")
    (restart [this] "Resets the automaton to its initial state.")
    (get-resolution [this] "Returns the resolution of the automaton.")
    (get-evolution [this] "Returns an immutable copy of the current evolution."))

  (deftype CellularAutomaton
      #?(:cljs [res initial-state ^:mutable evolution ^:mutable iterator]
         :clj  [res initial-state ^:unsynchronized-mutable evolution
                ^:unsynchronized-mutable iterator])
    PCellularAutomaton
    (step [_]
      (set! iterator (rest iterator))
      (conj! evolution (first iterator)))
    (restart [_]
      (set! iterator (rest initial-state))
      (set! evolution (transient [(first initial-state)])))
    (get-resolution [_]
      res)
    (get-evolution [_]
      (let [^clojure.lang.PersistentVector v (persistent! evolution)]
        (set! evolution (transient v))
        v)))

  ;; because direct method access for deftype JS objects is weird
  ;; remember to turn dashes into underscores for method names!
  #?(:cljs (extend-type CellularAutomaton
             Object
             (step [_this] (step _this))
             (restart [_this] (restart _this))
             (get_resolution [_this] (get-resolution _this))
             (get_evolution [_this] (get-evolution _this))))

  (defn create-ca
    [initial-state]
    (let [gen1 (first initial-state)
          res (get-resolution-from-generation gen1)
          iterator (rest initial-state)
          evolution [gen1]]
      (->CellularAutomaton res initial-state (transient evolution) iterator)))


  ,)
