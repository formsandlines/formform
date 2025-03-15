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

(defn get-resolution-from-generation
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
  (let [res (get-resolution-from-generation gen)]
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


(comment
  
  (get-resolution-from-generation [[[1 1 1] [1 1 1]]])

  (def foo (atom []))

  (def ca (create-ca (make-selfi [10] (calc/rand-dna 2) [:random])))
  ca

  (get-resolution ca)
  (get-evolution ca)
  (step ca)
  (restart ca)

  (.get-resolution ca)
  (.get-evolution ca)
  (.step ca)
  (.restart ca)

  
  #_
  (let [ca (automaton (make-selfi [10] (calc/rand-dna 2) [:random])
                      [10])]
    (println ((ca :get-evolution)))
    ((:step! ca)) ;; yields next gen
    (println ((ca :get-evolution)))
    ((:step! ca)) ;; yields next gen
    (println ((ca :get-evolution)))
    ((:reset! ca)) ;; clears evolution, resets ca to init
    (println ((ca :get-evolution)))
    (println (ca :get-resolution)) ;; returns resolution
    )

  
  ,)


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
