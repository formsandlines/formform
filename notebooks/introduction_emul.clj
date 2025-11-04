(ns introduction-emul
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/auto-expand-results? true}
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.string :as str]
            [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.emul :as emul :refer :all]
            [formform-viewer :refer :all]))

;; # Introduction to formform.emul

;; The `emul` (short for “emulation”) module in formform can help researchers, tool developers and enthusiasts to run _FORM logic_ expressions as cellular automata. It is designed (like all formform modules) to be declarative, composable and highly extendable.

;; All types of 1D and 2D cellular automata (CAs) that Ralf Peyn (at the time of this writing) introduced in his published research are supported:
;; - _SelFis_
;; - _mindFORMs_
;; - _lifeFORMs_
;; - _decisionFORMs_

;; ## Getting started

;; To get started, I suggest you first try out some of the pre-made “specimen” that are known from [_uFORM iFORM_](https://uformiform.info):

(keys common-specimen)

;; Let’s take a look at good old _Slit_:

(def slit-spec (:Slit common-specimen))

;; What you see here is a specification for a 1D cellular automaton that generates the _SelFi_ system called _Slit_. It consists (besides an optional `:label`) of three `-spec` entries that describe the necessary parts of the CA with their own special data shape.

;; > You learn more about these specs in [a later section](creating-custom-specifications). My use of the term “spec” here is not to be confused with [clojure.spec](https://clojure.org/about/spec), which formform uses for tests, validation and documentation.

;; We can plug this data into `ca-iterator` with our desired resolution, which will create a generator that calculates generations on demand:

#_
(clerk/with-viewer
  v/var-viewer
  (def slit-iter (ca-iterator slit-spec [33])))

(binding [nextjournal.clerk.config/*bounded-count-limit* 0]
  (def slit-iter (ca-iterator slit-spec [33])))

;; > Please ignore the code wrapping the `(def …)` form – it is required in this notebook environment to avoid evaluation of the infinite lazy sequence that `ca-iterator` creates.

;; Let’s run it for 15 iterations:

^{::clerk/budget nil}
(take 15 slit-iter)

;; This is the raw evolution data where each vector represents a generation of _constant_ values.

;; If you squint, you may see the characteristic pattern, but it is hard to recognize from the raw data. In a notebook environment like [_Clerk_](https://github.com/nextjournal/clerk) (where this article exists), we can use the color mapping from _uFORM iFORM_ instead to represent cell values:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/table
 [calc/nuim-code
  (mapv (partial clerk/with-viewer viewer-const)
        calc/nuim-code)])

;; This enables a more familiar representation:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/with-viewer viewer-ca1d
  (take 15 slit-iter))

;; > Note: these visualizations are created using Clerks “viewer” mechanism. To load them into your own notebook, copy [formform_viewer.clj](https://github.com/formsandlines/formform/tree/main/notebooks/formform_viewer.clj) and [formform_render.cljs](https://github.com/formsandlines/formform/tree/main/notebooks/formform_render.cljs) into your project and `:require` the namespace `formform-viewer`. See [Book of Clerk](https://book.clerk.vision/#applying-viewers) on how to use custom viewers.

;; Let’s see a snapshot after 1000 steps:

^{::clerk/viewer viewer-ca1d}
(take 15 (drop 1000 slit-iter))

;; Since these iterators are immutable, we do not have to worry about state mutation when we take different slices from their evolution.

;; > If you want something more like a state machine, take a look at [this section](#stateful-automata).

;; Here is another example with _CoOneAnother_, this time we have it directly return the first 50 generations through an additional argument:

(def coa-spec (:CoOneAnother common-specimen))

^{::clerk/viewer viewer-ca1d
  ::clerk/render-opts {:formform/cellsize 5
                       :formform/grid-px 0.5}}
(ca-iterator coa-spec [125] 50)


;; ## Common CA Types

;; What if you want to create a _SelFi_ that is not in the set of predefined species or create a 2D CA like a _mindFORM_? There are constructors for the four most common CA types in FORM logic:
;; - `make-selfi`
;; - `make-mindform`
;; - `make-lifeform`
;; - `make-decisionform`

;; Here is a _mindFORM_ whose rule acts on the results of the familiar _Slit_ expression and whose initialization looks like the _“ball”_ ini you may know from _uFORM iFORM_ (`:i :u :m :u :i`), but translated into 2D:

(def slit2d-spec
  (make-mindform (expr/==>* '[:- [[a] b] [[b] a]])
                 (make-ini :figure :n (ini-patterns :ball2d) :center)))

;; By the way, you can find useful ini patterns, among them the familiar 1D-“ball” pattern, in `ini-patterns`:

^{::clerk/viewer
  {:transform-fn
   (clerk/update-val
    (fn [m]
      (update-vals m
                   (fn [gen]
                     (if (vector? (first gen))
                       (clerk/with-viewer viewer-gen2d gen)
                       (clerk/with-viewer viewer-gen1d gen))))))}}
ini-patterns

;; Let’s see how this _mindFORM_ evolves:

^{::clerk/viewer viewer-ca2d
  ::clerk/render-opts {:formform/cellsize 6}}
(ca-iterator slit2d-spec [23 23] 8)

;; Did you notice that `make-mindform` doesn’t take the input expression itself, but its _formDNA_? Let’s explore how `formform.emul` uses it as a ruleset.

;; ### formDNA as a Ruleset

;; If you haven’t encountered _formDNA_ before in _formform_, it is essentially a value structure that acts like an (implicit/abstract) lookup-table between all possible inputs for the expression and their corresponding result.

;; When you evaluate any FORM expression using `expr/==>*` (shorthand for `expr/eval->val-all`), you will get the results from all possible interpretations of all variables in the expression as a _formDNA_ (instead of the table-like structure from `expr/eval-all`):

(def dna-slit (expr/==>* '[:- [[a] b] [[b] a]]))

;; In graphical form:

^{::clerk/visibility {:code :hide}
  ::clerk/viewer viewer-vrow}
dna-slit

;; Here is the _formDNA_ from _CoOneAnother_, which we can get directly from the binary selection of its _triple-selective decision system_ (see _uFORM iFORM_), using `expr/ts==>*` (shorthand for `expr/eval-tsds->val-all`):

^{::clerk/viewer viewer-vrow}
(def dna-coa (expr/ts==>* 1 0 1 1 0 0))

;; > Can you spot a familiar pattern in there?

;; _formDNA_ only stores the _results_, but the information about the input values (which would be the _“umwelt”_ in a CA) is implicit in its sequential order. Let’s make it explicit to get an idea on how the ruleset for this FORM looks like:

^{::clerk/visibility {:code :hide}
  ::clerk/auto-expand-results? false}
(clerk/caption
 "CoOneAnother as a Ruleset. The top row are the inputs (e.g. _L E R_), the bottom value is the result."
 (clerk/with-viewer
   (update viewer-vspace :add-viewers v/add-viewers [viewer-rule])
   (mapv vector
         (calc/vspace (calc/dna-dimension dna-coa))
         dna-coa)))


;; ## Initialization Patterns

;; Besides _formDNA_, `make-selfi` and `make-mindform` also require an “ini-spec”, meaning a specification of their initialization pattern.

;; To create an ini spec, call `make-ini` with the desired ini type and parameters for that type. Which types are available? Let’s find out with the `:help` argument:

^{::clerk/visibility {:code :hide :result :hide}}
(def quoted-markdown-viewer
  {:transform-fn (clerk/update-val
                  #(->> %
                        (str/split-lines)
                        (map (fn [s] (str "> " s)))
                        (str/join "\n")
                        (clerk/with-viewer v/markdown-viewer)))})

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :help))

;; The `?` before the `opts` argument means that it is optional. We can get more detailed information about a specific ini type like this:
^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :constant :help))

;; The ini spec essentially is just data, a description of how to construct a specific initial generation:

(def const-ini (make-ini :constant :u))

;; It is useful to test an ini before running it in a CA. Let’s use the spec to build a generation consisting of a single value:

^{::clerk/viewer viewer-gen1d}
(sys-ini const-ini [61])


;; ### Random Inis

;; To generate randomized inis, we can use `:random`:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :random :help))

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :random) [61])

;; Most inis are also defined for 2D generations, so we can just pass a 2D resolution `[width height]` without changing anything about the ini spec:

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :random) [31 9])

;; Since we are using a randomized ini, two specs with the same parameters may generate completely different values:

^{::clerk/no-cache true}
(= (sys-ini (make-ini :random) [61])
   (sys-ini (make-ini :random) [61])
   (sys-ini (make-ini :random) [61]))

;; But with a random seed (as the ini docs mentioned), we can reproduce the same random values reliably on every evaluation:

^{::clerk/no-cache true}
(= (sys-ini (make-ini :random) [61] {:seed 93})
   (sys-ini (make-ini :random) [61] {:seed 93})
   (sys-ini (make-ini :random) [61] {:seed 93}))


;; It is also possible to change the distribution of random values using the `:weights` option (see docs above):

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :random {:weights 0.1}) [61])

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :random {:weights 0.9}) [61])

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :random {:weights [6 3 1.5 0.5]}) [31 9])

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :random {:weights {:u 2 :m 1}}) [31 9])


;; ### Figure Inis

;; We have already seen a 2D “ball” ini. To create one, we have to use an ini specification type called `:figure`, which can be used to throw an arbitrary pattern against a specified background:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :figure :help))

;; Let’s try it with a familiar pattern first:

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :figure :n (ini-patterns :ball) :center)
         [31])

;; We can change the background to a different value and use a “hole” (or “inverted ball”) instead:

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :figure :m (ini-patterns :hole) :center)
         [31])

;; Instead of a simple value, we can also directly pass a background ini, such as `:cycle`:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :cycle :help))

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :figure (make-ini :cycle [:u :m])
                   (ini-patterns :hole2d) :center)
         [11 11])

;; Here we create a custom pattern and align it to the right:

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :figure :n (->> (cycle [:m :i :m :u]) (take 16) vec)
                   :right)
         [31])

;; We can also precisely set the coordinates of a pattern by providing a vector of integers as the _anchor_ argument:

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :figure (make-ini :random {:weights {:u 1 :n 3}})
                   [[:m :n] [:n :i]] [6 3])
         [20 14])

;; If you want even more precise control over the placement of the pattern, pass a map instead:

^{::clerk/visibility {:result :hide}}
(defn make-frame
  "Creates a rectangular frame pattern."
  [w h v]
  (mapv (fn [y]
          (mapv (fn [x]
                  (if (or (zero? x) (== (dec w) x)
                          (zero? y) (== (dec h) y))
                    v :_))
                (range h)))
        (range w)))

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :figure {:weights 1.0}
                   :n
                   (make-frame 9 9 :?)
                   {:pos :center :align :topleft})
         [17 17])

;; As demonstrated above, it is beneficial to define custom functions for more complex patterns.

;; There are two _special values_ that are useful in pattern definitions:

;; - `:_` lets the background “shine through”, which enables more refined and reusable patterns
;; - `:?` selects a random value (the `:weights` option we have seen before in the `:random` ini applies here as well)

;; A shortcut to create random figure inis is `:rand-figure`:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :rand-figure :help))

;; It basically generates an area of random values:

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :rand-figure {:weights 1.0} :n 7 :center)
         [17 17])

;; Sometimes you may want more of a “splash” of values instead of a neatly packed unit. The `:decay` option makes this possible:

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :rand-figure {:weights 1.0 :decay 0.7} :n 7 :center)
         [17 17])

;; It is essentially a probability of random “holes” to appear in the pattern, where the background value “shines through”. Of course it is also available in other figure-related inis:

^{::clerk/visibility {:result :hide}}
(defn fill-rect
  "Creates a rectangular filled pattern."
  [w h v]
  (mapv vec (repeat h (repeat w v))))

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :figure {:decay 0.4}
                   (make-ini :cycle [:u :n])
                   (fill-rect 7 7 :m)
                   :center)
         [17 17])


;; ### Combining Figures

;; A single pattern can be repeated with `:figure-repeat`:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :figure-repeat :help))

^{::clerk/visibility {:result :hide}}
(def arrow1d [:u :i :m])

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :figure-repeat :n arrow1d :center 10 3)
         [69])

^{::clerk/visibility {:result :hide}}
(def arrow2d [[:u :_ :_]
              [:_ :i :_]
              [:_ :_ :m]
              [:_ :i :_]
              [:u :_ :_]])

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :figure-repeat :n arrow2d {:pos :right :align :center}
                   [7 2] [1 2])
         [32 14])

;; > Note that ini patterns will also wrap around when they exceed generation boundaries.

;; The ini type `:comp-figures` composes multiple figures by stacking them on top of each other. This can be useful if you want to observe how a system reacts to different simultaneous events:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :comp-figures :help))

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :comp-figures
                   :n
                   [(make-ini :figure :n (fill-rect 6 6 :u)
                              {:pos :center :align :bottomright :offset 1})
                    (make-ini :figure :n (make-frame 8 8 :i)
                              {:pos :center :align :topleft :offset -3})
                    (make-ini :figure-repeat :n arrow2d
                              {:pos :left :align :left :offset [1 0]}
                              [3 1] 1)])
         [16 16])

;; As you can see, we can even compose `:figure-repeat`!


;; ## Creating custom Specifications

;; Sometimes you may want to experiment with cellular automata that cannot be described in terms of the four common CA types. In this case, you have to create your own specifications. You do that by composing a selection of parts for the three spec types we encountered before:
;; - `:ini-spec` describes the CA [initialization](#initialization-patterns), which will create the first generation of cells
;; - `:rule-spec` describes the CA rule (usually with [_formDNA_](#formdna-as-a-ruleset)) that gets applied to calculate the next generation
;; - `:umwelt-spec` describes how to select the neighborhood for each cell on which the rule gets applied

;; If you just want to modify an existing CA type, the easiest way is to use the `:overwrites` option, where you can replace parts of the spec:

(def lifeform-clustered
  (make-lifeform
   (expr/ts==>* 1 0 1 1 0 0)
   {:overwrites {:ini-spec (make-ini :figure-repeat {:weights 0.2}
                                     :n (fill-rect 7 7 :?)
                                     :center [2 2] [5 5])}}))

^{::clerk/viewer viewer-ca2d
  ::clerk/render-opts {:formform/cellsize 3 :formform/grid-px 0}}
(ca-iterator lifeform-clustered [35 35] 6)

;; For entirely new creations, `specify-ca` lets you define a CA specification from scratch:

(def crossform-ca
  (specify-ca {:ini-spec (make-ini :random {:weights 0.05})
               :rule-spec (make-rule :match (expr/==>* '[a [b [c [d]]]]))
               :umwelt-spec (make-umwelt :von-neumann :row-first false)}
              "crossFORM"))

^{::clerk/viewer viewer-ca2d
  ::clerk/render-opts {:formform/cellsize 3 :formform/grid-px 0}}
(ca-iterator crossform-ca [35 35] 6)

;; > Note: since there just a few predefined umwelt- and rule-specs right now and they work best in specific combinations, variation in CA specifications is very limited. However, you can extend formform.emul with your own ini-, rule- or umwelt-types via `defini`, `defumwelt` and `defrule`. This is beyond the scope of this introduction; take a look at the docs for these macros to learn more.

;; ## Stateful Automata

;; Se far we have seen the simple and immutable `ca-iterator`, which is very handy in research notebooks and data exploration, where you want reproducable and predictable behavior. But if you are working on more integrated systems such as a standalone app, you often need a stateful CA for performance or other practical reasons.

;; The `create-ca` function creates a cellular automaton that acts as a state machine. It takes the exact same specification data that you use to create CA iterators and returns a `formform.emul.core.CellularAutomaton` object:

^{::clerk/no-cache true}
(defonce mark1-ca (create-ca (common-specimen :Mark1) [39]))

;; Let’s get the initial generation:

^{::clerk/visibility {:code :hide :result :hide}}
(restart mark1-ca)

(get-system-time mark1-ca)

^{::clerk/viewer viewer-gen1d}
(get-current-generation mark1-ca)

;; Now we step through the evolution one by one:

^{::clerk/visibility {:result :hide}}
(step mark1-ca)
(get-system-time mark1-ca)

^{::clerk/viewer viewer-gen1d}
(get-current-generation mark1-ca)

^{::clerk/visibility {:result :hide}}
(step mark1-ca)
(get-system-time mark1-ca)

^{::clerk/viewer viewer-gen1d}
(get-current-generation mark1-ca)

;; We can restart the CA to re-initialize it:

(restart mark1-ca)

(get-system-time mark1-ca)

^{::clerk/viewer viewer-gen1d}
(get-current-generation mark1-ca)

;; ### Cached Evolution

;; The CA keeps the generations it has calculated thus far up to a certain _cache limit_, which can be very high for lower resolutions:

(get-history-cache-limit mark1-ca)

;; > The history cache limit gets calculated dynamically when you call `create-ca` to avoid memory overload with higher resolutions and CA dimensions while still keeping a useful evolution window to work with. It takes an additional argument to set the cache limit as needed.

;; Even though we restarted the CA, the cached evolution is still retained. This way, it doesn’t have to recalculate all its previous generations:

(get-cached-history mark1-ca)

;; ### Optimizations

;; `create-ca` automatically makes use of host-platform native arrays and various performance optimizations if both, the umwelt-spec and the rule-spec for the specified CA implement optimized variants, which is true for all predefined rule- and umwelt-specs.

;; The interface methods normally convert the native arrays to Clojure vectors, but if you really need them, set the `optimized?` option to true:

(def arr (get-current-generation mark1-ca {:optimized? true}))

(aget arr 17)

