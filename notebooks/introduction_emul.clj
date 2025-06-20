(ns introduction-emul
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/auto-expand-results? true}
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.string :as str]
            [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.emul :as emul :refer :all]
            [formform.io :as io]
            [formform-viewer :refer :all]))

;; # Introduction to formform.emul

;; The new `emul` (short for “emulation”) module in formform can help researchers, tool developers and enthusiasts to run _FORM logic_ expressions as cellular automata. It is designed (like all formform modules) to be declarative, composable and highly extendable.

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


;; ## Creating common CA Types

;; What if you want to create a _SelFi_ that is not in the set of predefined species or create a 2D CA like a _mindFORM_? There are constructors for the four most common CA types in FORM logic:
;; - `make-selfi`
;; - `make-mindform`
;; - `make-lifeform`
;; - `make-decisionform`

;; Here is a _mindFORM_ whose rule acts on the results of the familiar _Slit_ expression and whose initialization looks like the _“ball”_ ini you may know from _uFORM iFORM_ (`:i :u :m :u :i`), but translated into 2D:

(def slit2d-spec
  (make-mindform (exprs->dna '[[a] b] '[[b] a])
                 (make-ini :ball :n nil {:pos :center :align :center})))

^{::clerk/viewer viewer-ca2d
  ::clerk/render-opts {:formform/cellsize 6}}
(ca-iterator slit2d-spec [23 23] 25)

;; Notice that it doesn’t take the input expression itself, but its _formDNA_. Let’s explore how `formform.emul` uses it as a ruleset.

;; ### formDNA as a Ruleset

;; If you haven’t encountered _formDNA_ before in _formform_, it is essentially a value structure that acts like an (implicit/abstract) lookup-table between all possible inputs for the expression and their corresponding result.

;; When you evaluate any FORM expression using `formform.expr/=>*`, you will get a _formDNA expression_, which wraps the actual _formDNA_ data. Using `op-get`, we can extract it, but the `exprs->dna` function makes it more convenient:

(def dna-slit (exprs->dna '[[a] b] '[[b] a]))

;; In graphical form:

^{::clerk/visibility {:code :hide}
  ::clerk/viewer viewer-vrow}
dna-slit

;; Here is the _formDNA_ from _CoOneAnother_, which we can get directly from the binary selection of its _triple-selective decision system_ (see _uFORM iFORM_), using `tsds-sel->dna`:

^{::clerk/viewer viewer-vrow}
(def dna-coa (tsds-sel->dna [1 0 1 1 0 0]))

;; > Hmmm… a part of this pattern seems familiar. I’ll leave the interpretation to the reader as it doesn’t fit the topic of this introduction. (:

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

#_
^{::clerk/visibility {:code :hide}
  ::clerk/viewer viewer-vspace
  ::clerk/viewers (v/add-viewers [viewer-rule])
  ::clerk/auto-expand-results? false}
(mapv vector
      (calc/vspace (calc/dna-dimension dna-coa))
      (rseq dna-coa))


#_
(comment
  ;; We can construct a _vspace_ (an abstraction of all possible combinations of the input values) to see how it looks like:

  ^{::clerk/viewer viewer-vspace
    ::clerk/viewers (v/add-viewers [viewer-vrow])
    ::clerk/auto-expand-results? false}
  (def vspace (calc/vspace (calc/dna-dimension dna-tsds)))

  ;; When we match up the _vspace_ with our _formDNA_, we can see how the ruleset for the original expression looks like:

  ^{::clerk/viewer viewer-vspace
    ::clerk/viewers (v/add-viewers [viewer-rule])
    ::clerk/auto-expand-results? false}
  (mapv vector vspace (rseq dna-tsds)))

;; ### Common Initialization Types

;; Besides _formDNA_, `make-selfi` and `make-mindform` also require an “ini-spec”, meaning a specification of their initialization pattern.

;; To create an ini spec, call `make-ini` with the desired ini type and parameters for that type. What types are available? Let’s find out with the `:help` argument:

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
  (make-ini :random :help))

#_
^{::clerk/viewer
  v/markdown-viewer}
(make-ini :random :help)

;; The ini spec essentially is just data, a description of how to construct a specific initial generation:

(make-ini :random 0.5)

;; It is useful to test an ini before running it in a CA. Let’s use the spec to build a random generation:

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :random 0.5) 61)

;; Most inis are also defined for 2D generations, so we can just pass a second resolution without changing anything about the ini spec:

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :random 0.5) 31 9)

;; Since we are using a randomized ini, two specs with the same parameters may generate completely different values:

^{::clerk/no-cache true}
(= (sys-ini (make-ini :random 0.5) 61)
   (sys-ini (make-ini :random 0.5) 61)
   (sys-ini (make-ini :random 0.5) 61))

;; But with a random seed (as the ini docs mentioned), we can reproduce the same random values reliably:

^{::clerk/no-cache true}
(= (sys-ini (make-ini :random {:seed 93} 0.5) 61)
   (sys-ini (make-ini :random {:seed 93} 0.5) 61)
   (sys-ini (make-ini :random {:seed 93} 0.5) 61))

;; Let’s take a look at the classic _ball_ ini:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :ball :help))

;; It is a specialization of a more general ini, called `:figure`, which tells us more about how to specify the `bg` and the `anchor` props:

^{::clerk/viewer quoted-markdown-viewer}
(with-out-str
  (make-ini :figure :help))

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :ball :n nil {})
         61)

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :ball :n nil {:pos :center :align :center})
         61)

^{::clerk/viewer viewer-gen1d}
(sys-ini (make-ini :ball :n nil {:pos :left :align :left})
         61)

^{::clerk/viewer viewer-gen2d}
(sys-ini (make-ini :ball :n nil {:pos :bottomright :align :bottomright})
         31 17)



;; ## Creating custom Specifications

;; Sometimes you may want to experiment with cellular automata that cannot be described in terms of the four common CA types. In this case, you have to create your own specifications. You do that by composing a selection of parts for the three spec types we encountered before:
;; - `:ini-spec` describes the CA initialization, which will create the first generation of cells
;; - `:rule-spec` describes the CA rule (usually with _formDNA_) that gets applied to calculate the next generation
;; - `:umwelt-spec` describes how to select the neighborhood for each cell on which the rule gets applied



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

;; The CA keeps the generations it has calculated thus far up to a certain _cache limit_, which can be very high for lower resolutions:

(get-history-cache-limit mark1-ca)

;; > The history cache limit gets calculated dynamically when you call `create-ca` to avoid memory overload with higher resolutions and CA dimensions while still keeping a useful evolution window to work with. It takes an additional argument to set the cache limit as needed.

;; Even though we restarted the CA, the cached evolution is still retained. This way, it doesn’t have to recalculate all its previous generations:

(get-cached-history mark1-ca)



#_
(comment
  (clerk/with-viewer
    viewer-ca2d
    {::clerk/render-opts {:formform/cellsize 4
                          :formform/grid-px 0.5}}
    (take 20 (get-cached-history ca-lifeform))))

;; ### More Performance


