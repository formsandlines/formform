(ns formform-viewer
  (:require [formform.calc :as calc]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

(def viewer-const
  {:require-cljs true
   :render-fn 'formform-render/render-vcell-svg})

(def viewer-vrow
  {:pred (every-pred sequential? #(every? calc/const? %))
   :transform-fn (clerk/update-val
                  #(map (partial clerk/with-viewer
                                 viewer-const) %))
   :require-cljs true
   :render-fn 'formform-render/render-vrow-svg})

(def viewer-rule
  {:pred (every-pred sequential?
                     (comp sequential? first)
                     (comp keyword? second))
   :transform-fn (clerk/update-val
                  #(vector (clerk/with-viewer viewer-vrow (first %))
                           (clerk/with-viewer viewer-const (second %))))
   :require-cljs true
   :render-fn 'formform-render/render-rule-svg})


(def viewer-gen1d
  {:transform-fn (clerk/update-val
                  #(map (partial clerk/with-viewer
                                 viewer-const) %))
   :require-cljs true
   :render-fn 'formform-render/render-vrow-svg})

(def viewer-gen2d
  {:transform-fn (clerk/update-val
                  #(map (partial clerk/with-viewer
                                 viewer-gen1d) %))
   :require-cljs true
   :render-fn 'formform-render/render-vgrid-svg})


(def viewer-ca1d
  {:transform-fn (clerk/update-val #(map (partial clerk/with-viewer
                                                  viewer-gen1d) %))
   :render-fn
   '#(nextjournal.clerk.render/render-html
      (into [:div.flex.flex-col]
            (nextjournal.clerk.render/inspect-children %2)
            %1))})

(def viewer-ca2d
  {:transform-fn (clerk/update-val #(map (partial clerk/with-viewer
                                                  viewer-gen2d) %))
   :render-fn
   '#(nextjournal.clerk.render/render-html
      (into [:div.flex.flex-row.gap-2.flex-wrap]
            (nextjournal.clerk.render/inspect-children %2)
            %1))})


(def viewer-vspace
  {:render-fn '#(nextjournal.clerk.render/render-html
                 (into [:div
                        {:class (str "grid grid-cols-8 gap-4")}]
                       (nextjournal.clerk.render/inspect-children %2)
                       %1))})

