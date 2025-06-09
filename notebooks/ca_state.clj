(ns ca-state
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/auto-expand-results? true}
  (:require [nextjournal.clerk :as clerk]
            ;; [nextjournal.clerk.viewer :as v]
            [formform.calc :as calc]
            [formform.expr :as expr]
            [formform.emul :as emul :refer :all]
            [formform.io :as io]))


^{::clerk/visibility {:code :hide :result :hide}}
(def const-viewer
  {:pred calc/consts
   :render-fn '#(nextjournal.clerk.viewer/html
                 (let [size (or (:formform/cellsize %2) 10)
                       grid-px (or (:formform/grid-px %2) 1)]
                   [:div.inline-block
                    {:style {:width size :height size
                             :border-width (str grid-px "px")}
                     :class (str (case %
                                   :N "bg-black"
                                   :U "bg-red-500"
                                   :I "bg-green-500"
                                   :M "bg-blue-500")
                                 " border-solid border border-gray-700 dark:border-gray-700")}]))})

^{::clerk/visibility {:code :hide :result :hide}}
(def viewer-gen1d
  {:transform-fn (clerk/update-val #(map (partial clerk/with-viewer
                                                  const-viewer) %))
   :render-fn
   '#(nextjournal.clerk.viewer/html
      (into [:div.flex.flex-row]
            (nextjournal.clerk.viewer/inspect-children %2)
            %1))})

^{::clerk/visibility {:code :hide :result :hide}}
(def viewer-gen2d
  {:transform-fn (clerk/update-val #(map (partial clerk/with-viewer
                                                  viewer-gen1d) %))
   :render-fn
   '#(nextjournal.clerk.viewer/html
      (into [:div.flex.flex-col]
            (nextjournal.clerk.viewer/inspect-children %2)
            %1))})

^{::clerk/visibility {:code :hide :result :hide}}
(def viewer-ca1d
  {:transform-fn (clerk/update-val #(map (partial clerk/with-viewer
                                                  viewer-gen1d) %))
   :render-fn
   '#(nextjournal.clerk.viewer/html
      (into [:div.flex.flex-col]
            (nextjournal.clerk.viewer/inspect-children %2)
            %1))})

^{::clerk/visibility {:code :hide :result :hide}}
(def viewer-ca2d
  {:transform-fn (clerk/update-val #(map (partial clerk/with-viewer
                                                  viewer-gen2d) %))
   :render-fn
   '#(nextjournal.clerk.viewer/html
      (into [:div.flex.flex-row.gap-2.flex-wrap]
            (nextjournal.clerk.viewer/inspect-children %2)
            %1))})


;; ## Direct Generation

^{::clerk/no-cache true}
(defonce ca01
  (create-ca (make-lifeform {:ini-opts {:seed 10}}
                            (tsds-sel->dna [1 0 0 1 0 1]) 0.5)
             [23 23]))

^{::clerk/sync true}
(defonce !t (atom 0))

(defonce !curr-gen (atom (get-current-generation ca01)))

(add-watch !t nil
           (fn [_ _ _ t]
             (if (zero? t)
               (do (restart ca01)
                   (reset! !curr-gen (get-current-generation ca01)))
               (do (step ca01)
                   (reset! !curr-gen (get-current-generation ca01))))))

(clerk/with-viewer
  {:render-fn
   '#(nextjournal.clerk.viewer/html
      [:div.flex.gap-4
       [:button.text-white.bg-blue-500.px-3.py-2.hover:bg-blue-600.rounded-sm.font-mono.text-sm
        {:on-click (fn [_] (swap! !t inc))}
        "Step CA!"]
       [:button.text-white.bg-red-500.px-3.py-2.hover:bg-red-600.rounded-sm.font-mono.text-sm
        {:on-click (fn [_] (reset! !t 0))}
        "Reset CA!"]])}
  {})

@!t

^{::clerk/viewer viewer-gen2d}
@!curr-gen


;; ## With cached Evolution


^{::clerk/no-cache true}
(def ca02
  (create-ca (make-lifeform {:ini-opts {:seed 9143}}
                            (tsds-sel->dna [1 0 0 1 0 1]) 0.5)
             [23 23]))

(restart ca02)

(dotimes [_ 20] (step ca02))


(clerk/with-viewer
  {:transform-fn (clerk/update-val
                  #(mapv (partial clerk/with-viewer viewer-gen2d) %))
   :render-fn '(fn [evol opts]
                 (reagent.core/with-let [!gen-i (reagent.core/atom 0)]
                   [:<>
                    [:div.flex.gap-4
                     [:button.text-white.bg-blue-500.px-3.py-2.hover:bg-blue-600.rounded-sm.font-mono.text-sm.disabled:bg-gray-500.disabled:hover:bg-gray-500.disabled:cursor-not-allowed
                      {:disabled (>= @!gen-i (dec (count evol)))
                       :on-click (fn [_] (if (< @!gen-i (dec (count evol)))
                                          (swap! !gen-i inc)
                                          nil))}
                      "Step CA!"]
                     [:button.text-white.bg-red-500.px-3.py-2.hover:bg-red-600.rounded-sm.font-mono.text-sm
                      {:on-click (fn [_] (reset! !gen-i 0))}
                      "Reset CA!"]]
                    [:div.mt-4.flex.flex-column.gap-2.font-mono
                     [nextjournal.clerk.render/inspect-presented
                      opts (get evol @!gen-i)]
                     (str @!gen-i "/" (dec (count evol)))]]))}
  {::clerk/render-opts {:formform/cellsize 8}}
  (get-cached-history ca02))

