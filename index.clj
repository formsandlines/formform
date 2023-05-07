(ns index
  {:nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/open-graph
   {:url "https://formform.dev"
    :title "formform: tools for FORM logic research"
    :description "Clojure(Script) library to represent, transform and evaluate all the FORMs of uFORM iFORM"}}
  (:require [nextjournal.clerk :as clerk]))


;; # formform

;; A modular Clojure/ClojureScript library all about the 4-valued logic of cognition first introduced 2017 by Ralf Peyn in [uFORM iFORM](https://uformiform.info). In its core, the purpose of the library is to enable the representation, evaluation and algebraic simplification of all (undetermined, imaginary, unclear, …) FORMs that have been introduced in the book. 

;; * [Source](https://github.com/formsandlines/formform)
;; * [API Docs](docs)
;; * [Introduction](notebooks/introduction.html)

;; ## Apps built on formform

;; > Note: *FORM tricorder* and *FORM plotter* still use the [legacy JS-version](https://github.com/formsandlines/formform-js) of formform and are currently being re-designed and re-developed. In the meantime, feel free to use the old versions, they will work just fine.

;; * [FORM tricorder](tricorder)
;; * [FORM plotter](plotter)

;; ## Helpful resources

;; * [Overview on FORMs of self-equivalent Re-entries [EN]](downloads/re-entry-forms_v1-en.pdf)
;; * [Übersicht zu FORMen selbst-äquivalenter Re-entries [DE]](downloads/re-entry-forms_v1-de.pdf)

;; ---

(clerk/html
 [:footer
  [:p {:style {:font-size "0.8rem"}}
   [:a {:href "/impressum.html"}
    "Impressum / Datenschutzerklärung"]]])
