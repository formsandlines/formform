(ns ^:no-doc formform.emul.interfaces
  (:require [formform.utils :as utils])
  #?(:cljs (:require-macros
            [formform.emul.interfaces
             :refer [defini defumwelt defrule defspecies]])))

(defprotocol Ini
  (make-gen
    [this w]
    [this w h]))

;; ? get w/h from generation length
(defprotocol Umwelt
  (observe-umwelt
    [this gen1d cell w]
    [this gen2d cell w h]))

(defprotocol UmweltOptimized
  (observe-umwelt--fast
    [this gen1d-arr cell w]
    [this gen2d-arr cell w h]))

;; ? provide cell instead of only val for more flexibility
(defprotocol Rule
  (apply-rule [this umwelt self-v]))

(defprotocol RuleOptimized
  (apply-rule--fast [this umwelt-qtn self-v]))

(defprotocol Specifier
  (specify-ca
    [this options w]
    [this options w h]))

(defprotocol CASystem
  (step [this] "Advances the automaton by one generation.")
  (restart [this] "Resets the automaton to its initial state.")
  (get-resolution [this] "Returns the resolution of the automaton.")
  (get-current-generation [this optimized?] "Returns the current generation.")
  (get-cached-history [this optimized?] "Returns the cached history.")
  (get-system-time [this] "Returns the current generation index.")
  (get-history-cache-limit [this] "Returns the history cache limit.")
  ;; (get-evolution [this] "Returns an immutable copy of the current evolution.")
  )

(defonce !types (atom {}))

(defn register-type!
  [cat-k type-k constructor-fn params doc-string]
  (swap! !types assoc-in [cat-k type-k]
         {:constructor constructor-fn
          :params params
          :docs doc-string}))

(defn- make-record-api
  [cat-k rec-prefix protocol]
  (fn [type-k fields doc-string? & methods]
    (let [[doc-string methods] (if (string? doc-string?)
                                 [doc-string? methods]
                                 [nil (cons doc-string? methods)])
          rec-sym (symbol (str rec-prefix "-"
                               (utils/kebab->camel (name type-k) true)))
          rec-def `(defrecord ~rec-sym ~fields
                     ~protocol
                     ~@methods)
          constr-sym (symbol (str *ns* "/->" rec-sym))
          register-type-sym 'formform.emul.interfaces/register-type!]
      `(do ~rec-def
           ~(list register-type-sym
                  cat-k type-k constr-sym
                  (mapv (comp keyword str) fields)
                  doc-string)))))

(def defini-impl
  (make-record-api :ini "Ini" (symbol (str *ns* ".Ini"))))
(def defumwelt-impl
  (make-record-api :umwelt "Umwelt" (symbol (str *ns* ".Umwelt"))))
(def defrule-impl
  (make-record-api :rule "Rule" (symbol (str *ns* ".Rule"))))
(def defspecies-impl
  (make-record-api :species "Species" (symbol (str *ns* ".Specifier"))))

(defmacro defini
  [type-k fields doc-string? & methods]
  (apply defini-impl type-k fields doc-string? methods))

(defmacro defumwelt
  [type-k fields doc-string? & methods]
  (apply defumwelt-impl type-k fields doc-string? methods))

(defmacro defrule
  [type-k fields doc-string? & methods]
  (apply defrule-impl type-k fields doc-string? methods))

(defmacro defspecies
  [type-k fields doc-string? & methods]
  (apply defspecies-impl type-k fields doc-string? methods))

