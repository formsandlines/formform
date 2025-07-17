(ns ^:no-doc formform.emul.interfaces
  (:require [formform.utils :as utils]
            [clojure.string :as str])
  #?(:cljs (:require-macros
            [formform.emul.interfaces
             :refer [defini defumwelt defrule]])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol Ini
  (make-gen
    [this opts w]
    [this opts w h]))

(defprotocol IniTransducer
  (ini-xform1d [this])
  (ini-xform2d [this]))

(defprotocol Umwelt
  (observe-umwelt
    [this gen1d cell w]
    [this gen2d cell w h]))

(defprotocol UmweltOptimized
  (^String observe-umwelt--fast
   #?(:clj  [this ^"[Lclojure.lang.Keyword;" gen1d-arr
             ^clojure.lang.PersistentVector cell w]
      :cljs [this gen1d-arr cell w])
   #?(:clj  [this ^"[[Lclojure.lang.Keyword;" gen2d-arr
             ^clojure.lang.PersistentVector cell w h]
      :cljs [this gen2d-arr cell w h])))

;; ? provide cell instead of only val for more flexibility
(defprotocol Rule
  (apply-rule [this umwelt self-v]))

(defprotocol RuleOptimized
  (^clojure.lang.Keyword apply-rule--fast
   [this ^String umwelt-qtn ^clojure.lang.Keyword self-v]))

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

(defmacro defini
  [type-k fields doc-string? & methods]
  (apply defini-impl type-k fields doc-string? methods))

(defmacro defumwelt
  [type-k fields doc-string? & methods]
  (apply defumwelt-impl type-k fields doc-string? methods))

(defmacro defrule
  [type-k fields doc-string? & methods]
  (apply defrule-impl type-k fields doc-string? methods))

(defn record->kw-ids
  [^clojure.lang.IRecord record]
  (let [type-name (-> record type pr-str (str/split #"\.") last)]
    (->> (str/split type-name #"-")
         (mapv (comp keyword utils/camel->kebab)))))

(defn kw-ids->record-type
  [cat-k type-k]
  (-> (str "formform.emul.core."
           (-> cat-k name str/capitalize) "-"
           (-> type-k name (utils/kebab->camel true)))
      symbol))

(defn ->rec
  "Wrapper for type record constructors to add non-essential information"
  [rec-constructor & args]
  (let [rec (apply rec-constructor args)
        [cat-k type-k] (record->kw-ids rec)]
    (cond-> rec
      cat-k  (assoc :formform.emul/kind cat-k)
      type-k (assoc :formform.emul/type type-k))))
