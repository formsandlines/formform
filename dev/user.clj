(ns user
  (:require [nextjournal.clerk :as clerk]))

(def root "notebooks/")

(comment
  ;; start Clerk's built-in webserver on the default port 7777, opening the browser when done
  (clerk/serve! {:browse? true})

  ;; either call `clerk/show!` explicitly
  ; (clerk/show! "notebooks/formform/index.clj")

  ; (clerk/build! {:paths [(str root "introduction.clj")]})

  ;; or let Clerk watch the given `:paths` for changes
  (clerk/serve! {:watch-paths ["index.clj" root]}))
