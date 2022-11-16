(ns user
  (:require [flow-storm.api :as fs-api]
            [nextjournal.clerk :as clerk]))


(comment
  ;; start Clerk's built-in webserver on the default port 7777, opening the browser when done
  (clerk/serve! {:browse? true})

  ;; either call `clerk/show!` explicitly
  ; (clerk/show! "notebooks/formform/index.clj")

  ;; or let Clerk watch the given `:paths` for changes
  (clerk/serve! {:watch-paths ["notebooks"]})

  (fs-api/local-connect)


  )
