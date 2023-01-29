(ns tasks
  (:require [clojure.string :as str]))

;; may become obsolete
(defn grammar->cljc
  "Reads separate grammar file (.ebnf, etc.) and converts it to equally named .cljc file with direct defparser definition so that instaparse works with ClojureScript."
  [path {:keys [root parser-opts] :or {root "src" parser-opts {}}}]
  (let [grammar (-> (slurp (str root "/" path))
                    (str/escape {\\ "\\\\", \" "\\\""}))
        [ns-parts extension] (let [parts (str/split path #"[./]")]
                               [(butlast parts) (last parts)])
        header  (str ";; !! This is an auto-generated file and should not be edited directly !!\n"
                     ";;\n"
                     ";; Please edit the source file of the grammar instead:\n"
                     ";;   `" root "/" path "`\n"
                     ";; and then execute the dedicated task in `bb.edn` to re-generate this file.\n\n"
                     "(ns " (str/replace (str/join "." ns-parts) #"_" "-") "\n"
                     "  (:require #?(:clj  [instaparse.core :as insta :refer [defparser]]\n"
                     "               :cljs [instaparse.core :as insta :refer-macros [defparser]])))")
        code    (str header "\n\n"
                     "(defparser parser\n\""
                     grammar
                     "\"\n  " (let [s (str parser-opts)]
                                (subs s 1 (dec (count s))))
                     ")\n")]
    (spit (str root "/" (str/join "/" ns-parts) ".cljc") code)))
