(ns simple-stackless-lisp.main
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.edn :as edn]
   [simple-stackless-lisp.core :refer [eval]]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.util :as u]))

(defn repl []
  (let [env (env/fresh-env)
        k   #(println "=>" (pr-str %) "\n")
        exe (u/executor)]
    (println "============================")
    (println "|Simple Stackless Lisp REPL|")
    (println "============================")
    (while true
      (try
        (print "> ")
        (flush)
        (eval (u/read-exp)
              env
              k
              exe)
        (catch Exception e
          (env/bind! env '*e e)
          (println "Error: " (.getMessage e)))))))

(defn -main
  [& [filename]]
  (if filename
    (let [text (str "(do " (slurp filename) ")")
          code (edn/read-string text)]
      (eval code))
    (repl)))
