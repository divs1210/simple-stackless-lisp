(ns simple-stackless-lisp.main
  (:gen-class)
  (:require
   [clojure.edn :as edn]
   [simple-stackless-lisp.core :as core]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.util :as u]))

(defn run-file
  [filename]
  (let [text (str "(do " (slurp filename) ")")
        code (edn/read-string text)]
    (core/eval code)))

(defn run-repl []
  (let [env (env/fresh-env core/builtins)
        k   #(println "=>" (pr-str %) "\n")
        exe (u/executor)]
    (println "============================")
    (println "|Simple Stackless Lisp REPL|")
    (println "============================")
    (while true
      (try
        (print "> ")
        (flush)
        (core/eval (u/read-exp)
                   env
                   k
                   exe)
        (catch Exception e
          (env/bind! env '*e e)
          (println "Error: " (.getMessage e)))))))

(defn -main
  [& [filename]]
  (if filename
    (run-file filename)
    (run-repl)))
