(ns simple-stackless-lisp.main
  (:gen-class)
  (:require
   [clojure.edn :as edn]
   [simple-stackless-lisp.builtins :as b]
   [simple-stackless-lisp.core :as core]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.util :as u]))

(defn run-file
  [filename]
  (let [text (str "(do " (slurp filename) ")")
        code (edn/read-string text)]
    (core/eval code)))

(defn run-repl []
  (println "============================")
  (println "|Simple Stackless Lisp REPL|")
  (println "============================")
  (let [env (env/fresh-env b/builtins)
        k   (fn [ret]
              (env/bind! env '%1 ret)
              (println "=>" (pr-str ret) "\n"))
        exe (u/executor)]
    (while true
      (try
        (print "> ")
        (flush)
        (core/eval (u/read-exp) env k exe)
        (catch Exception e
          (env/bind! env '*e e)
          (println "Error: " (.getMessage e)))))))

(defn -main
  [& [filename]]
  (if filename
    (run-file filename)
    (run-repl)))
