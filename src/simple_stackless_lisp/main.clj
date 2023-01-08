(ns simple-stackless-lisp.main
  (:gen-class)
  (:require
   [simple-stackless-lisp.builtins :as b]
   [simple-stackless-lisp.core :as core]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.multi :as m]
   [simple-stackless-lisp.reader :as r]
   [simple-stackless-lisp.types :as t]
   [simple-stackless-lisp.util :as u]))

(defn run-file
  [filename]
  (let [text (str "(do " (slurp filename) ")")
        code (r/read-string text)]
    (core/eval code)))

(defn run-repl []
  (println "============================")
  (println "|Simple Stackless Lisp REPL|")
  (println "============================")
  (let [env (env/fresh-env b/builtins)
        k   (fn [ret]
              (env/bind! env '%1 ret)
              (println "=>"
                       (t/string->java-string (m/k-to-readable-string identity ret))
                       "\n"))
        exe (u/executor)]
    (while true
      (try
        (print "> ")
        (flush)
        (core/eval (r/read-exp) env k exe)
        (catch Exception e
          (println "Error: " (.getMessage e))
          (println))))))

(defn -main
  [& [filename]]
  (if filename
    (run-file filename)
    (run-repl)))
