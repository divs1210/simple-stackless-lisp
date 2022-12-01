(ns simple-stackless-lisp.main
  (:gen-class)
  (:require
   [clojure.edn :as edn]
   [simple-stackless-lisp.core :as core]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.util :as u]))

(defn run-file
  [filename]
  (try
    (let [text (str "(do " (slurp filename) ")")
          code (edn/read-string text)]
      (core/eval code))
    (catch Exception e
      (println "Error: " (.getMessage e))
      (println "Writing error log to sclj-err-log.edn")
      (spit "sclj-err-log.edn" (pr-str e)))))

(defn run-repl []
  (let [env (env/fresh-env core/builtins)
        k   (fn [ret]
              (env/bind! env '*1 ret)
              (println "=>" (pr-str ret) "\n"))
        exe (u/executor)]
    (env/bind! env
               'load-file
               (fn [k fname]
                 (let [text (str "(do " (slurp fname) ")")
                       code (edn/read-string text)]
                   (core/eval code
                              env
                              k
                              exe))))
    (println "=====================================================")
    (println "|            Simple Stackless Lisp REPL             |")
    (println "|---------------------------------------------------|")
    (println "| https://github.com/divs1210/simple-stackless-lisp |")
    (println "=====================================================")
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
