(ns simple-stackless-lisp.main
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.edn :as edn]
   [simple-stackless-lisp.core :as core]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.util :as u]
   [clojure.java.io :as io]))

(defn run-file
  [filename]
  (try
    (let [file (io/file filename)
          _ (when-not (.exists file)
              (u/throw+ "File not found:" filename))
          cwd  (-> file .getAbsoluteFile .getParent)
          text (str "(do " (slurp file) ")")
          code (edn/read-string text)
          ns-reg (env/fresh-ns-registry)
          _ (env/create-ns! ns-reg 'sclj.core core/builtins)
          _ (env/set-current-wd! ns-reg cwd)]
      (core/eval code ns-reg))
    (catch Exception e
      (println "Error: " (.getMessage e))
      (println "Writing error log to sclj-err-log.edn")
      (spit "sclj-err-log.edn" (pr-str e)))))

(defn run-repl []
  (let [ns-reg (env/fresh-ns-registry)
        env (env/create-ns! ns-reg 'sclj.core core/builtins)
        k   (fn [ret]
              (env/bind! env '*1 ret)
              (println "=>" (pr-str ret) "\n"))
        exe (u/executor)
        eval (fn [exp k]
               (core/eval exp ns-reg k exe))]
    (println "=====================================================")
    (println "|            Simple Stackless Lisp REPL             |")
    (println "|---------------------------------------------------|")
    (println "| https://github.com/divs1210/simple-stackless-lisp |")
    (println "=====================================================")
    (while true
      (try
        (print (str (env/current-ns env) "> "))
        (flush)
        (eval (u/read-exp) k)
        (catch Exception e
          (env/bind! env '*e e)
          (println "Error: " (.getMessage e)))))))

(defn -main
  [& [filename]]
  (if filename
    (run-file filename)
    (run-repl)))
