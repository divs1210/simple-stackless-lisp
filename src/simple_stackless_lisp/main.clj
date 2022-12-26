(ns simple-stackless-lisp.main
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [simple-stackless-lisp.core :as core]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.util :as u]))

(defn run-file
  [filename]
  (try
    (let [file (io/file filename)
          _    (when-not (.exists file)
                 (u/throw+ "File not found:" filename))
          cwd  (-> file .getAbsoluteFile .getParent)
          text (str "(do " (slurp file) ")")
          code (edn/read-string text)
          ns-reg (env/fresh-ns-registry)]
      (env/create-ns! ns-reg 'sclj.core core/builtins)
      (env/create-ns! ns-reg 'user)
      (env/set-current-wd! ns-reg cwd)
      (core/eval code ns-reg))
    (catch Exception e
      (println "Error: " (.getMessage e))
      (println "Writing error log to sclj-err-log.edn")
      (spit "sclj-err-log.edn" (pr-str e)))))

(defn run-repl []
  (println "=====================================================")
  (println "|            Simple Stackless Lisp REPL             |")
  (println "|---------------------------------------------------|")
  (println "| https://github.com/divs1210/simple-stackless-lisp |")
  (println "=====================================================")  
  (let [ns-reg (env/fresh-ns-registry)]
    (env/create-ns! ns-reg 'sclj.core core/builtins)
    (env/create-ns! ns-reg 'user)
    (while true
      (try
        (print (str (env/current-ns ns-reg) "> "))
        (flush)
        (let [ret (core/eval (u/read-exp) ns-reg)]
          (env/bind! (env/current-env ns-reg) '*1 ret)
          (println "=>" (pr-str ret) "\n"))
        (catch Exception e
          (env/bind! (env/current-env ns-reg) '*e e)
          (println "Error: " (.getMessage e)))))))

(defn -main
  [& [filename]]
  (if filename
    (run-file filename)
    (run-repl)))
