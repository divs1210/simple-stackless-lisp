(ns simple-stackless-lisp.core
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn]
            [simple-stackless-lisp.continuations :as k]
            [simple-stackless-lisp.env :as env]
            [simple-stackless-lisp.util :as u]))

(defn walk
  [exp env k GUARD]
  (GUARD walk [exp env k GUARD])
  (cond
    (or (number? exp)
        (string? exp))
    (k exp)

    (contains? #{nil true false} exp)
    (k exp)

    (symbol? exp)
    (k (env/lookup env exp))

    (seq? exp)
    (let [[op & args] exp]
      (case op
        fn
        (k (k/k-fn walk args env GUARD))

        def
        (k/k-def walk args env k GUARD)

        if
        (k/k-if walk args env k GUARD)

        do
        (k/k-do walk args env k GUARD)

        ;; function call
        (k/k-call walk exp env k GUARD)))

    :else
    (u/throw+ "Can't evaluate: " exp)))

(defn eval
  ([exp]
   (eval exp (env/fresh-env)))
  ([exp env]
   (eval exp env identity))
  ([exp env k]
   (eval exp env k (u/executor)))
  ([exp env k exe]
   (let [{:keys [guard execute]} exe]
     (execute walk [exp env k guard]))))

(defn repl []
  (let [env (env/fresh-env)
        k   #(println "=>" (pr-str %))
        exe (u/executor)]
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
