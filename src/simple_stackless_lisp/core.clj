(ns simple-stackless-lisp.core
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn]
            [simple-stackless-lisp.env :as env]
            [simple-stackless-lisp.util :as u]))

(declare walk)

(defn make-fn
  [[argv body] env GUARD]
  (fn CC [k & args]
    (GUARD CC (cons k args))
    (let [params (zipmap argv args)
          fn-env (env/extend! env params)]
      (walk body fn-env k GUARD))))

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
        def
        (let [[sym val-exp] args]
          (recur val-exp
                 env
                 (fn CC [val]
                   (GUARD CC [val])
                   (k (env/bind! env sym val)))
                 GUARD))

        fn
        (k (make-fn args env GUARD))

        if
        (let [[test-exp then-exp else-exp] args]
          (recur test-exp
                 env
                 (fn CC [test-val]
                   (GUARD CC [test-val])
                   (walk (if test-val then-exp else-exp)
                         env
                         k
                         GUARD))
                 GUARD))

        do
        (let [args (vec args)
              len (count args)]
          ((fn -loop [last idx]
             (GUARD -loop [last idx])
             (if (< idx len)
               (walk (args idx)
                     env
                     (fn CC [val]
                       (GUARD CC [val])
                       (-loop val (inc idx)))
                     GUARD)
               (k last))) nil 0))

        ;; function call
        (recur op
               env
               (fn CC [f]
                 (GUARD CC [f])
                 (let [arg-exps (vec args)
                       len (count arg-exps)]
                   ((fn -loop [args idx]
                      (GUARD -loop [args idx])
                      (if (< idx len)
                        (walk (arg-exps idx)
                              env
                              (fn CC [arg]
                                (GUARD CC [arg])
                                (-loop (conj args arg) (inc idx)))
                              GUARD)
                        (apply f args))) [k] 0)))
               GUARD)))

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
