(ns simple-stackless-lisp.core
  (:refer-clojure :exclude [eval])
  (:require
   [simple-stackless-lisp.builtins :as b]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.impl :as impl]
   [simple-stackless-lisp.util :as u]))

(defn walk
  [this exp env k GUARD]
  (GUARD walk [this exp env k GUARD])
  (cond
    (or (number? exp)
        (string? exp)
        (keyword? exp))
    (k exp)

    (contains? #{nil true false} exp)
    (k exp)

    (symbol? exp)
    (k (env/lookup env exp))

    (vector? exp)
    (impl/k-read-vector this exp env k GUARD)

    (map? exp)
    (impl/k-read-map this exp env k GUARD)

    (seq? exp)
    (let [[op & args] exp]
      (case op
        def
        (impl/k-def this args env k GUARD)

        let
        (impl/k-let this args env k GUARD)

        if
        (impl/k-if this args env k GUARD)

        do
        (impl/k-do this args env k GUARD)

        quote
        (impl/k-quote this args env k GUARD)

        fn
        (impl/k-fn this [args false] env k GUARD)

        macro
        (impl/k-fn this [args true] env k GUARD)

        trace!
        (impl/k-trace! this args env k GUARD)

        eval
        (impl/k-eval this args env k GUARD)

        ;; function / macro call
        (impl/k-apply this [op args] env k GUARD)))

    :else
    (u/throw+ "Can't evaluate: " exp)))

(defn eval
  ([exp]
   (eval exp (env/fresh-env b/builtins)))
  ([exp env]
   (eval exp env identity))
  ([exp env k]
   (eval exp env k (u/executor)))
  ([exp env k exe]
   (let [{:keys [guard execute]} exe]
     (execute walk [walk exp env k guard]))))
