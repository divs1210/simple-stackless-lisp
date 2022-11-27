(ns simple-stackless-lisp.core
  (:refer-clojure :exclude [eval])
  (:require
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.impl :as impl]
   [simple-stackless-lisp.util :as u :refer [->cps]]))

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
        (impl/k-def walk args env k GUARD)

        let
        (impl/k-let walk args env k GUARD)

        if
        (impl/k-if walk args env k GUARD)

        do
        (impl/k-do walk args env k GUARD)

        quote
        (impl/k-quote walk args env k GUARD)

        fn
        (impl/k-fn walk [args false] env k GUARD)

        macro
        (impl/k-fn walk [args true] env k GUARD)

        eval
        (impl/k-eval walk args env k GUARD)

        ;; function call
        (impl/k-apply walk exp env k GUARD)))

    :else
    (u/throw+ "Can't evaluate: " exp)))

(def builtins
  {'list  (->cps list)
   'first (->cps first)
   'rest  (->cps rest)
   'seq   (->cps seq)
   'cons  (->cps cons)

   'print   (->cps print)
   'println (->cps println)

   'gensym  (->cps gensym)

   'call-cc
   (fn [k f]
     (f k (fn CC [_ ret]
            (k ret))))

   '= (->cps =)
   '< (->cps <)
   '> (->cps >)
   '<= (->cps <=)
   '>= (->cps >=)

   '+ (->cps +')
   '- (->cps -)
   '* (->cps *')
   '/ (->cps /)})

(defn eval
  ([exp]
   (eval exp (env/fresh-env builtins)))
  ([exp env]
   (eval exp env identity))
  ([exp env k]
   (eval exp env k (u/executor)))
  ([exp env k exe]
   (let [{:keys [guard execute]} exe]
     (execute walk [exp env k guard]))))
