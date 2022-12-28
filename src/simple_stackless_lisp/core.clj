(ns simple-stackless-lisp.core
  (:refer-clojure :exclude [eval])
  (:require
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.impl :as impl]
   [simple-stackless-lisp.types :as t]
   [simple-stackless-lisp.util :as u :refer [->cps]]))

(defn walk
  [this exp env k GUARD]
  (GUARD walk [this exp env k GUARD])
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

(def builtins
  {;; Types
   ;; =====
   'type (->cps t/type)

   'instance?
   (fn [k t obj]
     (k (= t (t/type obj))))

   ;; Primitives
   ;; ==========
   'Nil     'Nil
   'Boolean 'Boolean
   'Number  'Number
   'String  'String
   'Symbol  'Symbol
   'Fn      'Fn

   ;; Arrays
   ;; ======
   'Array         'Array
   'array         (->cps t/array)
   'array-size    (->cps t/array-size)
   'array-get     (->cps t/array-get)
   'array-slice   (->cps t/array-slice)
   'array-set!    (->cps t/array-set!)
   'array-insert! (->cps t/array-insert!)

   ;; Vectors
   ;; =======
   'Vector        'Vector
   'vector        (->cps vector)
   'vector-size   (->cps t/vector-size)
   'vector-get    (->cps t/vector-get)
   'vector-slice  (->cps t/vector-slice)
   'vector-set    (->cps t/vector-set)
   'vector-insert (->cps t/vector-insert)

   ;; I/O
   ;; ===
   'print   (->cps print)
   'println (->cps println)

   ;; Misc
   ;; ====
   'identical? (->cps identical?)
   'gensym     (->cps gensym)

   'apply
   (fn [k f args]
     (apply f (cons k args)))

   'call-cc
   (fn [k f]
     (f k (fn CC [_ ret]
            (k ret))))

   ;; Logic
   ;; =====
   '= (->cps =)
   '< (->cps <)
   '> (->cps >)
   '<= (->cps <=)
   '>= (->cps >=)

   ;; Math
   ;; ====
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
     (execute walk [walk exp env k guard]))))
