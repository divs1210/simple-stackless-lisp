(ns simple-stackless-lisp.core
  (:refer-clojure :exclude [eval])
  (:require
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.impl :as impl]
   [simple-stackless-lisp.util :as u :refer [->cps]]))

(defn walk
  ([exp env k GUARD]
   (walk walk exp env k GUARD))
  ([this exp env k GUARD]
   (GUARD this [exp env k GUARD])
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

         ;; function call
         (impl/k-apply this [op args] env k GUARD)))

     :else
     (u/throw+ "Can't evaluate: " exp))))

(def builtins
  {'list  (->cps list)
   'first (->cps first)
   'rest  (->cps rest)
   'seq   (->cps seq)
   'seq?  (->cps seq?)
   'cons  (->cps cons)

   'print   (->cps print)
   'println (->cps println)

   'gensym (->cps gensym)

   'apply
   (fn [k f args]
     (apply f (cons k args)))

   'call-cc
   (fn [k f]
     (f k (fn CC [_ ret]
            (k ret))))

   '=  (->cps =)

   '== (->cps ==)
   '<  (->cps <)
   '>  (->cps >)
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
