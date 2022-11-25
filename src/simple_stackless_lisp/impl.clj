(ns simple-stackless-lisp.impl
  (:require [clojure.walk :refer [postwalk]]
            [simple-stackless-lisp.env :as env]))

(defn k-def
  [walk args env k GUARD]
  (let [[sym val-exp] args]
    (walk val-exp
          env
          (fn CC [val]
            (GUARD CC [val])
            (k (env/bind! (env/top-level env) sym val)))
          GUARD)))

(defn k-let
  [walk args env k GUARD]
  (let [[bindings body] args]
    (if (empty? bindings)
      (walk body env k GUARD)
      (let [[var val-exp & remaining] bindings
            env (env/extend! env {})
            then (fn CC [val]
                   (GUARD CC [val])
                   (env/bind! env var val)
                   (k-let walk [remaining body] env k GUARD))]
        (walk val-exp env then GUARD)))))

(defn k-if
  [walk args env k GUARD]
  (let [[test-exp then-exp else-exp] args]
    (walk test-exp
          env
          (fn CC [test-val]
            (GUARD CC [test-val])
            (walk (if test-val then-exp else-exp)
                  env
                  k
                  GUARD))
          GUARD)))

(defn k-do
  [walk exps env k GUARD]
  (let [exps (vec exps)
        len (count exps)]
    ((fn -loop [last idx]
       (GUARD -loop [last idx])
       (if (< idx len)
         (walk (exps idx)
               env
               (fn CC [val]
                 (GUARD CC [val])
                 (-loop val (inc idx)))
               GUARD)
         (k last))) nil 0)))

(defn k-quote
  [walk [exp] env GUARD]
  (postwalk (fn [node]
              (if (seq? node)
                (let [[op arg] node]
                  (if (= 'unquote op)
                    (walk arg env identity GUARD)
                    node))
                node))
            exp))

(defn k-fn
  [walk [argv body-exp] macro? env GUARD]
  ^{:macro? macro?}
  (fn CC [k & args]
    (GUARD CC (cons k args))
    (let [params (zipmap argv args)
          fn-env (env/extend! env params)]
      (walk body-exp fn-env k GUARD))))

(defn k-call
  [walk exp env k GUARD]
  (let [[op & args] exp]
    (walk op
          env
          (fn CC [f]
            (GUARD CC [f])
            (if (:macro? (meta f))
              (letfn [(then [new-exp]
                        (walk new-exp env k GUARD))]
                (apply f (cons then args)))
              (let [arg-exps (vec args)
                    len (count arg-exps)]
                (letfn [(-loop [args idx]
                          (GUARD -loop [args idx])
                          (if (< idx len)
                            (walk (arg-exps idx)
                                  env
                                  (fn CC [arg]
                                    (GUARD CC [arg])
                                    (-loop (conj args arg) (inc idx)))
                                  GUARD)
                            (apply f args)))]
                  (-loop [k] 0)))))
          GUARD)))
