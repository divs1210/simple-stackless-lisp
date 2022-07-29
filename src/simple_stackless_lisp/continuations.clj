(ns simple-stackless-lisp.continuations
  (:require [simple-stackless-lisp.env :as env]))

(defn k-fn
  [walk [argv body-exp] env GUARD]
  (fn CC [k & args]
    (GUARD CC (cons k args))
    (let [params (zipmap argv args)
          fn-env (env/extend! env params)]
      (walk body-exp fn-env k GUARD))))

(defn k-def
  [walk args env k GUARD]
  (let [[sym val-exp] args]
    (walk val-exp
          env
          (fn CC [val]
            (GUARD CC [val])
            (k (env/bind! env sym val)))
          GUARD)))

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

(defn k-call
  [walk exp env k GUARD]
  (let [[op & args] exp]
    (walk op
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
