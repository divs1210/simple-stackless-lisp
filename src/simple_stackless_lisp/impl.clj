(ns simple-stackless-lisp.impl
  (:require [clojure.walk :refer [postwalk]]
            [simple-stackless-lisp.env :as env]
            [simple-stackless-lisp.util :as u]
            [clojure.string :as str]))

(defn k-def
  [walk args env k GUARD]
  (let [[sym val-exp] args]
    (walk walk
          val-exp
          env
          (fn CC [val]
            (GUARD CC [val])
            (k (env/bind! (env/top-level env) sym val)))
          GUARD)))

(defn k-let
  [walk args env k GUARD]
  (let [[bindings body] args]
    (if (empty? bindings)
      (walk walk body env k GUARD)
      (let [[var val-exp & remaining] bindings
            env (env/extend! env {})
            then (fn CC [val]
                   (GUARD CC [val])
                   (env/bind! env var val)
                   (k-let walk [remaining body] env k GUARD))]
        (walk walk val-exp env then GUARD)))))

(defn k-if
  [walk args env k GUARD]
  (let [[test-exp then-exp else-exp] args]
    (walk walk
          test-exp
          env
          (fn CC [test-val]
            (GUARD CC [test-val])
            (walk walk
                  (if test-val then-exp else-exp)
                  env
                  k
                  GUARD))
          GUARD)))

(defn k-do
  [walk exps env k GUARD]
  (u/k-reduce (fn CC [_ exp then GUARD]
                (GUARD CC [nil exp then GUARD])
                (walk walk exp env then GUARD))
              nil
              exps
              k
              GUARD))

(defn k-quote
  "TODO: implement and use stackless postwalk"
  [walk [exp] env k GUARD]
  (k (postwalk (fn [node]
                 (if (seq? node)
                   (let [[op arg] node]
                     (if (= 'unquote op)
                       (walk walk arg env identity GUARD)
                       node))
                   node))
               exp)))

(defn k-fn
  [walk [[argv body-exp] macro?] env k GUARD]
  (k ^{::macro? macro?}
     (fn CC [k & args]
       (GUARD CC (cons k args))
       (let [params (zipmap argv args)
             fn-env (env/extend! env params)]
         (walk walk body-exp fn-env k GUARD)))))


(defn k-eval
  [walk [code-exp] env k GUARD]
  (letfn [(with-code [code]
            (GUARD with-code [code])
            (walk walk code env k GUARD))]
    (walk walk code-exp env with-code GUARD)))

(defn- k-apply-fn
  [walk [f arg-exps] env k GUARD]
  (letfn [(with-args [args]
            (GUARD with-args [args])
            (apply f (cons k args)))]
    (u/k-map (fn CC [x-exp with-x GUARD]
               (GUARD CC [x-exp with-x GUARD])
               (walk walk x-exp env with-x GUARD))
             arg-exps
             with-args
             GUARD)))

(defn- k-apply-macro
  [walk [m arg-exps] env k GUARD]
  (letfn [(with-new-exp [new-exp]
            (GUARD with-new-exp [new-exp])
            (walk walk new-exp env k GUARD))]
    (apply m (cons with-new-exp arg-exps))))

(defn k-apply
  [walk [f-exp arg-exps] env k GUARD]
  (letfn [(with-f [f]
            (GUARD with-f [f])
            (let [apply-fn (if (::macro? (meta f))
                             k-apply-macro
                             k-apply-fn)]
              (apply-fn walk [f arg-exps] env k GUARD)))]
    (walk walk f-exp env with-f GUARD)))
