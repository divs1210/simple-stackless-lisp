(ns simple-stackless-lisp.impl
  (:require [clojure.walk :refer [postwalk]]
            [simple-stackless-lisp.env :as env]
            [simple-stackless-lisp.util :as u]
            [clojure.string :as str]))

(defn k-def
  [walk args ns-reg k GUARD]
  (let [[sym val-exp] args]
    (walk walk
          val-exp
          ns-reg
          (fn CC [val]
            (GUARD CC [val])
            (k (env/bind! (env/top-level (env/current-env ns-reg)) sym val)))
          GUARD)))

(defn k-let
  [walk args ns-reg k GUARD]
  (let [[bindings body] args]
    (if (empty? bindings)
      (walk walk body ns-reg k GUARD)
      (let [[var val-exp & remaining] bindings
            curr-env (env/current-env ns-reg)
            env (env/extend! curr-env {})
            _   (env/set-current-env! ns-reg env)
            then (fn CC [val]
                   (GUARD CC [val])
                   (env/bind! env var val)
                   (k-let walk [remaining body] ns-reg k GUARD))]
        (walk walk val-exp ns-reg then GUARD)))))

(defn k-if
  [walk args ns-reg k GUARD]
  (let [[test-exp then-exp else-exp] args]
    (walk walk
          test-exp
          ns-reg
          (fn CC [test-val]
            (GUARD CC [test-val])
            (walk walk
                  (if test-val then-exp else-exp)
                  ns-reg
                  k
                  GUARD))
          GUARD)))

(defn k-do
  [walk exps ns-reg k GUARD]
  (u/k-reduce (fn CC [_ exp then GUARD]
                (GUARD CC [nil exp then GUARD])
                (walk walk exp ns-reg then GUARD))
              nil
              exps
              k
              GUARD))

(defn k-quote
  "TODO: implement and use stackless postwalk"
  [walk [exp] ns-reg k GUARD]
  (k (postwalk (fn [node]
                 (if (seq? node)
                   (let [[op arg] node]
                     (if (= 'unquote op)
                       (walk walk arg ns-reg identity GUARD)
                       node))
                   node))
               exp)))

(defn k-fn
  [walk [[argv body-exp] macro?] ns-reg k GUARD]
  (k ^{::macro? macro?}
     (fn CC [k & args]
       (GUARD CC (cons k args))
       (let [params (zipmap argv args)
             fn-env (env/extend! (env/current-env ns-reg) params)]
         (env/set-current-env! ns-reg fn-env)
         (walk walk body-exp ns-reg k GUARD)))))

(defn k-trace!
  [walk [code-exp] ns-reg k GUARD]
  (let [top-level-env (env/top-level (env/current-env ns-reg))]
    (when (not (::stack-depth @top-level-env))
      (env/bind! top-level-env ::stack-depth (atom 0)))
    (letfn [(tracing-walk [_ t-exp _ t-k _]
              (let [stack-depth (::stack-depth @top-level-env)]
                (GUARD tracing-walk [tracing-walk t-exp ns-reg t-k GUARD])
                (print (str/join (repeat @stack-depth "│")))
                (prn t-exp)
                (swap! stack-depth inc)
                (walk tracing-walk
                      t-exp
                      ns-reg
                      (fn t-k+ [ret]
                        (GUARD t-k+ [ret])
                        (swap! stack-depth dec)
                        (print (str/join (repeat @stack-depth "│")))
                        (print "└>")
                        (prn ret)
                        (t-k ret))
                      GUARD)))]
      (tracing-walk tracing-walk code-exp ns-reg k GUARD))))

(defn k-eval
  [walk [code-exp] ns-reg k GUARD]
  (letfn [(with-code [code]
            (GUARD with-code [code])
            (walk walk code ns-reg k GUARD))]
    (walk walk code-exp ns-reg with-code GUARD)))

(defn- k-apply-fn
  [walk [f arg-exps] ns-reg k GUARD]
  (letfn [(with-args [args]
            (GUARD with-args [args])
            (apply f (cons k args)))]
    (u/k-map (fn CC [x-exp with-x GUARD]
               (GUARD CC [x-exp with-x GUARD])
               (walk walk x-exp ns-reg with-x GUARD))
             arg-exps
             with-args
             GUARD)))

(defn- k-apply-macro
  [walk [m arg-exps] ns-reg k GUARD]
  (letfn [(with-new-exp [new-exp]
            (GUARD with-new-exp [new-exp])
            (walk walk new-exp ns-reg k GUARD))]
    (apply m (cons with-new-exp arg-exps))))

(defn k-apply
  [walk [f-exp arg-exps] ns-reg k GUARD]
  (letfn [(with-f [f]
            (GUARD with-f [f])
            (let [apply-fn (if (::macro? (meta f))
                             k-apply-macro
                             k-apply-fn)]
              (apply-fn walk [f arg-exps] ns-reg k GUARD)))]
    (walk walk f-exp ns-reg with-f GUARD)))
