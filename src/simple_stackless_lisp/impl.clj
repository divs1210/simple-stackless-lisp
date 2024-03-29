(ns simple-stackless-lisp.impl
  (:require
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.multi :as m]
   [simple-stackless-lisp.util :as u]
   [simple-stackless-lisp.types :as t]))

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
             params (assoc params '%args (vec args))
             fn-env (env/extend! env params)]
         (walk walk body-exp fn-env k GUARD)))))

(defn k-trace!
  [walk [code-exp] env k GUARD]
  (let [top-level-env (env/top-level env)]
    (when (not (::stack-depth @top-level-env))
      (env/bind! top-level-env ::stack-depth (atom 0)))
    (letfn [(tracing-walk [_ t-exp _ t-k _]
              (let [stack-depth (::stack-depth @top-level-env)]
                (GUARD tracing-walk [tracing-walk t-exp env t-k GUARD])
                (print (str/join (repeat @stack-depth "│")))
                (prn t-exp)
                (swap! stack-depth inc)
                (walk tracing-walk
                      t-exp
                      env
                      (fn t-k+ [ret]
                        (GUARD t-k+ [ret])
                        (swap! stack-depth dec)
                        (print (str/join (repeat @stack-depth "│")))
                        (print "└>")
                        (println
                         (t/string->java-string
                          (m/k-to-readable-string identity ret)))
                        (t-k ret))
                      GUARD)))]
      (tracing-walk tracing-walk code-exp env k GUARD))))

(defn k-eval
  [walk [code-exp] env k GUARD]
  (letfn [(with-code [code]
            (GUARD with-code [code])
            (walk walk code env k GUARD))]
    (walk walk code-exp env with-code GUARD)))

(defn k-dot-notation
  [walk [op args] env k GUARD]
  (let [obj-exp  (first args)
        key-name (-> op str (.substring 1) symbol)
        new-exp (case key-name
                  __keys__     (list 'hash-map-keys obj-exp)
                  __has-key?__ (list 'hash-map-contains? obj-exp (second args))
                  ;; else
                  (list 'hash-map-get
                        obj-exp
                        (keyword key-name)
                        nil))]
    (walk walk new-exp env k GUARD)))

(defn- k-apply-fn
  [walk [f arg-exps] env k GUARD]
  (letfn [(with-args [args]
            (GUARD with-args [args])
            (m/k-apply k f args))]
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


;; Reader
;; ======
(defn k-read-vector
  [walk vlit env k GUARD]
  (letfn [(with-xs [xs]
            (GUARD with-xs [xs])
            (k (vec xs)))]
    (u/k-map (fn CC [x-exp with-x GUARD]
               (GUARD CC [x-exp with-x GUARD])
               (walk walk x-exp env with-x GUARD))
             vlit
             with-xs
             GUARD)))

(defn k-read-map
  [walk mlit env k GUARD]
  (letfn [(with-entries [entries]
            (GUARD with-entries [entries])
            (k (into {} entries)))]
    (u/k-map (fn CC [kv with-kv GUARD]
               (GUARD CC [kv with-kv GUARD])
               (k-read-vector walk kv env with-kv GUARD))
             mlit
             with-entries
             GUARD)))
