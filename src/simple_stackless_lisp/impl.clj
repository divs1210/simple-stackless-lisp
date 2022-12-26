(ns simple-stackless-lisp.impl
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.util :as u]))

(defn k-ns [ns-reg args k]
  (env/create-ns! ns-reg (first args))
  (k nil))

(defn k-load-file!
  [walk filename ns-reg k GUARD]
  (let [cwd (env/current-wd ns-reg)
        text (str "(do " (slurp (str cwd "/" filename)) ")")
        code (edn/read-string text)]
    (walk walk code ns-reg k GUARD)))

(defn k-load-ns-file!
  [walk required-ns ns-reg k GUARD]
  (GUARD walk [required-ns ns-reg k GUARD])
  (let [lib-path (str/split (name required-ns) #"\.")
        lib-file-path (str "modules/" (str/join "/" lib-path) ".sclj")
        curr-ns (env/current-ns ns-reg)
        on-load (fn CC [_]
                  (GUARD CC [nil])
                  (env/create-ns! ns-reg curr-ns)
                  (swap! ns-reg update ::loaded-files conj lib-file-path)
                  (k nil))]
    (if-not (env/file-loaded? ns-reg lib-file-path)
      (k-load-file! walk lib-file-path ns-reg on-load GUARD)
      (k nil))))

(defn k-require
  [walk args ns-reg k GUARD]
  (GUARD k-require [walk args ns-reg k GUARD])
  (let [required-ns (first args)
        curr-env (env/current-env ns-reg)
        then (fn CC [_]
               (GUARD CC [nil])
               (let [required-ns-env @(get @ns-reg required-ns)]
                 (swap! curr-env merge
                        (dissoc required-ns-env ::env/parent))
                 (k nil)))]
    (k-load-ns-file! walk required-ns ns-reg then GUARD)))

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
             curr-env (env/current-env ns-reg)
             fn-env (env/extend! curr-env params)
             k+ (fn CC+ [res]
                  (GUARD CC+ [res])
                  (env/set-current-env! ns-reg curr-env)
                  (k res))]
         (env/set-current-env! ns-reg fn-env)
         (walk walk body-exp ns-reg k+ GUARD)))))

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
