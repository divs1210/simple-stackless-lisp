(ns simple-stackless-lisp.core
  (:refer-clojure :exclude [eval])
  (:require
   [simple-stackless-lisp.env :as env]
   [simple-stackless-lisp.impl :as impl]
   [simple-stackless-lisp.util :as u :refer [->cps]]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(defn walk
  [this exp ns-reg k GUARD]
  (GUARD this [this exp ns-reg k GUARD])
  (cond
    (or (number? exp)
        (string? exp))
    (k exp)

    (contains? #{nil true false} exp)
    (k exp)

    (symbol? exp)
    (k (env/lookup (env/current-env ns-reg) exp))

    (seq? exp)
    (let [[op & args] exp]
      (case op
        ns
        (do
          (env/create-ns! ns-reg (first args))
          (k nil))

        require
        (let [required-ns (first args)
              lib-path (str/split (name required-ns) #"\.")
              cwd (::env/current-wd @ns-reg)
              curr-ns (env/current-ns ns-reg)
              lib-file-path (str cwd "/"
                                 "modules/"
                                 (str/join "/" lib-path)
                                 ".sclj")
              lib-text (str "(do\n"
                            (slurp lib-file-path) "\n"
                            "(ns " curr-ns  "))")
              lib-code (edn/read-string lib-text)]
          (this this
                lib-code
                ns-reg
                (fn CC [_]
                  (GUARD CC [nil])
                  (let [curr-env (env/current-env ns-reg)
                        required-ns-env @(get @ns-reg required-ns)]
                    (swap! curr-env merge
                           (dissoc required-ns-env ::env/parent))
                    (k nil)))
                GUARD))

        def
        (impl/k-def this args ns-reg k GUARD)

        let
        (impl/k-let this args ns-reg k GUARD)

        if
        (impl/k-if this args ns-reg k GUARD)

        do
        (impl/k-do this args ns-reg k GUARD)

        quote
        (impl/k-quote this args ns-reg k GUARD)

        fn
        (impl/k-fn this [args false] ns-reg k GUARD)

        macro
        (impl/k-fn this [args true] ns-reg k GUARD)

        trace!
        (impl/k-trace! this args ns-reg k GUARD)

        eval
        (impl/k-eval this args ns-reg k GUARD)

        ;; function call
        (impl/k-apply this [op args] ns-reg k GUARD)))

    :else
    (u/throw+ "Can't evaluate: " exp)))

(def builtins
  {'list  (->cps list)
   'first (->cps first)
   'rest  (->cps rest)
   'next  (->cps next)
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
   (let [ns-reg (env/fresh-ns-registry)]
     (env/create-ns! ns-reg 'sclj.core builtins)
     (eval exp ns-reg)))
  ([exp ns-reg]
   (eval exp ns-reg identity))
  ([exp ns-reg k]
   (eval exp ns-reg k (u/executor)))
  ([exp ns-reg k exe]
   (let [{:keys [guard execute]} exe]
     (execute walk [walk exp ns-reg k guard]))))
