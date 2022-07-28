(ns simple-stackless-lisp.env
  (:require [simple-stackless-lisp.util :refer [->cps throw+]]))

(defn fresh-env []
  (atom {::parent nil

         'list  (->cps list)
         'first (->cps first)
         'rest  (->cps rest)
         'seq   (->cps seq)
         'cons  (->cps cons)

         'print   (->cps print)
         'println (->cps println)

         '= (->cps =)
         '< (->cps <)
         '> (->cps >)
         '<= (->cps <=)
         '>= (->cps >=)
         
         '+ (->cps +')
         '- (->cps -)
         '* (->cps *')
         '/ (->cps /)}))

(defn lookup
  [env symbol]
  (cond
    (contains? @env symbol)
    (get @env symbol)

    (::parent @env)
    (recur (::parent @env) symbol)

    :else
    (throw+ "Symbol " symbol " is unbound!")))

(defn extend!
  [env mapping]
  (let [new-env (atom mapping)]
    (swap! new-env
           assoc ::parent env)
    new-env))

(defn bind!
  [env sym val]
  (swap! env
         assoc sym val)
  nil)
