(ns simple-stackless-lisp.env
  (:require [simple-stackless-lisp.util :refer [->cps throw+]]))

(defn lookup
  [env symbol]
  (cond
    (contains? @env symbol)
    (get @env symbol)

    (::parent @env)
    (recur (::parent @env) symbol)

    :else
    (throw+ "Symbol " symbol " is unbound!")))

(defn top-level
  [env]
  (if (::parent env)
    (recur (::parent env))
    env))

(defn extend!
  [env mapping]
  (atom (assoc mapping ::parent env)))

(defn bind!
  [env sym val]
  (swap! env
         assoc sym val)
  nil)

(defn fresh-env
  ([]
   (fresh-env {}))
  ([initial-bindings]
   (extend! nil initial-bindings)))
