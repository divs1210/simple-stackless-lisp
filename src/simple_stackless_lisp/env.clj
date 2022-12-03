(ns simple-stackless-lisp.env
  (:require [simple-stackless-lisp.util :refer [throw+]]))

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
  (if (::parent @env)
    (recur (::parent @env))
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

(defn fresh-ns-registry []
  (atom {::current-ns nil}))

(defn create-ns!
  "Creates ns if it doesn't exist.
  Sets `::current-ns` to this ns.
  Returns the env for this ns."
  ([registry name]
   (create-ns! registry name {}))
  ([registry name initial-bindings]
   (if-not (contains? @registry name)
     (let [bindings (assoc initial-bindings ::ns name)
           env (fresh-env bindings)]
       (swap! registry assoc
              name env
              ::current-ns name
              ::current-env env)
       env)
     (get @registry name))))

(defn current-env
  [ns-reg]
  (::current-env @ns-reg))

(defn set-current-env!
  [ns-reg env]
  (swap! ns-reg assoc
         ::current-env env)
  env)
