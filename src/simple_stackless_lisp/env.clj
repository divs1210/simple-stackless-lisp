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
  (let [user-env (fresh-env {::ns 'user})]
    (atom {::current-wd (System/getProperty "user.dir")
           ::current-ns 'user
           ::current-env user-env
           ::loaded-files #{}
           'user user-env})))

(defn create-ns!
  "Creates ns if it doesn't exist.
  Sets `::current-ns` to this ns.
  Returns the env for this ns."
  ([registry name]
   (create-ns! registry name {}))
  ([registry name initial-bindings]
   (if (contains? @registry name)
     (let [env (get @registry name)]
       (swap! registry assoc
              ::current-ns name
              ::current-env env)
       env)
     (let [bindings (assoc initial-bindings ::ns name)
           env (fresh-env bindings)]
       (swap! registry assoc
              name env
              ::current-ns name
              ::current-env env)
       env))))

(defn current-wd
  [ns-reg]
  (::current-wd @ns-reg))

(defn current-ns
  [ns-reg]
  (::current-ns @ns-reg))

(defn current-env
  [ns-reg]
  (::current-env @ns-reg))

(defn set-current-wd!
  [ns-reg cwd]
  (swap! ns-reg assoc ::current-wd cwd)
  nil)

(defn set-current-env!
  [ns-reg env]
  (swap! ns-reg assoc ::current-env env)
  env)

(defn file-loaded?
  [ns-reg filename]
  (contains? (::loaded-files @ns-reg) filename))
