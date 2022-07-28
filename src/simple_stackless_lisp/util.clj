(ns simple-stackless-lisp.util
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defrecord Retry [bindings])

(defmacro with-retry
  "It's really inconvenient not being able to recur from within (catch)
  expressions. This macro wraps its body in a (loop [bindings] (try ...)).
  Provides a (retry & new bindings) form which is usable within (catch) blocks.
  When this form is returned by the body, the body will be retried with the new
  bindings.

  Taken from: https://gist.github.com/aphyr/a81825ab80656679db78"
  [initial-bindings & body]
  (assert (vector? initial-bindings))
  (assert (even? (count initial-bindings)))
  (let [bindings-count (/ (count initial-bindings) 2)
        body (walk/prewalk (fn [form]
                             (if (and (list? form)
                                      (= 'retry (first form)))
                               (do (assert (= bindings-count
                                              (count (rest form))))
                                   `(Retry. [~@(rest form)]))
                               form))
                           body)
        retval (gensym 'retval)]
    `(loop [~@initial-bindings]
       (let [~retval (try ~@body)]
        (if (instance? Retry ~retval)
          (recur ~@(->> (range bindings-count)
                        (map (fn [i] `(nth (.bindings ~retval) ~i)))))
          ~retval)))))

(defn- make-guard
  [last-call f args]
  (reset! last-call {:f f :args args}))

(defn- make-execute
  [last-call f args]
  (with-retry [f f
               args args]
    (apply f args)
    (catch StackOverflowError _
      (retry (:f @last-call)
             (:args @last-call)))))

(defn executor []
  (let [last-call (atom nil)]
    {:guard (partial make-guard last-call)
     :execute (partial make-execute last-call)}))

(defn throw+
  [& strs]
  (throw (Exception. (str/join strs))))

(defn ->cps
  [f]
  (fn [k & args]
    (k (apply f args))))

(defn read-exp []
  (with-retry [text (read-line)]
    (edn/read-string text)
    (catch RuntimeException e
      (let [msg (.getMessage e)]
        (if (= "EOF while reading" msg)
          (retry (str text (read-line)))
          (throw e))))))
