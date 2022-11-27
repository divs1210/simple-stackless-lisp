(ns simple-stackless-lisp.util
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import clojure.lang.ExceptionInfo))

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
                         (map (fn [i] `(nth (:bindings ~retval) ~i)))))
           ~retval)))))

(defn- make-guard
  [guard-state f args]
  (swap! guard-state update :stack-depth inc)
  (when (= (:stack-depth @guard-state)
           (:max-stack-depth @guard-state))
    (throw (ex-info "" {:type ::max-stack-depth-reached
                        :f f
                        :args args}))))

(defn- make-execute
  [guard-state f args]
  (with-retry [f f
               args args]
    (apply f args)
    (catch ExceptionInfo e
      (let [{:keys [type f args]} (ex-data e)]
        (if (= ::max-stack-depth-reached type)
          (do
            (swap! guard-state assoc :stack-depth 0)
            (retry f args))
          (throw e))))))

(defn executor
  [& {:keys [max-stack-depth]
      :or {max-stack-depth 200}}]
  (let [guard-state (atom {:stack-depth 0
                           :max-stack-depth max-stack-depth})]
    {:guard (partial make-guard guard-state)
     :execute (partial make-execute guard-state)}))

(defn throw+
  "Throws a generic error with the given message."
  [& strs]
  (throw (Exception. (str/join strs))))

(defn ->cps
  "Converts function to Continuation Passing Style.
  This code:
    ((->cps inc) println 1)
  prints 2."
  [f]
  (fn [k & args]
    (k (apply f args))))

(defn k-reduce
  "A stackless reduce implementation.
  `f` will be called like:
    (f acc x k GUARD)"
  [f acc xs k GUARD]
  (if-let [[x & remaining] (seq xs)]
    (letfn [(with-new-acc [new-acc]
              (GUARD with-new-acc [new-acc])
              (k-reduce f new-acc remaining k GUARD))]
      (f acc x with-new-acc GUARD))
    (k acc)))

(defn k-map
  "A stackless map implementation.
  Built on top of `k-reduce`."
  [f xs k GUARD]
  (k-reduce (fn CC [acc x then GUARD]
              (GUARD CC [acc x then GUARD])
              (f x
                 (fn CC [fx]
                   (GUARD CC [fx])
                   (then (conj acc fx)))
                 GUARD))
            []
            xs
            k
            GUARD))

(defn read-exp
  "Reads a multi-line edn expression from stdin."
  []
  (with-retry [text (read-line)]
    (edn/read-string text)
    (catch RuntimeException e
      (if (= "EOF while reading"
             (.getMessage e))
        (retry (str text (read-line)))
        (throw e)))))
