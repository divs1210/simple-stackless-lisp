(ns simple-stackless-lisp.reader
  (:refer-clojure :exclude [read-string])
  (:require [clojure.edn :as edn]
            [simple-stackless-lisp.types :as t]
            [simple-stackless-lisp.util :refer [with-retry]]))

(defn read-string
  "Returns AST from given code string."
  [^String s]
  (edn/read-string
   {:readers {'char (fn [^String ch-str]
                      (t/char (.codePointAt ch-str 0) false))}}
   s))

(defn read-exp
  "Reads a multi-line edn expression from stdin."
  []
  (with-retry [text (read-line)]
    (read-string text)
    (catch RuntimeException e
      (if (= "EOF while reading"
             (.getMessage e))
        (retry (str text (read-line)))
        (throw e)))))
