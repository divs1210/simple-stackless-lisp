(ns experiments.types
  (:refer-clojure :exclude [get])
  (:require [simple-stackless-lisp.types :as t]))

;; Interface
;; =========
(defmulti get
  (fn [this & _]
    (t/type this)))

(defmulti slice
  (fn [this & _]
    (t/type this)))

(defmulti put!
  (fn [this & _]
    (t/type this)))


;; Implementations
;; ===============
(defmethod get 'Array
  [this idx]
  (t/array-get this idx))

(defmethod slice 'Array
  [this from to]
  (t/array-slice this from to))

(defmethod put! 'Array
  [this idx val]
  (t/array-put! this idx val))

;; ===

(defmethod get 'ArrayWindow
  [this idx]
  (t/array-window-get this idx))

(defmethod put! 'ArrayWindow
  [this idx val]
  (t/array-window-put! this idx val))

(defmethod slice 'ArrayWindow
  [this from to]
  (t/array-window-slice this from to))
