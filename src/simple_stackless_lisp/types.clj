(ns simple-stackless-lisp.types
  (:refer-clojure :exclude [type])
  (:require
   [simple-stackless-lisp.util :as u])
  (:import
   (clojure.lang PersistentVector)
   (java.util Collection Vector)))

;; Types
;; =====
(declare array?)

(defn type [obj]
  (cond
    (nil?     obj) 'Nil
    (boolean? obj) 'Boolean
    (number?  obj) 'Number
    (string?  obj) 'String
    (symbol?  obj) 'Symbol
    (fn?      obj) 'Fn
    (array?   obj) 'Array
    (vector?  obj) 'Vector
    :else (u/throw+ "Don't know type of: " obj)))

;; Arrays
;; ======
;; backed by Java's Vectors
(defn array
  [& xs]
  (Vector. ^Collection xs))

(defn array?
  [^Object obj]
  (instance? Vector obj))

(defn array-size
  [^Vector array]
  (.size array))

(defn array-get
  [^Vector array idx]
  (.get array idx))

(defn array-slice
  [^Vector array from to]
  (Vector. (.subList array from to)))

(defn array-set!
  [^Vector array idx val]
  (.set array idx val)
  array)

(defn array-insert!
  [^Vector array idx val]
  (.add array idx val)
  array)

;; Vectors
;; =======
;; backed by Clojure's Persistent Vectors
(defn vector-size
  [^PersistentVector v]
  (.size v))

(defn vector-get
  [^PersistentVector v idx]
  (v idx))

(defn vector-slice
  [^PersistentVector v from to]
  (subvec v from to))

(defn vector-set
  [^PersistentVector v idx val]
  (assoc v idx val))

(defn vector-insert
  [^PersistentVector v idx val]
  (let [size (.size v)]
    (assert (<= 0 idx size)
            (str "Index " idx " out of bounds for length " size))
    (let [till-idx  (subvec v 0 idx)
          with-idx  (conj till-idx val)
          after-idx (subvec v idx)]
      (into with-idx after-idx))))
