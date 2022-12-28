(ns simple-stackless-lisp.types
  (:refer-clojure :exclude [type])
  (:require
   [simple-stackless-lisp.util :as u])
  (:import
   (clojure.lang IPersistentMap PersistentVector)
   (java.util Collection Vector)))

;; Types
;; =====
(declare array?)

(defn type [obj]
  (cond
    (number?  obj) 'Number
    (fn?      obj) 'Fn
    (nil?     obj) 'Nil
    (boolean? obj) 'Boolean
    (string?  obj) 'String
    (symbol?  obj) 'Symbol
    (array?   obj) 'Array
    (vector?  obj) 'Vector
    :else (or (-> obj meta :type)
              (u/throw+ "Don't know type of: " obj))))

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

(defn array-put!
  [^Vector array idx val]
  (.set array idx val)
  array)

(defn array-insert!
  [^Vector array idx val]
  (.add array idx val)
  array)


;; Array Windows
;; =============
(defn array-window
  [^Vector array from to]
  (locking array
    (let [size (.size array)]
      (assert (<= from to))
      (assert (<= 0 from (dec size)))
      (assert (<= 0 to size)))
    ^{:type 'ArrayWindow}
    {:array array
     :from  from
     :to    to}))

(defn- check-window-overflow
  [window min-idx idx max-idx]
  (let [{:keys [^Vector array from]} window
        size (.size array)]
    (assert (<= min-idx idx max-idx)
            (str "ArrayWindow overflow!\n"
                 "array-size: " size "\n"
                 "window-idx: " (- idx from) "\n"
                 "array-idx:  " idx))))

(defn array-window-size
  [^IPersistentMap window]
  (let [{:keys [from to]} window]
    (if (= to from)
      0
      (- to from))))

(defn array-window-get
  [^IPersistentMap window idx]
  (let [{:keys [^Vector array from to]} window]
    (locking array
      (let [size  (array-size array)
            from' (min from size)
            idx'  (+ from idx)
            to'   (min (dec to) (dec size))]
        (check-window-overflow window from' idx' to')
        (.get array idx')))))

(defn array-window-slice
  [^IPersistentMap window from to]
  (array-window (:array window)
                (+ from (:from window))
                (+ to   (:from window))))

(defn array-window-put!
  [^IPersistentMap window idx val]
  (let [{:keys [^Vector array from to]} window]
    (locking array
      (let [size  (array-size array)
            from' (min from size)
            idx'  (+ from idx)
            to'   (min (dec to) (dec size))]
        (check-window-overflow window from' idx' to')
        (array-put! array idx' val)
        window))))


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

(defn vector-put
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
