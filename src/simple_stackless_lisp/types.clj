(ns simple-stackless-lisp.types
  (:refer-clojure :exclude [char type instance?])
  (:require
   [clojure.core :as core]
   [clojure.string :as str]
   [simple-stackless-lisp.util :as u])
  (:import
   (clojure.lang Atom IPersistentMap PersistentVector)))

;; Types
;; =====
(defn atom? [obj]
  (core/instance? Atom obj))

(defn typed-map? [obj]
  (and (map? obj)
       (contains? obj :type)))

(defn multimethod? [obj]
  (and (fn? obj)
       (:multimethod? (meta obj))))

(defn type [obj]
  (cond
    (nil?         obj) 'Nil
    (boolean?     obj) 'Boolean
    (number?      obj) 'Number
    (symbol?      obj) 'Symbol
    (keyword?     obj) 'Keyword
    (vector?      obj) 'Vector
    (atom?        obj) 'Atom
    (multimethod? obj) 'MultiMethod
    (fn?          obj) 'Fn
    (typed-map?   obj) (:type obj)
    (map?         obj) 'HashMap
    :else (u/throw+ "Don't know type of: " obj)))

(defn instance?
  [t obj]
  (= t (type obj)))

;; Array Windows
;; =============
;; (defn array-window
;;   [^Vector array from to]
;;   (locking array
;;     (let [size (.size array)]
;;       (assert (<= from to))
;;       (assert (<= 0 from (dec size)))
;;       (assert (<= 0 to size)))
;;     {:type 'ArrayWindow
;;      :array array
;;      :from  from
;;      :to    to}))

;; (defn- check-window-overflow
;;   [window min-idx idx max-idx]
;;   (let [{:keys [^Vector array from]} window
;;         size (.size array)]
;;     (assert (<= min-idx idx max-idx)
;;             (str "ArrayWindow overflow!\n"
;;                  "array-size: " size "\n"
;;                  "window-idx: " (- idx from) "\n"
;;                  "array-idx:  " idx))))

;; (defn array-window-size
;;   [^IPersistentMap window]
;;   (let [{:keys [from to]} window]
;;     (if (= to from)
;;       0
;;       (- to from))))

;; (defn array-window-get
;;   [^IPersistentMap window idx]
;;   (let [{:keys [^Vector array from to]} window]
;;     (locking array
;;       (let [size  (array-size array)
;;             from' (min from size)
;;             idx'  (+ from idx)
;;             to'   (min (dec to) (dec size))]
;;         (check-window-overflow window from' idx' to')
;;         (.get array idx')))))

;; (defn array-window-slice
;;   [^IPersistentMap window from to]
;;   (array-window (:array window)
;;                 (+ from (:from window))
;;                 (+ to   (:from window))))

;; (defn array-window-put!
;;   [^IPersistentMap window idx val]
;;   (let [{:keys [^Vector array from to]} window]
;;     (locking array
;;       (let [size  (array-size array)
;;             from' (min from size)
;;             idx'  (+ from idx)
;;             to'   (min (dec to) (dec size))]
;;         (check-window-overflow window from' idx' to')
;;         (array-put! array idx' val)
;;         window))))


;; Immutable Vectors
;; =================
;; backed by Clojure's PersistentVectors
(defn vector-size
  [^PersistentVector v]
  (.size v))

(defn vector-get
  [^PersistentVector v idx]
  (v idx))

(defn vector-put
  [^PersistentVector v idx val]
  (assoc v idx val))

(defn vector-slice
  [^PersistentVector v from to]
  (subvec v from to))

(defn vector-concat
  [^PersistentVector this ^PersistentVector that]
  (into this that))


;; Immutable Hashmaps
;; ==================
;; backed by Clojure's PersistentMaps
(defn hash-map-size
  [^IPersistentMap m]
  (count m))

(defn hash-map-get
  ([m key]
   (hash-map-get m key nil))
  ([^IPersistentMap m key not-found]
   (get m key not-found)))

(defn hash-map-put
  [^IPersistentMap m key val]
  (assoc m key val))

(defn hash-map-select
  [^IPersistentMap m keys]
  (select-keys m keys))

(defn hash-map-merge
  [^IPersistentMap this ^IPersistentMap that]
  (merge this that))

(defn hash-map-keys
  [^IPersistentMap m]
  (try
    (vec (keys m))
    (catch Throwable _
      ;; Handles cases where .__keys__
      ;; is called on buitin types
      ;; like numbers and vectors.
      [])))

(defn hash-map-contains?
  [^IPersistentMap m key]
  (try
    (contains? m key)
    (catch Throwable _
      ;; Handles cases where .__has-key?__
      ;; is called on buitin types
      ;; like numbers and vectors.
      false)))


;; Unicode Code Points
;; ===================
(defn code-point-valid?
  [^Integer i]
  (Character/isValidCodePoint i))

(defn- code-point-non-breaking-whitespace?
  [^Integer cp]
  (contains? #{\u00A0 \u2007 \u2060 \u202F}
             (core/char cp)))

(defn- code-point-breaking-whitespace?
  [^Integer cp]
  (Character/isWhitespace cp))

(defn- code-point-whitespace?
  [^Integer cp]
  (or (code-point-non-breaking-whitespace? cp)
      (code-point-breaking-whitespace? cp)))

(defn- code-point-upcase
  [^Integer cp]
  (Character/toUpperCase cp))

(defn- code-point-downcase
  [^Integer cp]
  (Character/toLowerCase cp))

(defn- code-point->java-string
  [^Integer cp]
  (Character/toString cp))


;; Unicode Characters
;; ==================
(defn char
  ([cp]
   (char cp true))
  ([^Integer cp check?]
   (when check?
     (assert (code-point-valid? cp)
             (str "Not a Unicode code point: " cp)))
   {:type 'Character
    :code-point cp}))

(defn char-non-breaking-whitespace?
  [{:keys [^Integer code-point]}]
  (code-point-non-breaking-whitespace? code-point))

(defn char-breaking-whitespace?
  [{:keys [^Integer code-point]}]
  (code-point-breaking-whitespace? code-point))

(defn char-whitespace?
  [{:keys [^Integer code-point]}]
  (code-point-whitespace? code-point))

(defn char-upcase
  [{:keys [^Integer code-point]}]
  (char (code-point-upcase code-point)))

(defn char-downcase
  [{:keys [^Integer code-point]}]
  (char (code-point-downcase code-point)))

(defn char->java-string
  [{:keys [^Integer code-point]}]
  (code-point->java-string code-point))


;; Immutable Unicode Strings
;; =========================
(defn string
  [^PersistentVector chars]
  {:type 'String
   :chars chars})

(defn string-size
  [{:keys [chars]}]
  (vector-size chars))

(defn string-get
  [{:keys [chars]} idx]
  (chars idx))

(defn string-put
  [{:keys [chars]} idx char]
  (string (vector-put chars idx char)))

(defn string-slice
  [{:keys [chars]} from to]
  (string (vector-slice chars from to)))

(defn string-concat
  [this that]
  (let [this-chars (:chars this)
        that-chars (:chars that)]
    (string (vector-concat this-chars that-chars))))

(defn string-blank?
  [{:keys [chars]}]
  (every? char-whitespace? chars))

(defn string-join
  [separator strings]
  (loop [acc (string [])
         remaining strings]
    (if-let [[s & more] (seq remaining)]
      (if (seq more)
        (recur (-> acc
                   (string-concat s)
                   (string-concat separator))
               more)
        (string-concat acc s))
      acc)))

(defn java-string->string
  [^String s]
  (->> s
       (.codePoints)
       (.boxed)
       (.toList)
       (mapv #(char % false))
       (string)))

(defn string->java-string
  [{:keys [chars]}]
  (->> chars
       (map char->java-string)
       (str/join)))

(defn java-string-escape
  [^String s]
  (-> s
      (.replace "\\", "\\\\")
      (.replace "\t", "\\t")
      (.replace "\b", "\\b")
      (.replace "\n", "\\n")
      (.replace "\r", "\\r")
      (.replace "\f", "\\f")
      (.replace "\"", "\\\"")))

(defn string-escape
  [s]
  (-> s
      string->java-string
      java-string-escape
      java-string->string))


;; Atoms
;; =====
;; backed by Clojure's Atoms
(defn atom-deref
  [^Atom a]
  @a)

(defn atom-set!
  [^Atom a x]
  (reset! a x))

(defn atom-cas!
  [^Atom a oldval newval]
  (compare-and-set! a oldval newval))

(defn k-atom-swap!
  [k ^Atom a k-f]
  (k (swap! a (partial k-f identity))))
