(ns simple-stackless-lisp.builtins
  (:refer-clojure :exclude [print println])
  (:require
   [simple-stackless-lisp.multi :as m]
   [simple-stackless-lisp.types :as t]
   [simple-stackless-lisp.util :refer [->cps]]
   [clojure.core :as core]))

(defn print
  [& objs]
  (let [unicode-strs (map #(m/k-to-string identity %) objs)
        joined-str (t/string-join (t/string []) unicode-strs)
        java-str (t/string->java-string joined-str)]
    (core/print java-str)
    (flush)))

(defn println
  [& objs]
  (apply print objs)
  (core/println))

(def builtins
  {;; Types
   ;; =====
   'type      (->cps t/type)
   'instance? (->cps t/instance?)

   ;; Bootstrapped Functions
   ;; ======================
   'apply      m/k-apply
   'multi      m/k-multi
   'method     m/k-method
   'methods    (->cps m/methods)
   'get-method (->cps m/get-method)

   ;; Primitives
   ;; ==========
   'Nil         'Nil
   'Boolean     'Boolean
   'Number      'Number
   'Symbol      'Symbol
   'Keyword     'Keyword
   'Fn          'Fn

   'MultiMethod         'MultiMethod
   'MethodNotFoundError 'MethodNotFoundError

   ;; Characters
   ;; ==========
   'Character     'Character
   'char          (->cps t/char)
   'char-nbsp?    (->cps t/char-non-breaking-whitespace?)
   'char-bsp?     (->cps t/char-breaking-whitespace?)
   'char-space?   (->cps t/char-whitespace?)
   'char-upcase   (->cps t/char-upcase)
   'char-downcase (->cps t/char-downcase)

   ;; Strings
   ;; =======
   'String        'String
   'string        (->cps t/string)
   'string-size   (->cps t/string-size)
   'string-get    (->cps t/string-get)
   'string-put    (->cps t/string-put)
   'string-slice  (->cps t/string-slice)
   'string-concat (->cps t/string-concat)
   'string-join   (->cps t/string-join)
   'string-blank? (->cps t/string-blank?)
   '->str         m/k-to-string
   '->rstr        m/k-to-readable-string

   ;; Vectors
   ;; =======
   'Vector        'Vector
   'vector        (->cps vector)
   'vector-size   (->cps t/vector-size)
   'vector-get    (->cps t/vector-get)
   'vector-put    (->cps t/vector-put)
   'vector-slice  (->cps t/vector-slice)
   'vector-concat (->cps t/vector-concat)

   ;; Hashmaps
   ;; ========
   'HashMap         'HashMap
   'hash-map        (->cps hash-map)
   'hash-map-size   (->cps t/hash-map-size)
   'hash-map-get    (->cps t/hash-map-get)
   'hash-map-put    (->cps t/hash-map-put)
   'hash-map-select (->cps t/hash-map-select)
   'hash-map-merge  (->cps t/hash-map-merge)

   ;; I/O
   ;; ===
   'print   (->cps print)
   'println (->cps println)

   ;; Misc
   ;; ====
   'identical? (->cps identical?)
   'gensym     (->cps gensym)

   'call-cc
   (fn [k f]
     (f k (fn CC [_ ret]
            (k ret))))

   'throw!
   (fn [k m]
     (throw (ex-info (:msg m) m))
     (k nil))

   ;; Logic
   ;; =====
   '= (->cps =)
   '< (->cps <)
   '> (->cps >)
   '<= (->cps <=)
   '>= (->cps >=)

   ;; Math
   ;; ====
   '+ (->cps +')
   '- (->cps -)
   '* (->cps *')
   '/ (->cps /)})
