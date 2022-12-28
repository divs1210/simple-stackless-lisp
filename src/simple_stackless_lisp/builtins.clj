(ns simple-stackless-lisp.builtins
  (:require
   [simple-stackless-lisp.types :as t]
   [simple-stackless-lisp.util :refer [->cps]]))

(def builtins
  {;; Types
   ;; =====
   'type (->cps t/type)

   'instance?
   (fn [k t obj]
     (k (= t (t/type obj))))

   ;; Primitives
   ;; ==========
   'Nil     'Nil
   'Boolean 'Boolean
   'Number  'Number
   'String  'String
   'Symbol  'Symbol
   'Keyword 'Keyword
   'Fn      'Fn

   ;; Arrays
   ;; ======
   'Array         'Array
   'array         (->cps t/array)
   'array-size    (->cps t/array-size)
   'array-get     (->cps t/array-get)
   'array-slice   (->cps t/array-slice)
   'array-put!    (->cps t/array-put!)
   'array-insert! (->cps t/array-insert!)

   ;; Array Windows
   ;; =============
   'ArrayWindow        'ArrayWindow
   'array-window       (->cps t/array-window)
   'array-window-size  (->cps t/array-size)
   'array-window-get   (->cps t/array-window-get)
   'array-window-slice (->cps t/array-window-slice)
   'array-window-put!  (->cps t/array-window-put!)

   ;; Vectors
   ;; =======
   'Vector        'Vector
   'vector        (->cps vector)
   'vector-size   (->cps t/vector-size)
   'vector-get    (->cps t/vector-get)
   'vector-slice  (->cps t/vector-slice)
   'vector-put    (->cps t/vector-put)
   'vector-insert (->cps t/vector-insert)

   ;; Mutable Hashmaps
   ;; ================
   'MutableHashMap          'MutableHashMap
   'mutable-hash-map        (->cps t/mutable-hash-map)
   'mutable-hash-map-size   (->cps t/mutable-hash-map-size)
   'mutable-hash-map-get    (->cps t/mutable-hash-map-get)
   'mutable-hash-map-put!   (->cps t/mutable-hash-map-put!)

   ;; Immutable Hashmaps
   ;; ==================
   'HashMap       'HashMap
   'hash-map      (->cps hash-map)
   'hash-map-size (->cps t/hash-map-size)
   'hash-map-get  (->cps t/hash-map-get)
   'hash-map-put! (->cps t/hash-map-put)

   ;; I/O
   ;; ===
   'print   (->cps print)
   'println (->cps println)

   ;; Misc
   ;; ====
   'identical? (->cps identical?)
   'gensym     (->cps gensym)

   'apply
   (fn [k f args]
     (apply f (cons k args)))

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
