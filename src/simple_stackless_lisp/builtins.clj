(ns simple-stackless-lisp.builtins
  (:refer-clojure :exclude [apply])
  (:require
   [simple-stackless-lisp.types :as t]
   [simple-stackless-lisp.util :refer [->cps]]
   [clojure.core :as core]))

(def ^:private multi-registry
  (t/mutable-hash-map))

(defn- method-not-found-error
  [name dispatch-val]
  (fn [k & _]
    (let [msg (str "No implementation of method: " name
                   " found for dispatch-val: " dispatch-val)]
      (throw (ex-info
              msg
              {:type 'MethodNotFoundError
               :multi name
               :dispatch-val dispatch-val
               :msg msg}))
      (k nil))))

(def k-apply
  (let [name "apply"

        implementations
        (t/mutable-hash-map)

        dispatch
        (fn [with-result & args]
          (let [dispatch-val (t/type (first args))
                default-impl (method-not-found-error name dispatch-val)
                k-impl (t/mutable-hash-map-get implementations
                                               [dispatch-val]
                                               default-impl)]
            (core/apply k-impl (cons with-result args))))]
    (t/mutable-hash-map-put! multi-registry
                             [dispatch]
                             {:name name
                              :implementations implementations})
    (t/mutable-hash-map-put! implementations
                             ['Fn]
                             (fn [k f args]
                               (core/apply f (cons k args))))
    dispatch))

(defn- k-multi
  [k name k-args->dispatch-val opts]
  (let [default-impl (:default-impl opts)
        implementations (t/mutable-hash-map)

        dispatch
        (fn [with-result & args]
          (k-apply
           (fn [dispatch-val]
             (let [k-impl (t/mutable-hash-map-get implementations [dispatch-val] default-impl)
                   k-impl (or k-impl (method-not-found-error name dispatch-val))]
               (k-apply with-result k-impl args)))
           k-args->dispatch-val
           args))]
    (t/mutable-hash-map-put! multi-registry
                             [dispatch]
                             {:name name
                              :implementations implementations})
    (k dispatch)))

(defn- k-method
  [k multi dispatch-val k-impl]
  (let [multi-record (t/mutable-hash-map-get multi-registry [multi] nil)
        impls (:implementations multi-record)]
    (t/mutable-hash-map-put! impls [dispatch-val] k-impl)
    (k nil)))

(def builtins
  {;; Types
   ;; =====
   'type   (->cps t/type)
   'multi  k-multi
   'method k-method
   'apply  k-apply

   'instance?
   (fn [k t obj]
     (k (= t (t/type obj))))

   'MethodNotFoundError 'MethodNotFoundError

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
