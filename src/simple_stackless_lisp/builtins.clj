(ns simple-stackless-lisp.builtins
  (:refer-clojure :exclude [methods get-method])
  (:require
   [clojure.core :as core]
   [simple-stackless-lisp.types :as t]
   [simple-stackless-lisp.util :refer [->cps]]))

(def ^:private multi-registry
  (t/mutable-hash-map))

(defn- k-default-method
  [k name dispatch-val & args]
  (let [msg (str "No implementation of method: " name
                 " found for dispatch-val: " dispatch-val)]
    (throw (ex-info
            msg
            {:type 'MethodNotFoundError
             :multi name
             :dispatch-val dispatch-val
             :msg msg}))
    (k nil)))

(defn- make-k-default-method
  [name]
  (fn [k dispatch-val & args]
    (core/apply k-default-method k name dispatch-val args)))

(def k-apply
  (let [name "apply"

        implementations
        (t/mutable-hash-map)

        dispatch
        (with-meta
          (fn [with-result & args]
            (let [dispatch-val (t/type (first args))]
              (if-let [k-impl (t/mutable-hash-map-get implementations [dispatch-val] nil)]
                (core/apply k-impl (cons with-result args))
                (let [k-default-impl (t/mutable-hash-map-get implementations [:MultiMethod/default] nil)]
                  (core/apply k-default-impl with-result dispatch-val args)))))
          {:multimethod? true})]
    (t/mutable-hash-map-put! multi-registry
                             [dispatch]
                             {:name name
                              :implementations implementations})
    (t/mutable-hash-map-put! implementations
                             [:MultiMethod/default]
                             (make-k-default-method name))
    (t/mutable-hash-map-put! implementations
                             ['Fn]
                             (fn [k f args]
                               (core/apply f (cons k args))))
    (t/mutable-hash-map-put! implementations
                             ['MultiMethod]
                             (fn [k f args]
                               (core/apply f (cons k args))))
    dispatch))

(defn- k-multi
  [k name k-args->dispatch-val]
  (let [implementations (t/mutable-hash-map)

        dispatch
        (with-meta
          (fn [with-result & args]
            (k-apply
             (fn [dispatch-val]
               (if-let [k-impl (t/mutable-hash-map-get implementations [dispatch-val] nil)]
                 (k-apply with-result k-impl args)
                 (let [k-default-impl (t/mutable-hash-map-get implementations [:MultiMethod/default] nil)]
                   (k-apply with-result k-default-impl (cons dispatch-val args)))))
             k-args->dispatch-val
             args))
          {:multimethod? true})]
    (t/mutable-hash-map-put! multi-registry
                             [dispatch]
                             {:name name
                              :implementations implementations})
    (t/mutable-hash-map-put! implementations
                             [:MultiMethod/default]
                             (make-k-default-method name))
    (k dispatch)))

(defn- k-method
  [k multi dispatch-val k-impl]
  (let [multi-record (t/mutable-hash-map-get multi-registry [multi] nil)
        impls (:implementations multi-record)]
    (t/mutable-hash-map-put! impls [dispatch-val] k-impl)
    (k nil)))

(defn- multi-info
  [multi]
  (some-> multi-registry
          (t/mutable-hash-map-get [multi] nil)
          (update :implementations t/mutable-hash-map-snapshot)))

(defn- multi-display-info
  [multi]
  (let [{:keys [name implementations]} (multi-info multi)]
    {:name name
     :dispatch-vals (vec (keys implementations))}))

(defn methods
  [multi]
  (-> multi multi-info :implementations))

(defn get-method
  [multi dispatch-val]
  (let [impls (methods multi)]
    (get impls dispatch-val (:MultiMethod/default impls))))

;; Primitive multimethods
;; ======================
(def k-to-string
  (k-multi identity "->str"
   (fn [k obj]
     (k (t/type obj)))))

;; Number, Boolean, String, Symbol, Keyword
(k-method identity k-to-string :MultiMethod/default
  (fn [k _ obj]
    (k (str obj))))

(k-method identity k-to-string 'Nil
  (fn [k n]
    (k "nil")))

(k-method identity k-to-string 'Fn
  (fn [k f]
    (k (str "#Fn"))))

(k-method identity k-to-string 'MultiMethod
  (fn [k m]
    (let [info (multi-display-info m)
          info-str (k-to-string identity info)]
      (k (str "#MultiMethod" info-str)))))

(k-method identity k-to-string 'MutableHashMap
  (fn [k m]
    (k-to-string k (t/mutable-hash-map-snapshot m))))

;; Core library
;; ============
(def builtins
  {;; Types
   ;; =====
   'type      (->cps t/type)
   'instance? (->cps t/instance?)

   ;; Bootstrapped Functions
   ;; ======================
   'apply      k-apply
   'multi      k-multi
   'method     k-method
   'methods    (->cps methods)
   'get-method (->cps get-method)

   ;; Primitives
   ;; ==========
   'Nil         'Nil
   'Boolean     'Boolean
   'Number      'Number
   'String      'String
   'Symbol      'Symbol
   'Keyword     'Keyword
   'Fn          'Fn

   'MultiMethod         'MultiMethod
   'MethodNotFoundError 'MethodNotFoundError

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
   '/ (->cps /)

   ;; Strings
   ;; =======
   '->str k-to-string})
