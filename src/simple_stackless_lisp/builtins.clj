(ns simple-stackless-lisp.builtins
  (:refer-clojure :exclude [methods get-method])
  (:require
   [clojure.core :as core]
   [simple-stackless-lisp.types :as t]
   [simple-stackless-lisp.util :refer [->cps]]
   [clojure.string :as str]))

(def ^:private multi-registry
  (atom {}))

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
        (atom {})

        dispatch
        (with-meta
          (fn [with-result & args]
            (let [dispatch-val (t/type (first args))]
              (if-let [k-impl (get @implementations dispatch-val)]
                (core/apply k-impl (cons with-result args))
                (let [k-default-impl (get @implementations :MultiMethod/default)]
                  (core/apply k-default-impl with-result dispatch-val args)))))
          {:multimethod? true})]
    (swap! multi-registry assoc
           dispatch {:name name
                     :implementations implementations})
    (swap! implementations assoc
           :MultiMethod/default (make-k-default-method name))
    (swap! implementations assoc
           'Fn (fn [k f args]
                 (core/apply f (cons k args))))
    (swap! implementations assoc
           'MultiMethod (fn [k f args]
                          (core/apply f (cons k args))))
    dispatch))

(defn- k-multi
  [k name k-args->dispatch-val]
  (let [implementations
        (atom {})

        dispatch
        (with-meta
          (fn [with-result & args]
            (k-apply
             (fn [dispatch-val]
               (if-let [k-impl (get @implementations dispatch-val)]
                 (k-apply with-result k-impl args)
                 (let [k-default-impl (get @implementations :MultiMethod/default)]
                   (k-apply with-result k-default-impl (cons dispatch-val args)))))
             k-args->dispatch-val
             args))
          {:multimethod? true})]
    (swap! multi-registry assoc
           dispatch {:name name
                     :implementations implementations})
    (swap! implementations assoc
           :MultiMethod/default (make-k-default-method name))
    (k dispatch)))

(defn- k-method
  [k multi dispatch-val k-impl]
  (let [multi-record (get @multi-registry multi)
        impls (:implementations multi-record)]
    (swap! impls assoc dispatch-val k-impl)
    (k nil)))

(defn- multi-info
  [multi]
  (some-> @multi-registry
          (get multi)
          (update :implementations deref)))

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
(k-method
 identity k-to-string :MultiMethod/default
 (fn [k _ obj]
   (k (pr-str obj))))

(k-method
 identity k-to-string 'Nil
 (fn [k n]
   (k "nil")))

(k-method
 identity k-to-string 'Fn
 (fn [k f]
   (k (str "#Fn"))))

(k-method
 identity k-to-string 'MultiMethod
 (fn [k m]
   (let [info (multi-display-info m)
         info-str (k-to-string identity info)]
     (k (str "#MultiMethod" info-str)))))

(k-method
 identity k-to-string 'Vector
 (fn [k v]
   (let [item-strs (map #(k-to-string identity %) v)
         items (str/join ", " item-strs)]
     (k (str "[" items "]")))))

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
   '/ (->cps /)

   ;; Strings
   ;; =======
   '->str k-to-string})
