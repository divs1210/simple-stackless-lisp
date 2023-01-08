(ns simple-stackless-lisp.multi
  (:refer-clojure :exclude [methods get-method])
  (:require [clojure.core :as core]
            [simple-stackless-lisp.types :as t]))

(def ^:private multi-registry
  (atom {}))

(declare k-to-readable-string)

(defn- k-default-method
  [k name dispatch-val & args]
  (let [msg (str "No implementation of method: "
                 (t/string->java-string name)
                 " found for dispatch-val: "
                 (t/string->java-string
                  (k-to-readable-string identity dispatch-val)))]
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
  (let [name (t/java-string->string "apply")

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

(defn k-multi
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

(defn k-method
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
  (k-multi identity (t/java-string->string "->str")
           (fn [k obj]
             (k (t/type obj)))))

;; Number, Boolean, Symbol, Keyword
(k-method
 identity k-to-string :MultiMethod/default
 (fn [k _ obj]
   (k (t/java-string->string (str obj)))))

(k-method
 identity k-to-string 'Nil
 (fn [k n]
   (k (t/java-string->string "nil"))))

(k-method
 identity k-to-string 'Character
 (fn [k c]
   (k (t/string [c]))))

(k-method
 identity k-to-string 'String
 (fn [k s]
   (k s)))

(k-method
 identity k-to-string 'Fn
 (fn [k f]
   (k (t/java-string->string "#Fn"))))

(k-method
 identity k-to-string 'MultiMethod
 (fn [k m]
   (let [info (multi-display-info m)
         info-str (k-to-string identity info)]
     (k (t/string-concat (t/java-string->string "#MultiMethod")
                         info-str)))))

(k-method
 identity k-to-string 'Vector
 (fn [k v]
   (let [item-strs (map #(k-to-string identity %) v)
         items-str (t/string-join (t/java-string->string ", ")
                                  item-strs)]
     (k (t/string-join (t/string [])
                       [(t/java-string->string "[")
                        items-str
                        (t/java-string->string "]")])))))

(k-method
 identity k-to-string 'HashMap
 (fn [k m]
   (let [item-strs (map (fn [[k v]]
                          (t/string-join (t/java-string->string " ")
                                         [(k-to-string identity k)
                                          (k-to-string identity v)]))
                        m)
         items-str (t/string-join (t/java-string->string ", ")
                                  item-strs)]
     (k (t/string-join (t/string [])
                       [(t/java-string->string "{")
                        items-str
                        (t/java-string->string "}")])))))

(def k-to-readable-string
  (k-multi identity (t/java-string->string "->rstr")
           (fn [k obj]
             (k (t/type obj)))))

(k-method
 identity k-to-readable-string :MultiMethod/default
 (fn [k _ obj]
   (k-to-string k obj)))

(k-method
 identity k-to-readable-string 'Character
 (fn [k c]
   (k (t/string-join (t/string [])
                     [(t/java-string->string "#char \"")
                      (t/string [c])
                      (t/java-string->string "\"")]))))

(k-method
 identity k-to-readable-string 'String
 (fn [k s]
   (k (t/string-join (t/string [])
                     [(t/java-string->string "\"")
                      s
                      (t/java-string->string "\"")]))))

(k-method
 identity k-to-readable-string 'Vector
 (fn [k v]
   (let [item-strs (map #(k-to-readable-string identity %) v)
         items-str (t/string-join (t/java-string->string ", ")
                                  item-strs)]
     (k (t/string-join (t/string [])
                       [(t/java-string->string "[")
                        items-str
                        (t/java-string->string "]")])))))

(k-method
 identity k-to-readable-string 'HashMap
 (fn [k m]
   (let [item-strs (map (fn [[k v]]
                          (t/string-join (t/java-string->string " ")
                                         [(k-to-readable-string identity k)
                                          (k-to-readable-string identity v)]))
                        m)
         items-str (t/string-join (t/java-string->string ", ")
                                  item-strs)]
     (k (t/string-join (t/string [])
                       [(t/java-string->string "{")
                        items-str
                        (t/java-string->string "}")])))))