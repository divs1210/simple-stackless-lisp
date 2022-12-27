(ns experiments.generators
  (:refer-clojure
   :exclude [concat iterate range map filter reduce sort])
  (:require [clojure.core :as core]))

(defn EMPTY []
  [::end EMPTY])

(def END (EMPTY))

(defmacro gn
  {:style/indent 1}
  [initial-bindings & body]
  `(fn []
     (loop ~initial-bindings
       ~@body)))

(defn ->gen [coll]
  (gn []
    (if-let [[x & more] (seq coll)]
      [x (->gen more)]
      END)))

(defn iterate
  [f init]
  (gn []
    [init (iterate f (f init))]))

(defn range
  ([]
   (iterate inc 0))
  ([end]
   (range 0 end))
  ([start end]
   (range start end 1))
  ([start end step]
   (let [compare (if (< start end) < >)]
     (gn []
       (if (compare start end)
         [start (range (+ step start) end step)]
         END)))))

(defn concat
  ([] EMPTY)
  ([gen] gen)
  ([gen & more]
   (gn [gens (cons gen more)]
     (if-let [[gen & more] (seq gens)]
       (let [[x gen-xs] (gen)]
         (if (not= ::end x)
           [x (apply concat gen-xs more)]
           (recur more)))
       END))))

(defn map
  [f gen]
  (gn [[x next-gen] (gen)]
    (if (not= ::end x)
      [(f x) (map f next-gen)]
      END)))

(defn filter
  [pred gen]
  (gn [[x next-gen] (gen)]
    (if (not= ::end x)
      (if (pred x)
        [x (filter pred next-gen)]
        (recur (next-gen)))
      END)))

(defn insert-with
  [pred val gen]
  (gn [[x next-gen] (gen)]
    (if (not= ::end x)
      (if (pred x)
        [x (insert-with pred val next-gen)]
        [val gen])
      [val EMPTY])))

(defn reduce
  [f init gen]
  (let [[x next-gen] (gen)]
    (if (not= ::end x)
      (recur f (f init x) next-gen)
      init)))

(defn sort
  ([gen]
   (reduce (fn [acc x]
             (insert-with #(< % x) x acc))
           EMPTY
           gen)))

(defn ->seq [gen]
  (lazy-seq
   (let [[x next-gen] (gen)]
     (when-not (= ::end x)
       (cons x (->seq next-gen))))))

(comment "Benchmarks"

  (defn bench [f]
    (time (f)))

  (def Ls (doall (repeatedly 10 #(shuffle (core/range 1000)))))

  (do "Array sort"
    (bench #(->> Ls
                 (core/map core/sort)
                 (core/map first)
                 (core/reduce + 0))))

  (do "LazySeq sort"
    (defn lazy-insert-with
      [pred x coll]
      (let [[before after] (core/split-with pred coll)]
        (core/concat before [x] after)))

    (defn lazy-sort
      ([coll]
       (lazy-sort [] coll))
      ([acc coll]
       (if-let [[x & more] (seq coll)]
         (recur (lazy-insert-with #(< % x) x acc)
                more)
         acc)))
    
    (bench #(->> Ls
                 (core/map lazy-sort)
                 (core/map first)
                 (core/reduce + 0))))

  (do "Generator sort"
    (def Gs (core/map ->gen Ls))

    (bench #(->> Gs
                 (core/map sort)
                 (core/map (fn [g] (first (g))))
                 (core/reduce + 0))))

  )
