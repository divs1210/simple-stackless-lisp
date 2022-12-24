(ns generators
  (:refer-clojure
   :exclude [concat iterate range
             map filter reduce sort])
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

(defn sort
  ([gen]
   (sort EMPTY gen))
  ([acc gen]
   (let [[x next-gen] (gen)]
     (if (not= ::end x)
       (recur (insert-with #(< % x) x acc)
              next-gen)
       acc))))

(defn reduce
  [f init gen]
  (let [[x next-gen] (gen)]
    (if (not= ::end x)
      (recur f (f init x) next-gen)
      init)))

(defn ->seq [gen]
  (lazy-seq
   (let [[x next-gen] (gen)]
     (when-not (= ::end x)
       (cons x (->seq next-gen))))))

(comment
  "TODO"

  (defn GeneratorFn [map]
    (let [X (:X map)]
      (Fn {:ARG EmptyArray,
           :RET (Pair {:FIRST (Or {:TYPES [X, (Literal {:VAL ::end})]})
                       :LAST  (Generator {:X X})})})))

  (def IGn
    (Protocol [X]
     {:next {:args [{:name this
                     :type (GeneratorFn {:X X})}]}}))

  (deftype Generator [X]
    {:f {:type (GeneratorFn {:X X})}}

    (IGn
      (next [this]
            (let [f (:f this)]
              (f)))))

  (def EMPTY
    (Generator
     {:X (Literal {:VAL ::end})}
     {:f (Fn {}
          {:args []
           :body '(Pair {}
                   {:first ::end
                    :last 'EMPTY})})}))

  (extend-protocols Generator

   (IFn
    (apply [this _]
           (next this)))

   (IListable
    (->list [this]
            (let [pair (this)
                  x (:first pair)]
              (when (not= ::end x)
                (cons x (->list (:last pair)))))))

   (ISeqable
    (first [this])

    (rest [this]))

   (IArrayable
    (->array [this]
             (->array (->list this))))

   (IStringable
    (->str [_]
           (str "#Generator{:x " x ", :next ...}"))))

  )

(comment "Benchmarks"

  (def L (shuffle (core/range 1000)))

  (do "Array sort"
    (time (first (core/sort L))))

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
    
    (time (first (lazy-sort L))))

  (do "Generator sort"
    (def G (->gen (shuffle (core/range 1000))))
    
    (time (first ((sort G)))))

  )
