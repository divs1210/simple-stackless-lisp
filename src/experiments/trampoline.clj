(ns experiments.trampoline
  (:refer-clojure :exclude [trampoline]))

(deftype Thunk [f source]
  clojure.lang.IFn
  (invoke [_]
    (f))
  (applyTo [_ _]
    (f))

  Object
  (toString [_]
    (pr-str source)))

(defmacro thunk
  "Wraps recursive calls."
  [& body]
  (let [f `(fn [] ~@body)]
    `(Thunk. ~f '~f)))

(defn trampoline
  "Can be used to convert algorithms requiring mutual recursion
  without stack consumption.
  If maybe-thunk is a Thunk, calls it, and repeats with the returned value.
  If maybe-thunk is not a Thunk, returns it."
  [maybe-thunk]
  (if (instance? Thunk maybe-thunk)
    (recur (maybe-thunk))
    maybe-thunk))

(comment
  (defn cps-fact [k n]
    (if (< n 2)
      (k 1)
      (thunk
       (cps-fact (fn [n-1!]
                   (thunk
                    (k (*' n n-1!))))
                 (dec n)))))

  (def fact
    (trampolined-cps-fn cps-fact))

  (fact 5)
  )
