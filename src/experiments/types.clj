(ns experiments.types)

(comment

  ;; Numbers
  ;; =======
  (deftype core/Number [val])

  ;; (def Type
  ;;   {:type (var Type)
  ;;    :val  (var Type)})
  ;;
  ;; (defmacro deftype [name argv]
  ;;   `(do
  ;;      (def ~name
  ;;        {:type Type
  ;;         :val  (var ~name)})
  ;;      (defn ~(symbol (str "->" name)) [& args]
  ;;        {:type ~name
  ;;         :val  ~(zipmap argv args)})))
  ;;
  ;; > 1
  ;; => {:type core/Number
  ;;     :val 1}
  ;;
  ;; > core/Number
  ;; => {:type core/Type
  ;;     :val {:name 'Number}}
  ;;
  ;; > core/Type
  ;; => {:type core/Type
  ;;     :val {:name 'Type}}

  (defmulti core/+ [this that]
    [(:type this) (:type that)])

  ;; (defmacro defmulti [name argv dispatch-body]
  ;;   `(do
  ;;      (def ~(symbol (str name "-dispatch-table"))
  ;;        (atom {}))
  ;;      (defn ~name [& args]
  ;;        (let [dispatch-fn (fn ~argv ~dispatch-body)
  ;;              dispatch-val (core/apply dispatch-fn args)
  ;;              target-fn (get ~(symbol (str name "-dispatch-table")) dispatch-val)]
  ;;          (apply target-fn args)))))

  (defmethod core/+
    [core/Number core/Number]
    [a b]
    (core/Number (bultin/+ (:val a) (:val b))))

  ;; (defmacro defmethod [multiname dispatch-val argv body]
  ;;  `(swap! ~(symbol (str multiname "-dispatch-table"))
  ;;          assoc ~dispatch-val (fn ~argv ~body)))

  (deftype ComplexNumber [real imag])

  ;; > (ComplexNumber 1 -2)
  ;; => {:type ComplexNumber
  ;;     :val {:real {:type core/Number :val 1}
  ;;           :imag {:type core/Number :val -2}}}

  (defmethod core/+
    [ComplexNumber ComplexNumber]
    [a b]
    (ComplexNumber (core/+ (-> a :val :real) (-> b :val :real))
                   (core/+ (-> a :val :imag) (-> b :val :imag))))

  ;; Functions
  ;; =========
  (deftype core/Fn [argv body])

  (defmulti core/apply [f args]
    (:type f))

  (defmethod core/apply core/Fn
    [f args]
    (eval
     `(let ~(vec (zipmap (:argv f) args))
        (:body f))))

  )
