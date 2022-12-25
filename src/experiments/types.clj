(ns experiments.types)

(comment
  
  (defproto apply [f args])

  (deftype Fn [argv body]
    (core/apply [_ args]
      (with-extended-env (zipmap argv args)
        (eval body)))
    (core/->str [_]
      (str "(fn " ~argv ~body ")")))

  (defproto + [x y])

  (deftype Complex [a b]
    (core/+ [_ that]
      (Complex [(+ a (:a that)) (+ b (:b that))]))
    (core/->str [_]
      (str "#Complex{:a " a ", :b " b "}")))

  )

"
- protocols are like one-method-interfaces
- are implemented on a per-object basis
- dispatch to the relevant function in the first parameter

Type checking will be solely based on predicates.
"
