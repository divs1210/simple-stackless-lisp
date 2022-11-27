# Simple Stackless Lisp User's Manual

This language is a **Work In Progress**.

Feel free to play around with it, though!

# Builtins

## Types

- Nil: `nil`
- Arbitrary precision numbers: `1`, `0.0004`, `2334234.3423443`
- Booleans: `true`, `false`
- Strings: `"Hello, world!"`
- Symbols: `x`, `is-it-even?`
- Lists: `(list 1 2 3)`
- Functions: `(fn [x] (+ x 1))`

## Special forms

### def

Binds a symbol to a value in the top-level (global) environment.

```clojure
(def a 1)

(def b 2)

(def c (+ a b))

c ;; => 3
```

### let

Lexically scoped local bindings.

```clojure
(let [a 1
      b 2
      c (+ a b)]
  c) ;; => 3
```

### if

```clojure
(if (< 1 2)
  "a"
  "b") ;; => "a"
```

### do

```clojure
(do
  (println "a")
  (println "b")
  "c") ;; => "c" 
;; a
;; b
```

### quote

Returns its argument unevaluated.

```clojure
(quote undefined-symbol) ;; => undefined-symbol
(quote (1 2 a "d" (+ 2 4))) ;=> (1 2 a "d" (+ 2 4))
```

### fn

```clojure
(def add
  (fn [a b]
    (+ a b)))

(add 1 2) ;; => 3
```

### macro

CL-like unhygeinic macros for now.

```clojure
(def dbg
  (macro [exp]
    (let [exp# (gensym)]
      (quote
        (let [(unquote exp#) (unquote exp)]
          (do
            (println (quote (unquote exp)) "=>" (unquote exp#))
            (unquote exp#)))))))

(* 2 (dbg (+ 1 2))) ;; => 6
;; (+ 1 2) => 3
```

### eval

```clojure
(def a 1)

(let [b 2
      code (quote (+ a b))]
  (eval code)) ;; => 3
```

## Functions

### Sequences
- `list`
- `first`
- `rest`
- `seq`
- `cons`

### IO
- `print`
- `println`

### Math
- `=`
- `<`
- `>`
- `<=`
- `>=`
- `+`
- `-`
- `*`
- `/`

### Misc
- `gensym`
- `apply`
- `call-cc`
