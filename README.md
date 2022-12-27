# simple-stackless-lisp

A small but powerful Clojure-like Lisp.

Has first class continuations and macros.

This is a **Work In Progress**, and there is [more to come](docs/vision.md)!

## Example

```clojure
;; sample-project/fact.sclj
(def fact
  (fn [n]
    (if (< n 2)
      1
      (* n (fact (- n 1))))))

(println "(fact 5000) =>")
(println (fact 5000))
```

More [code examples](sample-project/) are available.

There's also a [User Manual](docs/manual.md).

## Usage

### Download

You can find pre-compiled executables [here](https://github.com/divs1210/simple-stackless-lisp/releases/latest).

```
Note:
=====
Run the REPL/files inside the sample-project directory.
The interpreter looks inside sample-project/modules for core namespaces.
```

### Start a REPL

```
$ ./sclj
```

Use `rlwrap` for a more pleasant REPL experience:

```
$ rlwrap ./sclj
```

### Run a file

```
$ ./sclj code.sclj
```

## Build from source

[The manual](docs/manual.md) contains more info on building and running the interpreter.

## Thanks

[Lisperator's technique](https://lisperator.net/pltut/cps-evaluator/) for building a stackless interpreter led me down this rabbit hole.

## License

Copyright © 2022 Divyansh Prakash

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
