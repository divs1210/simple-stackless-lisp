# simple-stackless-lisp

Interpreter for a simple Clojure-like Lisp.

Made stackless using [Lisperator's technique](https://lisperator.net/pltut/cps-evaluator/).

Has first class support for continuations and macros.

See [code examples](resources/).

## Example

```clojure
;; /resources/fact.sclj
(def fact
  (fn [n]
    (if (< n 2)
      1
      (* n (fact (- n 1))))))

(println "(fact 50000) =>")
(println (fact 50000))
```

## Usage

### I. Download

You can find pre-compiled executables [here](https://github.com/divs1210/simple-stackless-lisp/releases).

The download bundle contains execuatables for Ubuntu, MacOS, and Windows, and also a Java JAR.

##### Start a REPL

```
$ ./sclj
```

or

```
$ java -jar sclj.jar
```

##### Run a file

```
$ ./sclj code.sclj
```

or

```
$ java -jar sclj.jar code.sclj
```

### II. Build from source

#### Clone this project

```
$ git clone https://github.com/divs1210/simple-stackless-lisp.git

$ cd simple-stackless-lisp
```

#### Build a JAR

Requires Java and Leiningen to be installed.

```
$ lein uberjar
```

#### Build a native executable

Requires GraalVM and its native-image tool to be installed.

```
$ lein native-image
```

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
