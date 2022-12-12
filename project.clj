(defproject simple-stackless-lisp "0.1.0-SNAPSHOT"
  :description "A simple stackless lisp implementation"
  :url "http://github.com/divs1210/simple-stackless-lisp"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main simple-stackless-lisp.main
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[com.clojure-goes-fast/clj-async-profiler "1.0.3"]]
                   :global-vars {*warn-on-reflection* true}
                   :jvm-opts ["-XX:+UnlockDiagnosticVMOptions"
                              "-XX:+DebugNonSafepoints"
                              "-Djdk.attach.allowAttachSelf=true"]}
             :uberjar {:aot :all
                       :global-vars {*warn-on-reflection* true}}
             :native-image {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :plugins [[io.taylorwood/lein-native-image "0.3.1"]]
  :native-image {:name "sclj"
                 :opts ["--verbose"
                        "--no-fallback"
                        ;; "--report-unsupported-elements-at-runtime"
                        "--initialize-at-build-time"
                        "--no-server"]})
