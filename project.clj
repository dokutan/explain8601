(defproject explain8601 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/tools.cli "1.0.206"]
                 [instaparse "1.4.12"]
                 [zprint "1.2.3"]]
  :plugins [[lein-kibit "0.1.8"]
            [io.taylorwood/lein-native-image "0.3.1"]]
  :native-image {:graal-bin :env/GRAALVM_HOME
                 :opts ["--verbose"
                        "--report-unsupported-elements-at-runtime"
                        "--initialize-at-build-time"
                        ]}
  :main ^:skip-aot explain8601.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :native-image {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
