(defproject explain8601 "0.1.0"
  :description "Convert an ISO 8601 expression to a human readable description"
  :url "https://github.com/dokutan/explain8601"
  :license {:name "AGPL-3.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.0.206"]
                 [instaparse "1.4.12"]
                 [zprint "1.2.3"]]
  :plugins [[lein-kibit "0.1.8"]
            [lein-codox "0.10.8"]
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
