(defproject schedule "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2156"]]
  :profiles {:dev {:dependencies [[com.cemerick/clojurescript.test "0.2.2"]]}}
  :plugins [[com.keminglabs/cljx "0.3.2"]
            [lein-cljsbuild "1.0.2"]
            [com.cemerick/clojurescript.test "0.2.2"]]
  :test-paths ["target/test/classes"]
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  {:source-paths ["test/cljx"]
                   :output-path "target/test/classes"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/src/cljs"
                   :rules :cljs}
                  {:source-paths ["test/cljx"]
                   :output-path "target/src/test/cljs"
                   :rules :cljs}]}
  :cljsbuild {:builds [{:source-paths ["target/src/cljs"]
                        :compiler {:output-to "target/cljs/schedule.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}
                       {:source-paths ["target/src/cljs" "target/src/test/cljs"]
                        :compiler {:output-to "target/test/cljs/schedule_test.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]
              :test-commands {"unit-tests" ["phantomjs" :runner "target/test/cljs/schedule_test.js"]}}
  :hooks [cljx.hooks leiningen.cljsbuild])
