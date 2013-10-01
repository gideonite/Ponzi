(defproject interpreter "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1806"]
                 ;;[com.cemerick/piggieback "0.1.0"]
                 [com.cemerick/clojurescript.test "0.0.4"]]
  :plugins      [[lein-cljsbuild "0.3.2"]
                 [lein-ring "0.8.7"]]
  :source-paths ["src/clj"]
  ;:repl-options  {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :cljsbuild
  {:builds
   [{:source-paths ["src-cljs"],
     :id "main",
     :compiler
     {:pretty-print true,
      :output-to "resources/public/js/main.js",
      :optimizations :simple}}
    {:source-paths ["test-cljs"]
     :compiler {:output-to "target/cljs/testable.js"
                :optimizations :whitespace
                :pretty-print true}}]
   :test-commands {"unit-tests" ["runners/phantomjs.js" "target/cljs/testable.js"]}})
