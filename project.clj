(defproject interpreter "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1806"]]
  :plugins      [[lein-cljsbuild "0.3.2"]
                 [lein-ring "0.8.7"]]
  :source-paths ["src/clj"]
  :cljsbuild
  {:builds
   [{:source-paths ["src-cljs"],
     :id "main",
     :compiler
     {:pretty-print true,
      :output-to "resources/public/js/main.js",
      :optimizations :simple}}]})
