(defproject callanalyzer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clojurewerkz/elastisch "2.2.0-beta4"]
                 [com.taoensso/timbre "4.1.1"]
                 [slingshot "0.12.2"]
                 [compojure "1.3.4"]
                 [http-kit "2.1.18"]
                 [ring "1.1.3"]
                 [environ "0.5.0"]
                 [com.taoensso/sente "1.7.0"]
                 [org.clojure/clojurescript "1.7.122"]
                 [reagent "0.5.1" :exclusions [cljsjs/react]]
                 [cljsjs/react-with-addons "0.13.3-0"]]
  :plugins [[lein-cljsbuild "1.1.0"]]
  :global-vars {*assert* true}
  :cljsbuild {
              :builds [{:source-paths ["src-cljs"]
                        :compiler     {:output-to     "resources/public/js/main.js"
                                       :optimizations :whitespace
                                       :pretty-print  true}}]})
