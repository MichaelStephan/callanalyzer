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
                 [cljsjs/react-with-addons "0.13.3-0"]
                 [re-frame "0.6.0"]
                 [cljsjs/d3 "3.5.7-1"]]
  :plugins [[lein-cljsbuild "1.1.0"]
            [lein-figwheel "0.5.0-2"]]
  :global-vars {*assert* true}
  :clean-targets ^{:protect false} ["resources/public/js"]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src-cljs"]
                        :figwheel {:on-jsload "callanalyzer.core/fig-reload"} 
                        :compiler     {:main "callanalyzer.core" 
                                       :asset-path "js/out"
                                       :output-to "resources/public/js/main.js"
                                       :output-dir "resources/public/js/out"
                                       :optimizations :none
                                       :source-map-timestamp true
                                       :pretty-print  true}}
                       {:id "dev-no-figwheel"
                        :source-paths ["src-cljs"]
                        :compiler     {:output-to     "resources/public/js/main.js"
                                       :optimizations :whitespace
                                       :pretty-print  true}}]}
  ;:figwheel {}
  )
