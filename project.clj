(defproject coarse "0.1.0-SNAPSHOT"
  :description "Clojure lens library, implements a small subset of ekmett's lens"
  :url "http://github.com/RuneBlaze/coarse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :plugins [[lein-codox "0.10.3"]]
  :cljsbuild {
              :builds [{:source-paths ["src"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]})
