(defproject ez-time "0.1.0-SNAPSHOT"
  :description "Easy time manipulation"
  :url "https://github.com/emil0r/ez-time"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [joda-time "2.3"]
                 [slingshot "0.10.3"]]
  :profiles {:dev {:plugin [[lein-midje "3.1.1"]]
                   :dependencies [[midje "1.6.3"]]}})
