(defproject techascent/tech.jna "2.6-SNAPSHOT"
  :description "Bindings of tech.datatype system to jna."
  :url "http://github.com/tech-ascent/tech.jna"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.java.dev.jna/jna "5.0.0"]
                 [techascent/tech.resource "3.1"]]
  :profiles {:dev {:dependencies [[com.taoensso/timbre "4.10.0"]]}})
