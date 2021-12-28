(defproject techascent/tech.jna "4.07-SNAPSHOT"
  :description "Bindings to JNA.  Used with libpython-clj among others."
  :url "http://github.com/tech-ascent/tech.jna"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure      "1.10.1-beta2"]
                 [net.java.dev.jna/jna     "5.6.0"]
                 [techascent/tech.resource "5.02"]]
  :profiles {:codox
             {:dependencies [[codox-theme-rdash "0.1.2"]]
              :plugins [[lein-codox "0.10.7"]]
              :codox {:project {:name "tech.jna"}
                      :metadata {:doc/format :markdown}
                      :themes [:rdash]
                      :source-paths ["src"]
                      :output-path "docs"
                      :doc-paths ["topics"]
                      :source-uri "https://github.com/techascent/tech.jna/blob/master/{filepath}#L{line}"
                      :namespaces [tech.v3.jna]}}}
  :aliases {"codox" ["with-profile" "codox,dev" "codox"]}
  )
