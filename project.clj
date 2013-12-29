(defproject clojure.core "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]]

  :profiles {:dev {:dependencies [[com.aphyr/prism "0.1.1"]]
                   :plugins [[com.aphyr/prism "0.1.1"]]}}

  :source-paths ["src/clj" "src/jvm"]

  )
