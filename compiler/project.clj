(defproject clojure-ruby "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.8.0"]
                 [org.clojure/tools.analyzer "0.0.1-SNAPSHOT"]]

  :profiles {:dev {:dependencies [[com.aphyr/prism "0.1.1"]]
                   :plugins [[com.aphyr/prism "0.1.1"]]}}

  :java-source-paths ["src"]

  :main clojure.ruby.main

  )
