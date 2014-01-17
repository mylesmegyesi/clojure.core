(defproject clojure.core "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :profiles {:new-jvm {:dependencies [[org.clojure/clojure "1.5.1"]]
                       :source-paths ["src/new/clj" "src/new/jvm"]
                       :java-source-paths ["src/new/jvm"]
                       :test-paths   ["test/clj" "test/jvm" "test-helpers/new"]}
             :old-jvm {:dependencies [[org.clojure/clojure "1.6.0-alpha3"]]
                       :source-paths ["src/old-jvm"]
                       :test-paths   ["test/clj" "test/jvm" "test-helpers/old-jvm"]
                       :test-selectors {:default (comp (partial not-any? identity)
                                                       (juxt :patch :new))}}
             }

  )
