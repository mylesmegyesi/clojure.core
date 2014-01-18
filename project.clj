(defproject clojure.core "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths []
  :test-paths   []

  :profiles {:new-jvm {:dependencies [[org.clojure/clojure "1.5.1"]]
                       :source-paths ["src/new/clj" "src/new/jvm"]
                       :java-source-paths ["src/new/jvm"]
                       :test-paths   ["test/clj" "test/jvm" "test-helpers/new"]}
             :old-jvm {:dependencies [[org.clojure/clojure "1.6.0-alpha3"]]
                       :source-paths ["src/old-jvm"]
                       :test-paths   ["test/clj" "test/jvm" "test-helpers/old-jvm"]
                       :test-selectors {:default (comp (partial not-any? identity)
                                                       (juxt :patch :new))}}

             :clr-setup {:plugins [[lein-clr "0.2.1"]]
                         :clr {:cmd-templates  {:clj-exe   [[?PATH "mono"] [CLJCLR14_40 %1]]
                                                :clj-dep   [[?PATH "mono"] ["target/clr/clj/Debug" %1]]
                                                :clj-url   "http://sourceforge.net/projects/clojureclr/files/clojure-clr-1.5.0-Debug-4.0.zip/download"
                                                :clj-zip   "clojure-clr-1.5.0-Debug-4.0.zip"
                                                :curl      ["curl" "--insecure" "-f" "-L" "-o" %1 %2]
                                                :nuget-ver [[?PATH "mono"] [*PATH "nuget.exe"] "install" %1 "-Version" %2]
                                                :nuget-any [[?PATH "mono"] [*PATH "nuget.exe"] "install" %1]
                                                :unzip     ["unzip" "-d" %1 %2]
                                                :wget      ["wget" "--no-check-certificate" "--no-clobber" "-O" %1 %2]}
                               ;; for automatic download/unzip of ClojureCLR,
                               ;; 1. make sure you have curl or wget installed and on PATH,
                               ;; 2. uncomment deps in :deps-cmds, and
                               ;; 3. use :clj-dep instead of :clj-exe in :main-cmd and :compile-cmd
                               :deps-cmds      [[:wget :clj-zip :clj-url] ; edit to use :curl instead of :wget
                                                [:unzip "../clj" :clj-zip]
                                                ]
                               :main-cmd      [:clj-dep "Clojure.Main.exe"]
                               :compile-cmd   [:clj-dep "Clojure.Compile.exe"]}}

             :new-clr [:clr-setup {:source-paths ["src/new/clj" "src/new/clr"]
                                   :test-paths   ["test/clj" "test/clr" "test-helpers/new"]}]

             :old-clr [:clr-setup {:source-paths ["src/old-clr"]
                                   :test-paths   ["test/clj" "test/clr" "test-helpers/old-clr"]
                                   :test-selectors {:default (comp (partial not-any? identity)
                                                                   (juxt :patch :new))}}]
             }

  )
