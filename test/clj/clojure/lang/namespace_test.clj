(ns clojure.lang.namespace-test
  (:refer-clojure :only [defn -> let get-in identity nil? set keys get assoc-in])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.equals          :refer [=]]
            [clojure.lang.meta            :refer [meta with-meta]]
            [clojure.lang.named           :refer [name namespace]]
            [clojure.lang.symbol          :refer [symbol]]
            [clojure.lang.platform.object :refer [identical?]]
            [clojure.lang.namespace       :refer :all]))

(defn empty-ns-map []
  {})

(deftest create-ns-test
  (testing "creates a namespace"
    (is (= {(symbol "new-ns") {:mappings {}
                               :aliases  {}
                               :ns       (symbol "new-ns")}}
           (create-ns (empty-ns-map) (symbol "new-ns"))))))

(deftest alias-test
  (testing "adds an alias to another namespace"
    (is (= {(symbol "to-ns") {:mappings {}
                              :aliases  {(symbol "the-alias") (symbol "target-ns")}
                              :ns       (symbol "to-ns")}
            (symbol "target-ns") {:mappings {}
                                  :aliases  {}
                                  :ns       (symbol "target-ns")}}
           (-> (create-ns (empty-ns-map) (symbol "to-ns"))
             (create-ns (symbol "target-ns"))
             (alias (symbol "to-ns") (symbol "the-alias") (symbol "target-ns"))))))

  (testing "throws an exception if to-ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: new-ns found"
           (-> (create-ns (empty-ns-map) (symbol "other-ns"))
             (alias (symbol "new-ns") (symbol "the-alias") (symbol "other-ns"))))))

  (testing "throws an exception if target-ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: other-ns found"
           (-> (create-ns (empty-ns-map) (symbol "new-ns"))
             (alias (symbol "new-ns") (symbol "the-alias") (symbol "other-ns"))))))

  (testing "throws an exception if an alias already exists to another ns"
    (is (thrown-with-msg? Exception #"Alias the-alias already exists in namespace to-ns, aliasing target-ns"
           (-> (create-ns (empty-ns-map) (symbol "to-ns"))
             (create-ns (symbol "target-ns"))
             (create-ns (symbol "third-ns"))
             (alias (symbol "to-ns") (symbol "the-alias") (symbol "target-ns"))
             (alias (symbol "to-ns") (symbol "the-alias") (symbol "third-ns"))))))

  (testing "does nothing if the alias already exists"
    (is (= {(symbol "to-ns") {:mappings {}
                              :aliases  {(symbol "the-alias") (symbol "target-ns")}
                              :ns       (symbol "to-ns")}
            (symbol "target-ns") {:mappings {}
                                  :aliases  {}
                                  :ns       (symbol "target-ns")}}
           (-> (create-ns (empty-ns-map) (symbol "to-ns"))
             (create-ns (symbol "target-ns"))
             (alias (symbol "to-ns") (symbol "the-alias") (symbol "target-ns"))
             (alias (symbol "to-ns") (symbol "the-alias") (symbol "target-ns")))))))

(deftest intern-test
  (testing "intern creates a mapping in the given ns"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "test-ns") (symbol "something")))
          something-var (get-in namespaces [(symbol "test-ns") :mappings (symbol "something")])]
      (is (instance? clojure.lang.var.Var something-var))
      (is (instance? clojure.lang.var.Unbound @(.root something-var)))
      (is (identical? something-var (.var @(.root something-var))))
      (is (= "test-ns" (namespace something-var)))
      (is (= {} (meta something-var)))))

  (testing "intern uses the metadata from the symbol"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (with-meta (symbol "something") {:private true
                                                                                     :other-meta "here"})))
          something-var (ns-resolve namespaces (symbol "other-ns") (symbol "something"))]
      (is (= {:private true :other-meta "here"} (meta something-var)))))

  (testing "throws an exception if the namespace does not exist"
    (is (thrown-with-msg? Exception #"No namespace: the-ns found"
          (intern (empty-ns-map) (symbol "the-ns") (symbol "something")))))

  (testing "throws an exception if interning a namespace-qualified sym"
    (is (thrown-with-msg? Exception #"Can't intern namespace-qualified symbol"
          (intern (create-ns (empty-ns-map) (symbol "the-ns")) (symbol "the-ns") (symbol "clojure.core/something"))))))

(deftest def-test ; irony?
  (testing "def interns the symbol in the given ns"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (-def (symbol "test-ns") (symbol "something")))
          something-var (ns-resolve namespaces (symbol "test-ns") (symbol "something"))]
      (is (instance? clojure.lang.var.Var something-var))
      (is (instance? clojure.lang.var.Unbound @(.root something-var)))
      (is (identical? something-var (.var @(.root something-var))))
      (is (= "test-ns" (namespace something-var)))
      (is (= {} (meta something-var)))))

  (testing "def interns the symbol in a different ns"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (-def (symbol "test-ns") (symbol "something"))
                       (-def (symbol "other-ns") (symbol "something")))
          something-var2 (ns-resolve namespaces (symbol "other-ns") (symbol "something"))]
      (is (instance? clojure.lang.var.Var something-var2))
      (is (instance? clojure.lang.var.Unbound @(.root something-var2)))
      (is (identical? something-var2 (.var @(.root something-var2))))
      (is (= "other-ns" (namespace something-var2)))
      (is (= {} (meta something-var2)))))

  (testing "def uses the metadata from the symbol"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (-def (symbol "test-ns") (with-meta (symbol "something") {:private true
                                                                                 :other-meta "here"})))
          something-var (ns-resolve namespaces (symbol "test-ns") (symbol "something"))]
      (is (= {:private true :other-meta "here"} (meta something-var)))))

  (testing "def evaluates and sets the initial value"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (-def (symbol "test-ns") (symbol "something") (identity 1)))
          something-var (ns-resolve namespaces (symbol "test-ns") (symbol "something"))]
      (is (= 1 @(.root something-var)))))

  (testing "def sets the initial value and a doc string"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (-def (symbol "test-ns") (symbol "something") "some docs" (identity 1)))
          something-var (ns-resolve namespaces (symbol "test-ns") (symbol "something"))]
      (is (= "some docs" (:doc (meta something-var))))))

  (testing "def sets the root value, doc string and some extra meta"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (-def (symbol "test-ns")
                             (with-meta (symbol "something")
                                        {:private true :other-meta "here"}) "some docs" (identity 1)))
          something-var (ns-resolve namespaces (symbol "test-ns") (symbol "something"))]
      (is (= "some docs" (:doc (meta something-var))))
      (is (= true (:private (meta something-var))))
      (is (= "here" (:other-meta (meta something-var)))))))

(deftest ns-resolve-test
  (testing "resolves a symbol in a namespace"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (intern (symbol "test-ns") (symbol "something")))
          created (get-in namespaces [(symbol "test-ns") :mappings (symbol "something")])
          resolved (ns-resolve namespaces (symbol "test-ns") (symbol "something"))]
      (is (identical? created resolved))))

  (testing "returns nil if a symbol cannot be resolved"
    (let [namespaces (create-ns (empty-ns-map) (symbol "test-ns"))]
      (is (nil? (ns-resolve namespaces (symbol "test-ns") (symbol "something"))))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-resolve (empty-ns-map) (symbol "test-ns") (symbol "some1")))))

  (testing "resolves a namespaced symbol"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "something")))
          created (get-in namespaces [(symbol "other-ns") :mappings (symbol "something")])
          resolved (ns-resolve namespaces (symbol "test-ns") (symbol "other-ns/something"))]
      (is (identical? created resolved))))

  (testing "returns nil when the namespace of a namespace-qualified symbol ns does not exist"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "something")))]
      (is (nil? (ns-resolve namespaces (symbol "test-ns") (symbol "some-other/something"))))))

  (testing "resolves an aliased namespace"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "something"))
                       (alias (symbol "test-ns") (symbol "some-other") (symbol "other-ns")))
          created (get-in namespaces [(symbol "other-ns") :mappings (symbol "something")])
          resolved (ns-resolve namespaces (symbol "test-ns") (symbol "some-other/something"))]
      (is (identical? created resolved))))

  (testing "resolves an alias before an existing namespace"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "something"))
                       (create-ns (symbol "some-other"))
                       (intern (symbol "some-other") (symbol "something"))
                       (alias (symbol "test-ns") (symbol "some-other") (symbol "other-ns")))
          other-ns-something (get-in namespaces [(symbol "other-ns") :mappings (symbol "something")])
          some-other-something (get-in namespaces [(symbol "some-other") :mappings (symbol "something")])
          resolved (ns-resolve namespaces (symbol "test-ns") (symbol "some-other/something"))]
      (is (identical? other-ns-something
                      (ns-resolve namespaces (symbol "test-ns") (symbol "some-other/something"))))
      (is (identical? some-other-something
                      (ns-resolve namespaces (symbol "other-ns") (symbol "some-other/something")))))))

(deftest refer-test

  (defn test-ns-map []
    (-> (create-ns (empty-ns-map) (symbol "test-ns"))
      (create-ns (symbol "other-ns"))
      (intern (symbol "other-ns") (symbol "pub3"))
      (intern (symbol "test-ns") (symbol "pub1"))
      (intern (symbol "test-ns") (symbol "pub2"))
      (intern (symbol "test-ns") (with-meta (symbol "priv1") {:private true}))
      (intern (symbol "test-ns") (with-meta (symbol "priv2") {:private true}))))

  (testing "adds mappings to the to-ns for all ns-publics in the target-ns"
    (let [namespaces (test-ns-map)
          namespaces (refer namespaces (symbol "other-ns") (symbol "test-ns"))]
      (is (= (ns-resolve namespaces (symbol "other-ns") (symbol "pub1"))
             (ns-resolve namespaces (symbol "test-ns") (symbol "pub1"))))
      (is (= (ns-resolve namespaces (symbol "other-ns") (symbol "pub2"))
             (ns-resolve namespaces (symbol "test-ns") (symbol "pub2"))))))

  (testing "throws an exception if the to-ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (refer (empty-ns-map) (symbol "test-ns") (symbol "other-ns")))))

  (testing "throws an exception if the target-ns does not exist"
    (let [namespaces (create-ns (empty-ns-map) (symbol "test-ns"))]
      (is (thrown-with-msg? Exception #"No namespace: other-ns found"
            (refer namespaces (symbol "test-ns") (symbol "other-ns"))))))

  (testing "excludes symbols listed in :exclude"
    (let [namespaces (test-ns-map)
          excluding-pub1 (refer namespaces (symbol "other-ns") (symbol "test-ns") :exclude [(symbol "pub1")])
          excluding-all (refer namespaces (symbol "other-ns") (symbol "test-ns") :exclude [(symbol "pub1") (symbol "pub2")])]
      (is (nil? (ns-resolve excluding-pub1 (symbol "other-ns") (symbol "pub1"))))
      (is (= (ns-resolve excluding-pub1 (symbol "other-ns") (symbol "pub2"))
             (ns-resolve excluding-pub1 (symbol "test-ns") (symbol "pub2"))))
      (is (nil? (ns-resolve excluding-all (symbol "other-ns") (symbol "pub1"))))
      (is (nil? (ns-resolve excluding-all (symbol "other-ns") (symbol "pub2"))))))

  (testing "throws an exception if excluding a symbol that does not exist"
    (is (thrown-with-msg? Exception #"pub3 does not exist"
          (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns") :exclude [(symbol "pub3")]))))

  (testing "throws an exception if excluding a symbol that is not public"
    (is (thrown-with-msg? Exception #"priv1 is not public"
          (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns") :exclude [(symbol "priv1")]))))

  (testing "restricts the refers to only the symbols listed in :only"
    (let [namespaces (test-ns-map)
          only-pub1 (refer namespaces (symbol "other-ns") (symbol "test-ns") :only [(symbol "pub1")])
          only-pub2 (refer namespaces (symbol "other-ns") (symbol "test-ns") :only [(symbol "pub2")])
          only-pub1-pub2 (refer namespaces (symbol "other-ns") (symbol "test-ns") :only [(symbol "pub1") (symbol "pub2")])]

      (is (= (ns-resolve only-pub1 (symbol "other-ns") (symbol "pub1"))
             (ns-resolve only-pub1 (symbol "test-ns") (symbol "pub1"))))
      (is (nil? (ns-resolve only-pub1 (symbol "other-ns") (symbol "pub2"))))

      (is (nil? (ns-resolve only-pub2 (symbol "other-ns") (symbol "pub1"))))
      (is (= (ns-resolve only-pub2 (symbol "other-ns") (symbol "pub2"))
             (ns-resolve only-pub2 (symbol "test-ns") (symbol "pub2"))))

      (is (= (ns-resolve only-pub1-pub2 (symbol "other-ns") (symbol "pub1"))
             (ns-resolve only-pub1-pub2 (symbol "test-ns") (symbol "pub1"))))
      (is (= (ns-resolve only-pub1-pub2 (symbol "other-ns") (symbol "pub2"))
             (ns-resolve only-pub1-pub2 (symbol "test-ns") (symbol "pub2"))))))

  (testing "throws an exception if including a symbol that does not exist"
    (is (thrown-with-msg? Exception #"pub3 does not exist"
          (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns") :only [(symbol "pub3")]))))

  (testing "throws an exception if including a symbol that is not public"
    (is (thrown-with-msg? Exception #"priv1 is not public"
          (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns") :only [(symbol "priv1")]))))

  (testing "does not throw an exception if excluding a symbol that was not included"
    (let [namespaces (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns")
                             :only [(symbol "pub1")]
                             :exclude [(symbol "pub2")])]
      (is (= (ns-resolve namespaces (symbol "other-ns") (symbol "pub1"))
             (ns-resolve namespaces (symbol "test-ns") (symbol "pub1"))))
      (is (nil? (ns-resolve namespaces (symbol "other-ns") (symbol "pub2"))))))

  (testing "renames referred vars"
    (let [namespaces (test-ns-map)
          namespaces (refer namespaces (symbol "other-ns") (symbol "test-ns")
                             :only [(symbol "pub1") (symbol "pub2")]
                             :rename {(symbol "pub1") (symbol "somethingelse1")})]

      (is (= (ns-resolve namespaces (symbol "other-ns") (symbol "somethingelse1"))
             (ns-resolve namespaces (symbol "test-ns") (symbol "pub1"))))

      (is (= (ns-resolve namespaces (symbol "other-ns") (symbol "pub2"))
             (ns-resolve namespaces (symbol "test-ns") (symbol "pub2"))))))

  (testing "throws an exception if renaming a symbol that is not public"
    (is (thrown-with-msg? Exception #"priv1 is not public"
          (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns")
                  :rename {(symbol "priv1") (symbol "else")}))))

  (testing "throws an exception if renaming a symbol that does not exist"
    (is (thrown-with-msg? Exception #"pub3 does not exist"
          (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns")
                  :rename {(symbol "pub3") (symbol "else")}))))

  (testing "throws an exception if renaming a symbol that is not referred"
    (is (thrown-with-msg? Exception #"pub2 is not referred"
          (refer (test-ns-map) (symbol "other-ns") (symbol "test-ns")
                  :only [(symbol "pub1")]
                  :rename {(symbol "pub2") (symbol "else")})))))

(deftest ns-map-test
  (testing "returns all mappings"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub2"))
                       (intern (symbol "test-ns") (with-meta (symbol "priv1") {:private true}))
                       (intern (symbol "test-ns") (with-meta (symbol "priv2") {:private true}))
                       (refer  (symbol "test-ns") (symbol "other-ns")
                               :only [(symbol "pub1")]
                               :rename {(symbol "pub1") (symbol "other-ns-pub1")}))
          mappings (ns-map namespaces (symbol "test-ns"))]
      (is (= #{(symbol "pub1") (symbol "pub2") (symbol "priv1") (symbol "priv2") (symbol "other-ns-pub1")} (set (keys mappings))))
      (is (identical? (get mappings (symbol "other-ns-pub1"))
                      (ns-resolve namespaces (symbol "other-ns") (symbol "pub1"))))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-map (empty-ns-map) (symbol "test-ns"))))))

(deftest ns-interns-test
  (testing "returns all interns"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub2"))
                       (intern (symbol "test-ns") (with-meta (symbol "priv1") {:private true}))
                       (intern (symbol "test-ns") (with-meta (symbol "priv2") {:private true})))
          pubs (ns-interns namespaces (symbol "test-ns"))
          pub1-var (get pubs (symbol "pub1"))
          pub2-var (get pubs (symbol "pub2"))
          priv1-var (get pubs (symbol "priv1"))
          priv2-var (get pubs (symbol "priv2"))]
      (is (= #{(symbol "pub1") (symbol "pub2") (symbol "priv1") (symbol "priv2")} (set (keys pubs))))
      (is (= "test-ns" (namespace pub1-var)))
      (is (= "pub1" (name pub1-var)))
      (is (= "test-ns" (namespace pub2-var)))
      (is (= "pub2" (name pub2-var)))
      (is (= "test-ns" (namespace priv1-var)))
      (is (= "priv1" (name priv1-var)))
      (is (= {:private true} (meta priv1-var)))
      (is (= "test-ns" (namespace priv2-var)))
      (is (= "priv2" (name priv2-var)))
      (is (= {:private true} (meta priv2-var)))))

  (testing "does not return vars interned in another namespace"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub2"))
                       (refer (symbol "test-ns") (symbol "other-ns")
                               :only [(symbol "pub1")]
                               :rename {(symbol "pub1") (symbol "pub3")}))
          pubs (ns-interns namespaces (symbol "test-ns"))
          pub1-var (get pubs (symbol "pub1"))
          pub2-var (get pubs (symbol "pub2"))]
      (is (= #{(symbol "pub1") (symbol "pub2")} (set (keys pubs))))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-interns (empty-ns-map) (symbol "test-ns"))))))

(deftest ns-publics-test
  (testing "returns all interns without :private true meta"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub2"))
                       (intern (symbol "test-ns") (with-meta (symbol "priv1") {:private true}))
                       (intern (symbol "test-ns") (with-meta (symbol "priv2") {:private true})))
          pubs (ns-publics namespaces (symbol "test-ns"))
          pub1-var (get pubs (symbol "pub1"))
          pub2-var (get pubs (symbol "pub2"))]
      (is (= #{(symbol "pub1") (symbol "pub2")} (set (keys pubs))))
      (is (= "test-ns" (namespace pub1-var)))
      (is (= "pub1" (name pub1-var)))
      (is (= "test-ns" (namespace pub2-var)))
      (is (= "pub2" (name pub2-var)))))

  (testing "does not return vars interned in another namespace"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub2"))
                       (intern (symbol "test-ns") (with-meta (symbol "priv1") {:private true}))
                       (intern (symbol "test-ns") (with-meta (symbol "priv2") {:private true})))
          other-ns-pub1 (get-in namespaces [(symbol "other-ns") :mappings (symbol "pub1")])
          namespaces (assoc-in namespaces [(symbol "test-ns") :mappings (symbol "pub3")] other-ns-pub1)
          pubs (ns-publics namespaces (symbol "test-ns"))
          pub1-var (get pubs (symbol "pub1"))
          pub2-var (get pubs (symbol "pub2"))]
      (is (= #{(symbol "pub1") (symbol "pub2")} (set (keys pubs))))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-publics (empty-ns-map) (symbol "test-ns"))))))

(deftest ns-refers-test
  (testing "returns all vars refered by a namespace"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (intern (symbol "other-ns") (symbol "pub1"))
                       (intern (symbol "other-ns") (symbol "pub2"))
                       (intern (symbol "test-ns") (symbol "pub1"))
                       (intern (symbol "test-ns") (symbol "pub2"))
                       (refer (symbol "test-ns") (symbol "other-ns")
                               :rename {(symbol "pub1") (symbol "pub3")
                                        (symbol "pub2") (symbol "pub4")}))
          refers (ns-refers namespaces (symbol "test-ns"))
          pub3-var (get refers (symbol "pub3"))
          pub4-var (get refers (symbol "pub4"))]
      (is (= #{(symbol "pub3") (symbol "pub4")} (set (keys refers))))
      (is (= "other-ns" (namespace pub3-var)))
      (is (= "pub1" (name pub3-var)))
      (is (= "other-ns" (namespace pub4-var)))
      (is (= "pub2" (name pub4-var)))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-refers (empty-ns-map) (symbol "test-ns"))))))

(deftest ns-aliases-test
  (testing "returns aliases for an ns"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (create-ns (symbol "other-ns"))
                       (create-ns (symbol "the-other-ns"))
                       (alias (symbol "test-ns") (symbol "al1") (symbol "other-ns"))
                       (alias (symbol "test-ns") (symbol "al2") (symbol "the-other-ns")))]
      (is (= {(symbol "al1") (symbol "other-ns")
              (symbol "al2") (symbol "the-other-ns")}
             (ns-aliases namespaces (symbol "test-ns"))))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-aliases (empty-ns-map) (symbol "test-ns"))))))

(deftest all-ns-test
  (testing "returns all namespaces"
    (is (= #{(symbol "other-ns") (symbol "test-ns")}
           (-> (create-ns (empty-ns-map) (symbol "test-ns"))
             (create-ns (symbol "other-ns"))
             (all-ns)
             set)))))

(deftest find-ns-test
  (testing "returns the namespace symbol if it exists"
    (is (= (symbol "test-ns")
           (-> (create-ns (empty-ns-map) (symbol "test-ns"))
             (find-ns (symbol "test-ns"))))))

  (testing "returns nil if not found"
    (is (nil? (find-ns (empty-ns-map) (symbol "test-ns"))))))

(deftest ns-name-test
  (testing "returns the name of the namespace if it exists"
    (is (= (symbol "test-ns")
           (-> (create-ns (empty-ns-map) (symbol "test-ns"))
             (ns-name (symbol "test-ns"))))))

  (testing "throws an exception if the namespace does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-name (empty-ns-map) (symbol "test-ns"))))))

(deftest ns-remove-test
  (testing "removes an ns from the map"
    (let [namespaces (create-ns (empty-ns-map) (symbol "test-ns"))]
      (is (= {}
             (ns-remove namespaces (symbol "test-ns"))))))

  (testing "does nothing if the namespace does not exist"
    (let [namespaces (ns-remove (empty-ns-map) (symbol "test-ns"))]
      (is (nil? (find-ns namespaces (symbol "test-ns")))))))

(deftest ns-unmap-test
  (testing "removes a sym from the ns map"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (intern (symbol "test-ns") (symbol "v1"))
                       (intern (symbol "test-ns") (symbol "v2")))
          namespaces (ns-unmap namespaces (symbol "test-ns") (symbol "v1"))]
      (is (nil? (ns-resolve namespaces (symbol "test-ns") (symbol "v1"))))))

  (testing "throws an exception if the namespace does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (ns-unmap (empty-ns-map) (symbol "test-ns") (symbol "v1")))))

  (testing "does nothing if the mapping does not exist"
    (let [namespaces (-> (create-ns (empty-ns-map) (symbol "test-ns"))
                       (ns-unmap (symbol "test-ns") (symbol "v1")))]
      (is (nil? (ns-resolve namespaces (symbol "test-ns") (symbol "v1"))))))

  (testing "throws an exception if the sym is namespace-qualified"
    (is (thrown-with-msg? Exception #"Can't unintern namespace-qualified symbol"
          (ns-unmap (create-ns (empty-ns-map) (symbol "test-ns")) (symbol "test-ns") (symbol "something/qualified"))))))
