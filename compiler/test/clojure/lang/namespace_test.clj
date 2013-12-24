(ns clojure.lang.namespace-test
  (:require [clojure.test           :refer :all]
            [clojure.lang.namespace :refer :all]))

(defn empty-ns-map []
  {})

(deftest create-ns-test
  (testing "creates a namespace"
    (is (= {'new-ns {:mappings {}
                     :aliases  {}
                     :ns       'new-ns}}
           (-create-ns (empty-ns-map) 'new-ns)))))

(deftest alias-test
  (testing "adds an alias to another namespace"
    (is (= {'to-ns {:mappings {}
                    :aliases  {'the-alias 'target-ns}
                    :ns       'to-ns}
            'target-ns {:mappings {}
                        :aliases  {}
                        :ns       'target-ns}}
           (-> (-create-ns (empty-ns-map) 'to-ns)
             (-create-ns 'target-ns)
             (-alias 'to-ns 'the-alias 'target-ns)))))

  (testing "throws an exception if to-ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: new-ns found"
           (-> (-create-ns (empty-ns-map) 'other-ns)
             (-alias 'new-ns 'the-alias 'other-ns)))))

  (testing "throws an exception if target-ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: other-ns found"
           (-> (-create-ns (empty-ns-map) 'new-ns)
             (-alias 'new-ns 'the-alias 'other-ns)))))

  (testing "throws an exception if an alias already exists to another ns"
    (is (thrown-with-msg? Exception #"Alias the-alias already exists in namespace to-ns, aliasing target-ns"
           (-> (-create-ns (empty-ns-map) 'to-ns)
             (-create-ns 'target-ns)
             (-create-ns 'third-ns)
             (-alias 'to-ns 'the-alias 'target-ns)
             (-alias 'to-ns 'the-alias 'third-ns)))))

  (testing "does nothing if the alias already exists"
    (is (= {'to-ns {:mappings {}
                    :aliases  {'the-alias 'target-ns}
                    :ns       'to-ns}
            'target-ns {:mappings {}
                        :aliases  {}
                        :ns       'target-ns}}
           (-> (-create-ns (empty-ns-map) 'to-ns)
             (-create-ns 'target-ns)
             (-alias 'to-ns 'the-alias 'target-ns)
             (-alias 'to-ns 'the-alias 'target-ns)))))

    )

(deftest intern-test
  (testing "intern creates a mapping in the given ns"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'test-ns 'something))
          something-var (get-in namespaces ['test-ns :mappings 'something])]
      (is (instance? clojure.lang.var.Var something-var))
      (is (instance? clojure.lang.var.Unbound @(.root something-var)))
      (is (identical? something-var (.var @(.root something-var))))
      (is (= 'test-ns (.ns something-var)))
      (is (= {} (.meta something-var)))))

  (testing "intern uses the metadata from the symbol"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns (with-meta 'something {:private true
                                                            :other-meta "here"})))
          something-var (-ns-resolve namespaces 'other-ns 'something)]
      (is (= {:private true :other-meta "here"} (.meta something-var)))))

  (testing "throws an exception if the namespace does not exist"
    (is (thrown-with-msg? Exception #"No namespace: the-ns found"
          (-intern (empty-ns-map) 'the-ns 'something))))

  (testing "throws an exception if interning a namespace-qualified sym"
    (is (thrown-with-msg? Exception #"Can't intern namespace-qualified symbol"
          (-intern (-create-ns (empty-ns-map) 'the-ns) 'the-ns 'clojure.core/something)))
    )
  )

(deftest def-test ; irony?
  (testing "def interns the symbol in the given ns"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-def 'test-ns 'something))
          something-var (-ns-resolve namespaces 'test-ns 'something)]
      (is (instance? clojure.lang.var.Var something-var))
      (is (instance? clojure.lang.var.Unbound @(.root something-var)))
      (is (identical? something-var (.var @(.root something-var))))
      (is (= 'test-ns (.ns something-var)))
      (is (= {} (.meta something-var)))))

  (testing "def interns the symbol in a different ns"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-def 'test-ns 'something)
                  (-def 'other-ns 'something))
          something-var2 (-ns-resolve namespaces 'other-ns 'something)]
      (is (instance? clojure.lang.var.Var something-var2))
      (is (instance? clojure.lang.var.Unbound @(.root something-var2)))
      (is (identical? something-var2 (.var @(.root something-var2))))
      (is (= 'other-ns (.ns something-var2)))
      (is (= {} (.meta something-var2)))))

  (testing "def uses the metadata from the symbol"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-def 'test-ns (with-meta 'something {:private true
                                                        :other-meta "here"})))
          something-var (-ns-resolve namespaces 'test-ns 'something)]
      (is (= {:private true :other-meta "here"} (.meta something-var)))))

  (testing "def evaluates and sets the initial value"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-def 'test-ns 'something (identity 1)))
          something-var (-ns-resolve namespaces 'test-ns 'something)]
      (is (= 1 @(.root something-var)))))

  (testing "def sets the initial value and a doc string"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-def 'test-ns 'something "some docs" (identity 1)))
          something-var (-ns-resolve namespaces 'test-ns 'something)]
      (is (= "some docs" (:doc (.meta something-var))))))

  (testing "def sets the root value, doc string and some extra meta"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-def 'test-ns (with-meta 'something {:private true :other-meta "here"}) "some docs" (identity 1)))
          something-var (-ns-resolve namespaces 'test-ns 'something)]
      (is (= "some docs" (:doc (.meta something-var))))
      (is (= true (:private (.meta something-var))))
      (is (= "here" (:other-meta (.meta something-var)))))))

(deftest ns-resolve-test
  (testing "resolves a symbol in a namespace"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-intern 'test-ns 'something))
          created (get-in namespaces ['test-ns :mappings 'something])
          resolved (-ns-resolve namespaces 'test-ns 'something)]
      (is (identical? created resolved))))

  (testing "returns nil if a symbol cannot be resolved"
    (let [namespaces (-create-ns (empty-ns-map) 'test-ns)]
      (is (nil? (-ns-resolve namespaces 'test-ns 'something)))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-resolve (empty-ns-map) 'test-ns 'some1))))

  (testing "resolves a namespaced symbol"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns 'something))
          created (get-in namespaces ['other-ns :mappings 'something])
          resolved (-ns-resolve namespaces 'test-ns 'other-ns/something)]
      (is (identical? created resolved))))

  (testing "returns nil when the namespace of a namespace-qualified symbol ns does not exist"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns 'something))]
      (is (nil? (-ns-resolve namespaces 'test-ns 'some-other/something)))))

  (testing "resolves an aliased namespace"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns 'something)
                  (-alias 'test-ns 'some-other 'other-ns))
          created (get-in namespaces ['other-ns :mappings 'something])
          resolved (-ns-resolve namespaces 'test-ns 'some-other/something)]
      (is (identical? created resolved))))

  (testing "resolves an alias before an existing namespace"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns 'something)
                  (-create-ns 'some-other)
                  (-intern 'some-other 'something)
                  (-alias 'test-ns 'some-other 'other-ns))
          other-ns-something (get-in namespaces ['other-ns :mappings 'something])
          some-other-something (get-in namespaces ['some-other :mappings 'something])
          resolved (-ns-resolve namespaces 'test-ns 'some-other/something)]
      (is (identical? other-ns-something
                      (-ns-resolve namespaces 'test-ns 'some-other/something)))
      (is (identical? some-other-something
                      (-ns-resolve namespaces 'other-ns 'some-other/something))))))

(deftest refer-test

  (defn test-ns-map []
    (-> (-create-ns (empty-ns-map) 'test-ns)
      (-create-ns 'other-ns)
      (-intern 'other-ns 'pub3)
      (-intern 'test-ns 'pub1)
      (-intern 'test-ns 'pub2)
      (-intern 'test-ns (with-meta 'priv1 {:private true}))
      (-intern 'test-ns (with-meta 'priv2 {:private true}))))

  (testing "adds mappings to the to-ns for all ns-publics in the target-ns"
    (let [namespaces (test-ns-map)
          namespaces (-refer namespaces 'other-ns 'test-ns)]
      (is (= (-ns-resolve namespaces 'other-ns 'pub1)
             (-ns-resolve namespaces 'test-ns 'pub1)))
      (is (= (-ns-resolve namespaces 'other-ns 'pub2)
             (-ns-resolve namespaces 'test-ns 'pub2)))))

  (testing "throws an exception if the to-ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-refer (empty-ns-map) 'test-ns 'other-ns))))

  (testing "throws an exception if the target-ns does not exist"
    (let [namespaces (-create-ns (empty-ns-map) 'test-ns)]
      (is (thrown-with-msg? Exception #"No namespace: other-ns found"
            (-refer namespaces 'test-ns 'other-ns)))))

  (testing "excludes symbols listed in :exclude"
    (let [namespaces (test-ns-map)
          excluding-pub1 (-refer namespaces 'other-ns 'test-ns :exclude ['pub1])
          excluding-all (-refer namespaces 'other-ns 'test-ns :exclude ['pub1 'pub2])]
      (is (nil? (-ns-resolve excluding-pub1 'other-ns 'pub1)))
      (is (= (-ns-resolve excluding-pub1 'other-ns 'pub2)
             (-ns-resolve excluding-pub1 'test-ns 'pub2)))
      (is (nil? (-ns-resolve excluding-all 'other-ns 'pub1)))
      (is (nil? (-ns-resolve excluding-all 'other-ns 'pub2)))))

  (testing "throws an exception if excluding a symbol that does not exist"
    (is (thrown-with-msg? Exception #"pub3 does not exist"
          (-refer (test-ns-map) 'other-ns 'test-ns :exclude ['pub3]))))

  (testing "throws an exception if excluding a symbol that is not public"
    (is (thrown-with-msg? Exception #"priv1 is not public"
          (-refer (test-ns-map) 'other-ns 'test-ns :exclude ['priv1]))))

  (testing "restricts the refers to only the symbols listed in :only"
    (let [namespaces (test-ns-map)
          only-pub1 (-refer namespaces 'other-ns 'test-ns :only ['pub1])
          only-pub2 (-refer namespaces 'other-ns 'test-ns :only ['pub2])
          only-pub1-pub2 (-refer namespaces 'other-ns 'test-ns :only ['pub1 'pub2])]

      (is (= (-ns-resolve only-pub1 'other-ns 'pub1)
             (-ns-resolve only-pub1 'test-ns 'pub1)))
      (is (nil? (-ns-resolve only-pub1 'other-ns 'pub2)))

      (is (nil? (-ns-resolve only-pub2 'other-ns 'pub1)))
      (is (= (-ns-resolve only-pub2 'other-ns 'pub2)
             (-ns-resolve only-pub2 'test-ns 'pub2)))

      (is (= (-ns-resolve only-pub1-pub2 'other-ns 'pub1)
             (-ns-resolve only-pub1-pub2 'test-ns 'pub1)))
      (is (= (-ns-resolve only-pub1-pub2 'other-ns 'pub2)
             (-ns-resolve only-pub1-pub2 'test-ns 'pub2)))))

  (testing "throws an exception if including a symbol that does not exist"
    (is (thrown-with-msg? Exception #"pub3 does not exist"
          (-refer (test-ns-map) 'other-ns 'test-ns :only ['pub3]))))

  (testing "throws an exception if including a symbol that is not public"
    (is (thrown-with-msg? Exception #"priv1 is not public"
          (-refer (test-ns-map) 'other-ns 'test-ns :only ['priv1]))))

  (testing "does not throw an exception if excluding a symbol that was not included"
    (let [namespaces (-refer (test-ns-map) 'other-ns 'test-ns :only ['pub1] :exclude ['pub2])]
      (is (= (-ns-resolve namespaces 'other-ns 'pub1)
             (-ns-resolve namespaces 'test-ns 'pub1)))
      (is (nil? (-ns-resolve namespaces 'other-ns 'pub2)))))

  (testing "renames referred vars"
    (let [namespaces (test-ns-map)
          namespaces (-refer namespaces 'other-ns 'test-ns :only ['pub1 'pub2] :rename {'pub1 'somethingelse1})]

      (is (= (-ns-resolve namespaces 'other-ns 'somethingelse1)
             (-ns-resolve namespaces 'test-ns 'pub1)))

      (is (= (-ns-resolve namespaces 'other-ns 'pub2)
             (-ns-resolve namespaces 'test-ns 'pub2)))))

  (testing "throws an exception if renaming a symbol that is not public"
    (is (thrown-with-msg? Exception #"priv1 is not public"
          (-refer (test-ns-map) 'other-ns 'test-ns :rename {'priv1 'else}))))

  (testing "throws an exception if renaming a symbol that does not exist"
    (is (thrown-with-msg? Exception #"pub3 does not exist"
          (-refer (test-ns-map) 'other-ns 'test-ns :rename {'pub3 'else}))))

  (testing "throws an exception if renaming a symbol that is not referred"
    (is (thrown-with-msg? Exception #"pub2 is not referred"
          (-refer (test-ns-map) 'other-ns 'test-ns :only ['pub1] :rename {'pub2 'else})))))

(deftest ns-map-test
  (testing "returns all mappings"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                       (-create-ns 'other-ns)
                       (-intern 'other-ns 'pub1)
                       (-intern 'test-ns 'pub1)
                       (-intern 'test-ns 'pub2)
                       (-intern 'test-ns (with-meta 'priv1 {:private true}))
                       (-intern 'test-ns (with-meta 'priv2 {:private true}))
                       (-refer  'test-ns 'other-ns :only ['pub1] :rename {'pub1 'other-ns-pub1}))
          mappings (-ns-map namespaces 'test-ns)]
      (is (= #{'pub1 'pub2 'priv1 'priv2 'other-ns-pub1} (set (keys mappings))))
      (is (identical? (get mappings 'other-ns-pub1)
                      (-ns-resolve namespaces 'other-ns 'pub1)))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-map (empty-ns-map) 'test-ns)))))

(deftest ns-interns-test
  (testing "returns all interns"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns 'pub1)
                  (-intern 'test-ns 'pub1)
                  (-intern 'test-ns 'pub2)
                  (-intern 'test-ns (with-meta 'priv1 {:private true}))
                  (-intern 'test-ns (with-meta 'priv2 {:private true})))
          pubs (-ns-interns namespaces 'test-ns)
          pub1-var (get pubs 'pub1)
          pub2-var (get pubs 'pub2)
          priv1-var (get pubs 'priv1)
          priv2-var (get pubs 'priv2)]
      (is (= #{'pub1 'pub2 'priv1 'priv2} (set (keys pubs))))
      (is (= 'test-ns (.ns pub1-var)))
      (is (= 'pub1 (.sym pub1-var)))
      (is (= 'test-ns (.ns pub2-var)))
      (is (= 'pub2 (.sym pub2-var)))
      (is (= 'test-ns (.ns priv1-var)))
      (is (= 'priv1 (.sym priv1-var)))
      (is (= {:private true} (.meta priv1-var)))
      (is (= 'test-ns (.ns priv2-var)))
      (is (= 'priv2 (.sym priv2-var)))
      (is (= {:private true} (.meta priv2-var)))))

  (testing "does not return vars interned in another namespace"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                       (-create-ns 'other-ns)
                       (-intern 'other-ns 'pub1)
                       (-intern 'test-ns 'pub1)
                       (-intern 'test-ns 'pub2)
                       (-refer 'test-ns 'other-ns :only ['pub1] :rename {'pub1 'pub3}))
          pubs (-ns-interns namespaces 'test-ns)
          pub1-var (get pubs 'pub1)
          pub2-var (get pubs 'pub2)]
      (is (= #{'pub1 'pub2} (set (keys pubs))))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-interns (empty-ns-map) 'test-ns)))))

(deftest ns-publics-test
  (testing "returns all interns without :private true meta"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns 'pub1)
                  (-intern 'test-ns 'pub1)
                  (-intern 'test-ns 'pub2)
                  (-intern 'test-ns (with-meta 'priv1 {:private true}))
                  (-intern 'test-ns (with-meta 'priv2 {:private true})))
          pubs (-ns-publics namespaces 'test-ns)
          pub1-var (get pubs 'pub1)
          pub2-var (get pubs 'pub2)]
      (is (= #{'pub1 'pub2} (set (keys pubs))))
      (is (= 'test-ns (.ns pub1-var)))
      (is (= 'pub1 (.sym pub1-var)))
      (is (= 'test-ns (.ns pub2-var)))
      (is (= 'pub2 (.sym pub2-var)))))

  (testing "does not return vars interned in another namespace"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                  (-create-ns 'other-ns)
                  (-intern 'other-ns 'pub1)
                  (-intern 'test-ns 'pub1)
                  (-intern 'test-ns 'pub2)
                  (-intern 'test-ns (with-meta 'priv1 {:private true}))
                  (-intern 'test-ns (with-meta 'priv2 {:private true})))
          other-ns-pub1 (get-in namespaces ['other-ns :mappings 'pub1])
          namespaces (assoc-in namespaces ['test-ns :mappings 'pub3] other-ns-pub1)
          pubs (-ns-publics namespaces 'test-ns)
          pub1-var (get pubs 'pub1)
          pub2-var (get pubs 'pub2)]
      (is (= #{'pub1 'pub2} (set (keys pubs))))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-publics (empty-ns-map) 'test-ns)))))

(deftest ns-refers-test
  (testing "returns all vars refered by a namespace"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                       (-create-ns 'other-ns)
                       (-intern 'other-ns 'pub1)
                       (-intern 'other-ns 'pub2)
                       (-intern 'test-ns 'pub1)
                       (-intern 'test-ns 'pub2)
                       (-refer 'test-ns 'other-ns :rename {'pub1 'pub3
                                                           'pub2 'pub4}))
          refers (-ns-refers namespaces 'test-ns)
          pub3-var (get refers 'pub3)
          pub4-var (get refers 'pub4)]
      (is (= #{'pub3 'pub4} (set (keys refers))))
      (is (= 'other-ns (.ns pub3-var)))
      (is (= 'pub1 (.sym pub3-var)))
      (is (= 'other-ns (.ns pub4-var)))
      (is (= 'pub2 (.sym pub4-var)))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-refers (empty-ns-map) 'test-ns)))))

(deftest ns-aliases-test
  (testing "returns aliases for an ns"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                       (-create-ns 'other-ns)
                       (-create-ns 'the-other-ns)
                       (-alias 'test-ns 'al1 'other-ns)
                       (-alias 'test-ns 'al2 'the-other-ns))]
      (is (= {'al1 'other-ns
              'al2 'the-other-ns}
             (-ns-aliases namespaces 'test-ns)))))

  (testing "throws an exception if the ns does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-aliases (empty-ns-map) 'test-ns)))))

(deftest all-ns-test
  (testing "returns all namespaces"
    (is (= #{'other-ns 'test-ns}
           (-> (-create-ns (empty-ns-map) 'test-ns)
             (-create-ns 'other-ns)
             (-all-ns)
             set)))))

(deftest find-ns-test
  (testing "returns the namespace symbol if it exists"
    (is (= 'test-ns
           (-> (-create-ns (empty-ns-map) 'test-ns)
             (-find-ns 'test-ns)))))

  (testing "returns nil if not found"
    (is (nil? (-find-ns (empty-ns-map) 'test-ns)))))

(deftest ns-name-test
  (testing "returns the name of the namespace if it exists"
    (is (= 'test-ns
           (-> (-create-ns (empty-ns-map) 'test-ns)
             (-ns-name 'test-ns)))))

  (testing "throws an exception if the namespace does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-name (empty-ns-map) 'test-ns)))))

(deftest ns-remove-test
  (testing "removes an ns from the map"
    (let [namespaces (-create-ns (empty-ns-map) 'test-ns)]
      (is (= {}
             (-ns-remove namespaces 'test-ns)))))

  (testing "does nothing if the namespace does not exist"
    (let [namespaces (-ns-remove (empty-ns-map) 'test-ns)]
      (is (nil? (find-ns 'test-ns))))))

(deftest ns-unmap-test
  (testing "removes a sym from the ns map"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                       (-intern 'test-ns 'v1)
                       (-intern 'test-ns 'v2))
          namespaces (-ns-unmap namespaces 'test-ns 'v1)]
      (is (nil? (-ns-resolve namespaces 'test-ns 'v1)))))

  (testing "throws an exception if the namespace does not exist"
    (is (thrown-with-msg? Exception #"No namespace: test-ns found"
          (-ns-unmap (empty-ns-map) 'test-ns 'v1))))

  (testing "does nothing if the mapping does not exist"
    (let [namespaces (-> (-create-ns (empty-ns-map) 'test-ns)
                       (-ns-unmap 'test-ns 'v1))]
      (is (nil? (-ns-resolve namespaces 'test-ns 'v1)))))

  (testing "throws an exception if the sym is namespace-qualified"
    (is (thrown-with-msg? Exception #"Can't unintern namespace-qualified symbol"
          (-ns-unmap (-create-ns (empty-ns-map) 'test-ns) 'test-ns 'something/qualified)))))
