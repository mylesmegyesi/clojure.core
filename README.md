# clojure.core

## Goals

* Reimplement clojure.lang package in Clojure
* Reimplement stdlib (clojure.core and associated) as modular components
* Make clojure.lang and clojure stdlib host agnostic
* Build a comprehensive test suite for Clojure
* Maintain as 100% backwards compatibility

## Run the tests

In order to test that the new implementation of clojure.core is
backwards compatible, we run our test suite against the old
Clojure implementations.

To run against the new JVM implementaiton:

```bash
$ lein with-profile new-jvm test
```

To run against the old JVM implementaiton:

```bash
$ lein with-profile old-jvm test
```

To run against the new CLR implementaiton:

```bash
$ lein with-profile new-clr clr-test
```

To run against the old CLR implementaiton:

```bash
$ lein with-profile old-clr clr-test
```
