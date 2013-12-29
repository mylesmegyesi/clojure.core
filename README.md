# clojure-ruby

## Goals

1. Compile Clojure to Ruby.
2. Reimplement clojure.lang package in Clojure
3. Reimplement stdlib (clojure.core and associated) as modular components
4. Make clojure.lang and clojure stdlib host agnostic
5. Maintain as much backwards compatibility as possible

## Development

### Standard library

```bash
$ cd stdlib
$ lein test
```

### Clojure -> Ruby compiler

```bash
$ cd compiler
$ lein test
```
