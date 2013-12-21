# clojure-ruby

An experiment to compile Clojure to Ruby.

## Development

1. Install the Clojure analyzer

```bash
$ git clone git@github.com:clojure/tools.analyzer.git
$ cd tools.analyzer
$ lein install
```

2. Clone this repo

```bash
$ git clone git@github.com:mylesmegyesi/clojure-ruby.git
$ cd clojure-ruby
```

3. Run the tests for the compiler

```bash
$ cd compiler
$ lein test
```

3. Run the tests for the stdlib

```bash
$ cd compiler
$ lein run -- -o "out" ../stdlib/src ../stdlib/test
$ ruby main.rb
```
