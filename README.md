# scheme_x86

Writing a Scheme to x86 compiler by following [Abdulaziz Ghuloum's "An
Incremental Approach to Compiler
Construction"](http://lambda-the-ultimate.org/node/1752).

This also takes _a lot_ of inspiration from
[namin/inc](https://github.com/namin/inc) and the [Let's Build a
Compiler](https://generalproblem.net/lets_build_a_compiler/01-starting-out/)
series of blog posts.

### Requirements

* Linux (I use a Docker container on macOS for that)
* Chez Scheme

### Running the tests

```bash
docker build -t incremental . && \
  docker run -it -v $(pwd):/code incremental -c 'cd /code/src/ && make test'
```

Running a single test is possible by specifying the filename:

```
TEST=test-unary-primitives make test
```
