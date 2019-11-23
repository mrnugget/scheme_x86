# scheme_x86

Writing a Scheme to x86 compiler by following [Abdulaziz Ghuloum's "An Incremental Approach to Compiler Construction"](http://lambda-the-ultimate.org/node/1752).

This also takes _a lot_ of inspiration from [namin/inc](https://github.com/namin/inc) and the [Let's Build a Compiler](https://generalproblem.net/lets_build_a_compiler/01-starting-out/) series of blog posts.

### Features

The best way to get an up-to-date overview of the supported features is to look
into the `./src/tests/` folder.

But in terms of Ghuloum's paper: the compiler here implements most of the
functionality (except some macro expansions and functions) described in the
sections up to and including "3.18 Apply".

### Requirements

* Linux (I use a Docker container on macOS for that)
* Chez Scheme

### Running the tests

```bash
docker build -t scheme_x86 . && \
  docker run -it -v $(pwd):/code scheme_x86 -c 'cd /code/src/ && make test'
```

Running a single test is possible by specifying the filename:

```
TEST=test-unary-primitives make test
```
