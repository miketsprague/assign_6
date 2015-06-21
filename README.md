# MiniJS Interpreter

An interpreter to a simple language called miniJS.

# Overview

* README: this file
* interpreter.scala: the main entry point
* syntax.scala: defines the language AST and parser
* values.scala: defines the language values
* domains.scala: defines the store and environment classes
* freelist.scala: contains the heap and freelist implementations
* gc.scala: contains garbage collectors for miniJS
* makefile: rules for building the interpreter
* build/: where the compiled binaries go
* test suite/: a set of test programs

# Compiling and Running

1. make
2. cd build/
3. scala miniJS -gc (stub|semi|mark|gen) -size <heap size> [-tenured_size <heap size>] [-trace] program.not"

# Language Samples

See 'test_suite/' for sample syntax of MiniJS.
