# Ponzi Scheme

A simple Scheme interpreter.  Nothing fancy, mostly my own explorations heavily
influenced by

* [SICP Ch.4](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-25.html#%_chap_4)
* [Eli Bendersky](http://eli.thegreenplace.net/2007/12/06/sicp-sections-411-412/#fn1)
* [Eric Holk] (http://blog.theincredibleholk.org/blog/2013/11/27/continuation-passing-style-interpreters/)
* [Matt Might] (http://matt.might.net/articles/by-example-continuation-passing-style/)

## Usage

Run tests with `lein test`.  Get a Scheme repl with `lein run -m
interpreter.core`. Run a Scheme file with `lein run -m /path/to/file`.

## Implementation Details

### The Environment

An environment is an object that maps variables to values. Here it is
implemented as a combination of an env (yes, badly named) and a store. The env
maps variables to locations (implemented with `gensym`) and the store maps
locations to values. So the store is a level of indirection.

The store affords us a single place to manage mutation. Instead of envs which
get closed over when you create a lambda and are thus copied all over the place,
a store is a single universal place for all bindings ever created in the
program. When mutation occurs, the location in the current scope is looked up
the and the mutation can happen accordingly without affecting the location of
the "same" variable in other environments.

### Continuations

Continuations are a way of representing the next step in a computation. It can
be represented as a data structure or as a function. If it is a function, it is
a lot like a callback function, if not precisely that. Here they are used to
manage the list of computations that occur in a program. Using continuations you
can explicity handle how the environments are passed from one piece of a program
to the next.

### A Short Story

I started this project to learn about interpreters, I endedup learning about
Store-Passing-Style and Continuation-Passing-Style.

Clojure doesn't encourage using mutable data structures, so I started thinking
about how to implement `set!` without `set!`. Many conversations with Hacker
Schoolers led me to Store-Passing-Style and Continuation-Passing-Style which is
what is implemented here.
