---
sidebar_position: 2
---

# Setup

How to build, test, and run a-mir-formality:

* [Download and install racket](https://download.racket-lang.org/)
* Check out the a-mir-formality repository
* Run `raco test -j22 src` to run the tests
    * This will use 22 parallel threads; you may want to tune the number depending on how many cores you have.
* You can use [DrRacket](https://docs.racket-lang.org/drracket/), or you can use VSCode. We recommend the following VSCode extensions:
    * [Magic Racket](https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket) to give a Racket mode that supports most LSP operations decently well
    * [Rainbow brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets) is highly recommended to help sort out `()`

# Debugging tips

## Run racket manually for better stacktraces

When you use `raco test`, you often get stacktraces that are not very helpful. You can do better by running racket manually with the `-lerrortrace` flag, which adds some runtime overhead but tracks more information. This command runs raco with error trace enabled:

```bash
racket -l errortrace -l raco -- test src/decl/test/copy.rkt
```

Alternatively, *this* command runs the tests found in the `test` submodule of `src/decl/test/copy.rkt` without using `raco`:

```bash
racket -l errortrace -l racket/base -e '(require (submod "src/decl/test/copy.rkt" test))' 
```


## The `traced` macro

The `(traced '() expr)` macro is used to wrap tests throughout the code. The `'()` is a list of metafunctions and judgments you want to trace. Just add the name of something in there, like `lang-item-ok-goals`, and racket will print out the arguments when it is called, along with its return value:

```scheme
(traced '(lang-item-ok-goals) expr)
```
