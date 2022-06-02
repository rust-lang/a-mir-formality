---
sidebar_position: 2
---

# Setup

How to build, test, and run a-mir-formality:

* [Download and install racket](https://download.racket-lang.org/)
    * run `nix-shell` if you have nix available to get everything set up automatically
    * For tips on running without X11 on linux / wsl, see FAQ below.
* Check out the a-mir-formality repository
* Run `raco test -t -j 22 src` to run the tests
    * The `-t` option prints a summary table at the end so you can see which modules had failures.
    * The `-j 22` option uses 22 parallel threads; you may want to tune the number depending on how many cores you have (N.B. `-j22` does not work).
* You can use [DrRacket](https://docs.racket-lang.org/drracket/), or you can use VSCode. We recommend the following VSCode extensions:
    * [Magic Racket](https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket) to give a Racket mode that supports most LSP operations decently well
        * Note that you have to run `raco pkg install racket-langserver`!
    * [Rainbow brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets) is highly recommended to help sort out `()`
    * The [Unicode Math](https://marketplace.visualstudio.com/items?itemName=studykit.unicode-math) or [Unicode Latex](https://marketplace.visualstudio.com/items?itemName=oijaz.unicode-latex) extensions are useful for inserting characters like `∀`.

# Debugging tips

## Run racket manually for better stacktraces

When you use `raco test`, you often get stacktraces that are not very helpful. You can do better by running racket manually with the `-l errortrace` flag, which adds some runtime overhead but tracks more information. The easiest way to do this is to use [the `test` script](https://github.com/nikomatsakis/a-mir-formality/blob/main/test) and give it the name of some `rkt` file, e.g. `src/decl/test/copy.rkt`. This will run the tests found in the `test` submodule within that file.

```bash
./test src/decl/test/copy.rkt
```

This will expand to running `racket` with a command like this:

```bash
racket -l errortrace -l racket/base -e '(require (submod "src/decl/test/copy.rkt" test))' 
```


## The `traced` macro

The `(traced '() expr)` macro is used to wrap tests throughout the code. The `'()` is a list of metafunctions and judgments you want to trace. Just add the name of something in there, like `lang-item-ok-goals`, and racket will print out the arguments when it is called, along with its return value:

```scheme
(traced '(lang-item-ok-goals) expr)
```

# Frequently asked questions

## My language server keeps crashing! What gives?

The most common cause of this is that you are on linux/WSL and don't have X11.

If you check output and see errors like this:

```
Unable to init server: Could not connect: Connection refused
Gtk initialization failed for display ":0"
```

then this is indeed the problem.

The problem is that the language server works by running a headless version of racket and GUI functionality is required for some of that, even if it isn't used.

## Can I use racket without X11 support?

Yes, but. One workaround is to install racket into a directory like `~/racket` but not put that on your path. You can then create two binaries in `~/bin`:

`~/bin/racket`:

```
#!/bin/bash
xvfb-run -a $HOME/racket/bin/racket "$@"
```

`~/bin/raco`:

```
#!/bin/bash
xvfb-run -a $HOME/racket/bin/raco "$@"
```

and then make them executable:

```bash
chmod u+x ~/bin/racket ~/bin/raco
```

That worked for me, anyway.
