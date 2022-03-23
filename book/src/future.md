## Future directions

Here are some of the next steps I've been thinking about and we could discuss:

* There are some fixes to how `Hypothesize-implies` works that I want to
  make (I think it should be "bottom-up", not "top-down" as it is today,
  if that makes sense -- basically adopt the elaboration strategy from)
* Sketch out the MIR type-checker
* Integrate higher-ranked types and regions, polonius, etc -- I've been
  reading up on subtyping systems and I think I have some ideas for a fresh
  approach here
* Implement the "recursive solver" as a metafunction
    * it is useful to be able to compare the "cosld-solver",
      which I believe is sound+complete, against ours, which (I hope) is merely sound
* Extend rustc with `-Zformality` to allow it to generate formality programs
* Use PLT redex to fuzz the existing solver and test the "invariant logic"
