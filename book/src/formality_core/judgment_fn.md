# Judgment functions and inference rules

The next thing is the `judgment_fn!` macro. This lets you write a *judgment* using *inference rules*. A "judgment" just means some kind of predicate that the computer can judge to hold or not hold. Inference rules are those rules you may have seen in papers and things:

```
premise1
premise2
premise3
------------------
conclusion
```

i.e., the conclusion is judged to be true if all the premises are true.

[`prove_wc`]: https://github.com/rust-lang/a-mir-formality/blob/bca36ecd069d6bdff77bffbb628ae3b2ef4f8ef7/crates/formality-prove/src/prove/prove_wc.rs#L21-L125

Judgments in type system papers can look all kinds of ways. For example, a common type system judgment would be the following:

```
Γ ⊢ E : T
```

This can be read as, in the environment Γ, the expression E has type T. You might have rule like these:

```
Γ[X] = ty                   // lookup variable in the environment
--------------- "variable"
Γ ⊢ X : ty

Γ ⊢ E1 : T                  // must have the same type
Γ ⊢ E2 : T
--------------- "add"
Γ ⊢ E1 + E2 : T
```

In a-mir-formality, you might write those rules like so:

```rust
judgment_fn! {
    pub fn has_type(
        env: Env,
        expr: Expr,
    ) => Type {
        (
            (env.get(&name) => ty)
            ---------------
            (has_type(env, name: Variable) => ty)
        )
        
        (
            (has_type(env, left) => ty_left)
            (has_type(env, right) => ty_right)
            (if ty_left == ty_right)
            ---------------
            (has_type(env, Expr::Add(left, right)) => ty_left)
        )
    }
}
```

Unlike mathematical papers, where judgments can look like whatever you want, judgments in a-mir-formality always have a fixed form that distinguish inputs and outputs:

```
judgment_name(input1, input2, input3) => output
```

In this case, `has_type(env, expr) => ty` is the equivalent of `Γ ⊢ E : T`. Note that, by convention, we tend to use more English style names, so `env` and not `Γ`, and `expr` and not `E`. Of course nothing is stop you from using single letters, it's just a bit harder to read.

When we write the `judgement_fn`, it is going to desugar into an actual Rust function that looks like this:

```rust
pub fn has_type(arg0: impl Upcast<Env>, arg1: impl Upcast<Expr>) -> Set<Type> {
    let arg0: Env = arg0.upcast();
    let arg1: Expr = arg1.upcast();
    
    ...
}
```

Some things to note. First, the function arguments (`arg0`, `arg1`) implicitly accept anything that "upcasts" (infallibly converts) into the desired types. `Upcast` is a trait defined within a-mir-formality and implemented by the `#[term]` macro automatically. 

Second, the function always returns a `Set`. This is because there can be more rules, and they may match in any ways. The generated code is going to exhaustively search to find all the ways that the rules could match. At a high-level the code looks like this (at least if we ignore the possibility of cycles; we'll come back to that later):

```rust
pub fn has_type(arg0: impl Upcast<Env>, arg1: impl Upcast<Expr>) -> Set<Type> {
    let arg0: Env = arg0.upcast();
    let arg1: Expr = arg1.upcast();
    
    let mut results = set![]; // we define this macro

    if /* inference rule 1 matches */ {
        results.push(/* result from inference rule 1 */);
    }
    
    if /* inference rule 2 matches */ {
        results.push(/* result from inference rule 1 */);
    }
    
    // ... 
    
    if /* inference rule N matches */ {
        results.push(/* result from inference rule N */);
    }
    
    results
}
```

So how do we test if a particular inference rule matches? Let's look more closely at the code we would generate for this inference rule:

```rust
(
    (env.get(name) => ty)
    ---------------
    (has_type(env, name: Variable) => ty)
)
```

The first part of the final line, `has_type(env, name: Variable)`, defines patterns that are matched against the arguments. These are matched against the arguments (`arg0`, `arg1`) from the judgment. Patterns can either be trivial bindings (like `env`) or more complex (like `name: Variable` or `Expr::Add(left, right)`). In the latter case, they don't have to match the type of the argument precisely. Instead, we use the `Downcast` trait combined with pattern matching. So this inference rule would compile to something like...

```rust
// Simple variable bindings just clone...
let env = arg0.clone();

// More complex patterns are downcasted and testing...
if let Some(name) = arg1.downcast::<Variable>() {
    ... // rule successfully matched! See below.
}
```

Once we've matched the arguments, we start trying to execute the inference rule conditions. We have one, `env.get(&name) => ty`. What does that do? A condition written like `$expr => $pat` basically becomes a for loop, so you get...

```rust
let env = arg0.clone();
if let Some(name) = arg1.downcast::<Variable>() {
    for ty in env.get(&name) {
        ... // other conditions, if any
    }
}
```

Once we run out of conditions, we can generate the final result, which comes from the `=> $expr` in the conclusion of the rule. In this case, something like this:

```rust
let env = arg0.clone();
if let Some(name) = arg1.downcast::<Variable>() {
    for ty in env.get(&name) {
        result.push(ty);
    }
}
```

Thus each inference rule is converted into a little block of code that may push results onto the final set.

The second inference rule (for addition) looks like...

```rust
// given this...
// (
//     (has_type(env, left) => ty_left)
//     (has_type(env, right) => ty_right)
//     (if ty_left == ty_right)
//     ---------------
//     (has_type(env, Expr::Add(left, right)) => ty_left)
// )
// we generate this...
let env = arg0.clone();
if let Some(Expr::Add(left, right)) = arg1.downcast() {
    for ty_left in has_type(env, left) {
        for ty_right in has_type(env, right) {
            if ty_left == ty_right {
                result.push(ty_left);
            }
        }
    }
}
```

If you want to see a real judgement, take a look at the one for [proving where clauses][`prove_wc`].

[`prove_wc`]: https://github.com/rust-lang/a-mir-formality/blob/bca36ecd069d6bdff77bffbb628ae3b2ef4f8ef7/crates/formality-prove/src/prove/prove_wc.rs#L21-L125

### Handling cycles

Judgment functions must be **inductive**, which means that cycles are considered failures. We have a tabling implementation, which means we detect cycles and try to handle them intelligently. Basically we track a stack and, if a cycle is detected, we return an empty set of results. But we remember that the cycle happened. Then, once we are done, we'll have computed some intermediate set of results `R[0]`, and we execute again. This time, when we get the cycle, we return `R[0]` instead of an empty set. This will compute some new set of results, `R[1]`. So then we try again. We keep doing this until the new set of results `R[i]` is equal to the previous set of results `R[i-1]`. At that point, we have reached a fixed point, so we stop. Of course, it could be that you get an infinitely growing set of results, and execution never terminates. This means your rules are broken. Don't do that.
