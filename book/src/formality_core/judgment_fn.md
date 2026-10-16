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
pub fn has_type(arg0: impl Upcast<Env>, arg1: impl Upcast<Expr>) -> ProvenSet<Type> {
    let arg0: Env = arg0.upcast();
    let arg1: Expr = arg1.upcast();
    
    ...
}
```

Some things to note. First, the function arguments (`arg0`, `arg1`) implicitly accept anything that "upcasts" (infallibly converts) into the desired types. `Upcast` is a trait defined within a-mir-formality and implemented by the `#[term]` macro automatically. 

Second, the function always returns a `ProvenSet`. This is because there can be more rules, and they may match in any ways. The generated code is going to exhaustively search to find all the ways that the rules could match. Unlike a plain `Set`, a `ProvenSet` also preserves proof trees for successful results and structured failure information when no rule applies. At a high-level the code looks like this (at least if we ignore the possibility of cycles; we'll come back to that later):

```rust
pub fn has_type(arg0: impl Upcast<Env>, arg1: impl Upcast<Expr>) -> ProvenSet<Type> {
    let arg0: Env = arg0.upcast();
    let arg1: Expr = arg1.upcast();
    
    let mut results = Map::new();

    if /* inference rule 1 matches */ {
        results.insert(/* result from inference rule 1 */, /* proof tree */);
    }
    
    if /* inference rule 2 matches */ {
        results.insert(/* result from inference rule 2 */, /* proof tree */);
    }
    
    // ... 
    
    if /* inference rule N matches */ {
        results.insert(/* result from inference rule N */, /* proof tree */);
    }
    
    if !results.is_empty() {
        ProvenSet::proven(results)
    } else {
        ProvenSet::failed_rules(/* judgment */, /* location */, /* failed rules */)
    }
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

If you want to see a real judgement, take a look at the one for proving where clauses:

{judgment}`prove_predicate`

### Checking every element with `for_all`

A `for_all` premise requires its inner premises to succeed for **every** element
of a collection:

```rust,ignore
(for_all(item in items)
    (check_item(env, item) => ())
    (if item.is_supported()))
```

The collection expression is evaluated once, and elements are visited in its
iteration order. Each iteration binds `item` and runs the parenthesized premises
inside the `for_all`. Those premises use the same syntax as other rule premises,
including judgment calls, `let` bindings, guards, and nested `for_all` premises.

If any element has no successful derivation through all the inner premises, the
`for_all` fails and the enclosing rule does not continue. An empty collection
succeeds without running the inner premises. This differs from `(item in items)`,
which chooses an element and explores the rest of the rule for each choice.

Successful proofs contain a `for_all` subtree with a child for each element,
recording that element and the proofs of its inner premises. An unsuccessful
inner premise contributes its usual failure explanation.

### Carrying state with `for_all ... with(...)`

Use `with(...)` to pass updated values from one iteration to the next. Initialize
the variables before the loop, then shadow them inside the loop to provide their
next values:

```rust,ignore
(let state = initial_state)
(for_all(item in items) with(state)
    (check_step(env, state, item) => state))
(check_final_state(state) => ())
```

The first iteration receives the initial `state`; each later iteration receives
the preceding iteration's updated `state`. Premises after the `for_all` see the
final value. For an empty collection, that value is the initial value. Bindings
created inside the loop stay local unless they are named in `with(...)`.

Several values can be carried together, and ordinary `let` premises can update
them too:

```rust,ignore
(let count: usize = 0)
(for_all(item in items) with(state, count)
    (check_step(env, state, item) => state)
    (let count: usize = count + 1))
```

Like other judgment bindings, carried values are available by reference; there
is no need to mutate them in place.

The current implementation retains **one successful body result per element**
(the last one visited), together with its updated carried values and proof. It
does not explore every combination of results across iterations or backtrack to
an earlier element if a later element fails. Use this construct when one
successful result per element is sufficient; algorithms that need to explore all
combinations require a separate combinator.

### Constructing collections with `for_all`

A `let` premise can construct a collection by producing a value for every input:

```rust,ignore
(let expected_names: Set<FieldName> = for_all field in fields => field.name)
(let provided_names: Set<FieldName> = for_all field in field_exprs => field.name)
(if expected_names == provided_names)
```

After `=>`, write ordinary parenthesized judgment premises followed by an output
expression. The premises can call fallible judgments, bind auxiliary values, or
check guards; the output expression uses their bindings. Braces and parentheses
around the result are optional:

```rust,ignore
(let lowered_items: Vec<CoreItem> = for_all item in items =>
    (lower_item(env, item) => lowered)
    (validate(lowered) => ())
    lowered
)
```

As a shorthand, a final judgment premise that binds a single identifier yields
that binding directly:

```rust,ignore
(let lowered_items: Vec<CoreItem> = for_all item in items =>
    (lower_item(env, item) => lowered)
)
```

This is equivalent to adding `lowered` as the final expression. A destructuring
pattern does not select an output automatically; follow it with an explicit
output expression, as in the map example below. A final guard such as
`(if condition)` does not produce a result either; it must be followed by an
output expression.

The parser treats each non-final parenthesized group as a premise, then parses
the remaining tokens as the output expression (or the final judgment shorthand
above). Consequently, an expression starting with a parenthesized subexpression,
such as `(foo()) * 3`, needs to be grouped as a whole: write `((foo()) * 3)` or
`{ (foo()) * 3 }`. A final tuple such as `(key, value)` already forms a single
group and needs no extra parentheses. A `!` immediately after a premise is that
premise's confirmation marker; to return a negation after a premise, write
`(!flag)` or `{ !flag }`.

Every input must succeed: a failed guard or judgment **does not filter** the
input out. If any input has no successful derivation through the premises, the
whole construction fails and the enclosing rule does not continue. An empty
input produces an empty collection.

This is shorthand for collecting results with `for_all ... with(...)`, and has
the same proof and outcome-selection behavior: it retains one successful result
per input, rather than enumerating combinations of alternative results.

Each output is implicitly borrowed and upcast to the collection's element type.
For example, `field.name` above produces an owned `FieldName` without needing an
explicit clone or `.upcast()`, even when `field` is borrowed. The resulting values
are collected using `FromIterator`, so the destination type selects the
collection:

- `Vec<T>` preserves input order and duplicates.
- `Set<T>` removes duplicates and uses the set's ordering.
- `Map<K, V>` collects `(key, value)` pairs, with implicit upcasting of their
  components. Duplicate keys follow the map's ordinary `FromIterator` behavior;
  they do not cause a judgment failure.

For example:

```rust,ignore
(let items_by_name: Map<ItemName, CoreItem> = for_all item in items =>
    (lower_named_item(env, item) => (name, lowered))
    (name, lowered)
)
```

The type annotation is optional when surrounding uses determine the collection
and element types. Add an annotation if Rust cannot infer the target of the
implicit upcast or the collection type.

This syntax is available inside a judgment's `let` premise; it is not an ordinary
Rust expression. The `for_all` body does not select a collection type.

### Handling cycles

Judgment functions must be **inductive**, which means that cycles are considered failures. We have a tabling implementation, which means we detect cycles and try to handle them intelligently. Basically we track a stack and, if a cycle is detected, we return an empty set of results. But we remember that the cycle happened. Then, once we are done, we'll have computed some intermediate set of results `R[0]`, and we execute again. This time, when we get the cycle, we return `R[0]` instead of an empty set. This will compute some new set of results, `R[1]`. So then we try again. We keep doing this until the new set of results `R[i]` is equal to the previous set of results `R[i-1]`. At that point, we have reached a fixed point, so we stop. Of course, it could be that you get an infinitely growing set of results, and execution never terminates. This means your rules are broken. Don't do that.
