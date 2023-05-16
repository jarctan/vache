# Vache programming language

Rust without to care about memory management
Python-like but typed
No GC, no RC.

# Features
## Imperative style
A program is composed of functions.
Functions are list of statements.
Each statement can be
* an assignment
* a while loop
* an if/then/else
## Types
* Unbounded integers: `x = 5`
* Mutable strings: `x = "hello"`
* Booleans: `true` and `false`
* Unit/void value: `x = ()`
* Structures:
```
struct Person {
    x: int,
    y: str,
}
```
* Arrays (WIP)
* Enumerated types (WIP)
* Hashmaps (WIP)
* Sets (WIP)

# Semantics
Motto: _as if_ right-hand-side variables are cloned on every use

Implies:
* No way to pass a mutable reference of a variable to a function
* Following code will print "4 2"
```
n = 2
x = n
n = n + 2 // de-sugars to +(n, 2)
print(n, x)
```

Under the hood, two optimizations are done on the output code:
* do not clone on last use
* borrow instead of clone as much as possible

# Backend
Outputs Rust code that relies heavily on `Cow`s and the `rug` crate.