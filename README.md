# Vache programming language

Rust without caring about memory management

Python-like but typed

No GC, no RC.

# Features

## Imperative style

Kitchen sink:

```
fn main() {
    var x = 12;
    if x > 0 {
        debug("above 12");
    } else if x < 0 {
        debug(12);
    }
    while x < 20 {
        x = x + 1;
        if x > 15 {
            continue;
        }
        debug(x);
        if x < 18 {
            break;
        }
    }

    var a = [1, 2, 3];
    push(@a, 4);
    assert(a == [1, 2, 3, 4]);
}
```

Note: now with type inference.

## Types and values

* Unbounded integers: `5` of type `int`
* Mutable strings: `"hello"` of type `str`
* Booleans: `true` and `false` of type `bool`
* Unit/void value: `()` of type `()`
* Structures:

```
struct Person {
    x: int,
    y: str,
}
```

* Arrays `[a, b, c]` of type `[T]`
* Enumerated types (pattern matching is WIP):

```
enum Person {
    Test(int),
    Test2(int),
}
```

* Hashmaps (WIP)
* Sets (WIP)

# Semantics

Motto: _as if_ “Every use of a variable clones the value of the variable”

Implies:

* Following code will print "4 2"

```
n = 2
x = n
n = n + 2
debug(n, x) // 4 2
```

* To pass mutable references, you need to use the `@` operator

```
n = 2
x = @n
n = n + 2
debug(n, x) // 4 4
```

Under the hood, optimizations are done on the output code to clone as little as possible while respecting the semantics.

# Backend

Outputs Rust code that relies heavily on `Cow`s and the `big_num` crate.
