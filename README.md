*Long story short, this library solves the function colouring problem and some other stuff that the
Keyword Generics Initiative is looking at. It's an experiment in how an effect system could fit into
Rust.*

# effing-mad, an effect library for Rust
This library brings algebraic effects and effect handlers to Rust, by providing traits and macros
that allow writing effectful code in more or less the same style as Rust's existing `async`
functions.

## what does that mean
Effectful functions that use this library can explicitly suspend (`yield`) their execution and pass
control back to their callers, who later resume the function. Data can be carried through both of
these transitions, and this data is strongly typed in the same way as one would expect in Rust.

The data passed out of an effectful function specifies an action. It is given to an effect handler,
which performs the action and passes the result back into the effectful function. Then, execution
of the effectful function continues. It's like calling functions, but upside down.

As it turns out, calling functions upside down has some fantastic advantages. Since effects are
handled inside the caller instead of the callee, there are no bounds on whether they are handled
asynchronously, optionally, fallibly, or a bunch of other adverbs. On top of that, different
callers can use different handlers on the same effectful function, meaning an effectful function
that performs I/O can be called from either a regular `fn` or an `async fn`, and do the I/O in the
"natural" way in both. No more distinction between `popular_crate` and `popular_crate::blocking`!\*

<sup>\* `effing-mad` not guaranteed to become popular. Use at own risk.</sup>

This whole mess can be seen in action in the [`examples/`](./examples/) directory. Check out the
"basic" example first, unless you're really smart or brave or something.

## why
*TL;DR: API experimentation and fun*

I saw [a post](https://blog.rust-lang.org/inside-rust/2022/07/27/keyword-generics.html) about the
recent efforts to make functions be usable from both async and sync contexts. The authors also
wrote about the desire for higher-order functions such as `Option::map` to be able to be
asynchronous, optional, fallible, or a bunch of other adjectives - without having to write a
specialised function like `try_async_map` for each set of effects. This is the problem I was talking
about last paragraph, where I also explain how an effect system could solve it.

Knowing this, I was surprised to see that in the FAQ of the post they answered "are you building
an effect system?" with "not really". They never explained why not! So I did it instead, because
I wanted to see what would happen.

## how cool is it?
effing-mad is cool. Rad, even. Check out all these cool things it has:
* unstable compiler features (coroutines, coroutine_trait)
* a function with 22 type arguments
* `#![no_std]`
* scary category theory words (coproduct! 👻)
* procedural macros
* ~~declarative macros~~ (used to have them, but I needed them to generate idents which they can't do :( )
* `unsafe`
* raw pointers
* `Pin`, which means I'm really good at Rust
* a dependency called "frunk". hehehehe

a market analysis conducted in 2022 determined that no other library has as many cool things as
effing-mad, except maybe core. I mean, core *invented* Pin, which is cheating, so really I win.

## some other facts
In pure functional languages like Haskell, side effects such as I/O are traditionally encoded in
the language and its type system using monads. These are types which hold the effects and/or the
result of the pure computation. So, if you want to have multiple types of effect, things get tricky
because all your effect types want to hold the pure thing inside themselves, but you only have one
pure thing going on. This means you have to use monad transformers. I don't really understand monad
transformers, therefore they are bad.

On the other hand, I do understand algebraic effects, and therefore they are good. Also they compose
far more easily and in my opinion are more intuitive. Download effing-mad today!

## more facts - to do with rust this time
effing-mad uses coroutines. The implementation of it was made far easier by the lang team already
needing functions that can be suspended and resumed, because that caused them to invent coroutines.
Even though they were made for the compiler to be able to compile async fns, they're way more
general than async fns are.

Coroutines are used by the compiler, but directly using them has not been stabilised yet because no
one really needs it to be. That's why you need to use a nightly compiler to use effing-mad.
