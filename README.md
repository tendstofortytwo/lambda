# lambda

Lambda calculus interpreter written in Rust.

Accompanying blog post: https://prose.nsood.in/rust-lambda

To run, `cargo run .` and provide it lambda calculus expressions of your choice, substituting `\` for the lambda symbol.

```
(\a.\b.a) b
-> λb_1.b
```

This interpreter also supports storing things in a context:

```
(\a.\b.a) b
-> λb_1.b
true = \t.\f.t
-> true = λt.λf.t
false = \t.\f.f
-> false = λt.λf.f
if = \c.\t.\f.c t f
-> if = λc.λt.λf.c t f
if true one two
-> one
if false one two
-> two
```

## License

MIT license; see LICENSE.md.
