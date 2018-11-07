# The Ray Tracer Challenge (book) samples

Solutions to the Pragmatic Bookshelf book
[The Ray Tracer Challenge](https://pragprog.com/book/jbtracer/the-ray-tracer-challenge).

## 1. World Projectile

This is based on chapters 1 and 2 of the book.
```
cd 01-world-projectile
opam install gg svg vg cairo2
dune build
```

## Documentation

To generate the documentation install `odoc`:

```
opam install odoc
```

and generate it using `dune`:

```
dune build @doc
```

## Testing

```
opam install ppx_inline_test
dune runtest
```
