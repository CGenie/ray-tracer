# The Ray Tracer Challenge (book) samples

Solutions to the Pragmatic Bookshelf book
[The Ray Tracer Challenge](https://pragprog.com/book/jbtracer/the-ray-tracer-challenge).

```
opam switch create ray-tracer 4.07.1
opam install gg vg cairo2 utop batteries
```

## 1. World Projectile

This is based on chapters 1 and 2 of the book.
```
cd 01-world-projectile
dune build
```

## 2. Tracing

This is a real ray tracer, with Phong modeling (chapters 3-5 of the book).
```
cd 02-tracing
opam install parany ppx_inline_test
dune build
mkdir output
dune exec tracing
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
