# The Ray Tracer Challenge (book) samples

## 1. World Projectile

This is based on chapters 1 and 2 of the book.
```
cd 01-world-projectile
opam install gg svg vg
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