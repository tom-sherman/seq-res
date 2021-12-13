# seq-res
> **⚠️ Very experimental, proceed with caution**

A lazy sequence library for ReScript with built in support for iterators and the iterable protocol for better JS interop.

## Examples

### Usage as an iterable

```rescript
let seq = Seq.fromArray([1,2,3])
let iterator = Iterator.fromSeq(seq)
// Or
// let iterable = Iterable.fromSeq(seq)

// `raw` is used here for example's sake, it could be in a JS module importing the outputted JS
%%raw("
  for (const val of iterator) {
    console.log(val)
  }
")
```

## Todo

- [ ] Iterable -> Seq conversion
- [ ] Iterator -> Seq conversion
- [ ] Gentype support
- [ ] Find a better name and publish to npm
