# hw-json-simd

This library/tool will generate semi-indexes on JSON files as per the paper:
[Semi-Indexing Semi-Structured Data in Tiny Space](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf).

## The command line tool

For a given JSON file, the `hw-json-simd` it will generate two semi-index
files, which both together can be loaded into a single in-memory semi-index.

The semi-index files can be generated using two methods, which will be called
standard and simple, which correspond to sections 4 and 5 of the
[Semi-Indexing Semi-Structured Data in Tiny Space](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf)
paper respectively.

Navigation of the JSON text using the standard index is suppored by the [`hw-json` project](https://github.com/haskell-works/hw-json) for more information.  There is currently
no support for navigation of the JSON text using the simple index.

### Using on the command line

```bash
cat test.json | pv -t -e -b -a | time hw-json-simd create-index \
  -i /dev/stdin
  --output-ib-file test.json.ib.idx
  --output-bp-file test.json.bp.idx
  --method standard
```

```bash
cat test.json | pv -t -e -b -a | time hw-json-simd create-index \
  -i /dev/stdin
  --output-ib-file test.json.ib.idx
  --output-bp-file test.json.bp.idx
  --method simple
```

## Installation

### From hackage

```bash
cabal new-install hw-json-simd
```

