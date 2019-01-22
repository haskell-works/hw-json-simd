# hw-json-simd

## Using on the command line

```bash
cat test.json | pv -t -e -b -a | time hw-json-simd-exe create-index \
  -i /dev/stdin
  --output-ib-file test.json.ib.idx
  --output-bp-file test.json.bp.idx
  --method standard
```

```bash
cat test.json | pv -t -e -b -a | time hw-json-simd-exe create-index \
  -i /dev/stdin
  --output-ib-file test.json.ib.idx
  --output-bp-file test.json.bp.idx
  --method simple
```
