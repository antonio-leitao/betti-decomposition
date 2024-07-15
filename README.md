# Betti Decomposition

## Basic usage

Given a file that includes the simplicies of a simplicial complex:
```text
complex.txt
  1 2 3
  1 2
  2 3
  3 1
  3 4
  4 1
  4
  3
  2
  1
```

which represents the following simplicial complex:

<img src="example_complex.svg" width="300" alt="Example Simplicial Complex">

the code returns the following output

```text
output.txt
  b0 = 1
  b1 = 1
  ------
  Execution time: 12ms
```


