# Betti Decomposition

Given a a file with all simplicies of a simplicial complex:
<img src="assets/example_complex.svg" width="300" alt="Example Simplicial Complex" align=right>


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
The code returns the following output

```text
  b0 = 1
  b1 = 1
```

## Basic usage

```shell
git clone https://github.com/antonio-leitao/betti-decomposition.git
cd betti-decomposition
# compile from source:
make
#run it with your desired txt file
./betti "simplicial_complex.txt"
```

### Compiler Options

To toogle parallelization with OpenMP compile with teh following code

```shell
make OPENMP=1
```





