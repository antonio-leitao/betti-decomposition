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
output.txt
  b0 = 1
  b1 = 1
  ------
  Execution time: 12ms
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

### Adding paralelization


# To Do

[ ] - Use bit operations for matrix reduction.
[ ] - Change input reading to support maximal simplicies instead of all simplicies.






