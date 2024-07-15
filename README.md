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

<svg xmlns="http://www.w3.org/2000/svg" viewBox="-1 -4 6 8">
  <!-- Edges -->
  <g stroke="black" stroke-width="0.05">
    <line x1="0" y1="0" x2="2" y2="3" />
    <line x1="2" y1="3" x2="4" y2="0" />
    <line x1="0" y1="0" x2="4" y2="0" />
    <line x1="0" y1="0" x2="2" y2="-3" />
    <line x1="4" y1="0" x2="2" y2="-3" />
  </g>
  
  <!-- Face (triangle) -->
  <polygon points="0,0 2,3 4,0" fill="lightblue" fill-opacity="0.4" />
  
  <!-- Vertices -->
  <g fill="black">
    <circle cx="0" cy="0" r="0.1" />
    <circle cx="2" cy="3" r="0.1" />
    <circle cx="4" cy="0" r="0.1" />
    <circle cx="2" cy="-3" r="0.1" />
  </g>
  
  <!-- Labels -->
  <g font-size="0.3" text-anchor="middle">
    <text x="-0.2" y="0.3">v₁</text>
    <text x="2" y="3.3">v₂</text>
    <text x="4.2" y="0.3">v₃</text>
    <text x="2" y="-3.3">v₄</text>
    <text x="0.8" y="1.7">e₁</text>
    <text x="3.2" y="1.7">e₂</text>
    <text x="2" y="-0.2">e₃</text>
    <text x="0.8" y="-1.7">e₄</text>
    <text x="3.2" y="-1.7">e₅</text>
    <text x="2" y="1.2">f₁</text>
  </g>
</svg>

the code returns the following output

```text
output.txt
  b0 = 1
  b1 = 1
  ------
  Execution time: 12ms
```


