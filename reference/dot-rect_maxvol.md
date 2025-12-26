# Rectangular Maximum Volume Algorithm

Finds a submatrix with approximately maximum volume using greedy row
swapping. This is the rect_maxvol variant that allows selecting more
rows than columns (rectangular submatrix).

## Usage

``` r
.rect_maxvol(
  A,
  k,
  tol = 1.1,
  max_iters = 100,
  dist_coords = NULL,
  min_dist = NULL
)
```

## Arguments

- A:

  Numeric matrix (m x n) where m \> n

- k:

  Integer, number of rows to select (must be \>= n)

- tol:

  Numeric, tolerance for convergence (default 1.1)

- max_iters:

  Integer, maximum number of iterations (default 100)

- dist_coords:

  Optional matrix (m x 2) of coordinates for distance constraints

- min_dist:

  Numeric, minimum distance between selected points

## Value

List with:

- index:

  Integer vector of selected row indices

- converged:

  Logical, whether algorithm converged

- iterations:

  Integer, number of iterations performed
