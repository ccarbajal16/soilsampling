# Write KL Optimization Results and Plots to Disk

Write KL Optimization Results and Plots to Disk

## Usage

``` r
.write_kl_outputs(results, output_dir)
```

## Arguments

- results:

  List returned by
  [`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md).

- output_dir:

  Character, directory to write outputs to.

## Value

A named list of file paths written (`NA` for any output that was not
available or failed to save).
