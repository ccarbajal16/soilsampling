# Save KL Optimization Plots

Saves the KL-divergence and CDF plots from a
[`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md)
(or
[`ss_kl_size()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_size.md))
result to disk, without writing the CSV outputs. Useful for re-saving
plots on their own, e.g. with a different prefix.

## Usage

``` r
ss_kl_save_plots(results, output_dir, prefix = "kl")
```

## Arguments

- results:

  List returned by
  [`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md)
  or
  [`ss_kl_size()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_size.md).

- output_dir:

  Character, directory to save plots to. Created if it does not exist.
  Required: the caller must choose where files are written, so this
  function never writes to a default location.

- prefix:

  Character, file name prefix. Default `"kl"`.

## Value

Character vector of file paths written (invisibly, if none were saved, a
zero-length character vector).

## See also

[`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md),
[`ss_kl_size()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_size.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ss_kl_optimize(population_data)
ss_kl_save_plots(res, file.path(tempdir(), "kl"), prefix = "clhs")
} # }
```
