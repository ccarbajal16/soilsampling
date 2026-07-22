# Write cLHS/RF Sample Design Outputs to Disk

Write cLHS/RF Sample Design Outputs to Disk

## Usage

``` r
.write_rf_outputs(
  clhs_samples,
  rf_samples,
  comparison_table,
  plots,
  output_dir
)
```

## Arguments

- clhs_samples:

  Data frame of cLHS baseline sample locations.

- rf_samples:

  Data frame of RF-optimized sample locations.

- comparison_table:

  Data frame from
  [`.rf_comparison_table()`](https://ccarbajal16.github.io/soilsampling/reference/dot-rf_comparison_table.md).

- plots:

  List with `clhs_plot` and `rf_plot`, as returned by
  [`.plot_rf_comparison()`](https://ccarbajal16.github.io/soilsampling/reference/dot-plot_rf_comparison.md).

- output_dir:

  Character, directory to write outputs to.

## Value

A named list of file paths written.
