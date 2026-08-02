# Summary Method for Maxvol Sampling Results

Summary Method for Maxvol Sampling Results

## Usage

``` r
ss_summary.maxvol(object, ...)
```

## Arguments

- object:

  An object of class `ss_samples` from
  [`ss_maxvol()`](https://ccarbajal16.github.io/soilsampling/reference/ss_maxvol.md)

- ...:

  Additional arguments (ignored)

## Value

`object`, invisibly. Called for the side effect of printing a maxvol
design summary to the console. If `object` was not produced by
[`ss_maxvol()`](https://ccarbajal16.github.io/soilsampling/reference/ss_maxvol.md),
dispatch is passed on with
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) and that
method's value is returned instead.
