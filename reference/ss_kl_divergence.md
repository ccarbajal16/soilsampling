# KL Divergence Between Population and Sample Distributions

Computes the mean Kullback-Leibler divergence across all numeric
variables shared by a population data set and a sample drawn from it.

## Usage

``` r
ss_kl_divergence(population_data, sample_data, n_bins = 25)
```

## Arguments

- population_data:

  Data frame of population ancillary data.

- sample_data:

  Data frame of sample ancillary data.

- n_bins:

  Integer, number of histogram bins used to estimate each variable's
  distribution. Default `25`.

## Value

Numeric, the mean KL divergence across all numeric variables.

## Details

For each numeric variable, population and sample distributions are
estimated as histograms over the same bins (defined by the population
range). KL divergence is then \\\sum O_i \log(O_i / E_i)\\, where
\\O_i\\ is the sample density and \\E_i\\ the population density in bin
\\i\\. The result is averaged across variables.

## Examples

``` r
pop <- data.frame(x = rnorm(200), y = runif(200))
samp <- pop[sample(nrow(pop), 30), ]
ss_kl_divergence(pop, samp)
#> [1] 0.3444531
```
