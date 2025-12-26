# Assess Coverage Efficiency of Stratification

Evaluates how uniformly cells are distributed across strata using
multiple metrics including coefficient of variation (CV), min/max ratio,
and Gini coefficient. Lower values indicate more uniform distribution.

## Usage

``` r
ss_coverage_efficiency(x, digits = 3)
```

## Arguments

- x:

  An object of class `ss_strata`

- digits:

  Number of decimal places for rounding (default: 3)

## Value

An object of class `ss_coverage_efficiency` (list) containing:

- cell_counts:

  Integer vector of cells per stratum

- n_strata:

  Number of strata

- total_cells:

  Total number of cells

- mean_cells:

  Mean cells per stratum

- sd_cells:

  Standard deviation of cells per stratum

- cv:

  Coefficient of variation (sd/mean)

- min_cells:

  Minimum cells in any stratum

- max_cells:

  Maximum cells in any stratum

- min_max_ratio:

  Ratio of min to max (1.0 = perfect equality)

- range_cells:

  Range (max - min)

- gini:

  Gini coefficient (0 = perfect equality, 1 = inequality)

- efficiency_score:

  Qualitative assessment: "Excellent", "Good", "Fair", or "Poor"

- is_equal_area:

  Whether equal-area algorithm was used

## Details

The function computes several metrics to assess uniformity:

**Coefficient of Variation (CV)**: Ratio of standard deviation to mean.
Lower values indicate more uniform distribution. For equal-area
stratification, expect CV \< 0.05.

**Min/Max Ratio**: Ratio of smallest to largest stratum. Value of 1.0
indicates perfect equality. Values \> 0.9 are excellent.

**Gini Coefficient**: Standard inequality measure from economics. 0 =
perfect equality, 1 = maximal inequality. Lower values indicate better
uniformity.

**Efficiency Score**:

- Excellent: CV \< 0.1 and min/max \> 0.9

- Good: CV \< 0.2 and min/max \> 0.75

- Fair: CV \< 0.35 and min/max \> 0.5

- Poor: Otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create study area
poly <- st_polygon(list(rbind(
  c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
)))
study_area <- st_sf(geometry = st_sfc(poly))

# Create stratification
set.seed(42)
strata <- ss_stratify(study_area, n_strata = 20, n_try = 5)

# Assess coverage efficiency
efficiency <- ss_coverage_efficiency(strata)
print(efficiency)

# Compare regular vs equal-area
strata_equal <- ss_stratify(study_area, n_strata = 20,
                             equal_area = TRUE, n_try = 5)
eff_regular <- ss_coverage_efficiency(strata)
eff_equal <- ss_coverage_efficiency(strata_equal)

cat("Regular CV:", eff_regular$cv, "\n")
cat("Equal-area CV:", eff_equal$cv, "\n")
} # }
```
