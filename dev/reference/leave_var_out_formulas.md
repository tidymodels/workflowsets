# Create formulas without each predictor

From an initial model formula, create a list of formulas that exclude
each predictor.

## Usage

``` r
leave_var_out_formulas(formula, data, full_model = TRUE, ...)
```

## Arguments

- formula:

  A model formula that contains at least two predictors.

- data:

  A data frame.

- full_model:

  A logical; should the list include the original formula?

- ...:

  Options to pass to
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)

## Value

A named list of formulas

## Details

The new formulas obey the hierarchy rule so that interactions without
main effects are not included (unless the original formula contains such
terms).

Factor predictors are left as-is (i.e., no indicator variables are
created).

## See also

[`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)

## Examples

``` r
data(penguins, package = "modeldata")

leave_var_out_formulas(
  bill_length_mm ~ .,
  data = penguins
)
#> $species
#> bill_length_mm ~ island + bill_depth_mm + flipper_length_mm + 
#>     body_mass_g + sex
#> <environment: base>
#> 
#> $island
#> bill_length_mm ~ species + bill_depth_mm + flipper_length_mm + 
#>     body_mass_g + sex
#> <environment: base>
#> 
#> $bill_depth_mm
#> bill_length_mm ~ species + island + flipper_length_mm + body_mass_g + 
#>     sex
#> <environment: base>
#> 
#> $flipper_length_mm
#> bill_length_mm ~ species + island + bill_depth_mm + body_mass_g + 
#>     sex
#> <environment: base>
#> 
#> $body_mass_g
#> bill_length_mm ~ species + island + bill_depth_mm + flipper_length_mm + 
#>     sex
#> <environment: base>
#> 
#> $sex
#> bill_length_mm ~ species + island + bill_depth_mm + flipper_length_mm + 
#>     body_mass_g
#> <environment: base>
#> 
#> $everything
#> bill_length_mm ~ .
#> <environment: 0x55a310b626d8>
#> 

leave_var_out_formulas(
  bill_length_mm ~ (island + sex)^2 + flipper_length_mm,
  data = penguins
)
#> $island
#> bill_length_mm ~ sex + flipper_length_mm
#> <environment: base>
#> 
#> $sex
#> bill_length_mm ~ island + flipper_length_mm
#> <environment: base>
#> 
#> $flipper_length_mm
#> bill_length_mm ~ island + sex + island:sex
#> <environment: base>
#> 
#> $`island:sex`
#> bill_length_mm ~ island + sex + flipper_length_mm
#> <environment: base>
#> 
#> $everything
#> bill_length_mm ~ (island + sex)^2 + flipper_length_mm
#> <environment: 0x55a310b626d8>
#> 

leave_var_out_formulas(
  bill_length_mm ~ (island + sex)^2 + flipper_length_mm +
    I(flipper_length_mm^2),
  data = penguins
)
#> $island
#> bill_length_mm ~ sex + flipper_length_mm + I(flipper_length_mm^2)
#> <environment: base>
#> 
#> $sex
#> bill_length_mm ~ island + flipper_length_mm + I(flipper_length_mm^2)
#> <environment: base>
#> 
#> $flipper_length_mm
#> bill_length_mm ~ island + sex + island:sex
#> <environment: base>
#> 
#> $`I(flipper_length_mm^2)`
#> bill_length_mm ~ island + sex + flipper_length_mm + island:sex
#> <environment: base>
#> 
#> $`island:sex`
#> bill_length_mm ~ island + sex + flipper_length_mm + I(flipper_length_mm^2)
#> <environment: base>
#> 
#> $everything
#> bill_length_mm ~ (island + sex)^2 + flipper_length_mm + I(flipper_length_mm^2)
#> <environment: 0x55a310b626d8>
#> 
```
