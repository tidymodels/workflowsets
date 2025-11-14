# Make a classed list of options

This function returns a named list with an extra class of
`"workflow_set_options"` that has corresponding formatting methods for
printing inside of tibbles.

## Usage

``` r
option_list(...)
```

## Arguments

- ...:

  A set of named options (or nothing)

## Value

A classed list.

## Examples

``` r
option_list(a = 1, b = 2)
#> a list of options with names:  'a', 'b'
option_list()
#> an empty container for options
```
