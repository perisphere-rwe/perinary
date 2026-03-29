# Bind Dictionaries Together

Binding dictionaries together can be useful when you have variables that
exist in multiple datasets and you don't want to re-write their meta
information into each of those datasets.

## Usage

``` r
bind_dictionary(
  ...,
  .list = NULL,
  conflict_preference = "left",
  drop_unmatched_variables = FALSE
)

left_bind(..., .list = NULL, drop_unmatched_variables = FALSE)

right_bind(..., .list = NULL, drop_unmatched_variables = FALSE)
```

## Arguments

- ...:

  [Data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  objects to combine.

- .list:

  This argument allows `bind_dictionary` to be used programmatically and
  is optional. It is intended to be used as an alternative to `...`..
  Specifically, instead of `bind_dictionary(x, y)`, you can use
  `bind_dictionary(.list = list(x, y))`. This is helpful when you have a
  lot of dictionaries and want to bind them programmatically.

- conflict_preference:

  a character value indicating what to do when `x` and `y` have
  overlapping variables. If "left", then the definitions in the `x`
  dictionary are preferred because `x` is on the left of `y` in the
  function arguments. If "right", `y` definitions are preferred.

- drop_unmatched_variables:

  logical. If `TRUE`, variables in `y` that are not in `x` will be
  retained in the output. If `FALSE` (default), only variables from `x`
  and those in `y` that overlap with `x` will be included, subject to
  `conflict_preference`.

## Value

A [data
dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
object

## Examples

``` r
dd_age_years <- data_dictionary(
  numeric_variable(
    name = "age_years",
    label = "Age",
    units = 'years',
    divby_modeling = 10
  )
)

dd_age_group <- data_dictionary(
  nominal_variable(
    name = "age_group",
    label = "Age group",
    description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
    category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
    category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
  )
)

bind_dictionary(dd_age_years, dd_age_group)
#> Data Dictionary:
#> # A tibble: 2 × 8
#>   name      type    label     description   units divby_modeling category_levels
#>   <chr>     <chr>   <chr>     <chr>         <chr> <chr>          <chr>          
#> 1 age_years Numeric Age       none          years 10             none           
#> 2 age_group Nominal Age group Ages of 0 to… none  none           age_lt_50, age…
#> # ℹ 1 more variable: category_labels <chr>
```
