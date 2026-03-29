# Order columns by dictionary variables

Reorders the columns of a data frame so that columns whose names appear
in a `DataDictionary` come first, in the order they are defined in the
dictionary. Columns not present in the dictionary are optionally kept at
the end.

## Usage

``` r
index_columns(data, dictionary = NULL, keep_unmatched = TRUE)
```

## Arguments

- data:

  A data frame or tibble whose columns will be reordered.

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object. If `NULL`, uses the default dictionary set via
  [`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md).

- keep_unmatched:

  Logical. If `TRUE` (the default), columns in `data` that are not
  defined in the dictionary are retained and appended after the matched
  columns. If `FALSE`, those columns are dropped from the output.

## Value

The same type as `data`, with columns reordered (and optionally
filtered) according to the dictionary.

## Examples

``` r
dd <- data_dictionary(
  numeric_variable("bill_length_mm", label = "Bill length", units = "mm"),
  numeric_variable("bill_depth_mm",  label = "Bill depth",  units = "mm"),
  nominal_variable("species", label = "Species",
                   category_levels = c("Adelie", "Chinstrap", "Gentoo"))
)

df <- data.frame(
  species        = c("Adelie", "Chinstrap"),
  extra_col      = 1:2,
  bill_depth_mm  = c(18.7, 17.4),
  bill_length_mm = c(39.1, 46.5)
)

# Matched columns appear in dictionary order; extra_col is appended
index_columns(df, dictionary = dd)
#>   bill_length_mm bill_depth_mm   species extra_col
#> 1           39.1          18.7    Adelie         1
#> 2           46.5          17.4 Chinstrap         2

# Drop columns not in the dictionary
index_columns(df, dictionary = dd, keep_unmatched = FALSE)
#>   bill_length_mm bill_depth_mm   species
#> 1           39.1          18.7    Adelie
#> 2           46.5          17.4 Chinstrap
```
