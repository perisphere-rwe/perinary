# Select Variables in a Dictionary

Keep, drop, or reorder variables in a `DataDictionary` using
[tidyselect](https://tidyselect.r-lib.org/reference/language.html)
semantics, similar to
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
for data frames. This is useful for narrowing a dictionary down to the
variables relevant to a particular dataset or analysis, or for resolving
ambiguity (e.g. dropping one of two variables that happen to share a
label) before using functions like
[`index_rows()`](https://perisphere-rwe.github.io/perinary/reference/index_rows.md).

## Usage

``` r
select_variables(dictionary, ...)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- ...:

  One or more unquoted variable names, tidyselect helpers (e.g.
  `starts_with()`, `ends_with()`), or negated selections (e.g. `-age`)
  identifying which variables to keep. The order of the selection
  determines the variable order in the returned dictionary.

## Value

A [data
dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
object containing only the selected variables, in selection order.

## Examples

``` r

dd <- as_data_dictionary(iris)

# keep only Species and columns ending in "Length"
select_variables(dd, Species, ends_with("Length"))
#> Data Dictionary:
#> # A tibble: 3 × 8
#>   name         type    label description units divby_modeling category_levels   
#>   <chr>        <chr>   <chr> <chr>       <chr> <chr>          <chr>             
#> 1 Species      Nominal none  none        none  none           setosa, versicolo…
#> 2 Sepal.Length Numeric none  none        none  none           none              
#> 3 Petal.Length Numeric none  none        none  none           none              
#> # ℹ 1 more variable: category_labels <chr>

# drop a variable
select_variables(dd, -Species)
#> Data Dictionary:
#> # A tibble: 4 × 8
#>   name         type    label description units divby_modeling category_levels
#>   <chr>        <chr>   <chr> <chr>       <chr> <chr>          <chr>          
#> 1 Sepal.Length Numeric none  none        none  none           none           
#> 2 Sepal.Width  Numeric none  none        none  none           none           
#> 3 Petal.Length Numeric none  none        none  none           none           
#> 4 Petal.Width  Numeric none  none        none  none           none           
#> # ℹ 1 more variable: category_labels <chr>

# resolve a label collision by dropping one of the colliding variables
# before ordering rows with index_rows()
dd_dup <- data_dictionary(
  numeric_variable("age_baseline", label = "Age"),
  numeric_variable("age_followup", label = "Age")
)

select_variables(dd_dup, -age_followup)
#> Data Dictionary:
#> # A tibble: 1 × 8
#>   name         type    label description units divby_modeling category_levels
#>   <chr>        <chr>   <chr> <chr>       <chr> <chr>          <chr>          
#> 1 age_baseline Numeric Age   none        none  none           none           
#> # ℹ 1 more variable: category_labels <chr>
```
