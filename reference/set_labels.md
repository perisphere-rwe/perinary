# Modify Data Dictionary Elements

Set metadata in a `DataDictionary` object.

## Usage

``` r
set_labels(dictionary, ..., .list = NULL)

set_descriptions(dictionary, ..., .list = NULL)

set_units(dictionary, ..., .list = NULL)

set_divby_modeling(dictionary, ..., .list = NULL)

set_category_labels(dictionary, ..., .list = NULL)

set_category_order(dictionary, ..., .list = NULL)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- ...:

  Name-value pairs. The name gives the name of the variable that will be
  modified. The value must be a:

  - character value for `set_labels()`, `set_units()`, and
    `set_descriptions()`

  - numeric value for `set_divby_modeling()`

  - character vector for `set_category_levels()` and
    `set_category_labels()`

- .list:

  a list of name-value pairs. This argument is optional and intended to
  be used in place of `...` for programmatic setting of meta data.

## Value

a modified `dictionary`

## Examples

``` r
dd <- as_data_dictionary(data.frame(a = 1, b = "cat", id = 1)) |>
 set_identifiers(id) |>
 set_labels(a = "numeric example", b = "categorical example") |>
 set_units(a = "years") |>
 set_divby_modeling(a = 10) |>
 set_descriptions(a = "A variable used for examples") |>
 set_category_labels(b = c("cat" = "A small lion"))

dd
#> Data Dictionary:
#> # A tibble: 3 × 8
#>   name  type       label        description units divby_modeling category_levels
#>   <chr> <chr>      <chr>        <chr>       <chr> <chr>          <chr>          
#> 1 a     Numeric    numeric exa… A variable… years 10             none           
#> 2 b     Nominal    categorical… none        none  none           cat            
#> 3 id    Identifier none         none        none  none           none           
#> # ℹ 1 more variable: category_labels <chr>

# programmatic assignment

label_list <- list(a = "Listed numeric example",
                   b = "Listed categorical example")
set_labels(dd, .list = label_list)
#> Data Dictionary:
#> # A tibble: 3 × 8
#>   name  type       label        description units divby_modeling category_levels
#>   <chr> <chr>      <chr>        <chr>       <chr> <chr>          <chr>          
#> 1 a     Numeric    Listed nume… A variable… years 10             none           
#> 2 b     Nominal    Listed cate… none        none  none           cat            
#> 3 id    Identifier none         none        none  none           none           
#> # ℹ 1 more variable: category_labels <chr>



```
