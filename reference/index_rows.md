# Order rows by dictionary terms

Reorders a data frame so that rows associated with nominal variables
follow the order defined in a `DataDictionary`. This is useful when you
want multiple outputs to present variables in a consistent order.

## Usage

``` r
index_rows(data, dictionary = NULL, names = "name", levels = "level")
```

## Arguments

- data:

  A data frame or tibble containing rows to order. Must include columns
  that identify the variable name and category level.

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object. If `NULL`, uses the default dictionary set via
  [`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md).

- names:

  Character value giving the column name that stores the variable name
  in `data`. Default is `"name"`.

- levels:

  Character value giving the column name that stores the category
  code/level in `data`. Default is `"level"`.

## Value

The same type as `data`, with rows re-ordered to match the order of
variables in the dictionary and, within nominal variables, the order of
categories.

## Examples

``` r
dd <- data_dictionary(
  nominal_variable(
    "gender",
    label = "Gender",
    category_levels = c("M", "F"),
    category_labels = c("Male", "Female")
  )
)

df <- tibble::tibble(
  name = c("gender", "gender"),
  level = c("F", "M"),
  n = c(12, 18)
)

index_rows(df, dictionary = dd)
#> # A tibble: 2 × 3
#>   name   level     n
#>   <chr>  <chr> <dbl>
#> 1 gender M        18
#> 2 gender F        12
```
