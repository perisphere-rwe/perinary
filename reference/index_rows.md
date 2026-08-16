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
  in `data`. Default is `"name"`. Values in this column may be either
  variable names or variable labels (or a mix of both); both are matched
  against the dictionary. If a name and a label happen to collide (the
  same string is one variable's name and another variable's label), the
  name match takes priority. Values that match neither (e.g.
  `"(Intercept)"`) are left as-is and sorted after variables found in
  the dictionary.

- levels:

  Character value giving the column name that stores the category
  code/level in `data`. Default is `"level"`. Values in this column may
  be either category levels or category labels (or a mix of both); both
  are matched against the dictionary. If a level and a label happen to
  collide (the same string is one category's level and another
  category's label), the level match takes priority.

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

# Works the same when the column holds category labels instead
df_labels <- tibble::tibble(
  name = c("gender", "gender"),
  level = c("Female", "Male"),
  n = c(12, 18)
)

index_rows(df_labels, dictionary = dd)
#> # A tibble: 2 × 3
#>   name   level      n
#>   <chr>  <chr>  <dbl>
#> 1 gender Male      18
#> 2 gender Female    12

# The `names` column can hold variable labels too
df_var_labels <- tibble::tibble(
  name = c("Gender", "Gender"),
  level = c("Female", "Male"),
  n = c(12, 18)
)

index_rows(df_var_labels, dictionary = dd)
#> # A tibble: 2 × 3
#>   name   level      n
#>   <chr>  <chr>  <dbl>
#> 1 Gender Male      18
#> 2 Gender Female    12
```
