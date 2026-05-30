# Bind Variables to a Dictionary

A convenience wrapper for adding one or more new variables to an
existing dictionary. Each variable's name is taken from its
corresponding named argument in `...`, so there is no need to pass
`name` inside the variable constructor itself.

## Usage

``` r
bind_variables(dictionary, ..., .list = NULL)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- ...:

  Named arguments. Each name becomes the variable name, and each value
  is a call to a variable constructor such as
  [`nominal_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md),
  [`numeric_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md),
  [`logical_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md),
  [`identifier_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md),
  or
  [`date_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md).
  The `name` argument of the constructor may be omitted;
  `bind_variables` will inject it automatically.

- .list:

  This argument allows `bind_variables` to be used programmatically and
  is optional. It is intended to be used as an alternative to `...`.. A
  named list of `DataVariable` objects. Each element's list-entry name
  will be used to set (or override) the variable name.

## Value

A [data
dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
object

## Details

`bind_variables` does **not** allow overwriting variables already
present in `dictionary`. To modify existing variables use the `set_*`
family (e.g.
[`set_labels()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md),
[`set_units()`](https://perisphere-rwe.github.io/perinary/reference/set_labels.md)).

## Examples

``` r

meta <- as_data_dictionary(iris)

meta <- meta |>
  bind_variables(
    train_test = nominal_variable(
      label = "Train/test split",
      category_levels = c("train", "test"),
      category_labels = c("Training data", "Testing data")
    )
  )

meta
#> Data Dictionary:
#> # A tibble: 6 × 8
#>   name         type    label    description units divby_modeling category_levels
#>   <chr>        <chr>   <chr>    <chr>       <chr> <chr>          <chr>          
#> 1 Sepal.Length Numeric none     none        none  none           none           
#> 2 Sepal.Width  Numeric none     none        none  none           none           
#> 3 Petal.Length Numeric none     none        none  none           none           
#> 4 Petal.Width  Numeric none     none        none  none           none           
#> 5 Species      Nominal none     none        none  none           setosa, versic…
#> 6 train_test   Nominal Train/t… none        none  none           train and test 
#> # ℹ 1 more variable: category_labels <chr>
```
