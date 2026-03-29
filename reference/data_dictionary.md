# Create Data Dictionary from Variable Definitions

Initializes a `DataDictionary` object from a set of variables. Variables
should be made using
[`numeric_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
or
[`nominal_variable()`](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
and passed directly as arguments.

## Usage

``` r
data_dictionary(..., .list = NULL, copy_on_modify = TRUE)
```

## Arguments

- ...:

  One or more objects inheriting from A [numeric
  variable](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md)
  or A [nominal
  variable](https://perisphere-rwe.github.io/perinary/reference/variable_constructors.md).

- .list:

  A list of data variables. This argument allows `data_dictionary` to be
  used programmatically and is optional. It is intended to be used as an
  alternative to `...`.

- copy_on_modify:

  a logical value indicating whether `set` functions should modify the
  dictionary in place or copy it then modify. The default is `TRUE`
  because dictionaries are almost always small and trivial to copy.

## Value

A data dictionary object

## Details

Only one of `...` and `.list` should be specified.

## Examples

``` r
age_years <- numeric_variable(
  name = "age",
  label = "Age of participant",
  units = "years"
)

gender <- nominal_variable(
  name = "gender",
  label = "Gender of participant",
  category_levels = c("M", "F"),
  category_labels = c("Male", "Female")
)

dd <- data_dictionary(age_years, gender)
print(dd)
#> Data Dictionary:
#> # A tibble: 2 × 8
#>   name   type    label          description units divby_modeling category_levels
#>   <chr>  <chr>   <chr>          <chr>       <chr> <chr>          <chr>          
#> 1 age    Numeric Age of partic… none        years none           none           
#> 2 gender Nominal Gender of par… none        none  none           M and F        
#> # ℹ 1 more variable: category_labels <chr>

dd <- data_dictionary(.list = list(age_years, gender))
print(dd)
#> Data Dictionary:
#> # A tibble: 2 × 8
#>   name   type    label          description units divby_modeling category_levels
#>   <chr>  <chr>   <chr>          <chr>       <chr> <chr>          <chr>          
#> 1 age    Numeric Age of partic… none        years none           none           
#> 2 gender Nominal Gender of par… none        none  none           M and F        
#> # ℹ 1 more variable: category_labels <chr>
```
