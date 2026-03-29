# Modify order of variables in a data dictionary

This is a thin wrapper for using
[dplyr::relocate](https://dplyr.tidyverse.org/reference/relocate.html)
to set the order of variables in a A [data
dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
object. The inputs (apart from `dictionary`) are named and interpreted
exactly as they are in
[dplyr::relocate](https://dplyr.tidyverse.org/reference/relocate.html).
The variables will be reordered as specified in the dictionary returned.

## Usage

``` r
set_variable_order(dictionary, ..., .before = NULL, .after = NULL)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- ..., .before, .after:

  see [relocate
  documentation](https://dplyr.tidyverse.org/reference/relocate.html)

## Details

The order of variables in the dictionary determines the order of results
when indexing functions are applied, such as
[index_terms](https://perisphere-rwe.github.io/perinary/reference/index_terms.md).
Setting the order of variables to match their expected order of
presentation in tables can help streamline table generation (see
examples.)

## Examples

``` r
dd_ordered <- as_data_dictionary(iris) |>
  set_variable_order(Species, .before = Sepal.Length) |>
  set_variable_order(ends_with("Length"), .after = Petal.Width)

set_default_dictionary(dd_ordered)

# index_terms() is using the order of variables in `dd_ordered` to
# return a dataframe sorted the same way as `dd_ordered` is. Note
# this also respects the ordering of categories within variables,
# which is not straightforward to do with the usual dplyr::arrange()

lm(Sepal.Length ~ ., data = iris) |>
  broom::tidy() |>
  dplyr::filter(term != "(Intercept)") |>
  index_terms()
#> # A tibble: 6 × 9
#>   name        level label reference term  estimate std.error statistic   p.value
#>   <chr>       <chr> <chr> <lgl>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 Species     seto… seto… TRUE      Spec…   NA       NA          NA    NA       
#> 2 Species     vers… vers… FALSE     Spec…   -0.724    0.240      -3.01  3.06e- 3
#> 3 Species     virg… virg… FALSE     Spec…   -1.02     0.334      -3.07  2.58e- 3
#> 4 Sepal.Width NA    NA    FALSE     Sepa…    0.496    0.0861      5.76  4.87e- 8
#> 5 Petal.Width NA    NA    FALSE     Peta…   -0.315    0.151      -2.08  3.89e- 2
#> 6 Petal.Leng… NA    NA    FALSE     Peta…    0.829    0.0685     12.1   1.07e-23

```
