# Order model terms by dictionary variables and category levels

A convenience wrapper for
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) output
that attaches variable and category metadata from the dictionary, then
sorts rows into dictionary order via
[`index_rows()`](https://perisphere-rwe.github.io/perinary/reference/index_rows.md).

## Usage

``` r
index_terms(
  data,
  dictionary = NULL,
  term_separator = "",
  term_colname = "term",
  names = "name",
  levels = "level"
)
```

## Arguments

- data:

  A data frame of model output, typically from
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html).
  Must contain a column identifying model terms (see `term_colname`).

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object. If `NULL`, uses the default dictionary set via
  [`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md).

- term_separator:

  A string used to separate variable names and category values when
  constructing or matching terms. Default is `""` to match the default
  separator that most R modeling functions use.

- term_colname:

  Name of the column in `data` that contains term identifiers. Default
  is `"term"`.

- names:

  Character value giving the column name that stores the variable name
  in the enriched output. Default is `"name"`.

- levels:

  Character value giving the column name that stores the category
  code/level in the enriched output. Default is `"level"`.

## Value

The same type as `data` (with term-key columns appended), rows sorted to
match the variable and category order defined in the dictionary.

## Examples

``` r
library(broom)

dd <- as_data_dictionary(iris) |>
  set_variable_order(Species, .before = 1)

fit <- tidy(lm(Sepal.Length ~ ., data = iris))

index_terms(fit, dictionary = dd)
#> # A tibble: 7 × 9
#>   name        level label reference term  estimate std.error statistic   p.value
#>   <chr>       <chr> <chr> <lgl>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 Species     seto… seto… TRUE      Spec…   NA       NA          NA    NA       
#> 2 Species     vers… vers… FALSE     Spec…   -0.724    0.240      -3.01  3.06e- 3
#> 3 Species     virg… virg… FALSE     Spec…   -1.02     0.334      -3.07  2.58e- 3
#> 4 Sepal.Width NA    NA    FALSE     Sepa…    0.496    0.0861      5.76  4.87e- 8
#> 5 Petal.Leng… NA    NA    FALSE     Peta…    0.829    0.0685     12.1   1.07e-23
#> 6 Petal.Width NA    NA    FALSE     Peta…   -0.315    0.151      -2.08  3.89e- 2
#> 7 (Intercept) NA    NA    FALSE     (Int…    2.17     0.280       7.76  1.43e-12
```
