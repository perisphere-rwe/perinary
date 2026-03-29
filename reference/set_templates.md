# Set Label and Description Templates

Add label or description templates to a `DataDictionary`. Templates
allow you to store and re-use text with placeholders, making it easier
to describe multiple variables with similar labels and/or descriptions
(see examples).

## Usage

``` r
set_label_templates(dictionary, ..., show_warnings = TRUE)

set_description_templates(dictionary, ..., show_warnings = TRUE)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- ...:

  one or more formulas. See details.

- show_warnings:

  logical; whether to display warnings.

## Value

a modified `dictionary`.

## Examples

``` r
library(perinary)

dd <- as_data_dictionary(iris)
dd
#> Data Dictionary:
#> # A tibble: 5 × 8
#>   name         type    label description units divby_modeling category_levels   
#>   <chr>        <chr>   <chr> <chr>       <chr> <chr>          <chr>             
#> 1 Sepal.Length Numeric none  none        none  none           none              
#> 2 Sepal.Width  Numeric none  none        none  none           none              
#> 3 Petal.Length Numeric none  none        none  none           none              
#> 4 Petal.Width  Numeric none  none        none  none           none              
#> 5 Species      Nominal none  none        none  none           setosa, versicolo…
#> # ℹ 1 more variable: category_labels <chr>

dd <- set_label_templates(
  dictionary = dd,
  # dimension will be either "length" or "width"
  contains("Sepal") ~ "Sepal {dimension}.",
  contains("Petal") ~ "Petal {dimension}."
)

dd # the label column has changed
#> Data Dictionary:
#> # A tibble: 5 × 8
#>   name         type    label description units divby_modeling category_levels   
#>   <chr>        <chr>   <chr> <chr>       <chr> <chr>          <chr>             
#> 1 Sepal.Length Numeric none  none        none  none           none              
#> 2 Sepal.Width  Numeric none  none        none  none           none              
#> 3 Petal.Length Numeric none  none        none  none           none              
#> 4 Petal.Width  Numeric none  none        none  none           none              
#> 5 Species      Nominal none  none        none  none           setosa, versicolo…
#> # ℹ 1 more variable: category_labels <chr>

dd <- set_description_templates(
  dictionary = dd,
  # dimension will be either "length" or "width"
  contains("Sepal") ~ "The {dimension} of the sepal, in centimeters.",
  contains("Petal") ~ "The {dimension} of the petal, in centimeters."
)

dd # the description column has changed
#> Data Dictionary:
#> # A tibble: 5 × 8
#>   name         type    label description units divby_modeling category_levels   
#>   <chr>        <chr>   <chr> <chr>       <chr> <chr>          <chr>             
#> 1 Sepal.Length Numeric none  none        none  none           none              
#> 2 Sepal.Width  Numeric none  none        none  none           none              
#> 3 Petal.Length Numeric none  none        none  none           none              
#> 4 Petal.Width  Numeric none  none        none  none           none              
#> 5 Species      Nominal none  none        none  none           setosa, versicolo…
#> # ℹ 1 more variable: category_labels <chr>
```
