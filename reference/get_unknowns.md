# Get Unknown Fields in Dictionary

Get Unknown Fields in Dictionary

## Usage

``` r
get_unknowns(
  dictionary = NULL,
  as_request = FALSE,
  as_code = FALSE,
  show_optional = FALSE
)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- as_request:

  logical. If `TRUE`, text is returned in a readable format that can be
  pasted into other settings, e.g., an e-mail requesting for the unknown
  information. If `FALSE` (the default), unknowns are returned as a
  `tibble`.

- as_code:

  logical. If `TRUE`, text is returned as code that can be copy/pasted
  into an R script. If `FALSE` (the default), unknowns are returned as a
  `tibble`.

- show_optional:

  logical. If `TRUE`, unknowns will include all possible fields of the
  variables in `dictionary`. If `FALSE` (the default), only labels and
  units are presented.

## Value

If `as_request = FALSE` and `as_code = FALSE`, a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html) is
returned. If `as_request = TRUE` or `as_code = TRUE`, text is returned.

## Examples

``` r
# units isn't returned in the output because it is specified (known)
get_unknowns(data_dictionary(numeric_variable("a", units = 'years')))
#> # A tibble: 1 × 4
#>   variable type    name  value
#>   <chr>    <chr>   <fct> <chr>
#> 1 a        Numeric label none 

# label isn't returned in the output because it is specified (known)
get_unknowns(data_dictionary(nominal_variable("b", label = 'example')))
#> # A tibble: 1 × 4
#>   variable type    name            value
#>   <chr>    <chr>   <fct>           <chr>
#> 1 b        Nominal category_labels none 
dd <- as_data_dictionary(iris) |>
  set_labels(Sepal.Length = "Sepal length",
            Sepal.Width = "Sepal width",
            Species = "Flower species") |>
  set_descriptions(Species = "The species are all subtypes of iris") |>
  set_units(Sepal.Length = "cm")

# as_request = TRUE returns this information in a shareable format
get_unknowns(dd, as_request = TRUE)
#> A label to use for this variable in reports:
#> 
#>   - Petal.Length = ?
#>   - Petal.Width = ?
#> 
#> Category labels for this variable (labels are shown in reports):
#> 
#>   - Species: setosa = ?;  versicolor = ?;  virginica = ?
#> 
#> Variable units (e.g., age in years):
#> 
#>   - Sepal.Width = ?
#>   - Petal.Length = ?
#>   - Petal.Width = ? 

# as_code = TRUE gives a head start on filling in the missing info
get_unknowns(dd, as_code = TRUE)
#> set_labels(Petal.Length  = "",
#>            Petal.Width  = "") |> 
#> set_category_labels(Species = c(setosa = "",
#>                                 versicolor = "",
#>                                 virginica = "")) |> 
#> set_units(Sepal.Width  = "",
#>           Petal.Length  = "",
#>           Petal.Width  = "") 
```
