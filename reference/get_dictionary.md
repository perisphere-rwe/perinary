# Extract Dictionary Meta Data as a Tibble

Returns a tibble of the dictionary contents, optionally formatting
missing values and nominal variable categories. This can be useful for
custom workflows that require dictionary information in tabular form.

## Usage

``` r
get_dictionary(
  dictionary,
  format_missing = FALSE,
  format_categories = FALSE,
  as_code = FALSE
)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- format_missing:

  Logical. If `TRUE`, missing metadata fields (like labels or units) are
  returned as stored in the dictionary. If `FALSE` (default), missing
  values are replaced with `NA`.

- format_categories:

  Logical. If `TRUE`, nominal category information is returned exactly
  as stored in the dictionary. If `FALSE` (default), category levels and
  labels are returned as list-columns extracted from the variable
  objects.

- as_code:

  Logical. If `TRUE`, text is returned as code that can be copy/pasted
  into an R script to construct `dictionary`. If `FALSE` (the default),
  unknowns are returned as a `tibble`.

## Value

A tibble with one row per variable (`as_code = FALSE`), containing meta
data fields such as `label`, `description`, `units`, `divby_modeling`,
`category_levels`, and `category_labels`. If `as_code = TRUE`, a string
containing code to construct the original dictionary.

## Author

Byron Jaeger, Tyler Sagendorf

## Examples

``` r
dd <- as_data_dictionary(iris)
get_dictionary(dd)
#> # A tibble: 5 × 7
#>   name    label description units divby_modeling category_levels category_labels
#>   <chr>   <chr> <chr>       <chr>          <dbl> <named list>    <named list>   
#> 1 Sepal.… NA    NA          NA                NA <NULL>          <NULL>         
#> 2 Sepal.… NA    NA          NA                NA <NULL>          <NULL>         
#> 3 Petal.… NA    NA          NA                NA <NULL>          <NULL>         
#> 4 Petal.… NA    NA          NA                NA <NULL>          <NULL>         
#> 5 Species NA    NA          NA                NA <chr [3]>       <chr [3]>      

# Code to recreate dd
dd_code <- get_dictionary(dd, as_code = TRUE)
dd_code
#> [1] "data_dictionary(.list = list(numeric_variable(name='Sepal.Length',label=NULL,description=NULL,units=NULL,divby_modeling=NULL),numeric_variable(name='Sepal.Width',label=NULL,description=NULL,units=NULL,divby_modeling=NULL),numeric_variable(name='Petal.Length',label=NULL,description=NULL,units=NULL,divby_modeling=NULL),numeric_variable(name='Petal.Width',label=NULL,description=NULL,units=NULL,divby_modeling=NULL),nominal_variable(name='Species',label=NULL,description=NULL,category_levels=c('setosa','versicolor','virginica'),category_labels=c('setosa','versicolor','virginica'))))"

# Create data dictionary from code string
dd_new <- eval(parse(text = dd_code))
all.equal(dd, dd_new) # TRUE
#> [1] TRUE
```
