# Get Term Key for Nominal Variables

Returns a tibble linking each level or label of a nominal variable to a
modeling term, using a specified separator between the variable name and
category value. This is especially useful for joining dictionary
information to model output, such as coefficients or terms in regression
tables.

## Usage

``` r
get_term_key(
  dictionary,
  adjust_to = NULL,
  term_separator = "",
  term_colname = "term"
)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- adjust_to:

  Optional. A tibble that includes a column with term names (default:
  `'term'`). When supplied, the output is filtered to retain only terms
  found in `adjust_to` and that match the reference level.

- term_separator:

  A string used to separate the variable name and category value in the
  output term column. Default is `""`.

- term_colname:

  A character string giving the column name to use for the terms column
  in the returned tibble. Default is `"term"`.

## Value

A tibble with one row per variable-category combination and columns for
the variable, category type (`levels` or `labels`), and term. If
`adjust_to` is provided, the tibble is filtered accordingly and includes
the `category` column.

## Examples

``` r
dd <- as_data_dictionary(iris)
get_term_key(dd)
#> # A tibble: 3 × 6
#>   name    level      label      reference category_type term             
#>   <chr>   <chr>      <chr>      <lgl>     <chr>         <chr>            
#> 1 Species setosa     setosa     TRUE      levels        Speciessetosa    
#> 2 Species versicolor versicolor FALSE     levels        Speciesversicolor
#> 3 Species virginica  virginica  FALSE     levels        Speciesvirginica 
```
