# Create Data Dictionary from Existing Data

Create Data Dictionary from Existing Data

## Usage

``` r
as_data_dictionary(x, copy_on_modify = TRUE)
```

## Arguments

- x:

  a data frame

- copy_on_modify:

  a logical value indicating whether `set` functions should modify the
  dictionary in place or copy it then modify. The default is `TRUE`
  because dictionaries are almost always small and trivial to copy.

## Value

A [data
dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
object

## Details

variable labels stored in the 'label' attribute are incorporated in the
resulting dictionary.

## Examples

``` r

attr(iris$Species, 'label') <- "Flower species"

as_data_dictionary(iris)
#> Data Dictionary:
#> # A tibble: 5 × 8
#>   name         type    label    description units divby_modeling category_levels
#>   <chr>        <chr>   <chr>    <chr>       <chr> <chr>          <chr>          
#> 1 Sepal.Length Numeric none     none        none  none           none           
#> 2 Sepal.Width  Numeric none     none        none  none           none           
#> 3 Petal.Length Numeric none     none        none  none           none           
#> 4 Petal.Width  Numeric none     none        none  none           none           
#> 5 Species      Nominal Flower … none        none  none           setosa, versic…
#> # ℹ 1 more variable: category_labels <chr>
```
