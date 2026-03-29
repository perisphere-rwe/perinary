# Set and Remove Acronyms

Add acronyms and their descriptions to a `DataDictionary`. Acronyms can
also be removed if they are no longer used.

## Usage

``` r
set_acronyms(dictionary, ..., .list = NULL, show_warnings = TRUE)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- ...:

  one or more comma-separated name-value pairs. Pairs are of the form of
  `acronym = "spelled-out acronym"` or
  `"acronym" = "spelled-out acronym"`.

- .list:

  a list of name-value pairs. This argument is optional and intended to
  be used in place of `...` for programmatic setting of acronyms.

- show_warnings:

  logical; whether to display warnings.

## Value

A modified `dictionary`.

## See also

[`get_acronym_defs`](https://perisphere-rwe.github.io/perinary/reference/get_acronym_defs.md)

## Examples

``` r
dd <- as_data_dictionary(iris)
dd$get_acronyms() # NULL
#> NULL

dd <- set_acronyms(dictionary = dd,
                   SBP = "systolic blood pressure",
                   DBP = "diastolic blood pressure")

dd$get_acronyms()
#>                        DBP                        SBP 
#> "diastolic blood pressure"  "systolic blood pressure" 
```
