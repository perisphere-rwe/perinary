# Get Acronym Definitions

Get nicely formatted acronym definitions. Primarily intended for use in
footnotes.

## Usage

``` r
get_acronym_defs(
  dictionary,
  acronyms = NULL,
  sep = "; ",
  sep_last = NULL,
  show_warnings = TRUE
)
```

## Arguments

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object

- acronyms:

  `NULL` or a character vector of acronyms. One or more of
  `names(dictionary$get_acronyms())`. If `NULL`, all acronyms in
  `dictionary` will be selected.

- sep:

  character; separator for the acronyms.

- sep_last:

  `NULL` or character; the separator for the last two acronyms. If
  `NULL`, `sep` will be used instead.

- show_warnings:

  logical; whether to display warnings.

## Value

a character string of the form "acronym1 = acronym 1 description;
acronym2 = acronym 2 description."

## Author

Tyler Sagendorf

## Examples

``` r
dd <- as_data_dictionary(iris)
dd$get_acronyms() # NULL
#> NULL

dd <- set_acronyms(dictionary = dd,
                   BP = "blood pressure",
                   SBP = "systolic blood pressure",
                   DBP = "diastolic blood pressure")

dd$get_acronyms()
#>                         BP                        DBP 
#>           "blood pressure" "diastolic blood pressure" 
#>                        SBP 
#>  "systolic blood pressure" 

get_acronym_defs(dd, acronyms = NULL) # use all acronyms (default)
#> [1] "BP = blood pressure; DBP = diastolic blood pressure; SBP = systolic blood pressure."
get_acronym_defs(dd, acronyms = c("DBP", "BP"))
#> [1] "BP = blood pressure; DBP = diastolic blood pressure."

# Attempt to select an acronym that was not set
get_acronym_defs(dd, acronyms = "FOO") # character(0L)
#> Warning: No acronyms match `names(dictionary$get_acronyms())`.
#> character(0)
```
