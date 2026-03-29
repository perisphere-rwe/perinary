# Append Model Output with Missing Terms

Add columns indicating variable name, category level, category label,
and reference groups to the model output (a row is also added for each
reference group)

## Usage

``` r
append_term_key(
  data,
  dictionary = NULL,
  term_separator = "",
  term_colname = "term"
)
```

## Arguments

- data:

  A data frame or tibble containing model output, typically from
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html).
  Must contain a column that identifies model terms.

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

## Value

A tibble with the original `data` joined to the term key from the
dictionary. The result includes additional columns like `variable` and
`category`, and ensures consistent ordering of model terms with appended
reference rows (if missing).

## Details

The function uses the `term` column (or other specified column) in the
input `data` to match with a term key derived from the dictionary. The
final output includes a `variable` column and preserves term order,
appending reference terms (if needed) for completeness.
