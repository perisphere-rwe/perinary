# Internal inference helpers

Small utilities used across `perinary` to standardize inputs:

- **`infer_dotdot()`**: Canonicalizes `...`/`.list` into a single list.

- **`infer_meta()`**: Resolves the `DataDictionary` to use (argument or
  package default) and validates its class.

- **`infer_overlapping_variables()`**: Finds variables present in both
  `data` and the dictionary (optionally warning about unmatched
  columns).

These are **not exported** but documented for maintainers.

## Usage

``` r
infer_dotdot(..., .list = NULL)

infer_templates(.dots, dictionary, field)

infer_curlies(x)

infer_meta(dictionary)

infer_overlapping_variables(dictionary, data, warn_unmatched)
```

## Arguments

- ...:

  Name–value pairs or other objects to collect into a list.

- .list:

  A pre-assembled list. If supplied and non-`NULL`, it takes precedence
  over `...`.

- .dots:

  a list of name value pairs. Values with type environment are captured
  and translated to regular strings based on existing templates in
  `dictionary`

- dictionary:

  A `DataDictionary` or `NULL`. If `NULL`, the function attempts to use
  the package default set via
  [`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md).

- field:

  designate whether we are looking at the "description" field or the
  "label" field.

- x:

  character string

- data:

  A data frame or data.table whose column names will be checked against
  the dictionary.

- warn_unmatched:

  Logical. If `TRUE`, emit a single warning listing variables present in
  `data` but absent from the dictionary.

## Value

`infer_dotdot()` returns a **list**. If `.list` is non-`NULL`, it is
returned as-is; otherwise `list(...)` is returned.

character vector of all symbols in curly braces:

`infer_meta()` returns a validated **`DataDictionary`**.

`infer_overlapping_variables()` returns a **character vector** of column
names that are present in both `data` and the dictionary.

## Details

`infer_meta()` resolves the effective dictionary to use by preferring
the provided `dictionary` and falling back to the package default stored
in `.perinary_internal$dictionary`. If neither is available, an error is
thrown with guidance on creating
([`as_data_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/as_data_dictionary.md))
or setting
([`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md))
a dictionary.

“Unmatched” variables are those in `names(data)` that the dictionary
does not define (per `dictionary$get_names()`). When `warn_unmatched` is
`TRUE`, the function warns once with a concise list and guidance for
suppressing the warning.

## Conventions

- Functions never modify inputs.

- Errors use
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  with actionable guidance.

- Variable names in messages are shown explicitly (e.g.,
  `date_measure`).

## See also

[`data_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md),
[`as_data_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/as_data_dictionary.md),
[`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md)
