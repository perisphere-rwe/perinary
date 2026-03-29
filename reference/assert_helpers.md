# Internal assertion helpers

Lightweight assertions used across `perinary` to provide clear,
actionable error and warning messages:

- **`assert_valid_field()`**: error if a field is not valid for a
  variable type (e.g., category metadata on numeric variables).

- **`assert_in_set()`**: error if one or more values are not among the
  allowed choices, with a readable list of unknown and recognized
  values.

- **`assert_valid_dotdot()`**: validate that either `...` **or** `.list`
  is used (not both). Warn if both are empty.

These helpers are not intended for end-users; they exist to keep error
messages consistent and informative within the package.

## Usage

``` r
assert_valid_field(name, type, field, suggest = NULL)

assert_in_set(
  values,
  choices,
  value_type = "values",
  value_location = "dictionary",
  variable = NULL
)

assert_valid_dotdot(..., .list, names_required = TRUE)
```

## Arguments

- name:

  Character scalar. Variable name being validated.

- type:

  Character scalar. Variable type as a readable noun (e.g., `"numeric"`,
  `"nominal"`, `"identifier"`).

- field:

  Character scalar. The metadata field that was supplied (e.g.,
  `"category_labels"`, `"divby_modeling"`).

- suggest:

  Optional character scalar with guidance for the user (e.g.,
  `"use set_category_labels()"`).

- values:

  Character vector of user-supplied values to check.

- choices:

  Character vector of allowed values.

- value_type:

  Character scalar used in messages to describe the `values` (default
  `"values"`). Set to something specific like `"category levels"` when
  helpful.

- variable:

  Optional character scalar. If provided, the error message will name
  the variable associated with the failed check.

- ...:

  Zero or more unquoted inputs (to be collected into a list by the
  caller). Intended to be mutually exclusive with `.list`.

- .list:

  Optional pre-assembled list of inputs. Intended to be mutually
  exclusive with `...`.

## Value

`assert_valid_field()` returns `NULL` invisibly on success; otherwise it
aborts with an informative message.

`assert_in_set()` returns `NULL` invisibly when all `values` are in
`choices`; otherwise it aborts with a bulleted list of unknown values
and the recognized set.

`assert_valid_dotdot()` returns `NULL` invisibly; it aborts if both
`...` and `.list` are supplied, and it warns if both are empty.

## Conventions

- Errors use
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  with a bulleted message.

- Messages reference variable names and fields explicitly (e.g.,
  `` `date_measure` ``).

- Helpers do not modify inputs; they only validate and signal.
