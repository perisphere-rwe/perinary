# Translate data, names, and categories with a dictionary

The `translate_*()` helpers look up metadata in a `dictionary` and apply
it to your data:

- `translate_data()` attaches variable labels as column attributes,
  optionally recodes category labels and enforces category order, and
  optionally scales numeric variables for modeling.

- `translate_names()` replaces variable names with variable labels.

- `translate_categories()` replaces category levels with category labels
  and enforces category order specified in `dictionary`.

## Usage

``` r
translate_data(
  x,
  ...,
  dictionary = NULL,
  units = "none",
  warn_unmatched = TRUE,
  apply_variable_labels = TRUE,
  apply_category_labels = TRUE,
  nominals_to_factor = TRUE,
  drop_unused_levels = FALSE
)

translate_names(
  x,
  ...,
  .list = NULL,
  dictionary = NULL,
  units = "none",
  to_factor = FALSE,
  warn_unmatched = TRUE,
  allow_duplicates = FALSE,
  drop_unused_levels = FALSE
)

translate_categories(
  x,
  ...,
  .list = NULL,
  dictionary = NULL,
  names = NULL,
  to_factor = FALSE,
  warn_unmatched = TRUE,
  drop_unused_levels = FALSE
)
```

## Arguments

- x:

  An object to translate.

  - For `translate_data()`: `x` must be a data frame.

  - For `translate_names()` and `translate_categories()`: `x` must be a
    character/factor.

- ...:

  Additional arguments passed to the `translate` method.

  - For `translate_data`, `...` uses
    [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
    to choose which columns to translate. Any tidyselect expression is
    accepted (e.g., `starts_with("age")`, `c(sex, bmi)`). When `...` is
    empty the default is `everything()`, i.e. all columns in `x` are
    considered for translation.

  - For `translate_names`, `...` must contain name-value pairs. The name
    can be any value in `x` and the corresponding value indicates what
    to translate `x` to. This can be used to overrule the value that the
    dictionary would translate to, or to add extra translation maps in
    cases where the dictionary doesn't contain a map for all values in
    `x` (see examples).

- dictionary:

  A [data
  dictionary](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md)
  object. If `NULL`, uses the default dictionary set via
  [`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md).

- units:

  a character value indicating how units should be incorporated into
  variable labels. Valid choices are

  - `"none"` : do not include units in labels, e.g., "Age"

  - `"descriptive"` : include labels, e.g., "Age, *years*"

  - `"model"` : include label and model divisor, e.g., "Age, *per 10
    years*"

  Note that setting `units = "model"` **will modify values** of numeric
  variables in `data` with a `divby_modeling` value. See details for
  explanation.

- warn_unmatched:

  a logical value. If `TRUE` (default), then a warning is thrown
  whenever 1 or more variables in `data` do not have supporting
  documentation in `dictionary`. If `FALSE`, then this information will
  not be presented in a warning.

- apply_variable_labels:

  Logical (default `TRUE`). If `TRUE`, variable labels are attached as
  column attributes.

- apply_category_labels:

  Logical (default `TRUE`). If `TRUE`, category levels are replaced with
  labels. Output is also coerced to a factor so that the order of
  categories from the dictionary is applied.

- nominals_to_factor:

  Logical (default `TRUE`). If `TRUE`, nominal variables are converted
  to factors with level order determined using relevant information from
  `dictionary`.

- drop_unused_levels:

  Logical (default `FALSE`). If `TRUE` (and `nominals_to_factor = TRUE`
  or `to_factor = TRUE`), unused levels will be dropped from nominal
  variables.

- .list:

  For `translate_names()`: an optional **named** character vector
  mapping `old_name = "Label"`. When supplied, it takes precedence over
  the dictionary lookup (useful for ad-hoc overrides).

- to_factor:

  Logical (default `TRUE`). If `TRUE`, output will be converted to a
  factor with its level order determined using relevant information from
  `dictionary`.

- allow_duplicates:

  a logical value indicating whether to allow a single label to be
  attached to multiple variable names. This is `FALSE` by default
  because allowing duplicate labels can cause unexpected errors in
  downstream applications. Use with caution.

- names:

  For `translate_categories()`: optional character vector of variable
  names to translate (useful when `x` is a data frame or when
  disambiguation is needed).

## Value

- `translate_data()`: returns a data frame with labels/levels updated.
  Numeric columns may be scaled when `units = "model"`.

- `translate_names()`: returns a factor with variable labels
  corresponding to input names.

- `translate_categories()`: returns a factor with category labels
  corresponding to input levels.

## Details

Transfer variable labels/units and category labels/order into a vector
or data frame.

With the choice `units = "model"`, any numeric variable in `dictionary`
that has a `divby_modeling` value will be modified in the data,
specifically by being divided by `divby_modeling`. This transform will
also be indicated in the label for said variable. E.g., if a variable
`age` has label `"Age"`, a unit of `"years"` and `divby_modeling` of 10,
setting `units = "model"` will cause the `age` column to be divided by
10 with a label of "Age, per 10 years"

## See also

[`data_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/data_dictionary.md),
[`as_data_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/as_data_dictionary.md),
[`set_default_dictionary()`](https://perisphere-rwe.github.io/perinary/reference/set_default_dictionary.md)

## Examples

``` r
age_years <- numeric_variable(
  name = "age_years",
  label = "Age of participant",
  units = "years",
  divby_modeling = 10
)

gender <- nominal_variable(
  name = "gender",
  label = "Gender of participant",
  category_levels = c("M", "F"),
  category_labels = c("Male", "Female")
)

dd <- data_dictionary(age_years, gender)
dat <- data.frame(age_years = 55, gender = "M")

# Translate an entire data frame (labels + categories; descriptive units)
out1 <- translate_data(
  dat, dictionary = dd,
  units = "descriptive"
)
purrr::map(out1, ~ attr(.x, "label"))
#> $age_years
#> [1] "Age of participant, years"
#> 
#> $gender
#> [1] "Gender of participant"
#> 

# Prepare for modeling: scale numerics by divby_modeling and update labels
out2 <- translate_data(
  dat, dictionary = dd,
  units = "model"
)
out2$age_years # now 5.5 (i.e., 55 / 10)
#> [1] 5.5
#> attr(,"label")
#> [1] "Age of participant, per 10 years"

attr(out2$age_years, "label") # "Age of participant, per 10 years"
#> [1] "Age of participant, per 10 years"

# Translate only the age_years column (gender left unchanged)
out3 <- translate_data(
  dat, dictionary = dd,
  age_years
)
attr(out3$age_years, "label") # "Age of participant"
#> [1] "Age of participant"
attr(out3$gender, "label")    # NULL
#> NULL

# Replace variable names with labels (useful after pivot_longer)
translate_names(c("age_years", "gender"), dictionary = dd)
#> [1] "Age of participant"    "Gender of participant"

# Replace category codes with labels (and enforce order)
translate_categories(dat$gender, dictionary = dd)
#> [1] "Male"
```
