#' Translate data, names, and categories with a dictionary
#'
#' Transfer variable labels/units and category labels/order into a vector
#' or data frame.
#'
#' @description
#' The `translate_*()` helpers look up metadata in a `dictionary` and
#' apply it to your data:
#'
#' - `translate_data()` attaches variable labels as column attributes,
#'   optionally recodes category labels and enforces category order,
#'   and optionally scales numeric variables for modeling.
#'
#' - `translate_names()` replaces variable names with variable labels.
#'
#' - `translate_categories()` replaces category levels with
#'   category labels and enforces category order specified in `dictionary`.
#'
#'
#' @param x An object to translate.
#'   - For `translate_data()`: `x` must be a data frame.
#'   - For `translate_names()` and `translate_categories()`: `x` must be a
#'     character/factor.
#'
#' @param ... Additional arguments passed to the `translate` method.
#'
#' - For `translate_data`, nothing happens at the moment. Eventually
#'   this `...` will be used to select which columns to translate. The
#'   default will be `everything()`.
#'
#' - For `translate_names`, `...` must contain name-value pairs. The name can
#'   be any value in `x` and the corresponding value indicates what to
#'   translate `x` to. This can be used  to overrule the value that the
#'   dictionary would translate to, or to add extra translation maps in cases
#'   where the dictionary doesn't contain a map for all values in `x`
#'   (see examples).
#'
#' @param dictionary `r roxy_describe_dd()`. `r roxy_default_dd()`.
#'
#' @param units a character value indicating how units should be incorporated
#'   into variable labels. Valid choices are
#'   - `"none"` : do not include units in labels, e.g., "Age"
#'   - `"descriptive"` : include labels, e.g., "Age, *years*"
#'   - `"model"` : include label and model divisor, e.g., "Age, *per 10 years*"
#'
#' Note that setting `units = "model"` **will modify values** of numeric
#' variables in `data` with a `divby_modeling` value. See details for explanation.
#'
#' @param warn_unmatched a logical value. If `TRUE` (default), then
#'   a warning is thrown whenever 1 or more variables in `data` do not
#'   have supporting documentation in `dictionary`. If `FALSE`, then
#'   this information will not be presented in a warning.
#'
#' @param apply_variable_labels Logical (default `TRUE`). If `TRUE`,
#'   variable labels are attached as column attributes.
#'
#' @param apply_category_labels Logical (default `TRUE`). If `TRUE`,
#'   category levels are replaced with labels. Output is also coerced to
#'   a factor so that the order of categories from the dictionary is applied.
#'
#' @param nominals_to_factor Logical (default `TRUE`). If `TRUE`,
#'   nominal variables are converted to factors with level order
#'   determined using relevant information from `dictionary`.
#'
#' @param to_factor Logical (default `TRUE`). If `TRUE`,
#'   output will be converted to a factor with its level order determined
#'   using relevant information from `dictionary`.
#'
#' @details
#'
#' With the choice `units = "model"`, any numeric variable in
#' `dictionary` that has a `divby_modeling` value will be modified in the
#' data, specifically by being divided by `divby_modeling`. This transform
#' will also be indicated in the label for said variable. E.g., if a
#' variable `age` has label `"Age"`, a unit of `"years"` and `divby_modeling`
#' of 10, setting `units = "model"` will cause the `age` column to be divided
#' by 10 with a label of "Age, per 10 years"
#'
#' @returns
#' - `translate_data()`: returns a data frame with labels/levels updated.
#'   Numeric columns may be scaled when `units = "model"`.
#'
#' - `translate_names()`: returns a factor with variable labels
#'  corresponding to input names.
#'
#' - `translate_categories()`: returns a factor with category labels
#'  corresponding to input levels.
#'
#' @examples
#'
#' age_years <- numeric_variable(
#'   name = "age_years",
#'   label = "Age of participant",
#'   units = "years",
#'   divby_modeling = 10
#' )
#'
#' gender <- nominal_variable(
#'   name = "gender",
#'   label = "Gender of participant",
#'   category_levels = c("M", "F"),
#'   category_labels = c("Male", "Female")
#' )
#'
#' dd <- data_dictionary(age_years, gender)
#' dat <- data.frame(age_years = 55, gender = "M")
#'
#' # Translate an entire data frame (labels + categories; descriptive units)
#' out1 <- translate_data(
#'   dat, dictionary = dd,
#'   units = "descriptive"
#' )
#' purrr::map(out1, ~ attr(.x, "label"))
#'
#' # Prepare for modeling: scale numerics by divby_modeling and update labels
#' out2 <- translate_data(
#'   dat, dictionary = dd,
#'   units = "model"
#' )
#' out2$age_years # now 5.5 (i.e., 55 / 10)
#'
#' attr(out2$age_years, "label") # "Age of participant, per 10 years"
#'
#' # Replace variable names with labels (useful after pivot_longer)
#' translate_names(c("age_years", "gender"), dictionary = dd)
#'
#' # Replace category codes with labels (and enforce order)
#' translate_categories(dat$gender, dictionary = dd)
#'
#' @seealso
#' [data_dictionary()], [as_data_dictionary()], [set_default_dictionary()]
#'
#' @export
#'

translate_data <- function(x, ...,
                           dictionary = NULL,
                           units = 'none',
                           warn_unmatched = TRUE,
                           apply_variable_labels = TRUE,
                           apply_category_labels = TRUE,
                           nominals_to_factor = TRUE){

  checkmate::assert_character(units, len = 1, any.missing = FALSE)
  checkmate::assert_choice(units, choices=c('none','descriptive','model'))
  checkmate::assert_logical(warn_unmatched, len = 1)
  checkmate::assert_logical(apply_variable_labels, len = 1)
  checkmate::assert_logical(apply_category_labels, len = 1)

  infer_meta(dictionary)$translate_data(
    x = x, ...,
    units = units,
    warn_unmatched = warn_unmatched,
    apply_variable_labels = apply_variable_labels,
    apply_category_labels = apply_category_labels,
    nominals_to_factor = nominals_to_factor
  )

}


#' @rdname translate_data
#' @param .list For `translate_names()`: an optional **named** character
#'   vector mapping `old_name = "Label"`. When supplied, it takes precedence
#'   over the dictionary lookup (useful for ad-hoc overrides).
#' @export
translate_names <- function(x, ...,
                           .list = NULL,
                           dictionary = NULL,
                           units = "none",
                           to_factor = FALSE,
                           warn_unmatched = TRUE){

  checkmate::assert_character(units, len = 1, any.missing = FALSE)
  checkmate::assert_choice(units, choices=c('none','descriptive','model'))
  checkmate::assert_logical(warn_unmatched, len = 1)
  checkmate::assert_list(.list, types='character', names='named', null.ok=TRUE)

  infer_meta(dictionary)$translate_names(x = x, ...,
                                         .list = .list,
                                         units = units,
                                         to_factor = to_factor,
                                         warn_unmatched = warn_unmatched)

}

#' @rdname translate_data
#' @param names For `translate_categories()`: optional character vector of
#'   variable names to translate (useful when `x` is a data frame or when
#'   disambiguation is needed).
#' @export
translate_categories <- function(x, ...,
                                .list = NULL,
                                dictionary = NULL,
                                names = NULL,
                                to_factor = FALSE,
                                warn_unmatched = TRUE){

  checkmate::assert_logical(warn_unmatched, len = 1)
  checkmate::assert_list(.list, types='character', names='named', null.ok=TRUE)

  infer_meta(dictionary)$translate_categories(x = x, ...,
                                              .list = .list,
                                              names = names,
                                              to_factor = to_factor,
                                              warn_unmatched = warn_unmatched)

}



