
#' Select Variables in a Dictionary
#'
#' Keep, drop, or reorder variables in a `DataDictionary` using
#' [tidyselect][tidyselect::language] semantics, similar to `dplyr::select()`
#' for data frames. This is useful for narrowing a dictionary down to the
#' variables relevant to a particular dataset or analysis, or for resolving
#' ambiguity (e.g. dropping one of two variables that happen to share a
#' label) before using functions like [index_rows()].
#'
#' @param dictionary `r roxy_describe_dd()`
#'
#' @param ... One or more unquoted variable names, tidyselect helpers
#'   (e.g. `starts_with()`, `ends_with()`), or negated selections
#'   (e.g. `-age`) identifying which variables to keep. The order of the
#'   selection determines the variable order in the returned dictionary.
#'
#' @returns `r roxy_describe_dd()` containing only the selected variables,
#'   in selection order.
#'
#' @importFrom checkmate assert_class
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @examples
#'
#' dd <- as_data_dictionary(iris)
#'
#' # keep only Species and columns ending in "Length"
#' select_variables(dd, Species, ends_with("Length"))
#'
#' # drop a variable
#' select_variables(dd, -Species)
#'
#' # resolve a label collision by dropping one of the colliding variables
#' # before ordering rows with index_rows()
#' dd_dup <- data_dictionary(
#'   numeric_variable("age_baseline", label = "Age"),
#'   numeric_variable("age_followup", label = "Age")
#' )
#'
#' select_variables(dd_dup, -age_followup)
#'
select_variables <- function(dictionary, ...){

  assert_class(dictionary, "DataDictionary")

  tmp_data <- matrix(data = 0,
                     nrow = 1,
                     ncol = length(dictionary$variables),
                     dimnames = list(rows = NULL,
                                     cols = dictionary$get_names())) |>
    as_tibble()

  selected_names <- names(select(tmp_data, ...))

  DataDictionary$new(dictionary$variables[selected_names])

}
