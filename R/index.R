
#' Order rows by dictionary terms
#'
#' Reorders a data frame so that rows associated with nominal variables
#' follow the order defined in a `DataDictionary`. This is useful when
#' you want multiple outputs to present variables in a consistent order.
#'
#' @param data A data frame or tibble containing rows to order. Must
#'   include columns that identify the variable name and category level.
#'
#' @param dictionary `r roxy_describe_dd()`. `r roxy_default_dd()`.
#'
#' @param names Character value giving the column name that stores the
#'   variable name in `data`. Default is `"name"`.
#'
#' @param levels Character value giving the column name that stores the
#'   category code/level in `data`. Default is `"level"`.
#'
#' @returns The same type as `data`, with rows re-ordered to match the
#'   order of variables in the dictionary and, within nominal variables,
#'   the order of categories.
#'
#' @examples
#'
#' dd <- data_dictionary(
#'   nominal_variable(
#'     "gender",
#'     label = "Gender",
#'     category_levels = c("M", "F"),
#'     category_labels = c("Male", "Female")
#'   )
#' )
#'
#' df <- tibble::tibble(
#'   name = c("gender", "gender"),
#'   level = c("F", "M"),
#'   n = c(12, 18)
#' )
#'
#' index_rows(df, dictionary = dd)
#'
#' @export
index_rows <- function(data,
                       dictionary = NULL,
                       names = 'name',
                       levels = 'level'){

  infer_meta(dictionary)$index_rows(data, names = names, levels = levels)

}


#' Order rows by dictionary terms (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `index_terms()` has been renamed to [index_rows()] for consistency with
#' [index_columns()]. Please update your code to use `index_rows()`.
#'
#' @inheritParams index_rows
#'
#' @returns The same type as `data`, with rows re-ordered. See [index_rows()].
#'
#' @importFrom lifecycle deprecate_warn
#'
#' @export
index_terms <- function(data,
                        dictionary = NULL,
                        names = 'name',
                        levels = 'level'){

  deprecate_warn(
    when    = "0.0.2",
    what    = "index_terms()",
    with    = "index_rows()"
  )

  index_rows(data, dictionary = dictionary, names = names, levels = levels)

}


#' Order columns by dictionary variables
#'
#' Reorders the columns of a data frame so that columns whose names appear
#' in a `DataDictionary` come first, in the order they are defined in the
#' dictionary. Columns not present in the dictionary are optionally kept
#' at the end.
#'
#' @param data A data frame or tibble whose columns will be reordered.
#'
#' @param dictionary `r roxy_describe_dd()`. `r roxy_default_dd()`.
#'
#' @param keep_unmatched Logical. If `TRUE` (the default), columns in
#'   `data` that are not defined in the dictionary are retained and
#'   appended after the matched columns. If `FALSE`, those columns are
#'   dropped from the output.
#'
#' @returns The same type as `data`, with columns reordered (and
#'   optionally filtered) according to the dictionary.
#'
#' @examples
#'
#' dd <- data_dictionary(
#'   numeric_variable("bill_length_mm", label = "Bill length", units = "mm"),
#'   numeric_variable("bill_depth_mm",  label = "Bill depth",  units = "mm"),
#'   nominal_variable("species", label = "Species",
#'                    category_levels = c("Adelie", "Chinstrap", "Gentoo"))
#' )
#'
#' df <- data.frame(
#'   species        = c("Adelie", "Chinstrap"),
#'   extra_col      = 1:2,
#'   bill_depth_mm  = c(18.7, 17.4),
#'   bill_length_mm = c(39.1, 46.5)
#' )
#'
#' # Matched columns appear in dictionary order; extra_col is appended
#' index_columns(df, dictionary = dd)
#'
#' # Drop columns not in the dictionary
#' index_columns(df, dictionary = dd, keep_unmatched = FALSE)
#'
#' @export
index_columns <- function(data,
                          dictionary = NULL,
                          keep_unmatched = TRUE){

  infer_meta(dictionary)$index_columns(data, keep_unmatched = keep_unmatched)

}
