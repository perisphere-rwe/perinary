

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
#' index_terms(df, dictionary = dd)
#'
#' @export
index_terms <- function(data,
                        dictionary = NULL,
                        names = 'name',
                        levels = 'level'){

  infer_meta(dictionary)$index(data, names = names, levels = levels)

}
