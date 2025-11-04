

#' Bind Dictionaries Together
#'
#' Binding dictionaries together can be useful when you have variables that
#'   exist in multiple datasets and you don't want to re-write their meta
#'   information into each of those datasets.
#'
#' @param ... `r roxy_describe_dd(plural=TRUE)` to combine.
#'
#' @param .list `r roxy_dotlist(fun = "bind_dictionary")`. Specifically,
#'   instead of `bind_dictionary(x, y)`, you can use
#'   `bind_dictionary(.list = list(x, y))`. This is helpful when you
#'   have a lot of dictionaries and want to bind them programmatically.
#'
#' @param conflict_preference a character value indicating what to do
#'   when `x` and `y` have overlapping variables. If "left", then
#'   the definitions in the `x` dictionary are preferred because
#'   `x` is on the left of `y` in the function arguments. If "right",
#'   `y` definitions are preferred.
#'
#' @param drop_unmatched_variables logical. If `TRUE`, variables in `y` that are
#'   not in `x` will be retained in the output. If `FALSE` (default),
#'   only variables from `x` and those in `y` that overlap with `x`
#'   will be included, subject to `conflict_preference`.
#'
#'
#' @returns `r roxy_describe_dd()`
#'
#' @importFrom purrr reduce
#'
#' @export
#'
#' @examples
#'
#' dd_age_years <- data_dictionary(
#'   numeric_variable(
#'     name = "age_years",
#'     label = "Age",
#'     units = 'years',
#'     divby_modeling = 10
#'   )
#' )
#'
#' dd_age_group <- data_dictionary(
#'   nominal_variable(
#'     name = "age_group",
#'     label = "Age group",
#'     description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
#'     category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
#'     category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
#'   )
#' )
#'
#' bind_dictionary(dd_age_years, dd_age_group)

bind_dictionary <- function(..., .list = NULL,
                            conflict_preference = 'left',
                            drop_unmatched_variables = FALSE) {

  assert_valid_dotdot(..., .list = .list, names_required = FALSE)
  .dots <- infer_dotdot(..., .list = .list)

  reduce(.dots,  .f = .bind_dictionary, .dir = 'forward',
         conflict_preference = conflict_preference,
         drop_unmatched_variables = drop_unmatched_variables)

}

#' @rdname bind_dictionary
#' @export
left_bind <- function(..., .list = NULL,
                      drop_unmatched_variables = FALSE) {

  bind_dictionary(..., .list = .list,
                  conflict_preference = 'left',
                  drop_unmatched_variables = drop_unmatched_variables)

}

#' @rdname bind_dictionary
#' @export
right_bind <- function(..., .list = NULL,
                       drop_unmatched_variables = FALSE) {

  bind_dictionary(..., .list = .list,
                  conflict_preference = 'right',
                  drop_unmatched_variables = drop_unmatched_variables)

}

#' @importFrom purrr is_empty
.bind_dictionary <- function(x, y,
                             conflict_preference = NULL,
                             drop_unmatched_variables){

  vars_x <- x$variables
  vars_y <- y$variables

  dupes <- intersect(names(vars_x), names(vars_y))

  if (!is_empty(dupes) && is.null(conflict_preference)) {
    warning("Overlapping variable names in dictionaries:\n\n",
            paste(paste("-",dupes), collapse = "\n"),
            "\n\nVariable definitions from the `x` dictionary are ",
            "preferred by default. Use `conflict_preference` to ",
            "modify this pattern (and to quiet this warning message).",
            call. = FALSE)
  }

  vars_x_unmatched <- setdiff(names(vars_x), names(vars_y))
  vars_y_unmatched <- setdiff(names(vars_y), names(vars_x))

  combined_vars <- switch(
    conflict_preference %||% "left",
    left  = c(vars_x, vars_y[vars_y_unmatched]),
    right = c(vars_x[vars_x_unmatched], vars_y),
  )

  # keep the order of the x dictionary
  if(drop_unmatched_variables){
    combined_vars <- combined_vars[c(names(vars_x), vars_y_unmatched)]
  } else {
    combined_vars <- combined_vars[names(vars_x)]
  }

  DataDictionary$new(combined_vars)

}
