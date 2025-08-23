
#' Internal inference helpers
#'
#' @description
#' Small utilities used across `perinary` to standardize inputs:
#'
#' - **`infer_dotdot()`**: Canonicalizes `...`/`.list` into a single list.
#'
#' - **`infer_meta()`**: Resolves the `DataDictionary` to use (argument or
#'   package default) and validates its class.
#'
#' - **`infer_overlapping_variables()`**: Finds variables present in both
#'   `data` and the dictionary (optionally warning about unmatched columns).
#'
#' These are **not exported** but documented for maintainers.
#'
#' @section Conventions:
#'
#' - Functions never modify inputs.
#' - Errors use `rlang::abort()` with actionable guidance.
#' - Variable names in messages are shown explicitly (e.g., `date_measure`).
#'
#' @keywords internal
#'
#' @name infer_helpers
NULL

#' @rdname infer_helpers
#'
#' @param ... Name–value pairs or other objects to collect into a list.
#'
#' @param .list A pre-assembled list. If supplied and non-`NULL`, it takes
#'   precedence over `...`.
#'
#' @returns
#' `infer_dotdot()` returns a **list**. If `.list` is non-`NULL`, it is
#' returned as-is; otherwise `list(...)` is returned.
#'

infer_dotdot <- function(..., .list = NULL){
  .list %||% list(...)
}

#' @rdname infer_helpers
#'
#' @param dictionary A `DataDictionary` or `NULL`. If `NULL`, the function
#'   attempts to use the package default set via [set_default_dictionary()].
#'
#' @returns
#' `infer_meta()` returns a validated **`DataDictionary`**.
#'
#' @details
#' `infer_meta()` resolves the effective dictionary to use by preferring
#' the provided `dictionary` and falling back to the package default stored
#' in `.perinary_internal$dictionary`. If neither is available, an error is
#' thrown with guidance on creating ([as_data_dictionary()]) or setting
#' ([set_default_dictionary()]) a dictionary.
#'
#' @seealso [data_dictionary()], [as_data_dictionary()], [set_default_dictionary()]
#'


infer_meta <- function(dictionary){

  .dictionary <- dictionary %||% .perinary_internal$dictionary

  if(is.null(.dictionary)){
    rlang::abort(
      message = c(
        x = "no dictionary supplied and no dictionary found as default",
        i = "see `?as_data_dictionary` to convert your data to a dictionary",
        i = "see `?set_default_dictionary` to store your dictionary as a default"
      )
    )
  }

  checkmate::assert_class(.dictionary, "DataDictionary")

  .dictionary

}


#' @rdname infer_helpers
#'
#' @param data A data frame or data.table whose column names will be checked
#'   against the dictionary.
#'
#' @param warn_unmatched Logical. If `TRUE`, emit a single warning listing
#'   variables present in `data` but absent from the dictionary.
#'
#' @returns
#' `infer_overlapping_variables()` returns a **character vector** of column
#' names that are present in both `data` and the dictionary.
#'
#' @details
#' “Unmatched” variables are those in `names(data)` that the dictionary
#' does not define (per `dictionary$get_names()`). When `warn_unmatched`
#' is `TRUE`, the function warns once with a concise list and guidance for
#' suppressing the warning.
#'

infer_overlapping_variables <- function(dictionary, data, warn_unmatched){

  overlapping_variables <- names(data) %>%
    intersect(dictionary$get_names())

  unmatched_variables <- names(data) %>%
    setdiff(overlapping_variables)

  if(!is_empty(unmatched_variables) && warn_unmatched){

    msg <- paste0(
      "dictionary does not contain information for some variables in ",
      "data: ", paste(unmatched_variables, collapse = ', '),
      ". To suppress this warning, set `warn_unmatched` to `FALSE`"
    )

    warning(msg, call. = FALSE)

  }

  overlapping_variables

}





