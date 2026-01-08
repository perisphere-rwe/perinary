
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
#' @param .dots a list of name value pairs. Values with type environment
#'   are captured and translated to regular strings based on existing templates
#'   in `dictionary`
#'
#' @param dictionary `r roxy_describe_dd()`
#'
#' @param field designate whether we are looking at the "description" field
#'   or the "label" field.
#'
#' @importFrom glue glue
#' @importFrom purrr map map2 map_lgl map_chr map2_chr discard keep
#' @importFrom rlang set_names
#'
infer_templates <- function(.dots, dictionary, field){

  template_vars <- names(which(map_lgl(.dots, is.environment)))

  if(is_empty(template_vars)) return(.dots)

  templates <- map(
    .x = set_names(template_vars),
    .f = ~ switch(
      field,
      'label' = dictionary$get_template_label(.x),
      'description' = dictionary$get_template_description(.x)
    )
  )

  invalid_vars <- names(keep(templates, .p = is.null))

  if(!is_empty(invalid_vars)){
    info <- paste_collapse(invalid_vars)
    cli_abort(
      "No {field} template has been set in supplied dictionary for {info}"
    )
  }

  invalid_binds <- map2(
    .x = .dots[template_vars],
    .y = templates,
    .f = ~setdiff(names(.x), infer_curlies(.y))
  ) %>%
    discard(.p = is_empty)

  if (!is_empty(invalid_binds)) {

    nm <- names(invalid_binds)
    n <- length(nm)

    binds_explained <- map_chr(
      .x = nm,
      .f = ~ paste0(
        "valid bindings for ",
        paste_collapse(.x, as_code = TRUE), " are ",
        paste_collapse(infer_curlies(templates[.x]), as_code = TRUE),
        ", but supplied bindings included ",
        paste_collapse(invalid_binds[[.x]], as_code = TRUE)
      )
    ) %>%
      set_names("i")

    cli_abort(
      c(
        "Bindings in {.var use_template()} must match bindings of \\
        the variables that the template is used on.",
        "i" = paste(
          "There {?is/are} {n} variable{?s} with unmatched bindings:",
          paste_collapse(nm)
        ),
        binds_explained
      )
    )

  }

  # all potential errors should have been triggered by this stage.
  # all we do here is plug the bindings in to the corresponding variable's
  # label or description, then return .dots as a named list of strings.

  .dots[template_vars] <- map2_chr(
    .x = templates,
    .y = .dots[template_vars],
    .f = ~ glue(.x, .envir = .y)
  )

  .dots

}

#' @rdname infer_helpers
#'
#' @param x character string
#'
#' @returns character vector of all symbols in curly braces: {}
#'

infer_curlies <- function(x){

  # extract everything inside { }
  vals <- gregexpr("\\{([^}]*)\\}", x, perl = TRUE)
  matches <- regmatches(x, vals)[[1]]

  # if no matches, throw your custom error
  if (is_empty(matches)) {
    stop("Invalid template specification: no values in the template appear inside of curly brackets",
         call. = FALSE)
  }

  # strip braces
  gsub("^\\{|\\}$", "", matches)

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
#' @importFrom checkmate assert_class
#' @importFrom rlang abort


infer_meta <- function(dictionary){

  .dictionary <- dictionary %||% .perinary_internal$dictionary

  if(is.null(.dictionary)){
    abort(
      message = c(
        x = "no dictionary supplied and no dictionary found as default",
        i = "see `?as_data_dictionary` to convert your data to a dictionary",
        i = "see `?set_default_dictionary` to store your dictionary as a default"
      )
    )
  }

  assert_class(.dictionary, "DataDictionary")

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





