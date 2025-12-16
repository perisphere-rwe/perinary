#' Internal assertion helpers
#'
#' @description
#' Lightweight assertions used across `perinary` to provide clear,
#' actionable error and warning messages:
#'
#' - **`assert_valid_field()`**: error if a field is not valid for a
#'   variable type (e.g., category metadata on numeric variables).
#' - **`assert_in_set()`**: error if one or more values are not among the
#'   allowed choices, with a readable list of unknown and recognized values.
#' - **`assert_valid_dotdot()`**: validate that either `...` **or** `.list`
#'   is used (not both). Warn if both are empty.
#'
#' These helpers are not intended for end-users; they exist to keep error
#' messages consistent and informative within the package.
#'
#' @section Conventions:
#' - Errors use `rlang::abort()` with a bulleted message (via `glue`).
#' - Messages reference variable names and fields explicitly (e.g.,
#'   `` `date_measure` ``).
#' - Helpers do not modify inputs; they only validate and signal.
#'
#' @keywords internal
#' @name assert_helpers
NULL

#' @rdname assert_helpers
#'
#' @param name Character scalar. Variable name being validated.
#' @param type  Character scalar. Variable type as a readable noun
#'   (e.g., `"numeric"`, `"nominal"`, `"identifier"`).
#' @param field Character scalar. The metadata field that was supplied
#'   (e.g., `"category_labels"`, `"divby_modeling"`).
#' @param suggest Optional character scalar with guidance for the user
#'   (e.g., `"use set_category_labels()"`).
#'
#' @returns
#' `assert_valid_field()` returns `NULL` invisibly on success; otherwise
#' it aborts with an informative message.
#'
#' @importFrom glue glue
#' @importFrom rlang abort

assert_valid_field <- function(name, type, field, suggest = NULL){

  msg = c(
    glue(
      "Invalid specification of `{field}` for variable `{name}`"
    ),
    "x" = glue(
      "`{field}` cannot be specified for {type} variables"
    )
  )

  if(!is.null(suggest)){
    msg %<>% c(
      "i" = glue(
        "`{field}` may be specified for {paste_collapse(suggest)} variables."
      )
    )
  }

  abort(message = msg, call = NULL)

}

#' @rdname assert_helpers
#'
#' @param values  Character vector of user-supplied values to check.
#' @param choices Character vector of allowed values.
#' @param value_type Character scalar used in messages to describe the
#'   `values` (default `"values"`). Set to something specific like
#'   `"category levels"` when helpful.
#' @param variable Optional character scalar. If provided, the error
#'   message will name the variable associated with the failed check.
#'
#' @returns
#' `assert_in_set()` returns `NULL` invisibly when all `values` are in
#' `choices`; otherwise it aborts with a bulleted list of unknown values and
#' the recognized set.
#'
#' @importFrom glue glue
#' @importFrom rlang abort

assert_in_set <- function(values, choices,
                          value_type = "values",
                          value_location = 'dictionary',
                          variable = NULL){

  unmatched_values <- setdiff(values, choices)

  if(!is_empty(unmatched_values)){

    msg = c(
      glue("Invalid {value_type}:"),
      if(!is.null(variable)) c("i" = glue("Variable: `{variable}`")),
      "x" = glue("Unrecognized input values: \\
                 {paste_collapse(unmatched_values)}"),
      "i" = glue("Recognized values in {value_location}: \\
                  {paste_collapse(choices)}")
    )

    abort(message = msg, call = NULL)

  }

}


#' @rdname assert_helpers
#'
#' @param ... Zero or more unquoted inputs (to be collected into a list
#'   by the caller). Intended to be mutually exclusive with `.list`.
#' @param .list Optional pre-assembled list of inputs. Intended to be
#'   mutually exclusive with `...`.
#'
#' @returns
#' `assert_valid_dotdot()` returns `NULL` invisibly; it aborts if both
#' `...` and `.list` are supplied, and it warns if both are empty.
#'
#' @importFrom glue glue
#' @importFrom rlang abort warn

assert_valid_dotdot <- function(..., .list, names_required = TRUE){

  empty_dots <- is_empty(list(...))
  empty_list <- is_empty(.list)

  if(!empty_dots && !empty_list){
    abort(
      message = c(
        "`...` must be empty if `.list` is not `NULL`",
        i = glue("This function can work with either \\
                  unquoted inputs or a list of inputs, \\
                  but it is not intended to be used with both \\
                  at once.")
      )
    )
  }

  if(empty_dots && empty_list){
    warn(
      message = c(
        i = glue("`...` is empty and so is `.list`. No action can \\
                 be taken when both of these inputs are unspecified.")
      )
    )
  }

  if(!names_required) return(invisible(TRUE))

  if(empty_list) return(assert_named_dots(...))

  if(empty_dots) return(assert_named_list(.list))

}

#' @importFrom rlang abort is_empty
assert_valid_template <- function(x) {

  inner <- infer_curlies(x)

  # validate allowed characters (letters, digits, underscores). The text in the
  # curly braces can only start with an upper or lowercase letter, followed by
  # zero or more letters, numbers, underscores, or periods.
  invalid <- !grepl("^[A-Za-z]+[A-Za-z0-9_.]*$", inner)

  if (any(invalid)) {
    bad_vals <- paste0("{", inner[invalid], "}", collapse = ", ")
    abort(
      message = paste0(
        "Invalid template specification: illegal characters inside ",
        bad_vals
      ),
      call = FALSE
    )
  }

  invisible(TRUE)

}



#' @importFrom cli cli_abort
#' @importFrom purrr map_chr
#' @importFrom rlang as_label enquos
assert_named_dots <- function(...) {

  .dots <- enquos(...)

  nm <- names(.dots)

  unnamed_idx <- which(nm == "" | is.null(nm))

  if (!is_empty(unnamed_idx)) {

    n <- length(unnamed_idx)
    nm <- .dots[unnamed_idx] %>%
      map_chr(as_label) %>%
      paste_collapse(as_code = TRUE)

    cli_abort(
      c(
        "Unnamed inputs in {.var ...}",
        "i" = paste("There {?is/are} {n} unnamed input{?s}:", nm),
        "i" = "All arguments to `...` must be named."
      )
    )
  }

  invisible(TRUE)

}


#' @importFrom cli cli_abort
#' @importFrom purrr map_chr
assert_named_list <- function(x) {

  nm <- names(x) %||% rep("", length(x))

  # Identify unnamed entries
  unnamed_idx <- which(is.null(nm) | nm == "" | is.na(nm))

  if (length(unnamed_idx) > 0) {

    if (!is_empty(unnamed_idx)) {

      n <- length(unnamed_idx)

      # Try to give a friendly representation of each bad element
      nm <- x[unnamed_idx] %>%
        map_chr( ~ {
        if (is.atomic(.x) && length(.x) == 1)
          as.character(.x)
        else
          paste0("<", typeof(.x), ">")
      }) %>%
        paste_collapse(as_code = TRUE)

      cli_abort(
        c(
          "Unnamed inputs in {.var .list}",
          "i" = paste("There {?is/are} {n} unnamed input{?s}:", nm),
          "i" = "All items in {.var .list} must be named."
        )
      )
    }

  }

  invisible(TRUE)

}






