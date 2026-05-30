
#' Bind Variables to a Dictionary
#'
#' @description
#' A convenience wrapper for adding one or more new variables to an existing
#' dictionary. Each variable's name is taken from its corresponding named
#' argument in `...`, so there is no need to pass `name` inside the variable
#' constructor itself.
#'
#' @param dictionary `r roxy_describe_dd()`
#'
#' @param ... Named arguments. Each name becomes the variable name, and each
#'   value is a call to a variable constructor such as [nominal_variable()],
#'   [numeric_variable()], [logical_variable()], [identifier_variable()], or
#'   [date_variable()]. The `name` argument of the constructor may be omitted;
#'   `bind_variables` will inject it automatically.
#'
#' @param .list `r roxy_dotlist("bind_variables")`. A named list of
#'   `DataVariable` objects. Each element's list-entry name will be used to
#'   set (or override) the variable name.
#'
#' @details
#' `bind_variables` does **not** allow overwriting variables already present
#' in `dictionary`. To modify existing variables use the `set_*` family
#' (e.g. [set_labels()], [set_units()]).
#'
#' @returns `r roxy_describe_dd()`
#'
#' @importFrom checkmate assert_class
#' @importFrom cli cli_abort cli_warn
#' @importFrom purrr is_empty imap map_chr
#' @importFrom rlang enquos get_expr get_env eval_tidy as_label
#'
#' @export
#'
#' @examples
#'
#' meta <- as_data_dictionary(iris)
#'
#' meta <- meta |>
#'   bind_variables(
#'     train_test = nominal_variable(
#'       label = "Train/test split",
#'       category_levels = c("train", "test"),
#'       category_labels = c("Training data", "Testing data")
#'     )
#'   )
#'
#' meta
#'

bind_variables <- function(dictionary, ..., .list = NULL) {

  assert_class(dictionary, "DataDictionary")

  empty_list <- is_empty(.list)

  # Capture `...` lazily so variable constructors are not yet evaluated,
  # allowing us to inject `name` before each call runs.
  dots_q     <- enquos(...)
  empty_dots <- is_empty(dots_q)

  if (!empty_dots && !empty_list)
    cli_abort(c(
      "`...` must be empty if `.list` is not `NULL`.",
      "i" = "Supply variables through one interface at a time."
    ))

  if (empty_dots && empty_list) {
    cli_warn(c(
      "i" = "`...` is empty and `.list` is `NULL`. No variables were added."
    ))
    return(dictionary)
  }

  vars <- if (empty_list) {

    nms         <- names(dots_q)
    unnamed_idx <- which(is.null(nms) | nms == "")

    if (!is_empty(unnamed_idx)) {
      n   <- length(unnamed_idx)
      bad <- dots_q[unnamed_idx] |>
        map_chr(as_label) |>
        paste_collapse(as_code = TRUE)
      cli_abort(c(
        "Unnamed inputs in {.var ...}.",
        "i" = paste("There {?is/are} {n} unnamed input{?s}:", bad),
        "i" = "All arguments to `...` must be named."
      ))
    }

    imap(dots_q, function(q, nm) {
      expr <- get_expr(q)
      env  <- get_env(q)
      if (!is.call(expr)) {
        # Pre-built object passed as a symbol: evaluate, clone, rename
        obj <- eval_tidy(q)
        obj <- obj$clone()
        obj$set_name(nm)
        return(obj)
      }
      # Inject (or override) `name` before evaluating the constructor call
      expr[["name"]] <- nm
      eval(expr, envir = env)
    })

  } else {

    assert_valid_dotdot(.list = .list, names_required = TRUE)
    .dots <- infer_dotdot(.list = .list)

    imap(.dots, function(obj, nm) {
      obj <- obj$clone()
      obj$set_name(nm)
      obj
    })

  }

  existing_nms <- dictionary$get_names()
  conflicts    <- intersect(names(vars), existing_nms)

  if (!is_empty(conflicts))
    cli_abort(c(
      "Cannot overwrite existing variable{?s}: {.val {conflicts}}",
      "i" = "Use {.code set_*} functions (e.g. {.fn set_labels}) to modify existing variables."
    ))

  new_dict <- DataDictionary$new(vars)

  # Passing conflict_preference is not needed because conflicts are guarded
  # above; the two dictionaries are guaranteed to have disjoint variable names.
  bind_dictionary(dictionary, new_dict)

}
