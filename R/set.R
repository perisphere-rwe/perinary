



#' Modify Data Dictionary Elements
#'
#' Set metadata in a `DataDictionary` object.
#'
#' @param dictionary `r roxy_describe_dd()`
#' @param ... Name-value pairs. The name gives the name of the variable
#'   that will be modified. The value must be a:
#'
#'   - character value for `set_labels()`, `set_units()`,
#'     and `set_descriptions()`
#'   - numeric value for `set_divby_modeling()`
#'   - character vector for `set_category_levels()` and `set_category_labels()`
#'
#' @param .list a list of name-value pairs. This argument is optional and
#'   intended to be used in place of `...` for programmatic setting of
#'   meta data.
#'
#' @return a modified `dictionary`
#'
#' @export
#'
#' @examples
#'
#' dd <- as_data_dictionary(data.frame(a = 1, b = "cat", id = 1)) %>%
#'  set_identifiers(id) %>%
#'  set_labels(a = "numeric example", b = "categorical example") %>%
#'  set_units(a = "years") %>%
#'  set_divby_modeling(a = 10) %>%
#'  set_descriptions(a = "A variable used for examples") %>%
#'  set_category_labels(b = c("cat" = "A small lion"))
#'
#' dd
#'
#' # programmatic assignment
#'
#' label_list <- list(a = "Listed numeric example",
#'                    b = "Listed categorical example")
#' set_labels(dd, .list = label_list)
#'
#'
#'
#'

set_labels <- function(dictionary, ..., .list = NULL){
  dd_set(dictionary, ..., .list = .list, field = 'label')
}

#' @rdname set_labels
#' @export
set_descriptions <- function(dictionary, ..., .list = NULL){
  dd_set(dictionary, ..., .list = .list, field = 'description')
}

#' @rdname set_labels
#' @export
set_units <- function(dictionary, ..., .list = NULL){
  dd_set(dictionary, ..., .list = .list, field = 'units')
}

#' @rdname set_labels
#' @export
set_divby_modeling <- function(dictionary, ..., .list = NULL){
  dd_set(dictionary, ..., .list = .list, field = 'divby_modeling')
}

#' @importFrom checkmate assert_class
dd_set_prep <- function(dictionary, .dots, field){
  assert_class(dictionary, "DataDictionary")
  dictionary$check_modify_call(.dots, field = field)
  dictionary$clone(deep = dictionary$copy_on_modify)
}

dd_set <- function(dictionary, ..., .list, field){

  assert_valid_dotdot(..., .list = .list)

  # capture input and plug in templates from dictionary
  .dots <- infer_dotdot(..., .list = .list) %>%
    infer_templates(dictionary, field = field)

  if(is_empty(.dots)) return(dictionary)
  dictionary <- dd_set_prep(dictionary, .dots, field = field)
  dictionary$modify_dictionary(.dots, field = field)
  dictionary

}

#' @rdname set_labels
#'
#' @importFrom checkmate assert_class
#' @importFrom dplyr mutate
#' @importFrom purrr map map_lgl
#' @importFrom rlang abort set_names
#' @importFrom tibble enframe
#'
#' @export
set_category_labels <- function(dictionary, ..., .list = NULL){

  assert_valid_dotdot(..., .list = .list)
  .dots <- infer_dotdot(..., .list = .list)

  if(is_empty(.dots)) return(dictionary)

  assert_class(dictionary, "DataDictionary")

  input_frame <- enframe(.dots) %>%
    mutate(inputs = map(value, names))

  dictionary <- dd_set_prep(dictionary, .dots, field = 'category_label')

  for(i in names(.dots)){

    levels_to_modify <- names(.dots[[i]])

    current_cats <- dictionary$get_category_translater(i) %>%
      .[-which(names(.) %in% levels_to_modify)]

    if(any(.dots[[i]] %in% current_cats)){

      highlight <- names(current_cats[match(.dots[[i]], current_cats)])

      abort(
        message = c(
          glue("Invalid assignment of label \'{.dots[[i]]}\' to \\
               category \'{levels_to_modify}\' of variable `{i}`"),
          i = glue("This label is already assigned to category \\
                   \'{highlight}\' of variable `{i}`"),
          i = "category labels must be unique within variables"
        ),
        call = NULL
      )

    }
  }


  missing_level_names <- input_frame$value %>%
    map_lgl(.f = ~ "" %in% names(.x)) %>%
    which()

  if(!is_empty(missing_level_names)){

    problems <- .dots[missing_level_names] %>%
      paste_named_vec() %>%
      set_names(nm = "x")

    abort(
      message = c(
        "Inputs in `...` must be name-value pairs",
        "i" = "for `set_category_labels()`, values must also be named vectors",
        ">" = "problematic inputs are:",
        problems,
        "v" = "To fix: replace MISSING_NAME with an existing level in the given variable"
      ),
      call = NULL
    )

  }


  input_frame %<>% mutate(
    choices = map(
      name,
      ~dictionary$variables[[.x]]$get_category_levels()
    )
  )

  # check with a for-loop so we don't
  # trigger purrr-specific error message

  for(i in seq_along(input_frame$inputs)){

    assert_in_set(
      input_frame$inputs[[i]],
      input_frame$choices[[i]],
      value_type = "category levels",
      variable = input_frame$name[i]
    )

  }


  # if we got here, the loop below is safe

  for(i in names(.dots)){

    # create input lists for eventual call to modify_dictionary
    input_lvls <- set_names(list(names(.dots[[i]])), i)
    input_labs <- set_names(list(as.character(.dots[[i]])), i)

    current_lvls <- dictionary$variables[[i]]$get_category_levels()
    current_labs <- dictionary$variables[[i]]$fetch_category_labels()

    # I am taking this out b/c why have this feature when I already
    # have a function for setting category order. It just confuses
    # the user. That being said, keeping the commented code here
    # in case people end up asking for this feature.
    # # modify both the labels and the order of the levels
    # if(modify_order){
    #
    #   unused_lvls <- setdiff(current_lvls, input_lvls[[1]])
    #   unused_labs <- current_labs[current_lvls %in% unused_lvls]
    #
    #   input_lvls[[1]] %<>% append(unused_lvls)
    #   input_labs[[1]] %<>% append(unused_labs)
    #
    #   dictionary$modify_dictionary(input_lvls, field = 'category_levels')
    #   dictionary$modify_dictionary(input_labs, field = 'category_labels')
    #
    # } else {

    # put the inputs into the existing labels.
    input_key <- set_names(input_labs[[1]], input_lvls[[1]])

    # be careful not to mess up the established order
    updated_key <- set_names(current_labs, current_lvls)
    updated_key[names(input_key)] <- input_key

    input_labs[[1]] <- set_names(updated_key, NULL)

    dictionary$modify_dictionary(input_labs, field = 'category_labels')

    # }

  }

  dictionary

}


#' @rdname set_labels
#'
#' @importFrom checkmate assert_class
#' @importFrom dplyr mutate select
#' @importFrom purrr map map2
#' @importFrom rlang set_names
#' @importFrom tibble deframe enframe
#'
#' @export
set_category_order <- function(dictionary, ..., .list = NULL){


  assert_valid_dotdot(..., .list = .list)
  .dots <- infer_dotdot(..., .list = .list)

  assert_class(dictionary, "DataDictionary")

  if(is_empty(.dots)) return(dictionary)

  input_frame <- enframe(.dots) %>%
    mutate(
      inputs = map(value, names),
      choices = map(name, ~dictionary$get_category_levels(name = .x))
    )

  .check <- input_frame %>%
    mutate(value = map2(value, choices, union)) %>%
    select(name, value) %>%
    deframe()

  dictionary$check_modify_call(.check, field = "category_label")

  # check with a for-loop so we don't
  # trigger purrr-specific error message

  for(i in seq_along(input_frame$inputs)){

    assert_in_set(
      input_frame$inputs[[i]],
      input_frame$choices[[i]],
      value_type = "category levels",
      variable = input_frame$name[i]
    )

  }

  for(i in names(.dots)){

    # create input lists for eventual call to modify_dictionary
    inputs <- set_names(list(as.character(.dots[[i]])), i)

    current_lvls <- dictionary$variables[[i]]$get_category_levels()
    unused_lvls <- setdiff(current_lvls, inputs[[1]])

    inputs[[1]] %<>% append(unused_lvls)

    dictionary$modify_dictionary(inputs, field = 'category_levels')

    if(!is.null(dictionary$variables[[i]]$category_labels)){

      current_labs <- dictionary$variables[[i]]$get_category_labels() %>%
        set_names(current_lvls)

      input_labs <- inputs
      input_labs[[1]] <- as.character(current_labs[inputs[[1]]])
      dictionary$modify_dictionary(input_labs, field = 'category_labels')

    }

  }

  dictionary

}


#' Set Identifier Variables
#'
#' @param dictionary `r roxy_describe_dd()`
#' @param ... quoted or unquoted names of identifier variables
#'
#' @returns a modified `dictionary`
#'
#' @importFrom checkmate assert_class
#'
#' @export
#'
set_identifiers <- function(dictionary, ...){

  # TODO: this fails b/c ... can be bare
  # assert_valid_dotdot(..., .list = .list, names_required = FALSE)
  # .dots <- infer_dotdot(..., .list = .list)

  assert_class(dictionary, "DataDictionary")

  dictionary <- dictionary$clone(deep = dictionary$copy_on_modify)

  input_strings <- sapply(substitute(list(...)), deparse)[-1] %>%
    gsub("\"", "", .)

  dictionary$set_identifiers(input_strings)

  dictionary

}


#' Set the default dictionary used by `perinary` functions
#' @param dictionary `r roxy_describe_dd()`
#' @export
set_default_dictionary <- function(dictionary) {
  .perinary_internal$dictionary <- dictionary
  invisible(dictionary)
}





#' @title Set Label and Description Templates
#'
#' @description Add label or description templates to a `DataDictionary`.
#'
#' @param dictionary `r roxy_describe_dd()`
#' @param ... one or more formulas. See details.
#' @param show_duplicate_template_warnings logical; whether to display a warning
#'   if multiple templates will be assigned to the same row of
#'   `get_dictionary(dictionary)`.
#'
#' @returns a modified `dictionary`.
#'
#' @examples
#' library(perinary)
#'
#' dd <- as_data_dictionary(iris)
#' dd
#'
#' dd <- set_label_templates(
#'   dictionary = dd,
#'   # dimension will be either "length" or "width"
#'   contains("Sepal") ~ "Sepal {dimension}.",
#'   contains("Petal") ~ "Petal {dimension}."
#' )
#'
#' dd # the label column has changed
#'
#' dd <- set_description_templates(
#'   dictionary = dd,
#'   # dimension will be either "length" or "width"
#'   contains("Sepal") ~ "The {dimension} of the sepal, in centimeters.",
#'   contains("Petal") ~ "The {dimension} of the petal, in centimeters."
#' )
#'
#' dd # the description column has changed
#'
#' @name set_templates
NULL


#' @rdname set_templates
#' @export
set_label_templates <- function(dictionary,
                                ...,
                                show_duplicate_template_warnings = TRUE) {
  set_templates(
    dictionary,
    ...,
    field = "label",
    show_duplicate_template_warnings = show_duplicate_template_warnings
  )
}


#' @rdname set_templates
#' @export
set_description_templates <- function(dictionary,
                                      ...,
                                      show_duplicate_template_warnings = TRUE) {
  set_templates(
    dictionary,
    ...,
    field = "description",
    show_duplicate_template_warnings = show_duplicate_template_warnings
  )
}


#' @param field character; the field of the data dictionary that will receive
#'   the templates. Either "label" or "description".
#'
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom purrr map map_chr map_lgl
#' @importFrom rlang expr set_names warn
#' @importFrom tidyselect eval_select
#'
#' @noRd
set_templates <- function(dictionary,
                          ...,
                          field = c("label", "description"),
                          show_duplicate_template_warnings = TRUE) {
  field <- match.arg(field, choices = c("label", "description"))

  dots <- c(...)

  dot_classes <- map_chr(dots, class)
  invalid_idx <- which(dot_classes != "formula") # should be empty

  if (length(invalid_idx)) {
    out <- glue("{invalid_idx} ({dot_classes[invalid_idx]})")

    cli_abort(
      message = paste0(
        "Arguments passed to ... must be formulas. ",
        "Invalid arguments at position{?s} {.out {out}}." # pluralization
      )
    )
  }

  # List of strings giving the formula separator "~", the left-hand side of the
  # formula, and the right-hand side of the formula, in that order.
  formula_char <- map(eval(expr(dots)), as.character)

  # Left-hand side: a single variable name, vector of variable names, or
  # selection helpers like dplyr::starts_with()
  formula_left <- map_chr(formula_char, function(x) x[2L])

  # Right-hand side: the template to be assigned
  formula_right <- map_chr(formula_char, function(x) x[3L])

  # Vector of indices. Names are dictionary labels
  dd_names <- dictionary$get_names()
  dd_names <- set_names(seq_along(dd_names), dd_names)

  # Each element of loc is a named vector of row positions corresponding to
  # variables with templates that will be modified.
  tryCatch(
    expr = {
      loc <- map(formula_left, function(lhs_i) {
        eval_select(
          expr = str2lang(lhs_i),
          data = dd_names
        )
      })
    },
    error = function(e) {
      if (grepl("Column.*doesn't exist", e)) {
        cli_abort(
          message = paste0(
            "One or more variables do not exist in dictionary names. ",
            "To list all valid names, use `dictionary$get_names()`."
          ),
          call = NULL
        )
      } else {
        # Handle unknown error
        cli_abort(
          message = e
        )
      }
    }
  )

  # Named vector where names are variables and elements are templates
  var_labels <- set_names(
    x = rep.int(formula_right, lengths(loc)),
    nm = names(unlist(loc))
  )

  # Check if multiple templates will be assigned to a single variable. In those
  # cases, the last template will be used.
  is_duped <- duplicated(names(var_labels), fromLast = TRUE)
  duped_vars <- unique(names(var_labels)[is_duped])

  if (show_duplicate_template_warnings && length(duped_vars)) {
    warn(
      message = paste0(
        "Multiple templates specified for the following variables: ",
        paste(duped_vars, collapse = ", "),
        ". The last template specified for each variable will be used."
      )
    )
  }

  # Select the last occurrence of each variable
  var_labels <- var_labels[!is_duped]
  class(var_labels) <- "list" # named vector to named list

  # List containing TRUE if the template is valid. Otherwise, the error message
  # as a string
  err <- map(var_labels, function(template) {
    tryCatch(
      expr = {
        assert_valid_template(template)
      },
      error = function(e) {
        as.character(e)
      }
    )
  })

  # Identify variables where the template threw an error
  err_vars <- names(err)[map_lgl(err, is.character)]

  if (length(err_vars)) {
    # TODO limit the number of variables shown in the error message?
    cli_abort(
      message = paste0(
        "Invalid template specification for variable{?s} ",
        "{.err_vars {err_vars}}. Templates must contain {{}}, which surround ",
        "syntactically valid variable names. Please see the examples section ",
        "of {.topic perinary::set_templates} to learn more."
      )
    )
  }

  # Set the templates in the dictionary
  dictionary <- dd_set(
    dictionary = dictionary,
    .list = var_labels,
    field = field
  )

  return(dictionary)
}
