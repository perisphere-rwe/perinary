



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
  .dots <- infer_dotdot(..., .list = .list)
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












