



#' Modify Data Dictionary Elements
#'
#' Set metadata in a `DataDictionary` object.
#'
#' @param dictionary `r roxy_describe_dd()`
#' @param ... Name-value pairs. The name gives the name of the variable
#'   that will be modified. The value must be a:
#'
#'   - character value for `set_label()`, `set_units()`,
#'     and `set_descriptions()`
#'   - numeric value for `set_divby_modeling()`
#'   - character vector for `set_category_levels()` and `set_category_labels()`
#'
#' See examples for examples of each.
#'
#' @return a modified `dictionary`
#'
#' @export
#'
#' @examples
#'
#' dd <- as_data_dictionary(data.frame(a = 1, b = "cat")) %>%
#'  set_label(a = "numeric example", b = "categorical example") %>%
#'  set_unit(a = "years") %>%
#'  set_divby_modeling(a = 10) %>%
#'  set_description(a = "A variable used for examples") %>%
#'  set_category_label(b = c("cat" = "A small lion"))
#'
#' dd
#'

set_label <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$check_modify_call(list(...), field = 'label')
  dictionary$modify_dictionary(list(...), field = 'label')
  dictionary
}

#' @rdname set_label
#' @export
set_description <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$check_modify_call(list(...), field = 'description')
  dictionary$modify_dictionary(list(...), field = 'description')
  dictionary
}

#' @rdname set_label
#' @export
set_unit <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$check_modify_call(list(...), field = 'units')
  dictionary$modify_dictionary(list(...), field = 'units')
  dictionary
}

#' @rdname set_label
#' @export
set_divby_modeling <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$check_modify_call(list(...), field = 'divby_modeling')
  dictionary$modify_dictionary(list(...), field = 'divby_modeling')
  dictionary
}

#' @rdname set_label
#' @export
set_category_label <- function(dictionary, ..., modify_order = FALSE){

  checkmate::assert_class(dictionary, "DataDictionary")

  .dots <- list(...)

  if(is_empty(.dots)) return(dictionary)

  input_frame <- tibble::enframe(.dots) %>%
    dplyr::mutate(inputs = purrr::map(value, names))

  missing_variable_names <- which(input_frame$name == "")

  if(!is_empty(missing_variable_names)){

    problems <- .dots[missing_variable_names] %>%
      .paste_named_vec() %>%
      purrr::set_names(nm = "x")

    rlang::abort(
      message = c(
        "*" = "Inputs in `...` must be name-value pairs",
        "*" = "problematic inputs are:",
        problems,
        "v" = "replace MISSING_NAME to fix"
      ),
      call = NULL
    )
  }

  dictionary$check_modify_call(.dots, field = "category_label")

  missing_level_names <- input_frame$value %>%
    purrr::map_lgl(.f = ~ "" %in% names(.x)) %>%
    which()

  if(!is_empty(missing_level_names)){

    problems <- .dots[missing_level_names] %>%
      .paste_named_vec() %>%
      purrr::set_names(nm = "x")

    rlang::abort(
      message = c(
        "*" = "Inputs in `...` must be name-value pairs",
        "*" = "values must also be named vectors",
        "*" = "problematic inputs are:",
        problems,
        "v" = "replace MISSING_NAME to fix"
      ),
      call = NULL
    )

  }

  input_frame %<>% dplyr::mutate(
    choices = purrr::map(
      name,
      ~dictionary$variables[[.x]]$get_category_level()
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

  for(i in names(.dots)){

    # create input lists for eventual call to modify_dictionary
    input_lvls <- purrr::set_names(list(names(.dots[[i]])), i)
    input_labs <- purrr::set_names(list(as.character(.dots[[i]])), i)

    current_lvls <- dictionary$variables[[i]]$get_category_levels()
    current_labs <- dictionary$variables[[i]]$fetch_category_labels()

    # modify both the labels and the order of the levels
    if(modify_order){

      unused_lvls <- setdiff(current_lvls, input_lvls[[1]])
      unused_labs <- current_labs[current_lvls %in% unused_lvls]

      input_lvls[[1]] %<>% append(unused_lvls)
      input_labs[[1]] %<>% append(unused_labs)

      dictionary$modify_dictionary(input_lvls, field = 'category_levels')
      dictionary$modify_dictionary(input_labs, field = 'category_labels')

    } else {

      # put the inputs into the existing labels.
      input_key <- purrr::set_names(input_labs[[1]], input_lvls[[1]])

      # be careful not to mess up the established order
      updated_key <- purrr::set_names(current_labs, current_lvls)
      updated_key[names(input_key)] <- input_key

      input_labs[[1]] <- purrr::set_names(updated_key, NULL)

      dictionary$modify_dictionary(input_labs, field = 'category_labels')

    }

  }

  dictionary

}


#' @rdname set_label
#' @export
set_category_order <- function(dictionary, ...){

  checkmate::assert_class(dictionary, "DataDictionary")

  .dots <- list(...)

  if(is_empty(.dots)) return(dictionary)

  missing_names <- which(names(.dots) == "")

  if(!is_empty(missing_names)){
    rlang::abort(
      message = c(
        "Inputs to `...` in `set_category_label()` must be named vectors",
        "i" = "Changing category label from \"old\" to \"new\" for a variable named `x`:",
        "v" = " correct input format: `x = c(old = \"new\")`",
        "x" = " incorrect input format: `x = c(\"new\")`",
        "i" = "Did you mean to use `set_category_order`?"
      ),
      call = NULL
    )
  }

  dictionary$check_modify_call(.dots, field = "category_label")

  input_frame <- tibble::enframe(.dots) %>%
    dplyr::mutate(
      inputs = purrr::map(value, names),
      choices = purrr::map(
        name,
        ~dictionary$variables[[.x]] %$%
          get_category_levels()
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

  for(i in names(.dots)){

    # expects input of the form: name = c(level = label).
    checkmate::assert_character(i, .var.name = i, null.ok = FALSE)

    # Here, i is the name, so it should match an existing name in dictionary
    checkmate::assert_choice(i,
                             .var.name = i,
                             choices = dictionary$get_variable_names())

    # factors/characters only
    if(dictionary$variables[[i]]$type == "Numeric"){
      stop("only nominal variables can have factor levels modified:",
           " '", i, "' is a numeric variable", call. = FALSE)
    }

    if(!is.null(names(.dots[[i]]))){
      stop("Vector inputs to `set_category_order` must not be named.\n",
           " - x = c(\"new_first\", \"new_second\") works\n",
           " - x = c(a = \"new_first\", b = \"new_second\") does not work.\n",
           "See ?set_factors to re-order levels and modify labels simultaneously",
           call. = FALSE)
    }

    # create input lists for eventual call to modify_dictionary
    inputs <- purrr::set_names(list(as.character(.dots[[i]])), i)

    current_lvls <- dictionary$variables[[i]]$get_category_levels()

    # user inputs must be character valued and match existing
    # levels in the dictionary for this variable
    for(j in inputs[[1]]){
      checkmate::assert_character(j, .var.name = j, null.ok = FALSE)
      checkmate::assert_choice(j, .var.name = j, choices = current_lvls)
    }

    unused_lvls <- setdiff(current_lvls, inputs[[1]])

    inputs[[1]] %<>% append(unused_lvls)

    dictionary$modify_dictionary(inputs, field = 'category_levels')

    if(!is.null(dictionary$variables[[i]]$category_labels)){

      current_labs <- dictionary$variables[[i]]$get_category_labels() %>%
        purrr::set_names(current_lvls)

      input_labs <- inputs
      input_labs[[1]] <- as.character(current_labs[inputs[[1]]])
      dictionary$modify_dictionary(input_labs, field = 'category_labels')

    }

  }

  for(i in names(.dots)){

    # create input lists for eventual call to modify_dictionary
    inputs <- purrr::set_names(list(as.character(.dots[[i]])), i)

    current_lvls <- dictionary$variables[[i]]$get_category_levels()

    unused_lvls <- setdiff(current_lvls, inputs[[1]])

    inputs[[1]] %<>% append(unused_lvls)

    dictionary$modify_dictionary(inputs, field = 'category_levels')

    if(!is.null(dictionary$variables[[i]]$category_labels)){

      current_labs <- dictionary$variables[[i]] %$%
        get_category_labels() %>%
        purrr::set_names(current_lvls)

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
#' @export
#'
set_identifier <- function(dictionary, ...){

  input_strings <- sapply(substitute(list(...)), deparse)[-1] %>%
    stringr::str_remove_all("\"")

  for(i in input_strings){

    dictionary$variables[[i]] <-
      identifier_variable(name = i,
                          label = dictionary$variables[[i]]$label,
                          description = dictionary$variables[[i]]$description)

  }

  dictionary$create_dictionary(dictionary$variables)

  dictionary

}


















