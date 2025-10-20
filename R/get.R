#' Get Unknown Fields in Dictionary
#'
#' @param dictionary `r roxy_describe_dd()`
#'
#' @param as_request logical. If `TRUE`, text is returned in a readable
#'   format that can be pasted into other settings, e.g., an e-mail
#'   requesting for the unknown information. If `FALSE` (the default),
#'   unknowns are returned as a `tibble`.
#'
#' @param as_code logical. If `TRUE`, text is returned as code that can
#'   be copy/pasted into an R script. If `FALSE` (the default),
#'   unknowns are returned as a `tibble`.
#'
#' @param show_optional logical. If `TRUE`, unknowns will include all
#'   possible fields of the variables in `dictionary`. If `FALSE` (the
#'   default), only labels and units are presented.
#'
#' @return If `as_request = FALSE` and `as_code = FALSE`,
#'   a [tibble][tibble::tibble-package] is returned.
#'   If `as_request = TRUE` or `as_code = TRUE`, text is returned.
#'
#' @export
#'
#' @examples
#'
#' # units isn't returned in the output because it is specified (known)
#' get_unknowns(data_dictionary(numeric_variable("a", units = 'years')))
#'
#' # label isn't returned in the output because it is specified (known)
#' get_unknowns(data_dictionary(nominal_variable("b", label = 'example')))
#' dd <- as_data_dictionary(iris) %>%
#'   set_labels(Sepal.Length = "Sepal length",
#'             Sepal.Width = "Sepal width",
#'             Species = "Flower species") %>%
#'   set_descriptions(Species = "The species are all subtypes of iris") %>%
#'   set_units(Sepal.Length = "cm")
#'
#' # as_request = TRUE returns this information in a shareable format
#' get_unknowns(dd, as_request = TRUE)
#'
#' # as_code = TRUE gives a head start on filling in the missing info
#' get_unknowns(dd, as_code = TRUE)


get_unknowns <- function(dictionary = NULL,
                         as_request = FALSE,
                         as_code = FALSE,
                         show_optional = FALSE){


  dictionary <- dictionary %||% .perinary_internal$.dictionary

  if(as_request && as_code){
    stop("Unknowns can be presented as request or code, but not both",
         call. = FALSE)
  }

  data_unknowns <- dictionary$dictionary %>%
    dplyr::mutate(type = purrr::map_chr(dictionary$variables, "type"), .after = 1) %>%
    dplyr::rename(variable = name) %>%
    dplyr::mutate(
      category_labels = dplyr::if_else(category_labels==category_levels,
                                       true = 'none',
                                       false = category_labels)
    ) %>%
    tidyr::pivot_longer(cols = -c(variable, type)) %>%
    dplyr::filter(
      dplyr::case_when(
        name %in% c("label", "description") ~ value == 'none',
        type == "Numeric" ~ value == "none" & name %in% c("units",
                                                          "divby_modeling"),
        type == "Nominal" ~ value == "none" & name %in% c("category_levels",
                                                          "category_labels")
      )
    ) %>%
    dplyr::mutate(name = factor(name,
                                levels = c("label", "category_labels",
                                           "units", "divby_modeling",
                                           "description"))) %>%
    dplyr::arrange(name)

  if(!show_optional){
    data_unknowns <- data_unknowns %>%
      dplyr::filter(name %in% c("label", "units", "category_labels")) %>%
      droplevels()
  }

  if(!as_request && !as_code) return(data_unknowns)

  if(as_request){

    out <- split(data_unknowns, data_unknowns$name) %>%
      purrr::map_chr(
        .f = ~ {

          .x_name <- switch(
            as.character(.x$name[1]),
            label = "A label to use for this variable in reports",
            description = paste(
              "Optional: additional relevant details",
              "(e.g., method of collection or measurement)"
            ),
            units = "Variable units (e.g., age in years)",
            divby_modeling = paste(
              "Units for model output",
              "(e.g., per 10 years of age)"
            ),
            category_levels = paste(
              "Category levels for this variable",
              "(levels are the values for this variable in data)"
            ),
            category_labels = paste(
              "Category labels for this variable",
              "(labels are shown in reports)"
            )
          )

          text_mid <- ' = ?'

          if(.x$name[1] == 'category_labels'){

            text_mid <- purrr::map_chr(
              .x = .x$variable,
              .f = function(..x){
                paste(dictionary$variables[[..x]]$category_levels,
                      '= ?',
                      collapse = ";  ")
              }
            ) %>%
              paste0(": ", .)

          }

          paste0(.x_name, ":", "\n\n",
                 paste0("  - ", .x$variable, text_mid, collapse = '\n'))

        }
      ) %>%
      paste(collapse = "\n\n")

  }

  if(as_code){

    make_ws <- function(n){
      paste(rep(" ", times = n), collapse = "")
    }

    out <- split(data_unknowns, data_unknowns$name) %>%
      purrr::map_chr(
        .f = ~ {

          .fun <- paste(
            switch(
              as.character(.x$name[1]),
              label = "set_labels",
              description = "set_descriptions",
              units = "set_units",
              divby_modeling = "set_divby_modeling",
              category_labels = "set_category_labels"
            )
          )

          .ws <- make_ws(nchar(.fun)+1)

          .collapse <- paste0(",\n", .ws)

          if(.x$name[1] == "divby_modeling"){
            arg_placeholder <- " = 1"
          } else {
            arg_placeholder <- " = \"\""
          }

          .args <- .x$variable %>%
            paste(arg_placeholder) %>%
            paste(collapse = .collapse)


          if(.x$name[1] == 'category_labels'){

            .args <- purrr::map_chr(
              .x = .x$variable,
              .f = function(..x){

                ..ws <- make_ws(nchar(.ws) + nchar(..x) + 4)

                ..collapse <- paste(",\n", ..ws)

                ..args <- paste(dictionary$variables[[..x]]$category_levels,
                                "= \"\"",
                                collapse = ..collapse)


                glue::glue("{..x} = c({..args})")
              }
            ) %>%
              paste(collapse = .collapse)

          }

          glue::glue("{.fun}({.args})")

        }
      ) %>%
      paste(collapse = " %>% \n")
  }

  cat(out, "\n")

}

#' Get Term Key for Nominal Variables
#'
#' Returns a tibble linking each level or label of a nominal variable
#' to a modeling term, using a specified separator between the variable
#' name and category value. This is especially useful for joining
#' dictionary information to model output, such as coefficients or terms
#' in regression tables.
#'
#' @param dictionary `r roxy_describe_dd()`
#' @param adjust_to Optional. A tibble that includes a column with term names
#'   (default: `'term'`). When supplied, the output is filtered to retain only
#'   terms found in `adjust_to` and that match the reference level.
#' @param term_separator A string used to separate the variable name and
#'   category value in the output term column. Default is `""`.
#' @param term_colname A character string giving the column name to use
#'   for the terms column in the returned tibble. Default is `"term"`.
#'
#' @return A tibble with one row per variable-category combination and
#'   columns for the variable, category type (`levels` or `labels`),
#'   and term. If `adjust_to` is provided, the tibble is filtered
#'   accordingly and includes the `category` column.
#'
#' @export
#'
#' @examples
#' dd <- as_data_dictionary(iris)
#' get_term_key(dd)
#'
get_term_key <- function(dictionary,
                         adjust_to = NULL,
                         term_separator = "",
                         term_colname = 'term'){

  out <- dictionary$category_key %>%
    dplyr::mutate(
      term_levels = paste(name, level, sep = term_separator),
      term_labels = dplyr::if_else(
        label == level,
        true = NA_character_,
        false = paste(name, label, sep = term_separator)
      )
    ) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with('term_'),
                        values_to = 'term',
                        names_to = 'category_type',
                        names_prefix = 'term_') %>%
    tidyr::drop_na(term) %>%
    dplyr::rename_with(.fn = ~ term_colname, .cols = term)

  if(is.null(adjust_to)) return(out)

  # it isn't quite as simple as a right join because we want
  # to include reference categories whenever a term is part
  # of the variable that has said reference category. So,
  # we need to first keep only variables that are in the terms,
  # then keep either the levels or the labels from term key,
  # depending on which one is actually in the terms.

  overlap <- dplyr::right_join(out,
                               adjust_to,
                               by = term_colname)

  out %>%
    dplyr::filter(name %in% overlap$name) %>%
    # drop the labels or levels, depending on which is in terms
    dplyr::filter(any(term %in% adjust_to[[term_colname]]),
                  .by = c(name, category_type)) %>%
    dplyr::select(-category_type) %>%
    # drop any levels that aren't in terms and aren't references
    dplyr::filter(reference | term %in% adjust_to[[term_colname]])

}

#' Extract Dictionary Meta Data as a Tibble
#'
#' Returns a tibble of the dictionary contents, optionally formatting
#' missing values and nominal variable categories. This can be useful
#' for custom workflows that require dictionary information in tabular form.
#'
#' @param dictionary `r roxy_describe_dd()`
#' @param format_missing Logical. If `TRUE`, missing metadata fields (like
#'   labels or units) are returned as stored in the dictionary. If `FALSE`
#'   (default), missing values are replaced with `NA`.
#' @param format_categories Logical. If `TRUE`, nominal category information
#'   is returned exactly as stored in the dictionary. If `FALSE` (default),
#'   category levels and labels are returned as list-columns extracted from
#'   the variable objects.
#' @param as_code Logical. If `TRUE`, text is returned as code that can be
#'   copy/pasted into an R script to construct `dictionary`. If `FALSE` (the
#'   default), unknowns are returned as a `tibble`.
#'
#' @return A tibble with one row per variable (`as_code = FALSE`), containing
#'   meta data fields such as `label`, `description`, `units`, `divby_modeling`,
#'   `category_levels`, and `category_labels`. If `as_code = TRUE`, a string
#'   containing code to construct the original dictionary.
#'
#' @author Byron Jaeger, Tyler Sagendorf
#'
#' @export get_dictionary
#'
#' @importFrom dplyr select left_join mutate across pull case_when
#' @importFrom rlang abort
#'
#' @examples
#' dd <- as_data_dictionary(iris)
#' get_dictionary(dd)
#'
#' # Code to recreate dd
#' dd_code <- get_dictionary(dd, as_code = TRUE)
#' dd_code
#'
#' # Create data dictionary from code string
#' dd_new <- eval(parse(text = dd_code))
#' all.equal(dd, dd_new) # TRUE
#'
get_dictionary <- function(dictionary,
                           format_missing = FALSE,
                           format_categories = FALSE,
                           as_code = FALSE) {
  if (!as_code) {
    out <- .get_dictionary(
      dictionary = dictionary,
      format_missing = format_missing,
      format_categories = format_categories
    )

    return(out)
  }

  # Extract list columns
  temp <- .get_dictionary(dictionary) %>%
    select(name, category_levels, category_labels)

  dd <- dictionary$dictionary %>%
    # Replace category_levels/_labels by the list columns from temp
    select(-c(category_levels, category_labels)) %>%
    left_join(temp, by = "name") %>%
    mutate(
      # Create category_levels_char and category_labels_char columns
      across(
        .cols = c(category_levels, category_labels),
        .fns = ~ vapply(.x, function(vec_i) {
          # Format levels and labels as "c('A','B','C')"
          sprintf("c(%s)", paste(sQuote(vec_i, FALSE), collapse = ","))
        }, character(1L)),
        .names = "{.col}_char"
      )
    )

  # String containing code needed to reconstruct the dictionary
  out <- dd %>%
    mutate(
      across(
        .cols = c(description, label, units),
        .fns = ~ ifelse(.x == "none", "NULL", sQuote(.x, FALSE))
      ),
      name = sQuote(name, FALSE),
      # Get date formats from original dictionary
      date_format = vapply(dictionary$variables, function(var_i) {
        var_i[["date_format"]] %||% "NULL"
      }, character(1L)),
      # Code to build each row of the dictionary. These arguments are used by
      # all *_variable functions.
      code = sprintf(
        "name=%s,label=%s,description=%s",
        name, label, description
      ),
      # Arguments specific to the different *_variable functions
      code = case_when(
        type == "Nominal" ~ sprintf(
          "nominal_variable(%s,category_levels=%s,category_labels=%s)",
          code, category_levels_char, category_labels_char
        ),
        type == "Numeric" ~ sprintf(
          "numeric_variable(%s,units=%s,divby_modeling=%s)",
          code, units, divby_modeling
        ),
        type == "Logical" ~ sprintf(
          "logical_variable(%s,category_labels=%s)",
          code, category_labels_char
        ),
        type == "Date" ~ sprintf(
          "date_variable(%s,date_format=%s)",
          code, ""
        ),
        type == "Identifier" ~ sprintf(
          "identifier_variable(%s)",
          code
        ),
        TRUE ~ "STOP"
      ),
      code = ifelse(
        code == "STOP",
        # Catch other variable types. This will be triggered once by
        # eval(parse(text = code))
        sprintf(
          "abort(message = 'Invalid variable type(s): %s')",
          paste(unique(type), collapse = ", ")
        ),
        code
      )
    ) %>%
    pull(code) %>%
    paste(collapse = ",")

  out <- sprintf("data_dictionary(.list = list(%s))", out)

  # Verify that the dictionary that will be generated is the same as the input
  dictionary_new <- eval(parse(text = out))

  # Do not use identical, since ".__enclos_env__" will be different
  if (isFALSE(all.equal(dictionary, dictionary_new))) {
    # If the dictionary is different, it indicates an error in the code;
    # nothing the user can do besides tell us to fix it. This is the last line
    # of defense if the unit tests do not catch any issues.
    abort(
      message = paste0(
        "The input dictionary does not match the dictionary generated by ",
        "evaluating the output string. Please open a new issue: ",
        "https://github.com/perisphere-rwe/perinary/issues/new"
      )
    )
    # TODO return more specific information about the differences (e.g.,
    # missing columns)?
  }

  return(out)
}


.get_dictionary <- function(dictionary,
                            format_missing = FALSE,
                            format_categories = FALSE) {
  if(format_missing && format_categories) return(dictionary$dictionary)

  vars <- dictionary$variables

  tbl_names <- tibble::tibble(
    name = purrr::map_chr(vars, ~ .x$get_name()),
  )

  tbl_left <- dplyr::select(dictionary$dictionary,
                            label,
                            description,
                            units,
                            divby_modeling)

  if(!format_missing){

    tbl_left <- tibble::tibble(
      label           = purrr::map_chr(vars, ~ .x$get_label() %||% NA_character_),
      description     = purrr::map_chr(vars, ~ .x$get_description() %||% NA_character_),
      units           = purrr::map_chr(vars, ~ .x$get_units() %||% NA_character_),
      divby_modeling  = purrr::map_dbl(vars, ~ .x$get_divby_modeling() %||% NA_real_)
    )

  }

  tbl_right <- dplyr::select(dictionary$dictionary,
                             category_levels,
                             category_labels)

  if(!format_categories){

    tbl_right <- tibble::tibble(
      category_levels = purrr::map(vars, ~ .x$get_category_levels()),
      category_labels = purrr::map(vars, ~ .x$get_category_labels())
    )

  }

  dplyr::bind_cols(tbl_names, tbl_left, tbl_right)

}
