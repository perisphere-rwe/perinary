

#' Infuse data with a dictionary
#'
#' Transfer variable and category labels into a dataframe. This
#'   is useful if you are passing a data frame to a tabulation or plotting
#'   function that naturally incorporates labeled data.
#'
#' @param data a data frame
#'
#' @param dictionary `r roxy_describe_dd()`
#'
#' @param units a character value indicating how units should be incorporated
#'   into variable labels. Valid choices are
#'   - `"none"` : do not include units in labels, e.g., "Age"
#'   - `"descriptive"` : include labels, e.g., "Age, *years*"
#'   - `"model"` : include label and model divisor, e.g., "Age, *per 10 years*"
#'
#' @param divby_suffix a character value indicating what term will be appended
#'   to names of numeric variables when they are transformed by dividing them
#'   by the value given for `divby` in their dictionary information. Note that
#'   this is only relevant when `units = "model"`. For example, if our data
#'   contain a column called `foo` with `divby = 5` and `divby_suffix = "divby"`,
#'   then the output would contain a new column called `foo_divby_5`, and the
#'   values in this column would be equal to values of `foo` divided by 5.
#'   If you want to live dangerously (i.e., modify the columns and don't make
#'   new ones), use `divby_suffix = NULL` to overwrite existing columns
#'   with their modified versions.
#'
#' @param warn_undocumented a logical value. If `TRUE` (default), then
#'   a warning is thrown whenever 1 or more variables in `data` do not
#'   have supporting documentation in `dictionary`. If `FALSE`, then
#'   this information will not be presented in a warning.
#'
#' @details
#' Additional details...
#'
#'
#' @export
#'
#' @examples
#' age_years <- numeric_variable(
#'   name = "age_years",
#'   label = "Age of participant",
#'   units = "years",
#'   divby_modeling = 10
#' )
#'
#' gender <- nominal_variable(
#'   name = "gender",
#'   label = "Gender of participant",
#'   category_levels = c("M", "F"),
#'   category_labels = c("Male", "Female")
#' )
#'
#' dd <- data_dictionary(age_years, gender)
#' data <- data.frame(age_years = 55, gender = "M")
#'
#' data %>%
#'   infuse_dictionary(dd, units = "model") %>%
#'   purrr::map(~attr(.x, 'label'))

infuse_dictionary <- function(data,
                              dictionary = NULL,
                              units = "none",
                              divby_suffix = "divby",
                              warn_undocumented = TRUE){

  checkmate::assert_character(units, null.ok = FALSE, len = 1)
  checkmate::assert_choice(units, choices = c("none", "descriptive", "model"))
  checkmate::assert_logical(warn_undocumented)
  checkmate::assert_data_frame(data)

  dictionary <- dictionary %||% attr(data, 'perinary._._.dictionary')

  if(is.null(dictionary)){
    stop("Data dictionary was not supplied and was not found",
         "in data object", call. = FALSE)
  }

  checkmate::assert_class(dictionary, classes = "DataDictionary")

  overlapping_variables <- names(data) %>%
    intersect(dictionary$get_variable_names())

  undocumented_variables <- names(data) %>%
    setdiff(overlapping_variables)

  if(!is_empty(undocumented_variables) && warn_undocumented){

    msg <- paste0(
      "dictionary does not contain information for some variables in ",
      "data: ", paste(undocumented_variables, collapse = ', '),
      ". To suppress this warning, set `warn_undocumented` to `FALSE`"
    )

    warning(msg, call. = FALSE)

  }

  for(i in overlapping_variables){

    variable_type <- dictionary$variables[[i]]$type

    if(!variable_type %in% c("Nominal", "Numeric")){

      attr(data[[i]], 'label') <- dictionary$variables[[i]]$get_label()

      next

    }

    if(variable_type == "Nominal"){

      .levels <- dictionary$variables[[i]]$category_levels
      .labels <- dictionary$variables[[i]]$category_labels

      data[[i]] <- factor(data[[i]],
                          levels = .levels,
                          labels = .labels %||% .levels)

      attr(data[[i]], 'label') <- dictionary$variables[[i]]$get_label()

      next

    }

    if(variable_type == "Numeric"){

      if(units == 'model'){

        divby_name <- i
        divby_value <- dictionary$variables[[i]]$divby_modeling %||% 1

        if(!is.null(divby_suffix)){

          attr(data[[i]], 'label') <- dictionary$variables[[i]]$get_label()

          divby_name <- paste(i, divby_suffix, divby_value, sep = "_")

        }

        data[[divby_name]] <- data[[i]] / divby_value

        data <- data %>%
          dplyr::relocate(all_of(divby_name), .after = all_of(i))

        attr(data[[divby_name]], 'label') <-
          dictionary$variables[[i]]$get_label_divby()

      } else {

        attr(data[[i]], 'label') <- switch(
          units,
          'none'        = dictionary$variables[[i]]$get_label(),
          'descriptive' = dictionary$variables[[i]]$get_label_and_unit()
        )

      }

    }

  }

  data

}


