
#' Create Data Dictionary from Variable Definitions
#'
#' Initializes a `DataDictionary` object from a set of variables.
#' Variables should be made using [numeric_variable()] or [nominal_variable()]
#' and passed directly as arguments.
#'
#' @param ... One or more objects inheriting from `r roxy_describe_numeric()`
#'   or `r roxy_describe_nominal()`.
#'
#' @param .list A list of data variables. `r roxy_dotlist("data_dictionary")`
#'
#' @param copy_on_modify a logical value indicating whether `set` functions
#'   should modify the dictionary in place or copy it then modify. The
#'   default is `TRUE` because dictionaries are almost always small
#'   and trivial to copy.
#'
#' @return `r roxy_describe_dd()`
#'
#' @details Only one of  `...` and `.list` should be specified.
#'
#' @export
#'
#' @examples
#'
#' age_years <- numeric_variable(
#'   name = "age",
#'   label = "Age of participant",
#'   units = "years"
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
#' print(dd)
#'
#' dd <- data_dictionary(.list = list(age_years, gender))
#' print(dd)
#'
data_dictionary <- function(..., .list = NULL, copy_on_modify = TRUE){

  assert_valid_dotdot(..., .list = .list, names_required = FALSE)
  .dots <- infer_dotdot(..., .list = .list)
  DataDictionary$new(.dots, copy_on_modify = copy_on_modify)

}


#' Create Data Dictionary from Existing Data
#'
#' @param x a data frame
#'
#' @inheritParams data_dictionary
#'
#' @return `r roxy_describe_dd()`
#'
#' @details
#'  variable labels stored in the 'label' attribute are incorporated in the
#'  resulting dictionary.
#'
#' @export
#'
#' @examples
#'
#'
#' attr(iris$Species, 'label') <- "Flower species"
#'
#' as_data_dictionary(iris)
#'
as_data_dictionary <- function(x, copy_on_modify = TRUE){

  checkmate::assert_data_frame(x)

  vars <- purrr::imap(
    .x = x,
    .f = ~ {

      if(inherits(.x, c('factor', 'character'))){
        return(
          NominalVariable$new(
            name = .y,
            label = attr(.x, 'label'),
            category_levels = levels(.x) %||% unique(stats::na.omit(.x))
          )
        )
      }

      if(inherits(.x, c('logical'))){
        return(
          LogicalVariable$new(
            name = .y,
            label = attr(.x, 'label')
          )
        )
      }

      if(inherits(.x, c('numeric', 'integer'))){
        return(
          NumericVariable$new(
            name = .y,
            label = attr(.x, 'label')
          )
        )
      }

      if(inherits(.x, c('POSIXt', 'Date'))){
        return(
          DateVariable$new(
            name = .y,
            label = attr(.x, 'label'),
            date_format = attr(.x, 'date_format')
          )
        )
      }

      NULL

    }

  )

  keep <- which(purrr::map_lgl(vars, ~!is.null(.x)))

  leftovers <- setdiff(vars, vars[keep])

  if(!is_empty(leftovers)){
    rlang::warn(
      message = c(
        "Could not map the following variables to a specific type:",
        i = paste_collapse(leftovers)
      )
    )
  }

  DataDictionary$new(vars[keep])

}

