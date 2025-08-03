
#' Create Data Dictionary from Variable Definitions
#'
#' Initializes a `DataDictionary` object from a set of variables.
#' Variables should be made using [numeric_variable()] or [nominal_variable()]
#' and passed directly as arguments.
#'
#' @param ... One or more objects inheriting from `r roxy_describe_numeric()`
#'   or `r roxy_describe_nominal()`.
#'
#' @param .list A list of data variables. This argument allows
#'  `data_dictionary` to be used programmatically and is optional. It is
#'  intended to be used as an alternative to `...`.
#'
#' @return A `DataDictionary` object containing a tibble summary of all
#'   variables.
#'
#' @details if both `...` and `.list` are specified, the dictionary
#'   will only be created using `.list`.
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
data_dictionary <- function(..., .list = NULL){

  assert_valid_dotdot(..., .list = .list)
  .dots <- infer_dotdot(..., .list = .list)
  DataDictionary$new(.dots)

}


#' Create Data Dictionary from Existing Data
#'
#' @param x a data frame
#'
#' @return `r roxy_describe_dd()`
#'
#' @export
#'
#' @examples
#'
#' attr(iris$Species, 'label') <- "Flower species"
#'
#' as_data_dictionary(iris)
#'
as_data_dictionary <- function(x){

  stopifnot(is.data.frame(x))

  vars <- purrr::map2(
    .x = x,
    .y = names(x),
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

  DataDictionary$new(vars[keep])

}

