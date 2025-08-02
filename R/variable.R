#' @title Variable Constructors
#' @description
#' Constructors for variable metadata classes (`NumericVariable`,
#' `NominalVariable`, `DateVariable`, `IdentifierVariable`).
#'
#' @param name Character. The name of the variable (required).
#' @param label Character. A short label for the variable (optional).
#' @param description Character. A longer description of the variable
#'   (optional).
#'
#' @name variable_constructors
#' @keywords internal
NULL

#' Create a Numeric Variable
#'
#' Initializes a `NumericVariable` object, typically used to represent
#' continuous or quantitative data.
#'
#' @inheritParams variable_constructors
#' @param units Character. Units for the variable (e.g., "kg", "years")
#'   (optional).
#' @param divby_modeling Numeric. A constant indicating what to divide the
#'   variable by before modeling. This improves interpretability of model
#'   coefficients (optional).
#'
#' @return A `NumericVariable` object.
#' @export
#' @family variable_constructors
#'
#' @examples
#' age <- numeric_variable(
#'   name = "age",
#'   label = "Age of participant",
#'   units = "years",
#'   divby_modeling = 10
#' )
#' age
numeric_variable <- function(name,
                             label = NULL,
                             description = NULL,
                             units = NULL,
                             divby_modeling = NULL) {
  NumericVariable$new(
    name = name,
    label = label,
    description = description,
    units = units,
    divby_modeling = divby_modeling
  )
}

#' Create a Nominal (Categorical) Variable
#'
#' Initializes a `NominalVariable` object, typically used to represent
#' categorical data.
#'
#' @inheritParams variable_constructors
#' @param category_levels Character vector. The set of unique category codes.
#' @param category_labels Character vector. Human-readable labels for the
#'   categories. If not provided, defaults to `category_levels`.
#'
#' @return A `NominalVariable` object.
#' @export
#' @family variable_constructors
#'
#' @examples
#' gender <- nominal_variable(
#'   name = "gender",
#'   label = "Gender",
#'   category_levels = c("M", "F"),
#'   category_labels = c("Male", "Female")
#' )
#' gender
nominal_variable <- function(name,
                             label = NULL,
                             description = NULL,
                             category_levels = NULL,
                             category_labels = NULL) {
  NominalVariable$new(
    name = name,
    label = label,
    description = description,
    category_levels = category_levels,
    category_labels = category_labels
  )
}

#' Create a Date Variable
#'
#' Initializes a `DateVariable` object, used to represent calendar or
#' time-based data.
#'
#' @inheritParams variable_constructors
#' @param date_format Character. Optional format string for parsing date
#'   values (e.g., "%Y-%m-%d").
#'
#' @return A `DateVariable` object.
#' @export
#' @family variable_constructors
#'
#' @examples
#' visit_date <- date_variable(
#'   name = "visit_date",
#'   label = "Visit Date",
#'   date_format = "%Y-%m-%d"
#' )
#' visit_date
date_variable <- function(name,
                          label = NULL,
                          description = NULL,
                          date_format = NULL) {
  DateVariable$new(
    name = name,
    label = label,
    description = description,
    date_format = date_format
  )
}

#' Create an Identifier Variable
#'
#' Initializes an `IdentifierVariable` object, typically used for ID variables
#' like subject IDs.
#'
#' @inheritParams variable_constructors
#'
#' @return An `IdentifierVariable` object.
#' @export
#' @family variable_constructors
#'
#' @examples
#' patient_id <- identifier_variable(
#'   name = "patient_id",
#'   label = "Patient Identifier"
#' )
#' patient_id
identifier_variable <- function(name,
                                label = NULL,
                                description = NULL) {
  IdentifierVariable$new(
    name = name,
    label = label,
    description = description
  )
}

#' Create a Logical (Binary) Variable
#'
#' Initializes a `LogicalVariable` object, typically used for binary indicators
#' or flags that take values `TRUE` or `FALSE`.
#'
#' @inheritParams variable_constructors
#' @param category_labels Character vector of length 2. Optional labels for the
#'   two levels: `FALSE` and `TRUE`, respectively. Defaults to `c("FALSE", "TRUE")`.
#'
#' @return A `LogicalVariable` object.
#' @export
#' @family variable_constructors
#'
#' @examples
#' smoker <- logical_variable(
#'   name = "smoking_status",
#'   label = "Current smoker",
#'   category_labels = c("Non-smoker", "Smoker")
#' )
#' smoker
logical_variable <- function(name,
                             label = NULL,
                             description = NULL,
                             category_labels = c("FALSE", "TRUE")) {
  LogicalVariable$new(
    name = name,
    label = label,
    description = description,
    category_labels = category_labels
  )
}
