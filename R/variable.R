#' @title Variable Constructors
#'
#' @description Constructors for variable metadata classes.
#'
#' @param name Character. The name of the variable.
#' @param label Character or `NULL`. A short label for the variable.
#' @param description Character or `NULL`. A longer description of the variable
#'   (optional).
#' @param category_levels Character vector or `NULL`. The set of unique category
#'   codes.
#' @param category_labels Character vector or `NULL`. Labels for the categories.
#'   For `nominal_variable`, the labels will default to `category_levels` if not
#'   provided. For `logical_variable`, this is a length 2 vector containing
#'   optional labels for the two levels: `FALSE` and `TRUE`, respectively.
#' @param units Character or `NULL`. Units for the variable (e.g., "kg",
#'   "years").
#' @param divby_modeling Numeric or `NULL`. A constant indicating what to divide
#'   the variable by before modeling. This improves interpretability of model
#'   coefficients.
#' @param date_format Character or `NULL`. Optional format string for parsing date values
#'   (e.g., "%Y-%m-%d").
#'
#' @details
#' \describe{
#'   \item{`DateVariable`}{Calendar or time-based data.}
#'   \item{`IdentifierVariable`}{ID variables, like subject IDs.}
#'   \item{`LogicalVariable`}{Indicator variables.}
#'   \item{`NominalVariable`}{Categorical data.}
#'   \item{`NumericVariable`}{Continuous or quantitative data.}
#' }
#'
#' @name variable_constructors
#'
#' @keywords internal
#'
#' @examples
#' # NominalVariable
#' sex <- nominal_variable(
#'   name = "sex",
#'   label = "Sex",
#'   description = "This is a longer description of the variable.",
#'   category_levels = c("M", "F"),
#'   category_labels = c("Male", "Female")
#' )
#' sex
#'
#' # NumericVariable
#' age <- numeric_variable(
#'   name = "age",
#'   label = "Age of participant",
#'   units = "years",
#'   # In a linear regression model, for example, the coefficient for age will
#'   # be the change in the mean of the response per 10 year increase in age.
#'   divby_modeling = 10
#' )
#' age
#'
#' # DateVariable
#' visit_date <- date_variable(
#'   name = "visit_date",
#'   label = "Visit Date",
#'   date_format = "%Y-%m-%d"
#' )
#' visit_date
#'
#' # IdentifierVariable
#' patient_id <- identifier_variable(
#'   name = "patient_id",
#'   label = "Patient Identifier"
#' )
#' patient_id
#'
#' # LogicalVariable
#' smoker <- logical_variable(
#'   name = "smoking_status",
#'   label = "Current smoker",
#'   # Labels correspond to FALSE and TRUE, respectively
#'   category_labels = c("Non-smoker", "Smoker")
#' )
#' smoker
#'
NULL


# NOTE: The order of the functions below determines the order that they appear
# in the Usage section and the order of the arguments.


#' @rdname variable_constructors
#' @export
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


#' @rdname variable_constructors
#' @export
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


#' @rdname variable_constructors
#' @export
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


#' @rdname variable_constructors
#' @export
identifier_variable <- function(name,
                                label = NULL,
                                description = NULL) {
  IdentifierVariable$new(
    name = name,
    label = label,
    description = description
  )
}


#' @rdname variable_constructors
#' @export
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
