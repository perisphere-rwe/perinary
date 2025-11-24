

#' Use a template to document a variable
#'
#' @description This function should only be used inside of [set_labels()] or
#'   [set_descriptions()]. Calling `use_template()` in these functions will
#'   allow you to plug in values for an existing template, which can be
#'   set in a dictionary using `set_label_templates`
#'
#'
#' @param ... name-value pairs. The names must match with variables in the
#'   dictionary that have a template.
#'
#' @returns an environment
#'
#' @export
#'
use_template <- function(...){
  as.environment(list(...))
}

# idea for PR: Can this throw an error if it is not called within set_labels
# or set_description?
