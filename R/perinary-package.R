#' @keywords internal
#' @import R6
"_PACKAGE"

on_load(local_use_cli())


utils::globalVariables(
  names = c("name", "variable", "type", ".data", ".group_level", ":=")
)

## usethis namespace: start
#' @importFrom dplyr .data
#' @importFrom magrittr %<>% %$%
#' @importFrom purrr is_empty
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @import rlang
#' @import R6
## usethis namespace: end
NULL
