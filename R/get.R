#' Get Unknown Fields in Dictionary
#'
#' @param x `r roxy_describe_dd()`
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
#' # as_request = TRUE returns this information in a shareable format
#' as_data_dictionary(iris) %>%
#'   set_labels(Sepal.Length = "Sepal length",
#'             Sepal.Width = "Sepal width",
#'             Species = "Flower species") %>%
#'   set_descriptions(Species = "The species are all subtypes of iris") %>%
#'   set_units(Sepal.Length = "cm") %>%
#'   get_unknowns(as_request = TRUE)

get_unknowns <- function(x,
                         as_request = FALSE,
                         as_code = FALSE,
                         show_optional = FALSE){

  if(!is_data_dictionary(x)) {
    x <- as_data_dictionary(x)
  }

  if(as_request && as_code){
    stop("Unknowns can be presented as request or code, but not both",
         call. = FALSE)
  }

  data_unknowns <- x$dictionary %>%
    dplyr::mutate(type = purrr::map_chr(x$variables, "type"), .after = 1) %>%
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
                paste(x$variables[[..x]]$category_levels,
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
              category_labels = "set_factor_labels"
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

                ..args <- paste(x$variables[[..x]]$category_levels,
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
