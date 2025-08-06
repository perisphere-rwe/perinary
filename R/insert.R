#' Insert Term Key into Modeling Output
#'
#' Merges category-level metadata from a dictionary into a data frame
#' containing modeling terms (e.g., from [broom::tidy()]).
#' This function is most useful when you want to annotate model output
#' (such as regression coefficients) with human-readable labels or
#' structured metadata about nominal variables.
#'
#' The function uses the `term` column (or other specified column) in the
#' input `data` to match with a term key derived from the dictionary.
#' The final output includes a `variable` column and preserves term order,
#' appending reference terms (if needed) for completeness.
#'
#' @param data A data frame or tibble containing model output, typically
#'   from [broom::tidy()]. Must contain a column that identifies model terms.
#' @param dictionary A data dictionary created with [as_data_dictionary()]
#'   or related functions, containing meta data for nominal variables.
#' @param term_separator A string used to separate variable names and
#'   category values when constructing or matching terms. Default is `""`.
#' @param term_colname Name of the column in `data` that contains term
#'   identifiers. Default is `"term"`.
#'
#' @return A tibble with the original `data` joined to the term key from
#'   the dictionary. The result includes additional columns like `variable`
#'   and `category`, and ensures consistent ordering of model terms with
#'   appended reference rows (if missing).
#'
#' @export
#'
#' @examples
#'
#' library(perinary)
#' library(broom)
#' fit <- lm(Sepal.Length ~ ., data = iris)
#'
#' fit_tidy <- broom::tidy(fit)
#'
#' insert_term_key(fit_tidy, as_data_dictionary(iris))

insert_term_key <- function(data,
                            dictionary,
                            term_separator = "",
                            term_colname = 'term'){


  key <- get_term_key(dictionary,
                      adjust_to = data,
                      term_separator = term_separator,
                      term_colname = term_colname)

  term_order <- data[[term_colname]]

  append_instructions <- key %>%
    dplyr::rename(term = !!term_colname) %>%
    dplyr::mutate(order = match(term, term_order) - 1) %>%
    tidyr::fill(order, .direction = 'up') %>%
    dplyr::filter(reference) %>%
    dplyr::arrange(desc(order))

  for(i in seq_len(nrow(append_instructions))){

    term_order %<>% append(values = append_instructions$term[i],
                           after = append_instructions$order[i])

  }

  key %>%
    full_join(data, by = term_colname) %>%
    mutate(variable = if_else(is.na(variable), term, variable)) %>%
    mutate(term = factor(term, levels = term_order)) %>%
    arrange(term) %>%
    mutate(term = as.character(term))

}
