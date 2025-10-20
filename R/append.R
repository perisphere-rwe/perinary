#' Append Model Output with Missing Terms
#'
#' Add columns indicating variable name, category level, category label, and
#' reference groups to the model output (a row is also added for each reference
#' group)
#'
#' The function uses the `term` column (or other specified column) in the
#' input `data` to match with a term key derived from the dictionary.
#' The final output includes a `variable` column and preserves term order,
#' appending reference terms (if needed) for completeness.
#'
#' @param data A data frame or tibble containing model output, typically
#'   from [broom::tidy()]. Must contain a column that identifies model terms.
#'
#' @param dictionary `r roxy_describe_dd()`. `r roxy_default_dd()`.
#'
#' @param term_separator A string used to separate variable names and
#'   category values when constructing or matching terms. Default is `""`
#'   to match the default separator that most R modeling functions use.
#'
#' @param term_colname Name of the column in `data` that contains term
#'   identifiers. Default is `"term"`.
#'
#' @return A tibble with the original `data` joined to the term key from
#'   the dictionary. The result includes additional columns like `variable`
#'   and `category`, and ensures consistent ordering of model terms with
#'   appended reference rows (if missing).
#'
#' @importFrom dplyr arrange desc filter full_join if_else mutate rename
#' @importFrom rlang !! abort set_names
#' @importFrom tidyr fill
#'
#' @export
#'
#' @examples
#'
#' library(broom)
#'
#' fit <- tidy(lm(Sepal.Length ~ ., data = iris))
#'
#' append_term_key(fit, as_data_dictionary(iris))

append_term_key <- function(data,
                            dictionary = NULL,
                            term_separator = "",
                            term_colname = 'term'){

  dictionary <- infer_meta(dictionary)

  key <- get_term_key(dictionary,
                      adjust_to = data,
                      term_separator = term_separator,
                      term_colname = term_colname)

  term_order <- data[[term_colname]]

  refs <- key$term[key$reference]
  refs_in_terms <- refs[which(refs %in% term_order)]

  if (!is_empty(refs_in_terms)){
    abort(
      message = c("Reference category detected in model terms",
                  set_names(refs_in_terms, "x"),
                  "i" = "Use `translate_data()` on your data before modeling"),
      call = NULL
    )
  }

  append_instructions <- key %>%
    rename(term = !!term_colname) %>%
    mutate(order = match(term, term_order) - 1) %>%
    fill(order, .direction = 'up') %>%
    filter(reference) %>%
    arrange(desc(order))

  for(i in seq_len(nrow(append_instructions))){

    term_order %<>% append(values = append_instructions$term[i],
                           after = append_instructions$order[i])

  }

  key %>%
    full_join(data, by = term_colname) %>%
    mutate(name = if_else(is.na(name), term, name)) %>%
    mutate(term = factor(term, levels = term_order)) %>%
    arrange(term) %>%
    mutate(
      term = as.character(term),
      reference = if_else(is.na(reference), FALSE, reference)
    )

}



