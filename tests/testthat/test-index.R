
test_that(
  desc = "index outputs",
  code = {

    dd_iris <- as_data_dictionary(iris) %>%
      set_category_order(Species = c("setosa")) %>%
      set_category_labels(Species = c(versicolor = "Versi"))

    set_default_dictionary(dd_iris)

    fit_trafo <- lm(Sepal.Length ~ ., data = translate_data(iris)) %>%
      broom::tidy() %>%
      append_term_key() %>%
      index_terms()

    expect_snapshot(fit_trafo)

  }
)

