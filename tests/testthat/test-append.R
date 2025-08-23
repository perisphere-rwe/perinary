

test_that(
  desc = "append terms",
  code = {

    dd_iris <- as_data_dictionary(iris) %>%
      set_category_order(Species = c("versicolor")) %>%
      set_category_labels(Species = c(versicolor = "Versi"))

    fit_plain <- broom::tidy(lm(Sepal.Length ~ .,
                                data = iris))

    fit_trafo <- broom::tidy(lm(Sepal.Length ~ .,
                                data = translate_data(iris, dictionary = dd_iris)))

    expect_error(append_term_key(fit_plain, dictionary = dd_iris),
                 regexp = "Reference category detected")

    expect_snapshot(append_term_key(fit_trafo, dictionary = dd_iris))

  }
)

