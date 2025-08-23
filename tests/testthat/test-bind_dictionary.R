

dd_1 <- as_data_dictionary(iris) %>%
  set_labels(Sepal.Length = "Label 1")

dd_2 <- as_data_dictionary(iris) %>%
  set_labels(Sepal.Length = "Label 2")

dd_3 <- data_dictionary(
  numeric_variable("Sepal.Length", "Label 3"),
  numeric_variable("newboy")
)

test_that(
  desc = "Conflicts are warned about",
  code = {
    expect_warning(bind_dictionary(dd_1, dd_2, conflict_preference = NULL))
  }
)

test_that(
  desc = "Conflict preference works as intended",
  code  = {

    expect_equal(
      bind_dictionary(dd_1, dd_2, conflict_preference = 'left') %>%
        getElement("dictionary") %>%
        dplyr::slice(1) %>%
        dplyr::pull(label),
      c(Sepal.Length = "Label 1")
    )

    expect_equal(
      bind_dictionary(dd_1, dd_2, conflict_preference = 'right') %>%
        getElement("dictionary") %>%
        dplyr::slice(1) %>%
        dplyr::pull(label),
      c(Sepal.Length = "Label 2")
    )

    expect_equal(
      bind_dictionary(dd_1, dd_3, conflict_preference = 'right') %>%
        getElement("dictionary") %>%
        dplyr::slice(1) %>%
        dplyr::pull(label),
      c(Sepal.Length = "Label 3")
    )

    expect_equal(
      bind_dictionary(dd_1, dd_3,
                      conflict_preference = 'right',
                      keep_unmatched_y = TRUE) %>%
        getElement("dictionary") %>%
        dplyr::filter(name == "newboy") %>%
        dplyr::pull(name),
      c(newboy = "newboy")
    )

  }
)


