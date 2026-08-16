
test_that(
  desc = "select_variables keeps only the requested variables, in selection order",
  code = {

    dd <- as_data_dictionary(iris)

    result <- select_variables(dd, Species, Sepal.Length)

    expect_equal(result$get_names(), c("Species", "Sepal.Length"))

  }
)

test_that(
  desc = "select_variables supports tidyselect helpers",
  code = {

    dd <- as_data_dictionary(iris)

    result <- select_variables(dd, ends_with("Length"))

    expect_equal(result$get_names(), c("Sepal.Length", "Petal.Length"))

  }
)

test_that(
  desc = "select_variables supports negated (drop) selections",
  code = {

    dd <- as_data_dictionary(iris)

    result <- select_variables(dd, -Species)

    expect_false("Species" %in% result$get_names())
    expect_equal(result$get_names(),
                c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

  }
)

test_that(
  desc = "select_variables preserves variable definitions (labels, categories, etc.)",
  code = {

    dd <- data_dictionary(
      numeric_variable("age", label = "Age", units = "years"),
      nominal_variable(
        "grade",
        label = "Grade",
        category_levels = c("A", "B"),
        category_labels = c("Excellent", "Good")
      )
    )

    result <- select_variables(dd, grade)

    expect_equal(result$get_names(), "grade")
    expect_equal(result$get_label("grade"), "Grade")
    expect_equal(result$get_category_levels("grade"), c("A", "B"))
    expect_equal(result$get_category_labels("grade"), c("Excellent", "Good"))

  }
)

test_that(
  desc = "select_variables resolves a label collision so index_rows() no longer warns",
  code = {

    dd <- data_dictionary(
      numeric_variable("age_baseline", label = "Age"),
      numeric_variable("age_followup", label = "Age")
    )

    dd_fixed <- select_variables(dd, -age_followup)

    df <- tibble::tibble(name = c("Age", "age_baseline"), n = c(1, 2))

    expect_no_warning(result <- index_rows(df, dictionary = dd_fixed))

    # both rows resolve to the single remaining "age_baseline" variable
    expect_equal(result$name, c("Age", "age_baseline"))
    expect_equal(result$n, c(1, 2))

  }
)

test_that(
  desc = "select_variables errors on a non-DataDictionary input",
  code = {

    expect_error(select_variables(data.frame(a = 1), a))

  }
)
