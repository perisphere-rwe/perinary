
test_that(

  desc = 'initialization from ... or .list', code = {

    dd_from_vars <- data_dictionary(id,
                                    age_years,
                                    age_group,
                                    is_cool,
                                    date_recorded)

    expect_s3_class(dd_from_vars, "DataDictionary")
    expect_s3_class(dd_from_vars, "R6")
    expect_s3_class(dd_from_vars$dictionary, "data.frame")

    expect_equal(dd_from_vars$variables$id, id)
    expect_equal(dd_from_vars$variables$age_years, age_years)
    expect_equal(dd_from_vars$variables$age_group, age_group)
    expect_equal(dd_from_vars$variables$is_cool, is_cool)
    expect_equal(dd_from_vars$variables$date_recorded, date_recorded)

    dd_from_list <- data_dictionary(.list = variables_test)

    expect_equal(dd_from_vars, dd_from_list)

    # only one can be used
    expect_error(data_dictionary(age_years, .list = list(age_group)))

  }

)

test_that(

  desc = 'initialization from unlabeled data', code = {

    expect_true(all(dd_test$dictionary$label == 'none'))
    expect_true(all(dd_test$dictionary$description == 'none'))
    expect_true(all(dd_test$dictionary$units == 'none'))
    expect_true(all(dd_test$dictionary$divby_modeling == 'none'))

  }

)

test_that(

  desc = 'initialization from labeled data', code = {

    data_test_labeled <- data_test
    attr(data_test_labeled$number, "label") <- "A number"

    expect_equal(
      as_data_dictionary(data_test_labeled)$get_label('number'),
      "A number"
    )

  }

)

test_that(

  desc = "labels initialized as levels for nominal variables",

  code = {

    expect_equal(dd_test$variables$character$category_labels,
                 dd_test$variables$character$category_levels)

    expect_equal(dd_test$variables$factor$category_labels,
                 dd_test$variables$factor$category_levels)

  }

)

test_that(
  desc = "Catch input error with constructor",

  code = {
    expect_error(DataDictionary$new(vars = list()), 'At least one variable')
    expect_error(DataDictionary$new(vars = list(1)), 'inherit from')
  }
)

test_that(
  desc = "Deep clone",

  code = {

    dd_clone <- dd_test$clone(deep=TRUE) %>%
      set_labels(number = "Cloned label") %>%
      set_category_labels(character = c("b" = "B"))

    expect_equal(dd_clone$dictionary$label[dd_clone$dictionary$name=='number'],
                 c("number" = "Cloned label"))

    expect_equal(dd_test$dictionary$label[dd_test$dictionary$name=='number'],
                 c("number" = "none"))

    expect_equal(dd_clone$variables$number$get_label(), "Cloned label")
    expect_null(dd_test$variables$number$get_label())

    expect_equal(dd_clone$variables$character$get_category_labels()[2], "B")
    expect_equal(dd_test$variables$character$get_category_labels()[2], "b")

  }

)

