
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


test_that(
  "perinary_version is stamped on new dictionaries",
  code = {

    dd <- data_dictionary(
      numeric_variable("x", label = "X")
    )

    expect_identical(
      dd$perinary_version,
      as.character(utils::packageVersion("perinary"))
    )

  }
)

test_that(
  "no version warning when dictionary version matches current perinary",
  code = {

    dd <- data_dictionary(numeric_variable("x", label = "X"))

    # reset session tracker so this test is self-contained
    old_warned <- .perinary_internal$version_warned
    on.exit(.perinary_internal$version_warned <- old_warned)
    .perinary_internal$version_warned <- character(0)

    expect_no_warning(
      translate_names("x", dictionary = dd)
    )

  }
)

test_that(
  "version warning fires once for dictionary with NULL version (pre-versioning)",
  code = {

    dd <- data_dictionary(numeric_variable("x", label = "X"))
    dd_old <- dd$clone(deep = TRUE)
    dd_old$perinary_version <- NULL

    old_warned <- .perinary_internal$version_warned
    on.exit(.perinary_internal$version_warned <- old_warned)
    .perinary_internal$version_warned <- character(0)

    # first call: should warn
    expect_warning(
      translate_names("x", dictionary = dd_old),
      regexp = "unknown"
    )

    # second call: should be silent (already warned this session)
    expect_no_warning(
      translate_names("x", dictionary = dd_old)
    )

  }
)


test_that(
  "version warning fires once per (old_version, current_version) pair",
  code = {

    dd <- data_dictionary(numeric_variable("x", label = "X"))

    old_warned <- .perinary_internal$version_warned
    on.exit(.perinary_internal$version_warned <- old_warned)
    .perinary_internal$version_warned <- character(0)

    dd_v1 <- dd$clone(deep = TRUE)
    dd_v1$perinary_version <- "0.0.1"

    dd_v2 <- dd$clone(deep = TRUE)
    dd_v2$perinary_version <- "0.0.2"

    # each new version pair triggers exactly one warning
    expect_warning(translate_names("x", dictionary = dd_v1), regexp = "0[.]0[.]1")
    expect_no_warning(translate_names("x", dictionary = dd_v1))  # same pair: silent

    expect_warning(translate_names("x", dictionary = dd_v2), regexp = "0[.]0[.]2")
    expect_no_warning(translate_names("x", dictionary = dd_v2))  # same pair: silent

    expect_length(.perinary_internal$version_warned, 2)

  }
)
