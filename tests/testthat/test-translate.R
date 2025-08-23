

translate_with_extras <- c("integer", "extra_2", "extra_1", "number")

test_that("Using extra values in translate_names", code = {

  expect_warning(
    translate_names(translate_with_extras,
                    dictionary = dd_test_filled,
                    extra_1 = "Extra 1"),
    regexp = "extra_2"
  )

  # extras with a map get pushed to last level.
  # extras without a map get pushed to the level behind extra with a map

  translated <- translate_names(translate_with_extras,
                                dictionary = dd_test_filled,
                                extra_1 = "Extra 1",
                                to_factor = TRUE,
                                warn_unmatched = FALSE)

  expect_equal(
   levels(translated),
   c("A number", "An integer", "A logical", "A character", "A factor", "A date",
     "Extra 1", "extra_2")
  )

  # simple recode when to_factor is false

  expect_equal(
    translate_names(translate_with_extras,
                    extra_1 = "Extra 1",
                    extra_2 = "Extra 2",
                    to_factor = FALSE,
                    dictionary = dd_test_filled),
    c("An integer",
      "Extra 2",
      "Extra 1",
      "A number")
  )

  # levels appear in same order as extras, at the end of the output
  expect_equal(
    translate_names(translate_with_extras,
                    to_factor = TRUE,
                    dictionary = dd_test_filled,
                    extra_2 = "Extra 2",
                    extra_1 = "Extra 1"
                    ) %>%
      levels(),
    c(translate_names(dd_test_filled$get_names(),
                      to_factor = FALSE,
                      dictionary = dd_test_filled),
      "Extra 2",
      "Extra 1")
  )

  # units attach to variables with units
  expect_equal(
    translate_names(translate_with_extras,
                    units = 'descriptive',
                    extra_1 = "Extra 1",
                    extra_2 = "Extra 2",
                    to_factor = FALSE,
                    dictionary = dd_test_filled),
    c("An integer, widgets",
      "Extra 2",
      "Extra 1",
      "A number, quarks")
  )


})


test_that("Using extra values in translate_categories", code = {

  # an extra category mapping supplied in ...
  xt <- translate_categories(x = letters[1:5],
                             b = "B",
                             names = 'character',
                             to_factor = TRUE,
                             dictionary = dd_test_filled)

  expect_equal(levels(xt), c("A", "B", "c", "d", "e"))

  # same using .list and not coercing to factor
  xt <- translate_categories(x = letters[1:5], .list = list(b = "B"),
                             names = 'character',
                             dictionary = dd_test_filled)

  expect_equal(xt, c("A", "B", "c", "d", "e"))

  # multi-named translates can't be factors
  expect_warning(
    translate_categories(x = c(letters[1:10], "overall"),
                         names = rep(c('character', 'factor'), each = 5) %>% c(NA),
                         b = "B",
                         f = "FF",
                         overall = "Overall",
                         to_factor = TRUE,
                         dictionary = dd_test_filled),
    "more than one unique value in `name`"
  )

  xt_multi <- translate_categories(x = c(letters[1:10]),
                                   names = rep(c('character', 'factor'), each = 5),
                                   dictionary = dd_test_filled)

  expect_equal(xt_multi,
               c("A", "b", "c", "d", "e", "f", "GG", "h", "i", "j"))

  xt_extra <- translate_categories(
    x = c(letters[1:10], "overall"),
    names = rep(c('character', 'factor'), each = 5) %>% c("overall"),
    b = "B",
    overall = "Overall",
    f = "FF",
    dictionary = dd_test_filled
  )

  expect_equal(xt_extra,
               c("A", "B", "c", "d", "e", "FF", "GG", "h", "i", "j", "Overall"))


})



test_that(
  "Using translate_categories with duplicated labels",
  code = {

    dd <- data_dictionary(
      nominal_variable('htn', 'hypertension',
                       category_levels = c('no', 'yes'),
                       category_labels = c("No", "Yes")),
      nominal_variable('diab', 'diabetes',
                       category_levels = c('no', 'yes'),
                       category_labels = c("No", "Yes")),
      nominal_variable('age', "age group", category_levels = c("first", "second")),
      nominal_variable("bmi", "bmi group", category_levels = c("first", "second"))
    )

    # if multiple levels all go to the same label, there isn't ambiguity
    translate_categories(c("no", "yes", "second"), dictionary = dd)


    dd <- dd %>%
      set_category_labels(htn = c(no = "Nope", 'yes' = 'hypertension'),
                          diab = c(no = "Nope", 'yes' = 'diabetes'),
                          age = c("first" = "50-65"),
                          bmi = c("first" = "< 18"))

    # if at least one level goes to multiple labels, ask for clarification
    expect_error(translate_categories(c("no", "yes", "first"),
                                      dictionary = dd),
                 "one-to-many")

    # supplying names clarifies
    expect_equal(translate_categories(c("no", "yes", "first"),
                                      names = c("htn", "htn", "age"),
                                      dictionary = dd),
                 c("Nope", "hypertension", "50-65"))


  }
)

test_that(
  "translate_data",
  code = {

    data_translated <- translate_data(data_test, dictionary = dd_test_filled)

    # labels applied
    expect_equal(
      purrr::map_chr(data_translated, ~ attr(.x, 'label')),
      c(
        number = "A number",
        integer = "An integer",
        logical = "A logical",
        character = "A character",
        factor = "A factor",
        date = "A date"
      )
    )

    expect_equal(levels(data_translated$factor),
                 dd_test_filled$get_category_labels('factor'))

    expect_equal(levels(data_translated$character),
                 dd_test_filled$get_category_labels('character'))

  }
)

