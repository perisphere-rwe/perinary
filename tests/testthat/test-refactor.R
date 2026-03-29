
# Tests covering the code-refactor branch changes.
# Each test is focused on the specific behaviour that was fixed or simplified.

# ---- get_unknowns: default dictionary resolution ----

test_that("get_unknowns uses the global default dictionary when dictionary = NULL", {

  old <- .perinary_internal$dictionary
  on.exit(.perinary_internal$dictionary <- old)

  set_default_dictionary(dd_test_filled)

  # Should work without an explicit dictionary argument
  result <- get_unknowns()
  expect_s3_class(result, "data.frame")

  # Reset and confirm an error is raised with no default and no argument
  set_default_dictionary(NULL)
  expect_error(get_unknowns(), regexp = "no dictionary supplied")

})


# ---- recode replacement: recode_chr / recode_as_factor / recode_as_character ----

test_that("translate_categories produces correct output (recode replacement)", {

  dd <- data_dictionary(
    nominal_variable("status",
                     category_levels = c("a", "b", "c"),
                     category_labels = c("Active", "Blocked", "Closed"))
  )

  # Character output
  result <- translate_categories(c("a", "c", "b"), dictionary = dd)
  expect_equal(result, c("Active", "Closed", "Blocked"))

  # Factor output: levels in dictionary order
  result_f <- translate_categories(c("c", "a"), dictionary = dd, to_factor = TRUE)
  expect_s3_class(result_f, "factor")
  expect_equal(levels(result_f), c("Active", "Blocked", "Closed"))

  # Unmatched values pass through unchanged
  result_unk <- translate_categories(c("a", "z"), dictionary = dd)
  expect_equal(result_unk, c("Active", "z"))

  # NA values pass through unchanged
  result_na <- translate_categories(c("a", NA_character_), dictionary = dd)
  expect_equal(result_na, c("Active", NA_character_))

})

test_that("translate_names produces correct output (recode replacement)", {

  result <- translate_names(c("number", "integer"),
                            dictionary = dd_test_filled,
                            to_factor = FALSE)
  expect_equal(result, c("A number", "An integer"))

  result_f <- translate_names(dd_test_filled$get_names(),
                              dictionary = dd_test_filled,
                              to_factor = TRUE)
  expect_s3_class(result_f, "factor")

})


# ---- Dead code removed: translate_nominal / translate_numeric no longer public ----

test_that("translate_nominal and translate_numeric are not public methods", {

  dd <- as_data_dictionary(data.frame(x = 1))
  expect_false("translate_nominal" %in% names(dd))
  expect_false("translate_numeric" %in% names(dd))

})


# ---- find_dup_problems: duplicate-level detection is correct ----

test_that("translate_categories errors on one-to-many level-to-label mapping", {

  dd <- data_dictionary(
    nominal_variable("g1", category_levels = c("a", "b"),
                     category_labels = c("Alpha", "Beta")),
    nominal_variable("g2", category_levels = c("a", "b"),
                     category_labels = c("Apple", "Banana"))
  )

  # Both g1 and g2 have level "a", mapping to different labels → ambiguous
  expect_error(
    translate_categories(c("a", "b"), dictionary = dd),
    regexp = "one-to-many"
  )

  # Supplying names disambiguates
  result <- translate_categories(c("a", "b"),
                                 names = c("g1", "g2"),
                                 dictionary = dd)
  expect_equal(result, c("Alpha", "Banana"))

})


# ---- bind_list_to_translater: simplification correctness ----

test_that("translate_categories handles .list overrides and additions (bind_list_to_translater)", {

  dd <- data_dictionary(
    nominal_variable("x",
                     category_levels = c("a", "b", "c"),
                     category_labels = c("A", "B", "C"))
  )

  # Replace an existing label
  result <- translate_categories(c("a", "b"), .list = list(a = "Alpha"), dictionary = dd)
  expect_equal(result, c("Alpha", "B"))

  # Add a new mapping for a level not in dictionary
  result2 <- translate_categories(
    c("a", "z"),
    names = "x",
    .list = list(z = "Zeta"),
    dictionary = dd
  )
  expect_equal(result2, c("A", "Zeta"))

})


# ---- assert_inputs_unique: simplified version still warns on duplicates ----

test_that("assert_inputs_unique warns on duplicated names", {

  expect_warning(
    assert_inputs_unique(c(a = 1, a = 2, b = 3)),
    regexp = "Duplicated"
  )

  # No warning when all names are unique
  expect_no_warning(assert_inputs_unique(c(a = 1, b = 2)))

})


# ---- assert_named_list: no double-check regression ----

test_that("assert_named_list errors on unnamed items (double-check fix)", {

  expect_error(assert_named_list(list(1, 2, 3)), regexp = "Unnamed")
  expect_true(assert_named_list(list(a = 1, b = 2)))

})


# ---- get_names_with_units / divby: keep vs discard ----

test_that("get_names_with_units and get_names_with_divby return correct names", {

  dd <- data_dictionary(
    numeric_variable("age",  label = "Age",    units = "years", divby_modeling = 10),
    numeric_variable("bmi",  label = "BMI"),
    nominal_variable("sex",  label = "Sex",
                     category_levels = c("M", "F"),
                     category_labels = c("Male", "Female"))
  )

  expect_equal(dd$get_names_with_units(), "age")
  expect_equal(dd$get_names_with_divby(), "age")

  # bmi has no units; sex is nominal → neither should appear
  expect_false("bmi" %in% dd$get_names_with_units())
  expect_false("sex" %in% dd$get_names_with_units())

})


# ---- get_term_key: rename_with → rename correctness ----

test_that("get_term_key respects custom term_colname (rename fix)", {

  dd <- data_dictionary(
    nominal_variable("grp",
                     category_levels = c("a", "b"),
                     category_labels = c("A", "B"))
  )

  out <- get_term_key(dd, term_colname = "my_term")
  expect_true("my_term" %in% names(out))
  expect_false("term" %in% names(out))

})
