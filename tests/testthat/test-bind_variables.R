

# Setup -----------------------------------------------------------------------

dd_base <- data_dictionary(
  numeric_variable("age",  label = "Age",    units = "years"),
  nominal_variable("sex",  label = "Sex",
                   category_levels = c("M", "F"),
                   category_labels = c("Male", "Female"))
)

# Return type -----------------------------------------------------------------

test_that("bind_variables returns a DataDictionary", {
  result <- bind_variables(
    dd_base,
    bmi = numeric_variable(label = "BMI", units = "kg/m2")
  )
  expect_s3_class(result, "DataDictionary")
  expect_s3_class(result, "R6")
})

# Name injection --------------------------------------------------------------

test_that("name is injected from the ... key when omitted in constructor", {
  result <- bind_variables(
    dd_base,
    train_test = nominal_variable(
      label            = "Train/test split",
      category_levels  = c("train", "test"),
      category_labels  = c("Training data", "Testing data")
    )
  )
  expect_true("train_test" %in% result$get_names())
  expect_equal(result$variables[["train_test"]]$name, "train_test")
})

test_that("... key name overrides an explicit name arg in the constructor", {
  result <- bind_variables(
    dd_base,
    correct_name = nominal_variable(
      name            = "wrong_name",
      label           = "A variable",
      category_levels = c("a", "b")
    )
  )
  expect_true("correct_name" %in% result$get_names())
  expect_false("wrong_name" %in% result$get_names())
})

# Variable types --------------------------------------------------------------

test_that("all variable types can be added", {
  result <- bind_variables(
    dd_base,
    score     = numeric_variable(label = "Score",   units = "points"),
    group     = nominal_variable(label = "Group",   category_levels = c("a", "b")),
    flag      = logical_variable(label = "Flag"),
    record_id = identifier_variable(label = "Record ID"),
    visit_dt  = date_variable(label = "Visit date")
  )
  expect_setequal(
    result$get_names(),
    c("age", "sex", "score", "group", "flag", "record_id", "visit_dt")
  )
})

# Multiple variables at once --------------------------------------------------

test_that("multiple variables can be added in a single call", {
  result <- bind_variables(
    dd_base,
    v1 = numeric_variable(label = "V1", units = "cm"),
    v2 = numeric_variable(label = "V2", units = "kg"),
    v3 = nominal_variable(label = "V3", category_levels = c("x", "y"))
  )
  expect_true(all(c("v1", "v2", "v3") %in% result$get_names()))
})

# Metadata preservation -------------------------------------------------------

test_that("variable metadata is preserved after binding", {
  result <- bind_variables(
    dd_base,
    train_test = nominal_variable(
      label            = "Train/test split",
      category_levels  = c("train", "test"),
      category_labels  = c("Training data", "Testing data")
    )
  )
  var <- result$variables[["train_test"]]
  expect_equal(var$label,            "Train/test split")
  expect_equal(var$category_levels,  c("train", "test"))
  expect_equal(var$category_labels,  c("Training data", "Testing data"))
})

test_that("existing variables in the dictionary are unchanged after binding", {
  result <- bind_variables(
    dd_base,
    new_var = nominal_variable(label = "New", category_levels = c("a", "b"))
  )
  expect_equal(result$variables[["age"]]$label,  "Age")
  expect_equal(result$variables[["sex"]]$label,  "Sex")
  expect_equal(result$variables[["sex"]]$category_levels, c("M", "F"))
})

# Ordering --------------------------------------------------------------------

test_that("new variables appear after existing ones in the dictionary", {
  result <- bind_variables(
    dd_base,
    new_var = nominal_variable(label = "New", category_levels = c("a", "b"))
  )
  expect_equal(result$get_names(), c("age", "sex", "new_var"))
})

# Pre-built variable objects via ... ------------------------------------------

test_that("pre-built DataVariable objects passed via ... are renamed by key", {
  pre_built <- nominal_variable(
    name            = "old_name",
    label           = "Pre-built",
    category_levels = c("a", "b")
  )
  result <- bind_variables(dd_base, new_name = pre_built)
  expect_true("new_name" %in% result$get_names())
  expect_false("old_name" %in% result$get_names())
})

test_that("passing pre-built object does not mutate the original", {
  pre_built <- nominal_variable(
    name            = "original",
    label           = "Pre-built",
    category_levels = c("a", "b")
  )
  bind_variables(dd_base, renamed = pre_built)
  expect_equal(pre_built$name, "original")
})

# .list interface -------------------------------------------------------------

test_that(".list path works like ... path", {
  result <- bind_variables(
    dd_base,
    .list = list(
      new_var = nominal_variable(
        name            = "new_var",
        label           = "New",
        category_levels = c("a", "b")
      )
    )
  )
  expect_true("new_var" %in% result$get_names())
  expect_s3_class(result, "DataDictionary")
})

test_that(".list key name overrides the variable's internal name", {
  result <- bind_variables(
    dd_base,
    .list = list(
      correct_name = nominal_variable(
        name            = "internal_name",
        label           = "A variable",
        category_levels = c("a", "b")
      )
    )
  )
  expect_true("correct_name" %in% result$get_names())
  expect_false("internal_name" %in% result$get_names())
})

test_that(".list path does not mutate the original variable objects", {
  original <- nominal_variable(
    name            = "original",
    label           = "Pre-built",
    category_levels = c("a", "b")
  )
  bind_variables(dd_base, .list = list(renamed = original))
  expect_equal(original$name, "original")
})

# Pipe compatibility ----------------------------------------------------------

test_that("bind_variables works in a pipe chain", {
  result <- dd_base |>
    bind_variables(
      v1 = numeric_variable(label = "V1", units = "mm"),
      v2 = logical_variable(label = "V2")
    )
  expect_true(all(c("v1", "v2") %in% result$get_names()))
})

# Error: dictionary argument --------------------------------------------------

test_that("error when dictionary is not a DataDictionary", {
  expect_error(
    bind_variables("not_a_dict", v1 = nominal_variable(label = "x")),
    regexp = "DataDictionary"
  )
})

# Error: conflict with existing variable --------------------------------------

test_that("error when a new variable name already exists in the dictionary", {
  expect_error(
    bind_variables(dd_base, age = numeric_variable(label = "Age again", units = "yr")),
    regexp = "overwrite"
  )
})

test_that("error message names the conflicting variable(s)", {
  expect_error(
    bind_variables(dd_base, age = numeric_variable(label = "Age again", units = "yr")),
    regexp = "age"
  )
})

# Error: unnamed arguments ----------------------------------------------------

test_that("error when ... contains an unnamed argument", {
  expect_error(
    bind_variables(dd_base, nominal_variable(name = "x", label = "x")),
    regexp = "Unnamed"
  )
})

# Error: both ... and .list provided ------------------------------------------

test_that("error when both ... and .list are provided", {
  expect_error(
    bind_variables(
      dd_base,
      v1   = nominal_variable(label = "x", category_levels = "a"),
      .list = list(v2 = nominal_variable(name = "v2", label = "y",
                                         category_levels = "b"))
    ),
    regexp = "empty"
  )
})

# Warning: both ... and .list empty -------------------------------------------

test_that("warning and dictionary returned unchanged when both inputs are empty", {
  expect_warning(result <- bind_variables(dd_base), regexp = "empty")
  expect_equal(result$get_names(), dd_base$get_names())
})

# Error: unnamed .list entries ------------------------------------------------

test_that("error when .list contains unnamed entries", {
  expect_error(
    bind_variables(
      dd_base,
      .list = list(nominal_variable(name = "x", label = "x",
                                    category_levels = "a"))
    ),
    regexp = "Unnamed"
  )
})
