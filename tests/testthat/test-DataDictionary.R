
# Exported constructors ----

age_years <- numeric_variable(
  name = "age_years",
  label = "Age",
  units = 'years',
  divby_modeling = 10
)

age_group <- nominal_variable(
  name = "age_group",
  label = "Age group",
  description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
  category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
  category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
)

date_recorded <- date_variable(
  name = "date_recorded",
  label = "Date of observation",
  description = "The calendar date when the observation was recorded"
)

test_that(

  desc = 'dictionary from base classes', code = {

    dd_from_vars <- data_dictionary(age_years, age_group, date_recorded)

    expect_s3_class(dd_from_vars, "DataDictionary")
    expect_s3_class(dd_from_vars, "R6")
    expect_s3_class(dd_from_vars$dictionary, "data.frame")
    expect_equal(dd_from_vars$variables$age_years, age_years)
    expect_equal(dd_from_vars$variables$age_group, age_group)
    expect_equal(dd_from_vars$variables$date_recorded, date_recorded)

  }

)

test_that(
  desc = "construction works from list or ..., not both",
  code = {

    dd_1 <-
      data_dictionary(.list = list(age_years, age_group))

    dd_2 <-
      data_dictionary(age_years, age_group)

    expect_equal(dd_1, dd_2)

    expect_error(data_dictionary(age_years, .list = list(age_group)))

  }
)

# public get/set variable methods ----

test_that(

  desc = "get and set", code = {

    expect_equal(age_years$get_category_labels(), NULL)
    expect_equal(age_years$fmt_category_labels(), "none")

    expect_equal(date_recorded$get_units(), NULL)
    expect_equal(date_recorded$get_category_levels(), NULL)
    expect_equal(date_recorded$fmt_divby_modeling(), 'none')


    expect_equal(age_group$get_category_labels(),
                 c("0 to < 50", "50 to < 60", "\u2265 60"))

    # not allowed
    expect_error(age_years$chk_category_levels(value = 'no ty'))
    expect_error(age_group$chk_units(value = 'no ty'))
    expect_error(date_recorded$chk_units("days"))

    # okay
    age_years$set_units('days')
    expect_equal(age_years$get_units(), 'days')

    age_group$set_label('Age groups')
    expect_equal(age_group$get_label(), 'Age groups')

    date_recorded$set_label("Date collected")
    expect_equal(date_recorded$get_label(), "Date collected")

    # chain
    expect_equal(
      age_years$set_divby_modeling(5)$get_divby_modeling(), 5
    )

    # verify code above modified age variable in place
    expect_equal(
      age_years$get_divby_modeling(), 5
    )

  }

)

# Assertion on required inputs ----

test_that(

  desc = "name required", code = {

    expect_error(numeric_variable(), 'name')

  }

)

# inheritance ----

test_that(

  desc = "Class structure", code = {

    expect_s3_class(age_years, "NumericVariable")
    expect_s3_class(age_years, "DataVariable")
    expect_s3_class(age_years, "R6")

    expect_s3_class(age_group, "NominalVariable")
    expect_s3_class(age_group, "DataVariable")
    expect_s3_class(age_group, "R6")

    expect_s3_class(date_recorded, "DateVariable")
    expect_s3_class(date_recorded, "DataVariable")
    expect_s3_class(date_recorded, "R6")

  }

)


# dictionary from data ----

test_that(

  desc = 'empty dictionary', code = {

    expect_true(all(dd$dictionary$label == 'none'))
    expect_true(all(dd$dictionary$description == 'none'))
    expect_true(all(dd$dictionary$units == 'none'))
    expect_true(all(dd$dictionary$divby_modeling == 'none'))

  }

)

test_that(

  desc = "labels initialized as levels for nominal variables",

  code = {

    expect_equal(dd$variables$character$category_labels,
                 dd$variables$character$category_levels)

    expect_equal(dd$variables$factor$category_labels,
                 dd$variables$factor$category_levels)

  }

)

test_that(
  desc = "dictionary constructor is safe",

  code = {
    expect_error(DataDictionary$new(vars = list()), 'At least one variable')
    expect_error(DataDictionary$new(vars = list(1)), 'inherit from')
  }
)

# cloning ----

test_that(
  desc = "Clones extend to all fields",

  code = {

    dd_clone <- dd$clone(deep=TRUE) %>%
      set_labels(number = "Cloned label") %>%
      set_category_labels(character = c("a" = "A"))

    expect_equal(dd_clone$dictionary$label[dd_clone$dictionary$name=='number'],
                 c("number" = "Cloned label"))

    expect_equal(dd$dictionary$label[dd$dictionary$name=='number'],
                 c("number" = "none"))

    expect_equal(dd_clone$variables$number$get_label(), "Cloned label")
    expect_null(dd$variables$number$get_label())

    expect_equal(dd_clone$variables$character$get_category_labels()[1], "A")
    expect_equal(dd$variables$character$get_category_labels()[1], "a")

  }

)

