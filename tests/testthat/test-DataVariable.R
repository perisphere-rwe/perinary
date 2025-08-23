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

    expect_s3_class(is_cool, "LogicalVariable")
    expect_s3_class(id, "IdentifierVariable")

    expect_s3_class(date_recorded, "DateVariable")
    expect_s3_class(date_recorded, "DataVariable")
    expect_s3_class(date_recorded, "R6")

  }

)
