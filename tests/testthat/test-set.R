
test_that(

  desc = "Set using correct inputs", code = {

    dd_set_labels <- dd %>%
      set_labels(number = "A double value",
                integer = "An integer value") %>%
      set_descriptions(factor = "A factor variable with one level") %>%
      set_category_labels(character = c("a" = "A")) %>%
      set_category_order(character = "b") %>%
      set_divby_modeling(number = 10) %>%
      # verify that empty calls to set_whatever won't affect the dictionary
      set_labels()

    # access new values in dictionary

    expect_true(
      dd_set_labels$variables$number$get_label() != dd$variables$number$fmt_label()
    )

    expect_equal(dd_set_labels$dictionary$label[1],
                 c(number = "A double value"))

    expect_equal(dd_set_labels$variables$integer$get_label(),
                 "An integer value")

    expect_equal(dd_set_labels$variables$factor$get_description(),
                 "A factor variable with one level")

    expect_equal(dd_set_labels$variables$number$get_divby_modeling(), 10)

    # access new values in variables

    expect_equal(dd_set_labels$variables$number$get_label(),
                 "A double value")

    expect_equal(dd_set_labels$variables$integer$get_label(),
                 "An integer value")

    dd <- dd %>%
      set_category_labels(character = c("a"="A"),
                         factor = c("j" = "jj"))

    # modifications show up in both variables and dictionary
    expect_equal(dd$variables$factor$get_category_labels(),
                 c("f", "g", "h", "i", "jj"))

    expect_equal(dd$dictionary$category_labels[['character']],
                 .paste_collapse(c("A", "b", "c", "d", "e")))

  }

)

# errors ----

test_that(

  desc = "Set using incorrect inputs", code = {


    expect_error(set_units(dd, factor = 'ohno'),
                 regexp = 'cannot be specified for Nominal variables')

    expect_error(set_divby_modeling(dd, factor = 'ohno'),
                 regexp = 'cannot be specified for Nominal variables')

    expect_error(set_category_labels(dd,
                                    character = c("a" = "A"),
                                    c("b" = "B"),
                                    integer = c('ohno'='ohnooo')),
                 regexp = 'must be name-value pairs')

    expect_error(set_category_labels(dd,
                                    character = c("a" = "A",
                                                  "b" = "B"),
                                    integer = c('ohno'='ohnooo')),
                 regexp = 'Invalid specification')

    expect_error(set_category_labels(dd, character = c("a" = "A", "B")),
                 regexp = 'MISSING_NAME = B')


    expect_error(set_category_labels(dd,
                                    character = c("a" = "A",
                                                  "x" = "B"),
                                    factor = c("j" = "jj")),
                 regexp = 'Invalid category levels')

    # no modification should have happened yet since we have been
    # getting errors in the past attempts.
    expect_equal(dd$variables$character$get_category_levels(),
                 letters[1:5])

  }

)

# snapshots ----

test_that(

  desc = "Printed output", code = {

    expect_snapshot(

      # example code for numeric variable
      numeric_variable(
        name = "age",
        label = "Age of participant",
        units = "years",
        divby_modeling = 10
      )

    )

    expect_snapshot(

      # example code for nominal variable
      nominal_variable(
        name = "age_group",
        label = "Age group",
        description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
        category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
        category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
      )

    )

    expect_snapshot(

      # example code for set functions
      as_data_dictionary(data.frame(a = 1, b = "cat")) %>%
        set_labels(a = "example", b = "categorical example") %>%
        set_units(a = "years") %>%
        set_descriptions(a = "A variable used for examples") %>%
        set_category_labels(b = c(cat = "A small lion"))

    )

  }

)


test_that(
  desc = "order is preserved when you set factor labels out of order",
  code = {

    dd_wonk <- set_category_labels(dd, character = c("e" = "E", "b" = "B"))

    expect_equal(dd_wonk$variables$character$category_labels,
                 c("a", "B", "c", "d", "E"))

  }
)

test_that(
  desc = "tell me what variable is out of place",
  code = {

    expect_error(set_labels(dd,
                           not_in_there = "tell me",
                           number = "correct",
                           another_miss = "tell me too"),
                 regexp = "not_in_there")

    # the dictionary is not modified in place when errors occur.
    expect_null(dd$get_label('number'))

  }
)

test_that(
  desc = "identifiers can be bare or quoted",
  code = {

    dd_1 <- dd %>%
      set_identifiers("factor", character)

    dd_2 <- dd %>%
      set_identifiers(character, factor)

    expect_equal(dd_1, dd_2)


  }
)

test_that(
  desc = "identifiers won't blow up the screen",
  code = {

    expect_snapshot(
      set_identifiers(dd, character, factor) %>%
        get_unknowns(as_request = TRUE)
    )


  }
)
