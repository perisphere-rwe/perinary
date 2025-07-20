
test_that(

  desc = "Set dictionary values", code = {

    dd_set_label <- dd %>%
      set_label(number = "A double value",
                integer = "An integer value") %>%
      set_description(factor = "A factor variable with one level") %>%
      set_divby_modeling(number = 10)

    # access new values in dictionary

    expect_equal(dd_set_label$dictionary$label[1],
                 c(number = "A double value"))

    expect_equal(dd_set_label$variables$integer$get_label(),
                 "An integer value")

    expect_equal(dd_set_label$variables$factor$get_description(),
                 "A factor variable with one level")

    expect_equal(dd_set_label$variables$number$get_divby_modeling(), 10)

    # access new values in variables

    expect_equal(dd_set_label$variables$number$get_label(),
                 "A double value")

    expect_equal(dd_set_label$variables$integer$get_label(),
                 "An integer value")

  }

)

# errors ----

test_that(

  desc = "incorrect inputs", code = {


    expect_error(set_unit(dd, factor = 'ohno'),
                 regexp = 'cannot be specified for Nominal variables')

    expect_error(set_divby_modeling(dd, factor = 'ohno'),
                 regexp = 'cannot be specified for Nominal variables')

    expect_error(set_category_label(dd,
                                    character = c("a" = "A"),
                                    c("b" = "B"),
                                    integer = c('ohno'='ohnooo')),
                 regexp = 'must be named')

    expect_error(set_category_label(dd,
                                    character = c("a" = "A",
                                                  "b" = "B"),
                                    integer = c('ohno'='ohnooo')),
                 regexp = 'Invalid specification')

    expect_error(set_category_label(dd,
                                    character = c("a" = "A", "B")),
                 regexp = 'MISSING_NAME = B')


    expect_error(set_category_label(dd,
                                    character = c("a" = "A",
                                                  "x" = "B"),
                                    factor = c("j" = "jj")),
                 regexp = 'Invalid category levels')

    # no modification should have happened yet since we have been
    # getting errors in the past attempts.
    expect_equal(dd$variables$character$get_category_levels(),
                 letters[1:5])

    dd <- dd %>%
      set_category_label(character = c("a"="A"),
                         factor = c("j" = "jj"))

    # modifications show up in both variables and dictionary
    expect_equal(dd$variables$factor$get_category_labels(),
                 c("f", "g", "h", "i", "jj"))

    expect_equal(dd$dictionary$category_labels[['character']],
                 .paste_collapse(c("A", "b", "c", "d", "e")))

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
        set_unit(a = "years") %>%
        set_description(a = "A variable used for examples") %>%
        set_category_label(b = c(cat = "A small lion"))

    )

  }

)


data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
)

dd <- as_data_dictionary(data_test)

test_that(
  desc = "order is preserved when you set factor labels out of order",
  code = {

    dd_wonk <- set_category_label(dd, character = c("e" = "E", "b" = "B"))

    expect_equal(dd_wonk$variables$character$category_labels,
                 c("a", "B", "c", "d", "E"))

  }
)

test_that(
  desc = "tell me what variable is out of place",
  code = {

    expect_error(set_labels(dd, not_in_there = "tell me"),
                 regexp = "Inputs must match")

  }
)

test_that(
  desc = "identifiers can be bare or quoted",
  code = {

    dd_1 <- copy(dd) %>%
      set_identifiers("factor", character)

    dd_2 <- copy(dd) %>%
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
