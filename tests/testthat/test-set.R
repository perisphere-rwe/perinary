




test_that(

  desc = "Set using ... and .list", code = {

    dd_set_dotdots <- dd_test %>%
      set_labels(number = "A double value",
                 integer = "An integer value",
                 logical = "A logical value") %>%
      set_descriptions(factor = "A factor variable with one level",
                       date = "A date to remember") %>%
      set_category_labels(character = c("a" = "A",
                                        "b" = "BB",
                                        "c" = "CCC"),
                          factor = c(j = "JJ")) %>%
      set_category_order(character = "b",
                         factor = c("j", "g")) %>%
      set_units(number = 'units',
                integer = 'units') %>%
      set_divby_modeling(number = 10,
                         integer = 5) %>%
      set_variable_order(integer, .after = logical)

    dd_set_dotlist <- dd_test %>%
      set_labels(.list = list(number = "A double value",
                              integer = "An integer value",
                              logical = "A logical value")) %>%
      set_descriptions(.list = list(factor = "A factor variable with one level",
                                    date = "A date to remember")) %>%
      set_category_labels(.list = list(character = c("a" = "A",
                                                     "b" = "BB",
                                                     "c" = "CCC"),
                                       factor = c(j = "JJ"))) %>%
      set_category_order(.list = list(character = "b",
                                      factor = c("j", "g"))) %>%
      set_units(.list = list(number = 'units',
                             integer = 'units')) %>%
      set_divby_modeling(.list = list(number = 10,
                                      integer = 5)) %>%
      set_variable_order(integer, .after = logical)

    expect_equal(dd_set_dotdots, dd_set_dotlist)

    # verify that empty calls to set_whatever won't affect the dictionary
    expect_warning(set_labels(dd_set_dotdots))

    # verify that the dictionary table is modified by set functions
    expect_equal(dd_set_dotdots$dictionary$label[1],
                 c(number = "A double value"))

    # verify that the dictionary order was modified
    expect_equal(dd_set_dotdots$dictionary$name[2:3],
                 c('logical' = 'logical', 'integer' = 'integer'))

    # verify that dictionary variables are modified by set functions
    expect_equal(dd_set_dotdots$variables$integer$get_label(),
                 "An integer value")

    expect_equal(dd_set_dotdots$variables$factor$get_description(),
                 "A factor variable with one level")

    expect_equal(dd_set_dotdots$variables$number$get_divby_modeling(), 10)


  }

)

# errors ----

test_that(

  desc = "Set using incorrect inputs", code = {


    expect_error(set_units(dd_test, factor = 'ohno'),
                 regexp = 'cannot be specified for Nominal variables')

    expect_error(set_variable_order(dd_test, not_there, .before = integer),
                 regexp = 'exist')

    expect_error(set_divby_modeling(dd_test, factor = 'ohno'),
                 regexp = 'cannot be specified for Nominal variables')

    expect_error(set_category_labels(dd_test,
                                     character = c("a" = "A"),
                                     c("b" = "B"),
                                     integer = c('ohno'='ohnooo')),
                 regexp = 'must be named')

    expect_error(set_category_labels(dd_test,
                                     character = c("a" = "A",
                                                   "b" = "B"),
                                     integer = c('ohno'='ohnooo')),
                 regexp = 'Invalid specification')

    expect_error(set_category_labels(dd_test,
                                     factor = c("ohno", "j" = "J"),
                                     character = c("a" = "A", "B")),
                 regexp = 'values must also be named vectors')

    expect_error(set_category_labels(dd_test,
                                     character = c("a" = "A",
                                                   "x" = "B"),
                                     factor = c("j" = "jj")),
                 regexp = 'Invalid category levels')

    expect_error(set_category_labels(dd_test,
                                     character = c("a" = "B", "b" = "B",
                                                   "c" = "C", "d" = "C")),
                 regexp = "must be unique within variables")


    # no modification should have happened yet since we have been
    # getting errors in the past attempts.
    expect_equal(dd_test$variables$character$get_category_levels(),
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
        description = "Ages of 0 to < 50, 50 to < 60, and >=60 years",
        category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
        category_labels = c("0 to < 50", "50 to < 60", ">=60")
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

  desc = "you can't sneak in a duplicate a label", code = {

    expect_error(
      set_category_labels(dd_test, character = c(a="ok")) %>%
        set_category_labels(character = c(b="ok")),
      regexp = 'must be unique within variables'
    )

  }

)

test_that(
  desc = "order is preserved when you set factor labels out of order",
  code = {

    dd_wonk <- set_category_labels(dd_test, character = c("e" = "E", "b" = "B"))

    expect_equal(dd_wonk$variables$character$category_labels,
                 c("a", "B", "c", "d", "E"))

  }
)

test_that(
  desc = "missing names are reported",
  code = {

    expect_error(set_category_order(dd_test, "b"),
                 "must be named")

  }
)

test_that(
  desc = "tell me what variable is out of place",
  code = {

    expect_error(set_labels(dd_test,
                            not_in_there = "tell me",
                            number = "correct",
                            another_miss = "tell me too"),
                 regexp = "not_in_there")

    # the dictionary is not modified in place when errors occur.
    expect_null(dd_test$get_label('number'))

  }
)

test_that(
  desc = "identifiers can be bare or quoted",
  code = {

    dd_1 <- dd_test %>%
      set_identifiers("factor", character)

    dd_2 <- dd_test %>%
      set_identifiers(character, factor)

    expect_equal(dd_1, dd_2)


  }
)

test_that(
  desc = "identifiers won't blow up the screen",
  code = {

    expect_snapshot(
      set_identifiers(dd_test, character, factor) %>%
        get_unknowns(as_request = TRUE)
    )


  }
)

