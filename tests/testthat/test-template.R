
# these tests are using a back-end function, dd_set(), to run through
# the expected behavior or templates. Might update these tests after
# Tyler's PR implementing the API for users to set templates.

dd <- dd_test$clone(deep = TRUE)

dd$variables$number$template_label <- "the {animal} is {color}"
dd$variables$logical$template_description <- "I am {emotion}"

test_that(
  desc = "can't set a template for a variable that doesn't have one",
  code = {
    expect_error(
      dd_set(dd,
             .list = list(integer = use_template(animal = 'dog',
                                                 color = 'red')),
             field = 'label'),
      "No label template has been set"
    )
  }
)

test_that(
  desc = "can't set a non-existent binding in a template",
  code = {
    expect_error(
      dd_set(dd,
             .list = list(number = use_template(animal = 'dog',
                                                fur_color = 'red')),
             field = 'label'),
      "must match bindings of the variables that the template is used on"
    )
  }
)


test_that(
  desc = "can set a label and description using proper template bindings",
  code = {

    dd_modified <-
      dd_set(dd,
             .list = list(number = use_template(animal = 'dog',
                                                color = 'red')),
             field = 'label')

    expect_equal(dd_modified$variables$number$label,
                 'the dog is red')

    dd_modified <-
      dd_set(dd,
             .list = list(logical = use_template(emotion = "perplexed")),
             field = 'description')

    expect_equal(dd_modified$variables$logical$description,
                 'I am perplexed')
  }
)



test_that(
  desc = "... in set_templates must be formulas",
  code = {

    # Template is not a formula -> error
    expect_error(
      dd_modified <- set_templates(
        dictionary = dd,
        number = "This is a {template}."
      ),
      regexp = "Arguments passed to ... must be formulas."
    )

    # Template is not a formula -> no error
    expect_no_error(
      dd_modified <- set_templates(
        dictionary = dd,
        number ~ "This is a {template}."
      )
    )
  }
)


test_that(
  desc = "Templates are in the expected format",
  code = {

    # Templates should have curly braces, and the components inside the curly
    # braces should be syntactically valid variable names.

    err <- "Invalid template specification for variable"

    # Template must contain {}
    expect_error(
      dd_modified <- set_templates(
        dictionary = dd,
        number ~ "Invalid template."
      ),
      regexp = err
    )

    # The text in {} must begin with an upper or lowercase letter
    expect_error(
      dd_modified <- set_templates(
        dictionary = dd,
        number ~ "Invalid {1template}."
      ),
      regexp = err
    )

    expect_error(
      dd_modified <- set_templates(
        dictionary = dd,
        number ~ "Invalid {_template}."
      ),
      regexp = err
    )

    # Spaces are not allowed in {}
    expect_error(
      dd_modified <- set_templates(
        dictionary = dd,
        number ~ "Invalid {tem plate}."
      ),
      regexp = err
    )
  }
)


test_that(
  desc = "Template variables exist in the data dictionary",
  code = {

    expect_error(
      dd_modified <- set_templates(
        dictionary = dd,
        foo ~ "Valid {template}."
      ),
      regexp = "One or more variables do not exist in dictionary names"
    )

  }
)


test_that(
  desc = "Assigning multiple templates to the same variable throws a warning",
  code = {

    expect_warning(
      dd_modified <- set_templates(
        dictionary = dd,
        number ~ "First {template}.",
        number ~ "Second {template}." # duplicate name
      ),
      regexp = "Multiple templates specified for the following variables"
    )

    # If more than 3 variables have more than 1 template being assigned, the
    # first 3 variables are shown and the remaining are tallied in the warning
    expect_warning(
      dd_modified <- set_templates(
        dictionary = dd,
        c(number, integer, logical, character, factor) ~ "First {template}.",
        # Duplicate variables, different template
        c(number, integer, logical, character, factor) ~ "Second {template}."
      ),
      regexp = "2 other variables"
    )

    # Turn warnings off
    expect_no_warning(
      dd_modified <- set_templates(
        dictionary = dd,
        number ~ "First {template}.",
        number ~ "Second {template}.", # duplicate variable, different template
        show_warnings = FALSE
      )
    )

  }
)
