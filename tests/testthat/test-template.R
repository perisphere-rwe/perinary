
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


