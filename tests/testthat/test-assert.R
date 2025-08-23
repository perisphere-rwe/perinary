

expect_error(
  assert_in_set(values = letters[1:5], choices = letters[2:4]),
  regexp = "a and e"
)


test_that(
  desc = "... and .list must have named inputs",
  code = {

    expect_true(assert_named_dots(a=1, b=2))
    expect_true(assert_named_list(list(a=1)))

    expect_error(assert_named_dots(10), 'must be named')
    expect_error(assert_named_dots(list(a=1)), 'must be named')
    expect_error(assert_named_list(list(1, "AS", data.frame(a=1))), 'must be named')

  }
)


