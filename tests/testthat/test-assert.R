

expect_error(
  assert_in_set(values = letters[1:5], choices = letters[2:4]),
  regexp = "a and e"
)

