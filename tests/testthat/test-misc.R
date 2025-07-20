

test_that(
  desc = "match output expectation",
  code = {
    expect_equal(.paste_named_vec(x = "bad", name_fill = "missing"),
                 'missing = bad')
    expect_equal(.paste_named_vec(x = list(a = c(1)), name_fill = "<improper>"),
                 c(a = "a = c(<improper> = 1)"))
    expect_equal(.paste_named_vec(x = list(a = c(x=1))),
                 c(a = "a = c(x = 1)"))
  }
)

