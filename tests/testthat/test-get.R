


test_that(
  desc = "returns from get_unkowns",
  code = {

    unknown_tbl <- get_unknowns(dd_test, show_optional = TRUE)

    expect_s3_class(unknown_tbl, 'data.frame')

    expect_snapshot(unknown_tbl)

    expect_snapshot(
      get_unknowns(dd_test, as_request = TRUE)
    )

    expect_snapshot(
      get_unknowns(dd_test, as_code = TRUE)
    )

  }
)

test_that(
  desc = "returns from get_term_key",
  code = {

    expect_snapshot(get_term_key(dd_test))
    expect_snapshot(get_term_key(dd_test_filled))

    # adjusting to a case where just two non-referent levels are given
    # for one variable - this should be a three row tibble with the reference
    # class included at the top
    expect_snapshot(
      get_term_key(dd_test_filled,
                   adjust_to = data.frame(term = c('characterb',
                                                   'characterc')))
    )

    # adjusting to a case where two non-referent levels are given for two
    # variables. This should be a four row tibble that includes the given
    # terms plus their corresponding references.
    expect_snapshot(
      get_term_key(dd_test_filled,
                   adjust_to = data.frame(term = c('factorf',
                                                   'characterc')))
    )



  }
)

test_that(
  desc = "returns from get_dictionary",
  code = {

    expect_snapshot(get_dictionary(dd_test))

    expect_snapshot(get_dictionary(dd_test_filled))

    expect_snapshot(get_dictionary(dd_test,
                                   format_missing = TRUE))

    expect_snapshot(get_dictionary(dd_test_filled,
                                   format_missing = TRUE))

    expect_snapshot(get_dictionary(dd_test,
                                   format_missing = TRUE,
                                   format_categories = TRUE))

    expect_snapshot(get_dictionary(dd_test_filled,
                                   format_missing = TRUE,
                                   format_categories = TRUE))

  }
)


# get_acronym_defs ----
test_that(
  desc = "Acronyms must be set before getting definitions",
  code = {
    # An error is thrown when there are no acronyms
    expect_error(
      get_acronym_defs(dd_test), # dd_test has no acronyms
      regexp = "No acronyms"
    )

    # A warning is issued when there are acronyms, but none are selected.
    # get_acronym_defs should also return an empty character vector.
    dd_1 <- set_acronyms(dictionary = dd_test, FOO = "foo", BAR = "bar")

    expect_warning(
      defs <- get_acronym_defs(dd_1, acronyms = "invalid"),
      regexp = "No acronyms match names"
    )

    expect_warning(
      get_acronym_defs(dd_1, acronyms = character(0L)),
      regexp = "No acronyms match names"
    )

    expect_identical(
      defs,
      character(0L)
    )
  }
)

test_that(
  desc = "Acronym definitions are correct and formatted properly",
  code = {
    dd_1 <- set_acronyms(dictionary = dd_test, FOO = "foo", BAR = "bar")

    # Acronyms should be in alphabetic order (handled by set_acronyms)
    expected <- "BAR = bar; FOO = foo."

    # Select all acronyms
    expect_identical(
      get_acronym_defs(dd_1),
      expected
    )

    # Order doesn't matter
    expect_identical(
      get_acronym_defs(dd_1, acronyms = c("FOO", "BAR")),
      expected
    )

    # Select a single acronym
    expect_identical(
      get_acronym_defs(dd_1, acronyms = "FOO"),
      "FOO = foo."
    )
  }
)
