


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




