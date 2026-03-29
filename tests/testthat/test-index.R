
# ── index_rows ────────────────────────────────────────────────────────────────

test_that(
  desc = "index_rows output matches snapshot",
  code = {

    dd_iris <- as_data_dictionary(iris) %>%
      set_category_order(Species = c("setosa")) %>%
      set_category_labels(Species = c(versicolor = "Versi")) %>%
      set_variable_order(Species, .before = 1) %>%
      set_variable_order(ends_with("Length"), .after = Species)

    set_default_dictionary(dd_iris)

    fit_trafo <- lm(Sepal.Length ~ ., data = translate_data(iris)) %>%
      broom::tidy() %>%
      append_term_key() %>%
      index_rows()

    expect_snapshot(fit_trafo)

  }
)

test_that(
  desc = "index_terms is deprecated and forwards to index_rows",
  code = {

    dd_gender <- data_dictionary(
      nominal_variable(
        "gender",
        label = "Gender",
        category_levels = c("M", "F"),
        category_labels = c("Male", "Female")
      )
    )

    df <- tibble::tibble(
      name  = c("gender", "gender"),
      level = c("F", "M"),
      n     = c(12L, 18L)
    )

    # Should emit a deprecation warning pointing to index_rows
    expect_warning(
      result <- index_terms(df, dictionary = dd_gender),
      regexp = "index_rows"
    )

    # Output should be identical to index_rows
    expect_equal(
      result,
      index_rows(df, dictionary = dd_gender)
    )

  }
)


# ── index_columns ─────────────────────────────────────────────────────────────

# Shared dictionary for index_columns tests: 4 variables in a known order.
dd_cols <- data_dictionary(
  numeric_variable("a", label = "Alpha", units = "kg"),
  numeric_variable("b", label = "Beta",  units = "cm"),
  nominal_variable("c", label = "Gamma",
                   category_levels = c("x", "y")),
  numeric_variable("d", label = "Delta", units = "mg")
)

test_that(
  desc = "index_columns returns columns in dictionary order when all match",
  code = {

    df <- data.frame(d = 4, c = "x", b = 2, a = 1)

    result <- index_columns(df, dictionary = dd_cols)

    expect_equal(names(result), c("a", "b", "c", "d"))

  }
)

test_that(
  desc = "index_columns preserves row values after reordering",
  code = {

    df <- data.frame(d = 10L, c = "y", b = 5.5, a = 1.1)

    result <- index_columns(df, dictionary = dd_cols)

    expect_equal(result$a, 1.1)
    expect_equal(result$b, 5.5)
    expect_equal(result$c, "y")
    expect_equal(result$d, 10L)

  }
)

test_that(
  desc = "keep_unmatched = TRUE appends unmatched columns after matched ones",
  code = {

    df <- data.frame(z = 99, b = 2, extra = "hello", a = 1)

    result <- index_columns(df, dictionary = dd_cols, keep_unmatched = TRUE)

    # matched columns (in dictionary order), then unmatched (in original order)
    expect_equal(names(result), c("a", "b", "z", "extra"))

  }
)

test_that(
  desc = "keep_unmatched = FALSE drops columns absent from the dictionary",
  code = {

    df <- data.frame(z = 99, b = 2, extra = "hello", a = 1)

    result <- index_columns(df, dictionary = dd_cols, keep_unmatched = FALSE)

    expect_equal(names(result), c("a", "b"))
    expect_false("z" %in% names(result))
    expect_false("extra" %in% names(result))

  }
)

test_that(
  desc = "dictionary variables absent from data are simply not included",
  code = {

    # data only has 'a' and 'd'; 'b' and 'c' are in the dictionary but not here
    df <- data.frame(d = 4, a = 1)

    result <- index_columns(df, dictionary = dd_cols)

    expect_equal(names(result), c("a", "d"))

  }
)

test_that(
  desc = "index_columns with no matching columns and keep_unmatched = TRUE returns only unmatched",
  code = {

    df <- data.frame(x = 1, y = 2)

    result <- index_columns(df, dictionary = dd_cols, keep_unmatched = TRUE)

    # No matched columns; all unmatched columns preserved in original order
    expect_equal(names(result), c("x", "y"))
    expect_equal(nrow(result), 1L)

  }
)

test_that(
  desc = "index_columns with no matching columns and keep_unmatched = FALSE returns zero-column data frame",
  code = {

    df <- data.frame(x = 1, y = 2)

    result <- index_columns(df, dictionary = dd_cols, keep_unmatched = FALSE)

    expect_equal(ncol(result), 0L)
    expect_equal(nrow(result), 1L)

  }
)

test_that(
  desc = "index_columns preserves tibble class",
  code = {

    tbl <- tibble::tibble(d = 4, c = "x", b = 2, a = 1)

    result <- index_columns(tbl, dictionary = dd_cols)

    expect_s3_class(result, "tbl_df")
    expect_equal(names(result), c("a", "b", "c", "d"))

  }
)

test_that(
  desc = "index_columns works with the default dictionary",
  code = {

    set_default_dictionary(dd_cols)
    on.exit(set_default_dictionary(NULL))

    df <- data.frame(d = 4, b = 2, extra = 9, a = 1)

    result <- index_columns(df)

    expect_equal(names(result), c("a", "b", "d", "extra"))

  }
)

test_that(
  desc = "index_columns errors when no dictionary supplied and no default set",
  code = {

    set_default_dictionary(NULL)

    df <- data.frame(a = 1)

    expect_error(
      index_columns(df),
      regexp = "no dictionary supplied"
    )

  }
)

test_that(
  desc = "index_columns preserves multiple rows correctly",
  code = {

    df <- data.frame(
      c = c("x", "y", "x"),
      a = c(1.1, 2.2, 3.3),
      b = c(10, 20, 30)
    )

    result <- index_columns(df, dictionary = dd_cols)

    # Column order is dictionary order: a, b, c
    expect_equal(names(result), c("a", "b", "c"))
    # Rows are unchanged
    expect_equal(nrow(result), 3L)
    expect_equal(result$a, c(1.1, 2.2, 3.3))

  }
)

test_that(
  desc = "index_rows and index_columns are independent (separate method calls)",
  code = {

    # index_rows reorders rows
    df_terms <- data.frame(
      name  = c("a", "a", "b"),
      level = c("x", "y", "x"),
      n     = c(5L, 3L, 8L)
    )

    result_rows <- index_rows(df_terms, dictionary = dd_cols)
    expect_s3_class(result_rows, "data.frame")
    expect_equal(names(result_rows), c("name", "level", "n"))

    # index_columns reorders columns
    df_cols <- data.frame(d = 4, b = 2, a = 1)
    result_cols <- index_columns(df_cols, dictionary = dd_cols)
    expect_equal(names(result_cols), c("a", "b", "d"))

  }
)
