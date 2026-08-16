
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
  desc = "index_terms applies append_term_key then index_rows",
  code = {

    dd_iris <- as_data_dictionary(iris) %>%
      set_category_order(Species = c("setosa")) %>%
      set_category_labels(Species = c(versicolor = "Versi")) %>%
      set_variable_order(Species, .before = 1) %>%
      set_variable_order(ends_with("Length"), .after = Species)

    fit <- lm(Sepal.Length ~ ., data = iris) %>% broom::tidy()

    # index_terms should produce the same result as the manual pipeline
    result_shorthand <- index_terms(fit, dictionary = dd_iris)

    result_manual <- fit %>%
      append_term_key(dictionary = dd_iris) %>%
      index_rows(dictionary = dd_iris)

    expect_equal(result_shorthand, result_manual)

  }
)

test_that(
  desc = "index_terms output matches snapshot",
  code = {

    dd_iris <- as_data_dictionary(iris) %>%
      set_category_order(Species = c("setosa")) %>%
      set_category_labels(Species = c(versicolor = "Versi")) %>%
      set_variable_order(Species, .before = 1) %>%
      set_variable_order(ends_with("Length"), .after = Species)

    set_default_dictionary(dd_iris)

    fit_trafo <- lm(Sepal.Length ~ ., data = translate_data(iris)) %>%
      broom::tidy() %>%
      index_terms()

    expect_snapshot(fit_trafo)

  }
)

test_that(
  desc = "index_terms orders rows correctly regardless of levels vs labels in model terms",
  code = {

    # append_term_key() always resolves model terms back to the
    # dictionary's canonical variable name and category level, so
    # index_rows() sees the same name/level values whether the model
    # was fit on raw data or label-translated data. This pins down
    # that invariant: both pipelines should sort identically.
    dd_iris <- as_data_dictionary(iris) %>%
      set_category_order(Species = c("setosa")) %>%
      set_category_labels(Species = c(versicolor = "Versi")) %>%
      set_variable_order(Species, .before = 1) %>%
      set_variable_order(ends_with("Length"), .after = Species)

    fit_plain  <- lm(Sepal.Length ~ ., data = iris) %>% broom::tidy()
    fit_labels <- lm(Sepal.Length ~ ., data = translate_data(iris, dictionary = dd_iris)) %>%
      broom::tidy()

    result_plain  <- index_terms(fit_plain,  dictionary = dd_iris)
    result_labels <- index_terms(fit_labels, dictionary = dd_iris)

    expect_equal(result_plain$name,  result_labels$name)
    expect_equal(result_plain$level, result_labels$level)
    expect_equal(result_plain$estimate, result_labels$estimate)

  }
)


test_that(
  desc = "index_rows orders by category level (existing behavior)",
  code = {

    dd <- data_dictionary(
      nominal_variable(
        "gender",
        category_levels = c("M", "F"),
        category_labels = c("Male", "Female")
      )
    )

    df <- tibble::tibble(name = c("gender", "gender"),
                         level = c("F", "M"),
                         n = c(12, 18))

    result <- index_rows(df, dictionary = dd)

    expect_equal(result$level, c("M", "F"))
    expect_equal(result$n, c(18, 12))

  }
)

test_that(
  desc = "index_rows orders by category label",
  code = {

    dd <- data_dictionary(
      nominal_variable(
        "gender",
        category_levels = c("M", "F"),
        category_labels = c("Male", "Female")
      )
    )

    df <- tibble::tibble(name = c("gender", "gender"),
                         level = c("Female", "Male"),
                         n = c(12, 18))

    result <- index_rows(df, dictionary = dd)

    expect_equal(result$level, c("Male", "Female"))
    expect_equal(result$n, c(18, 12))

  }
)

test_that(
  desc = "index_rows orders correctly with a mix of levels and labels",
  code = {

    dd <- data_dictionary(
      nominal_variable(
        "gender",
        category_levels = c("M", "F"),
        category_labels = c("Male", "Female")
      )
    )

    df <- tibble::tibble(name = c("gender", "gender"),
                         level = c("Female", "M"),
                         n = c(12, 18))

    result <- index_rows(df, dictionary = dd)

    expect_equal(result$level, c("M", "Female"))
    expect_equal(result$n, c(18, 12))

  }
)

test_that(
  desc = "index_rows gives precedence to a level over a colliding label",
  code = {

    # Category "A" has level "A" and label "B"; category "B" has level "B"
    # and label "Z". The literal value "B" is ambiguous: it is category
    # A's label and category B's level. Levels should win the tie-break.
    dd <- data_dictionary(
      nominal_variable(
        "grp",
        category_levels = c("A", "B"),
        category_labels = c("B", "Z")
      )
    )

    df <- tibble::tibble(name = c("grp", "grp"),
                         level = c("B", "A"),
                         n = c(1, 2))

    result <- index_rows(df, dictionary = dd)

    # "B" is interpreted as the level of category B (rank 2), not the
    # label of category A (rank 1), so it sorts after "A".
    expect_equal(result$level, c("A", "B"))

  }
)

test_that(
  desc = "index_rows falls back to default ordering when categories are undefined",
  code = {

    dd <- data_dictionary(nominal_variable("grp"))

    df <- tibble::tibble(name = c("grp", "grp"),
                         level = c("b", "a"),
                         n = c(1, 2))

    result <- index_rows(df, dictionary = dd)

    expect_equal(result$level, c("a", "b"))

  }
)

test_that(
  desc = "index_rows groups and orders rows when `names` holds variable labels",
  code = {

    dd <- data_dictionary(
      numeric_variable("age", label = "Age", units = "years"),
      nominal_variable(
        "smoking_status",
        label = "Smoking status",
        category_levels = c("N", "F", "C"),
        category_labels = c("Never smoked", "Former smoker", "Current smoker")
      )
    )

    df <- tibble::tibble(
      name  = c("Smoking status", "Smoking status", "Smoking status", "Age"),
      level = c("Current smoker", "Never smoked", "Former smoker", NA),
      n     = c(42, 118, 65, NA)
    )

    result <- index_rows(df, dictionary = dd)

    expect_equal(result$name, c("Age", rep("Smoking status", 3)))
    expect_equal(result$level[-1], c("Never smoked", "Former smoker", "Current smoker"))

  }
)

test_that(
  desc = "index_rows works with a mix of variable names, labels, and unmatched terms",
  code = {

    dd <- data_dictionary(
      numeric_variable("age", label = "Age", units = "years"),
      nominal_variable(
        "smoking_status",
        label = "Smoking status",
        category_levels = c("N", "F", "C"),
        category_labels = c("Never smoked", "Former smoker", "Current smoker")
      )
    )

    df <- tibble::tibble(
      name  = c("(Intercept)", "age", "Smoking status", "Smoking status"),
      level = c(NA, NA, "Current smoker", "N"),
      n     = c(1, 2, 3, 4)
    )

    result <- index_rows(df, dictionary = dd)

    # "(Intercept)" is unrecognized and stays anchored at its original
    # (first) position; the recognized names around it still sort into
    # dictionary order (age before smoking_status)
    expect_equal(result$name, c("(Intercept)", "age", "Smoking status", "Smoking status"))
    expect_equal(result$level[3:4], c("N", "Current smoker"))

  }
)

test_that(
  desc = "index_rows anchors unmatched names in place rather than sorting them last",
  code = {

    dd <- data_dictionary(
      numeric_variable("age", label = "Age"),
      nominal_variable("grade", category_levels = c("A", "B", "C"),
                        category_labels = c("Excellent", "Good", "Poor"))
    )

    # "grade" appears before "age" in the input, which is the opposite
    # of dictionary order; "foo" and "bar" are unrecognized and should
    # stay anchored exactly where they first appear, interleaved with
    # the recognized variables rather than pushed to the end
    df <- tibble::tibble(
      name  = c("foo", "grade", "age", "bar"),
      level = c(NA, "A", NA, NA),
      n     = c(1, 2, 3, 4)
    )

    result <- index_rows(df, dictionary = dd)

    # "foo" stays first (its original slot); "grade" and "age" swap
    # into dictionary order (age, then grade) within their combined
    # original slots; "bar" stays anchored last
    expect_equal(result$name, c("foo", "age", "grade", "bar"))
    expect_equal(result$n, c(1, 3, 2, 4))

  }
)

test_that(
  desc = "index_rows gives precedence to a variable name over a colliding label",
  code = {

    # Variable "b" has label "a"; variable "a" has label "z". The
    # literal value "a" is ambiguous: it is variable b's label and
    # variable a's name. Names should win the tie-break.
    dd <- data_dictionary(
      numeric_variable("a", label = "z"),
      numeric_variable("b", label = "a")
    )

    df <- tibble::tibble(name = c("a", "b"), n = c(1, 2))

    result <- index_rows(df, dictionary = dd)

    expect_equal(result$name, c("a", "b"))

  }
)

test_that(
  desc = "index_rows orders correctly when the `level` column is a factor",
  code = {

    # Factor's *level order* deliberately does not match the
    # dictionary's category order, to catch the case where a factor
    # index gets silently treated as its underlying integer codes.
    dd <- data_dictionary(
      nominal_variable(
        "grade",
        category_levels = c("A", "B", "C"),
        category_labels = c("Excellent", "Good", "Poor")
      )
    )

    df <- tibble::tibble(
      name  = c("grade", "grade", "grade"),
      level = factor(c("C", "A", "B"), levels = c("C", "B", "A")),
      n     = c(5, 10, 15)
    )

    result <- index_rows(df, dictionary = dd)

    expect_equal(as.character(result$level), c("A", "B", "C"))
    expect_equal(result$n, c(10, 15, 5))

  }
)

test_that(
  desc = "index_rows orders correctly when the `level` column is a label factor",
  code = {

    dd <- data_dictionary(
      nominal_variable(
        "grade",
        category_levels = c("A", "B", "C"),
        category_labels = c("Excellent", "Good", "Poor")
      )
    )

    df <- tibble::tibble(
      name  = c("grade", "grade", "grade"),
      level = factor(c("Poor", "Excellent", "Good"),
                     levels = c("Poor", "Good", "Excellent")),
      n     = c(5, 10, 15)
    )

    result <- index_rows(df, dictionary = dd)

    expect_equal(as.character(result$level), c("Excellent", "Good", "Poor"))
    expect_equal(result$n, c(10, 15, 5))

  }
)

test_that(
  desc = "index_rows groups correctly when the `name` column is a factor",
  code = {

    dd <- data_dictionary(
      numeric_variable("age", label = "Age"),
      nominal_variable(
        "grade",
        category_levels = c("A", "B", "C"),
        category_labels = c("Excellent", "Good", "Poor")
      )
    )

    # Factor level order deliberately scrambled relative to dictionary
    # order, and includes a value absent from the dictionary.
    df <- tibble::tibble(
      name  = factor(c("grade", "grade", "grade", "(Intercept)"),
                     levels = c("grade", "(Intercept)", "age")),
      level = c("C", "A", "B", NA),
      n     = c(5, 10, 15, 1)
    )

    result <- index_rows(df, dictionary = dd)

    expect_equal(as.character(result$name),
                c("grade", "grade", "grade", "(Intercept)"))
    expect_equal(result$level[1:3], c("A", "B", "C"))
    expect_equal(result$n, c(10, 15, 5, 1))

  }
)

test_that(
  desc = "index_rows groups correctly when the `name` column is a variable-label factor",
  code = {

    dd <- data_dictionary(
      numeric_variable("age", label = "Age", units = "years"),
      nominal_variable(
        "smoking_status",
        label = "Smoking status",
        category_levels = c("N", "F", "C"),
        category_labels = c("Never smoked", "Former smoker", "Current smoker")
      )
    )

    df <- tibble::tibble(
      name  = factor(c("Smoking status", "Smoking status", "Smoking status", "Age"),
                     levels = c("Age", "Smoking status")),
      level = c("Current smoker", "Never smoked", "Former smoker", NA),
      n     = c(42, 118, 65, NA)
    )

    result <- index_rows(df, dictionary = dd)

    expect_equal(as.character(result$name), c("Age", rep("Smoking status", 3)))
    expect_equal(result$level[-1], c("Never smoked", "Former smoker", "Current smoker"))

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
