
# Data ----

data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  logical = c(F, F, T, F, T),
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
)

dd_test <- as_data_dictionary(data_test)

# Data variables  ----

age_years <- numeric_variable(
  name = "age_years",
  label = "Age",
  units = 'years',
  divby_modeling = 10
)

age_group <- nominal_variable(
  name = "age_group",
  label = "Age group",
  description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
  category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
  category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
)

is_cool <- logical_variable(
  name = "is_cool",
  label = "Is it cool?",
  description = "Who can know",
  category_labels = c("no", "yes")
)

id <- identifier_variable(
  name = "id",
  label = "Identifier"
)

date_recorded <- date_variable(
  name = "date_recorded",
  label = "Date of observation",
  description = "The calendar date when the observation was recorded"
)

variables_test <- list(
  identifier = id,
  numeric = age_years,
  nominal = age_group,
  logical = is_cool,
  date = date_recorded
)


dd_test_filled <- dd_test %>%
  set_labels(number = "A number",
             integer = "An integer",
             character = "A character",
             factor = "A factor",
             logical = "A logical",
             date = "A date") %>%
  set_category_labels(character = c(a = "A"),
                      factor = c(g = "GG")) %>%
  set_category_order(factor = "g") %>%
  set_units(number = 'quarks',
            integer = 'widgets') %>%
  set_divby_modeling(number = 10)

#
# dd_2bin <- data_dictionary(
#   nominal_variable('bin_1',
#                    category_levels = c("no", "yes"),
#                    category_labels = c("nope", "you bet")),
#   nominal_variable('bin_2',
#                    category_levels = c("no", "yes"),
#                    category_labels = c("never", "of course"))
# )
#
# dd_2bin$translate_categories(x = c("no", "yes", "yes", "overall", "no"),
#                              names = c("bin_1", "bin_1", "bin_2", NA_character_, "bin_1"),
#                              overall = "Overall")
