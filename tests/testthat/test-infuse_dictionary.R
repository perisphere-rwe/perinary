

age_years <- numeric_variable(
  name = "age_years",
  label = "Age of participant",
  units = "years",
  divby_modeling = 10
)

gender <- nominal_variable(
  name = "gender",
  label = "Gender of participant",
  category_levels = c("M", "F"),
  category_labels = c("Male", "Female")
)

dd <- data_dictionary(age_years, gender)
data <- data.frame(age_years = 55, gender = "M")

data_infused <- data %>%
  infuse_dictionary(dd, units = "model")

test_that(
  desc = "Input is unmodified",
  code = {
    expect_equal(purrr::map(data, ~attr(.x, 'label')),
                 list(age_years = NULL, gender = NULL))
  }
)

test_that(
  desc = 'labels attach correctly with divby suffix specified',
  code = {

    expect_equal(purrr::map(data_infused, ~attr(.x, 'label')),
                 list(age_years = "Age of participant",
                      age_years_divby_10 = "Age of participant, per 10 years",
                      gender = "Gender of participant"))
  }
)

data_infused_null_suffix <- data %>%
  infuse_dictionary(dd, divby_suffix = NULL, units = "model")

test_that(
  desc = 'labels attach correctly with divby suffix specified',
  code = {

    expect_equal(purrr::map(data_infused_null_suffix, ~attr(.x, 'label')),
                 list(age_years = "Age of participant, per 10 years",
                      gender = "Gender of participant"))
  }
)
