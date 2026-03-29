# Variable Constructors

Constructors for variable metadata classes.

## Usage

``` r
nominal_variable(
  name,
  label = NULL,
  description = NULL,
  category_levels = NULL,
  category_labels = NULL
)

numeric_variable(
  name,
  label = NULL,
  description = NULL,
  units = NULL,
  divby_modeling = NULL
)

date_variable(name, label = NULL, description = NULL, date_format = NULL)

identifier_variable(name, label = NULL, description = NULL)

logical_variable(
  name,
  label = NULL,
  description = NULL,
  category_labels = c("FALSE", "TRUE")
)
```

## Arguments

- name:

  Character. The name of the variable.

- label:

  Character or `NULL`. A short label for the variable.

- description:

  Character or `NULL`. A longer description of the variable (optional).

- category_levels:

  Character vector or `NULL`. The set of unique category codes.

- category_labels:

  Character vector or `NULL`. Labels for the categories. For
  `nominal_variable`, the labels will default to `category_levels` if
  not provided. For `logical_variable`, this is a length 2 vector
  containing optional labels for the two levels: `FALSE` and `TRUE`,
  respectively.

- units:

  Character or `NULL`. Units for the variable (e.g., "kg", "years").

- divby_modeling:

  Numeric or `NULL`. A constant indicating what to divide the variable
  by before modeling. This improves interpretability of model
  coefficients.

- date_format:

  Character or `NULL`. Optional format string for parsing date values
  (e.g., "%Y-%m-%d").

## Details

- `DateVariable`:

  Calendar or time-based data.

- `IdentifierVariable`:

  ID variables, like subject IDs.

- `LogicalVariable`:

  Indicator variables.

- `NominalVariable`:

  Categorical data.

- `NumericVariable`:

  Continuous or quantitative data.

## Examples

``` r
# NominalVariable
sex <- nominal_variable(
  name = "sex",
  label = "Sex",
  description = "This is a longer description of the variable.",
  category_levels = c("M", "F"),
  category_labels = c("Male", "Female")
)
sex
#> Nominal Variable:
#>   Name                 : sex 
#>   Label                : Sex 
#>   Description          : This is a longer description of the variable. 
#>   Label template       : none 
#>   Description template : none 
#>   Category Levels      : M and F 
#>   Category Labels      : Male and Female 

# NumericVariable
age <- numeric_variable(
  name = "age",
  label = "Age of participant",
  units = "years",
  # In a linear regression model, for example, the coefficient for age will
  # be the change in the mean of the response per 10 year increase in age.
  divby_modeling = 10
)
age
#> Numeric Variable:
#>   Name                 : age 
#>   Label                : Age of participant 
#>   Description          : none 
#>   Label template       : none 
#>   Description template : none 
#>   Units                : years 
#>   Modeling Divisor     : 10 

# DateVariable
visit_date <- date_variable(
  name = "visit_date",
  label = "Visit Date",
  date_format = "%Y-%m-%d"
)
visit_date
#> Date Variable:
#>   Name                 : visit_date 
#>   Label                : Visit Date 
#>   Description          : none 
#>   Label template       : none 
#>   Description template : none 
#>   Date Format          : %Y-%m-%d 

# IdentifierVariable
patient_id <- identifier_variable(
  name = "patient_id",
  label = "Patient Identifier"
)
patient_id
#> Identifier Variable:
#>   Name                 : patient_id 
#>   Label                : Patient Identifier 
#>   Description          : none 
#>   Label template       : none 
#>   Description template : none 

# LogicalVariable
smoker <- logical_variable(
  name = "smoking_status",
  label = "Current smoker",
  # Labels correspond to FALSE and TRUE, respectively
  category_labels = c("Non-smoker", "Smoker")
)
smoker
#> Logical Variable:
#>   Name                 : smoking_status 
#>   Label                : Current smoker 
#>   Description          : none 
#>   Label template       : none 
#>   Description template : none 
#>   Category Levels      : FALSE and TRUE 
#>   Category Labels      : Non-smoker and Smoker 
```
