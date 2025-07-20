# Printed output

    Code
      numeric_variable(name = "age", label = "Age of participant", units = "years",
        divby_modeling = 10)
    Output
      Numeric Variable:
        Name               : age 
        Label              : Age of participant 
        Description        : none 
        Units              : years 
        Modeling Divisor   : 10 

---

    Code
      nominal_variable(name = "age_group", label = "Age group", description = "Ages of 0 to < 50, 50 to < 60, and ≥ 60 years",
        category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
        category_labels = c("0 to < 50", "50 to < 60", "≥ 60"))
    Output
      Nominal Variable:
        Name               : age_group 
        Label              : Age group 
        Description        : Ages of 0 to < 50, 50 to < 60, and ≥ 60 years 
        Category Levels    : age_lt_50, age_gteq_50_lt_60, and age_gteq_60 
        Category Labels    : 0 to < 50, 50 to < 60, and ≥ 60 

