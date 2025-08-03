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
      nominal_variable(name = "age_group", label = "Age group", description = "Ages of 0 to < 50, 50 to < 60, and >=60 years",
        category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
        category_labels = c("0 to < 50", "50 to < 60", ">=60"))
    Output
      Nominal Variable:
        Name               : age_group 
        Label              : Age group 
        Description        : Ages of 0 to < 50, 50 to < 60, and >=60 years 
        Category Levels    : age_lt_50, age_gteq_50_lt_60, and age_gteq_60 
        Category Labels    : 0 to < 50, 50 to < 60, and >=60 

---

    Code
      as_data_dictionary(data.frame(a = 1, b = "cat")) %>% set_labels(a = "example",
        b = "categorical example") %>% set_units(a = "years") %>% set_descriptions(a = "A variable used for examples") %>%
        set_category_labels(b = c(cat = "A small lion"))
    Output
      Data Dictionary:
      # A tibble: 2 x 7
        name  label   description units divby_modeling category_levels category_labels
        <chr> <chr>   <chr>       <chr> <chr>          <chr>           <chr>          
      1 a     example A variable~ years none           none            none           
      2 b     catego~ none        none  none           cat             A small lion   

# identifiers won't blow up the screen

    Code
      set_identifiers(dd, character, factor) %>% get_unknowns(as_request = TRUE)
    Output
      A label to use for this variable in reports:
      
        - number = ?
        - integer = ?
        - logical = ?
        - character = ?
        - factor = ?
        - date = ?
      
      Variable units (e.g., age in years):
      
        - number = ?
        - integer = ? 

