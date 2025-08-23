# returns from get_unkowns

    Code
      unknown_tbl
    Output
      # A tibble: 18 x 4
         variable  type    name            value
         <chr>     <chr>   <fct>           <chr>
       1 number    Numeric label           none 
       2 integer   Numeric label           none 
       3 logical   Logical label           none 
       4 character Nominal label           none 
       5 factor    Nominal label           none 
       6 date      Date    label           none 
       7 character Nominal category_labels none 
       8 factor    Nominal category_labels none 
       9 number    Numeric units           none 
      10 integer   Numeric units           none 
      11 number    Numeric divby_modeling  none 
      12 integer   Numeric divby_modeling  none 
      13 number    Numeric description     none 
      14 integer   Numeric description     none 
      15 logical   Logical description     none 
      16 character Nominal description     none 
      17 factor    Nominal description     none 
      18 date      Date    description     none 

---

    Code
      get_unknowns(dd_test, as_request = TRUE)
    Output
      A label to use for this variable in reports:
      
        - number = ?
        - integer = ?
        - logical = ?
        - character = ?
        - factor = ?
        - date = ?
      
      Category labels for this variable (labels are shown in reports):
      
        - character: a = ?;  b = ?;  c = ?;  d = ?;  e = ?
        - factor: f = ?;  g = ?;  h = ?;  i = ?;  j = ?
      
      Variable units (e.g., age in years):
      
        - number = ?
        - integer = ? 

---

    Code
      get_unknowns(dd_test, as_code = TRUE)
    Output
      set_labels(number  = "",
                 integer  = "",
                 logical  = "",
                 character  = "",
                 factor  = "",
                 date  = "") %>% 
      set_category_labels(character = c(a = "",
                                        b = "",
                                        c = "",
                                        d = "",
                                        e = ""),
                          factor = c(f = "",
                                     g = "",
                                     h = "",
                                     i = "",
                                     j = "")) %>% 
      set_units(number  = "",
                integer  = "") 

# returns from get_term_key

    Code
      get_term_key(dd_test)
    Output
      # A tibble: 10 x 6
         name      level label reference category_type term      
         <chr>     <chr> <chr> <lgl>     <chr>         <chr>     
       1 character a     a     TRUE      levels        charactera
       2 character b     b     FALSE     levels        characterb
       3 character c     c     FALSE     levels        characterc
       4 character d     d     FALSE     levels        characterd
       5 character e     e     FALSE     levels        charactere
       6 factor    f     f     TRUE      levels        factorf   
       7 factor    g     g     FALSE     levels        factorg   
       8 factor    h     h     FALSE     levels        factorh   
       9 factor    i     i     FALSE     levels        factori   
      10 factor    j     j     FALSE     levels        factorj   

---

    Code
      get_term_key(dd_test_filled)
    Output
      # A tibble: 12 x 6
         name      level label reference category_type term      
         <chr>     <chr> <chr> <lgl>     <chr>         <chr>     
       1 character a     A     TRUE      levels        charactera
       2 character a     A     TRUE      labels        characterA
       3 character b     b     FALSE     levels        characterb
       4 character c     c     FALSE     levels        characterc
       5 character d     d     FALSE     levels        characterd
       6 character e     e     FALSE     levels        charactere
       7 factor    g     GG    TRUE      levels        factorg   
       8 factor    g     GG    TRUE      labels        factorGG  
       9 factor    f     f     FALSE     levels        factorf   
      10 factor    h     h     FALSE     levels        factorh   
      11 factor    i     i     FALSE     levels        factori   
      12 factor    j     j     FALSE     levels        factorj   

---

    Code
      get_term_key(dd_test_filled, adjust_to = data.frame(term = c("characterb",
        "characterc")))
    Output
      # A tibble: 3 x 5
        name      level label reference term      
        <chr>     <chr> <chr> <lgl>     <chr>     
      1 character a     A     TRUE      charactera
      2 character b     b     FALSE     characterb
      3 character c     c     FALSE     characterc

---

    Code
      get_term_key(dd_test_filled, adjust_to = data.frame(term = c("factorf",
        "characterc")))
    Output
      # A tibble: 4 x 5
        name      level label reference term      
        <chr>     <chr> <chr> <lgl>     <chr>     
      1 character a     A     TRUE      charactera
      2 character c     c     FALSE     characterc
      3 factor    g     GG    TRUE      factorg   
      4 factor    f     f     FALSE     factorf   

# returns from get_dictionary

    Code
      get_dictionary(dd_test)
    Output
      # A tibble: 6 x 7
        name    label description units divby_modeling category_levels category_labels
        <chr>   <chr> <chr>       <chr>          <dbl> <named list>    <named list>   
      1 number  <NA>  <NA>        <NA>              NA <NULL>          <NULL>         
      2 integer <NA>  <NA>        <NA>              NA <NULL>          <NULL>         
      3 logical <NA>  <NA>        <NA>              NA <lgl [2]>       <chr [2]>      
      4 charac~ <NA>  <NA>        <NA>              NA <chr [5]>       <chr [5]>      
      5 factor  <NA>  <NA>        <NA>              NA <chr [5]>       <chr [5]>      
      6 date    <NA>  <NA>        <NA>              NA <NULL>          <NULL>         

---

    Code
      get_dictionary(dd_test_filled)
    Output
      # A tibble: 6 x 7
        name    label description units divby_modeling category_levels category_labels
        <chr>   <chr> <chr>       <chr>          <dbl> <named list>    <named list>   
      1 number  A nu~ <NA>        quar~             10 <NULL>          <NULL>         
      2 integer An i~ <NA>        widg~             NA <NULL>          <NULL>         
      3 logical A lo~ <NA>        <NA>              NA <lgl [2]>       <chr [2]>      
      4 charac~ A ch~ <NA>        <NA>              NA <chr [5]>       <chr [5]>      
      5 factor  A fa~ <NA>        <NA>              NA <chr [5]>       <chr [5]>      
      6 date    A da~ <NA>        <NA>              NA <NULL>          <NULL>         

---

    Code
      get_dictionary(dd_test, format_missing = TRUE)
    Output
      # A tibble: 6 x 7
        name    label description units divby_modeling category_levels category_labels
        <chr>   <chr> <chr>       <chr> <chr>          <named list>    <named list>   
      1 number  none  none        none  none           <NULL>          <NULL>         
      2 integer none  none        none  none           <NULL>          <NULL>         
      3 logical none  none        none  none           <lgl [2]>       <chr [2]>      
      4 charac~ none  none        none  none           <chr [5]>       <chr [5]>      
      5 factor  none  none        none  none           <chr [5]>       <chr [5]>      
      6 date    none  none        none  none           <NULL>          <NULL>         

---

    Code
      get_dictionary(dd_test_filled, format_missing = TRUE)
    Output
      # A tibble: 6 x 7
        name    label description units divby_modeling category_levels category_labels
        <chr>   <chr> <chr>       <chr> <chr>          <named list>    <named list>   
      1 number  A nu~ none        quar~ 10             <NULL>          <NULL>         
      2 integer An i~ none        widg~ none           <NULL>          <NULL>         
      3 logical A lo~ none        none  none           <lgl [2]>       <chr [2]>      
      4 charac~ A ch~ none        none  none           <chr [5]>       <chr [5]>      
      5 factor  A fa~ none        none  none           <chr [5]>       <chr [5]>      
      6 date    A da~ none        none  none           <NULL>          <NULL>         

---

    Code
      get_dictionary(dd_test, format_missing = TRUE, format_categories = TRUE)
    Output
      # A tibble: 6 x 8
        name      type    label description units divby_modeling category_levels  
        <chr>     <chr>   <chr> <chr>       <chr> <chr>          <chr>            
      1 number    Numeric none  none        none  none           none             
      2 integer   Numeric none  none        none  none           none             
      3 logical   Logical none  none        none  none           FALSE and TRUE   
      4 character Nominal none  none        none  none           a, b, c, d, and e
      5 factor    Nominal none  none        none  none           f, g, h, i, and j
      6 date      Date    none  none        none  none           none             
      # i 1 more variable: category_labels <chr>

---

    Code
      get_dictionary(dd_test_filled, format_missing = TRUE, format_categories = TRUE)
    Output
      # A tibble: 6 x 8
        name      type    label       description units divby_modeling category_levels
        <chr>     <chr>   <chr>       <chr>       <chr> <chr>          <chr>          
      1 number    Numeric A number    none        quar~ 10             none           
      2 integer   Numeric An integer  none        widg~ none           none           
      3 logical   Logical A logical   none        none  none           FALSE and TRUE 
      4 character Nominal A character none        none  none           a, b, c, d, an~
      5 factor    Nominal A factor    none        none  none           g, f, h, i, an~
      6 date      Date    A date      none        none  none           none           
      # i 1 more variable: category_labels <chr>

