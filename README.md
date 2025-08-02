
<!-- README.md is generated from README.Rmd. Please edit that file -->

# perinary

<!-- badges: start -->

[![R-CMD-check](https://github.com/bcjaeger/perinary/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcjaeger/perinary/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bcjaeger/perinary/graph/badge.svg)](https://app.codecov.io/gh/bcjaeger/perinary)
<!-- badges: end -->

A grammar for data dictionaries with three primary verbs:

1.  `set`: initialize or modify meta data
2.  `get`: retrieve meta data
3.  `infuse`: apply meta data

Dictionaries are often developed outside of a computing session and
stored as static files, making it inconvenient to apply meta data from
the dictionary to your data. `perinary` aims to bring the magic of a
dictionary to your R session, providing dynamic and intuitive methods to
insert meta data into your results right when you need it.

## Installation

You can install the development version of `perinary` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("perisphere-rwe/perinary")
```

## Data dictionaries

Dictionaries help organize pertinent information about analytic
variables, putting information about each variable into tables and
figures using consistent labels and incorporating additional information
when appropriate.

There are two ways to initialize a dictionary. You can build them using
`NumericVariable` and `NominalVariable` objects (not shown here, see
vignette), but this can be tedious if you have a lot of variables. For
most cases, you’ll want to use the `as_data_dictionary()` to create a
starter dictionary from a given dataset.

``` r

library(perinary)
library(tidyverse)
library(palmerpenguins)

data_peng <- penguins %>% 
  select(species, sex, body_mass_g, bill_length_mm, bill_depth_mm)

dd_peng <- as_data_dictionary(data_peng)

dd_peng
#> Data Dictionary:
#> # A tibble: 5 × 7
#>   name    label description units divby_modeling category_levels category_labels
#>   <chr>   <chr> <chr>       <chr> <chr>          <chr>           <chr>          
#> 1 species none  none        none  none           Adelie, Chinst… Adelie, Chinst…
#> 2 sex     none  none        none  none           female and male female and male
#> 3 body_m… none  none        none  none           none            none           
#> 4 bill_l… none  none        none  none           none            none           
#> 5 bill_d… none  none        none  none           none            none
```

### Tell me what I don’t know

Our dictionary is initialized, but it doesn’t contain complete
information yet. Where should we start? Use the function
`get_unknowns()` to clarify what relevant information is missing. This
function returns a `tibble` by default, but if you set
`as_request = TRUE`, it provides a bullet point list that is easier to
read. If you work with subject matter experts, you can also send this
text to them and ask for help filling in the gaps.

``` r
get_unknowns(dd_peng, as_request = TRUE)
#> A label to use for this variable in reports:
#> 
#>   - species = ?
#>   - sex = ?
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
#> 
#> Category labels for this variable (labels are shown in reports):
#> 
#>   - species: Adelie = ?;  Chinstrap = ?;  Gentoo = ?
#>   - sex: female = ?;  male = ?
#> 
#> Variable units (e.g., age in years):
#> 
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
```

And if you want to get straight to filling these unknowns in using
dictionary functions (described in the next section), `get_unknowns`
gives you a full template to fill in when you specify `as_code = TRUE`:

``` r
get_unknowns(dd_peng, as_code = TRUE)
#> set_labels(species  = "",
#>            sex  = "",
#>            body_mass_g  = "",
#>            bill_length_mm  = "",
#>            bill_depth_mm  = "") %>% 
#> set_factor_labels(species = c(Adelie = "",
#>                               Chinstrap = "",
#>                               Gentoo = ""),
#>                   sex = c(female = "",
#>                           male = "")) %>% 
#> set_units(body_mass_g  = "",
#>           bill_length_mm  = "",
#>           bill_depth_mm  = "")
```

### Modify dictionary values

Once we have this information ready to embed in the dictionary, we can
use `perinary`’s family of `set` functions:

``` r

dd_peng <- dd_peng %>% 
  set_labels(species = "Species",
             sex = "Sex",
             bill_length_mm = "Bill length",
             bill_depth_mm = "Bill depth") %>% 
  set_units(bill_length_mm = "mm",
            bill_depth_mm = "mm",
            body_mass_g = "grams") %>% 
  set_divby_modeling(bill_length_mm = 5,
                     bill_depth_mm = 5)
```

#### Identifier variables

An identifier variable uniquely defines sampling units in a data set. An
identifier variable can cause disruption in a data dictionary if it is
treated like a factor. For example, we don’t want to supply labels for
each level of an identifier, and we don’t want to summarize data for
each level of an identifier. To demonstrate the problem identifiers can
pose, here’s what would have happened in our call to `get_unknowns()` if
we had an unmanaged identifier variable in the dictionary.

``` r

data_peng %>% 
  mutate(penguid_id = as.character(seq(n()))) %>% 
  as_data_dictionary() %>% 
  get_unknowns(as_request = TRUE)
#> A label to use for this variable in reports:
#> 
#>   - species = ?
#>   - sex = ?
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
#>   - penguid_id = ?
#> 
#> Category labels for this variable (labels are shown in reports):
#> 
#>   - species: Adelie = ?;  Chinstrap = ?;  Gentoo = ?
#>   - sex: female = ?;  male = ?
#>   - penguid_id: 1 = ?;  2 = ?;  3 = ?;  4 = ?;  5 = ?;  6 = ?;  7 = ?;  8 = ?;  9 = ?;  10 = ?;  11 = ?;  12 = ?;  13 = ?;  14 = ?;  15 = ?;  16 = ?;  17 = ?;  18 = ?;  19 = ?;  20 = ?;  21 = ?;  22 = ?;  23 = ?;  24 = ?;  25 = ?;  26 = ?;  27 = ?;  28 = ?;  29 = ?;  30 = ?;  31 = ?;  32 = ?;  33 = ?;  34 = ?;  35 = ?;  36 = ?;  37 = ?;  38 = ?;  39 = ?;  40 = ?;  41 = ?;  42 = ?;  43 = ?;  44 = ?;  45 = ?;  46 = ?;  47 = ?;  48 = ?;  49 = ?;  50 = ?;  51 = ?;  52 = ?;  53 = ?;  54 = ?;  55 = ?;  56 = ?;  57 = ?;  58 = ?;  59 = ?;  60 = ?;  61 = ?;  62 = ?;  63 = ?;  64 = ?;  65 = ?;  66 = ?;  67 = ?;  68 = ?;  69 = ?;  70 = ?;  71 = ?;  72 = ?;  73 = ?;  74 = ?;  75 = ?;  76 = ?;  77 = ?;  78 = ?;  79 = ?;  80 = ?;  81 = ?;  82 = ?;  83 = ?;  84 = ?;  85 = ?;  86 = ?;  87 = ?;  88 = ?;  89 = ?;  90 = ?;  91 = ?;  92 = ?;  93 = ?;  94 = ?;  95 = ?;  96 = ?;  97 = ?;  98 = ?;  99 = ?;  100 = ?;  101 = ?;  102 = ?;  103 = ?;  104 = ?;  105 = ?;  106 = ?;  107 = ?;  108 = ?;  109 = ?;  110 = ?;  111 = ?;  112 = ?;  113 = ?;  114 = ?;  115 = ?;  116 = ?;  117 = ?;  118 = ?;  119 = ?;  120 = ?;  121 = ?;  122 = ?;  123 = ?;  124 = ?;  125 = ?;  126 = ?;  127 = ?;  128 = ?;  129 = ?;  130 = ?;  131 = ?;  132 = ?;  133 = ?;  134 = ?;  135 = ?;  136 = ?;  137 = ?;  138 = ?;  139 = ?;  140 = ?;  141 = ?;  142 = ?;  143 = ?;  144 = ?;  145 = ?;  146 = ?;  147 = ?;  148 = ?;  149 = ?;  150 = ?;  151 = ?;  152 = ?;  153 = ?;  154 = ?;  155 = ?;  156 = ?;  157 = ?;  158 = ?;  159 = ?;  160 = ?;  161 = ?;  162 = ?;  163 = ?;  164 = ?;  165 = ?;  166 = ?;  167 = ?;  168 = ?;  169 = ?;  170 = ?;  171 = ?;  172 = ?;  173 = ?;  174 = ?;  175 = ?;  176 = ?;  177 = ?;  178 = ?;  179 = ?;  180 = ?;  181 = ?;  182 = ?;  183 = ?;  184 = ?;  185 = ?;  186 = ?;  187 = ?;  188 = ?;  189 = ?;  190 = ?;  191 = ?;  192 = ?;  193 = ?;  194 = ?;  195 = ?;  196 = ?;  197 = ?;  198 = ?;  199 = ?;  200 = ?;  201 = ?;  202 = ?;  203 = ?;  204 = ?;  205 = ?;  206 = ?;  207 = ?;  208 = ?;  209 = ?;  210 = ?;  211 = ?;  212 = ?;  213 = ?;  214 = ?;  215 = ?;  216 = ?;  217 = ?;  218 = ?;  219 = ?;  220 = ?;  221 = ?;  222 = ?;  223 = ?;  224 = ?;  225 = ?;  226 = ?;  227 = ?;  228 = ?;  229 = ?;  230 = ?;  231 = ?;  232 = ?;  233 = ?;  234 = ?;  235 = ?;  236 = ?;  237 = ?;  238 = ?;  239 = ?;  240 = ?;  241 = ?;  242 = ?;  243 = ?;  244 = ?;  245 = ?;  246 = ?;  247 = ?;  248 = ?;  249 = ?;  250 = ?;  251 = ?;  252 = ?;  253 = ?;  254 = ?;  255 = ?;  256 = ?;  257 = ?;  258 = ?;  259 = ?;  260 = ?;  261 = ?;  262 = ?;  263 = ?;  264 = ?;  265 = ?;  266 = ?;  267 = ?;  268 = ?;  269 = ?;  270 = ?;  271 = ?;  272 = ?;  273 = ?;  274 = ?;  275 = ?;  276 = ?;  277 = ?;  278 = ?;  279 = ?;  280 = ?;  281 = ?;  282 = ?;  283 = ?;  284 = ?;  285 = ?;  286 = ?;  287 = ?;  288 = ?;  289 = ?;  290 = ?;  291 = ?;  292 = ?;  293 = ?;  294 = ?;  295 = ?;  296 = ?;  297 = ?;  298 = ?;  299 = ?;  300 = ?;  301 = ?;  302 = ?;  303 = ?;  304 = ?;  305 = ?;  306 = ?;  307 = ?;  308 = ?;  309 = ?;  310 = ?;  311 = ?;  312 = ?;  313 = ?;  314 = ?;  315 = ?;  316 = ?;  317 = ?;  318 = ?;  319 = ?;  320 = ?;  321 = ?;  322 = ?;  323 = ?;  324 = ?;  325 = ?;  326 = ?;  327 = ?;  328 = ?;  329 = ?;  330 = ?;  331 = ?;  332 = ?;  333 = ?;  334 = ?;  335 = ?;  336 = ?;  337 = ?;  338 = ?;  339 = ?;  340 = ?;  341 = ?;  342 = ?;  343 = ?;  344 = ?
#> 
#> Variable units (e.g., age in years):
#> 
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
```

The issue is that we don’t want or need to supply category labels for
`penguin_id`, so it isn’t helpful for these to be included in our list
of unknowns. The fix is to designate `penguin_id` (or generally any
identifier variables) as an identifier variable, which should take care
of all potential downstream issues.

``` r

data_peng %>% 
  mutate(penguin_id = as.character(seq(n()))) %>% 
  as_data_dictionary() %>% 
  set_identifiers(penguin_id) %>% 
  get_unknowns(as_request = TRUE)
#> A label to use for this variable in reports:
#> 
#>   - species = ?
#>   - sex = ?
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
#>   - penguin_id = ?
#> 
#> Category labels for this variable (labels are shown in reports):
#> 
#>   - species: Adelie = ?;  Chinstrap = ?;  Gentoo = ?
#>   - sex: female = ?;  male = ?
#> 
#> Variable units (e.g., age in years):
#> 
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
```

#### Modifying factors

Modify factor labels, changing one or more labels in an existing
variable, with `set_category_labels()`:

``` r

dd_peng$variables$sex
#> Nominal Variable:
#>   Name               : sex 
#>   Label              : Sex 
#>   Description        : none 
#>   Category Levels    : female and male 
#>   Category Labels    : female and male

dd_peng <- dd_peng %>% 
  set_category_labels(sex = c(female = "F", male = "M"))

dd_peng$variables$sex
#> Nominal Variable:
#>   Name               : sex 
#>   Label              : Sex 
#>   Description        : none 
#>   Category Levels    : female and male 
#>   Category Labels    : F and M
```

Modify factor order, moving one or more levels to the front, with
`set_category_order()`:

``` r

dd_peng <- dd_peng %>% 
  set_category_order(sex = c("female"))

dd_peng$variables$sex
#> Nominal Variable:
#>   Name               : sex 
#>   Label              : Sex 
#>   Description        : none 
#>   Category Levels    : female and male 
#>   Category Labels    : F and M
```

### Apply dictionary information

Data dictionaries have a method called `recode()`, which leverages the
`dplyr::recode()` function in combination with information stored in the
dictionary to provide a ‘smart’ recoder. The ‘smart’ part is that this
recode function doesn’t require you to provide recode values. Instead,
it looks for appropriate recode values in the dictionary.

``` r

# This function is embedded in the dictionary itself, so it's called by 
# typing the name of the dictionary, followed by `$recode(x)`, where 
# `x` is the vector you intend to recode. For example:

data_peng %>% 
  mutate(sex = dd_peng$recode(sex))
#> # A tibble: 344 × 5
#>    species sex   body_mass_g bill_length_mm bill_depth_mm
#>    <fct>   <fct>       <int>          <dbl>         <dbl>
#>  1 Adelie  M            3750           39.1          18.7
#>  2 Adelie  F            3800           39.5          17.4
#>  3 Adelie  F            3250           40.3          18  
#>  4 Adelie  <NA>           NA           NA            NA  
#>  5 Adelie  F            3450           36.7          19.3
#>  6 Adelie  M            3650           39.3          20.6
#>  7 Adelie  F            3625           38.9          17.8
#>  8 Adelie  M            4675           39.2          19.6
#>  9 Adelie  <NA>         3475           34.1          18.1
#> 10 Adelie  <NA>         4250           42            20.2
#> # ℹ 334 more rows
```

This works both for variable names and variable categories. For example,
pivoting our data to a longer format will move several variable names
into a new column called `name`, and this column can be recoded just
like we did above.

``` r

data_peng %>% 
  pivot_longer(starts_with("bill_")) %>% 
  mutate(name = dd_peng$recode(name))
#> # A tibble: 688 × 5
#>    species sex    body_mass_g name        value
#>    <fct>   <fct>        <int> <chr>       <dbl>
#>  1 Adelie  male          3750 Bill length  39.1
#>  2 Adelie  male          3750 Bill depth   18.7
#>  3 Adelie  female        3800 Bill length  39.5
#>  4 Adelie  female        3800 Bill depth   17.4
#>  5 Adelie  female        3250 Bill length  40.3
#>  6 Adelie  female        3250 Bill depth   18  
#>  7 Adelie  <NA>            NA Bill length  NA  
#>  8 Adelie  <NA>            NA Bill depth   NA  
#>  9 Adelie  female        3450 Bill length  36.7
#> 10 Adelie  female        3450 Bill depth   19.3
#> # ℹ 678 more rows
```

If needed, there is a more direct way to get recode information for
specific parts of the dictionary:

``` r

# in case the smart dictionary recoder fails, you can fall back 
# on the more reliable methods get_variable_recoder and 
# get_level_recoder, which are also public methods for dictionaries

recode_bills <- dd_peng$get_variable_recoder(name = c("bill_length_mm",
                                                      "bill_depth_mm"))

recode_sex <- dd_peng$get_level_recoder(name = 'sex')

data_peng %>% 
  pivot_longer(starts_with("bill_")) %>% 
  mutate(name = recode(name, !!!recode_bills),
         sex = recode(sex, !!!recode_sex))
#> # A tibble: 688 × 5
#>    species sex   body_mass_g name        value
#>    <fct>   <fct>       <int> <chr>       <dbl>
#>  1 Adelie  M            3750 Bill length  39.1
#>  2 Adelie  M            3750 Bill depth   18.7
#>  3 Adelie  F            3800 Bill length  39.5
#>  4 Adelie  F            3800 Bill depth   17.4
#>  5 Adelie  F            3250 Bill length  40.3
#>  6 Adelie  F            3250 Bill depth   18  
#>  7 Adelie  <NA>           NA Bill length  NA  
#>  8 Adelie  <NA>           NA Bill depth   NA  
#>  9 Adelie  F            3450 Bill length  36.7
#> 10 Adelie  F            3450 Bill depth   19.3
#> # ℹ 678 more rows
```

### Attach all information at once

`infuse_dictionary()` puts all the relevant information from a data
dictionary into an existing dataset. This can smooth out your code when
you use packages that automatically incorporate labels into their
outputs, such as `gtsummary`. In the example below, we infuse our data
with the dictionary we created and specify that we want to format
continuous variables using their modeling units (e.g., bill length is
modeled per 10 mm). Note that when you infuse data with dictionaries and
specify `units = 'model'`, the corresponding variables will be divided
by their designated `divby_model` value and new columns will be created.
If you don’t want new columns, you can specify `divby_suffix = NULL`
when you infuse.

``` r

library(gtsummary)

data_infused <- data_peng %>% 
  infuse_dictionary(dd_peng, units = 'model', divby_suffix = NULL)

fit <- lm(formula = body_mass_g ~ ., data = data_infused)

tbl_regression(fit)
```

``` r
knitr::include_graphics('img/screen-regression_table.png')
```

<img src="img/screen-regression_table.png" width="100%" />
