# append terms

    Code
      append_term_key(fit_trafo, dictionary = dd_iris)
    Output
      # A tibble: 7 x 9
        name        level label reference term  estimate std.error statistic   p.value
        <chr>       <chr> <chr> <lgl>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>
      1 (Intercept) <NA>  <NA>  FALSE     (Int~    1.45     0.281       5.14  8.68e- 7
      2 Sepal.Width <NA>  <NA>  FALSE     Sepa~    0.496    0.0861      5.76  4.87e- 8
      3 Petal.Leng~ <NA>  <NA>  FALSE     Peta~    0.829    0.0685     12.1   1.07e-23
      4 Petal.Width <NA>  <NA>  FALSE     Peta~   -0.315    0.151      -2.08  3.89e- 2
      5 Species     vers~ Versi TRUE      Spec~   NA       NA          NA    NA       
      6 Species     seto~ seto~ FALSE     Spec~    0.724    0.240       3.01  3.06e- 3
      7 Species     virg~ virg~ FALSE     Spec~   -0.300    0.119      -2.52  1.28e- 2

