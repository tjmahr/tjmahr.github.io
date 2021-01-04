---
title: "Why is using list() critical for .dots = setNames() uses in dplyr?"
excerpt: Because the functions expect a list of expressions.
tags: 
 - dplyr
 - nonstandard evaluation
 - r
 - stack exchange
---

I wrote [an answer](https://stackoverflow.com/questions/36067533/why-is-using-list-critical-for-dots-setnames-uses-in-dplyr/36168162#36168162) 
about why `setNames()` shows up sometimes in standard evaluation with dplyr. 

My explanation turned into a mini-tutorial on why those standard evaluation
functions have a `.dots` argument. The basic idea is that the usual variadic
argument `...` is a series of expressions that get evaluated inside of the
dataframe.


```r
library(dplyr)

# standardize and round
z_round <- . %>% scale() %>% as.numeric() %>% round(2)

# The two expressions defining zMPG, zHP are the `...`
mtcars %>% 
  mutate_(
    zMPG = ~ z_round(mpg), 
    zHP = ~ z_round(hp)
  ) %>%
  as_tibble()
#> Warning: `mutate_()` is deprecated as of dplyr 0.7.0.
#> Please use `mutate()` instead.
#> See vignette('programming') for more help
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
#> # A tibble: 32 x 13
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb  zMPG   zHP
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4  0.15 -0.54
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4  0.15 -0.54
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1  0.45 -0.78
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1  0.22 -0.54
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2 -0.23  0.41
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1 -0.33 -0.61
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4 -0.96  1.43
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2  0.72 -1.24
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2  0.45 -0.75
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4 -0.15 -0.35
#> # ... with 22 more rows
```

If we programmatically assemble or manipulate those expressions before calling
`mutate_()`, we can't use that `...`, because we have a *list* of expressions,
not a series of individual expressions. We use the `.dots` argument instead.


```r
exps <- list(
  zMPG = ~ z_round(mpg), 
  zHP = ~ z_round(hp)
)

mtcars %>% 
  mutate_(exps)
#> Error: Can't convert a list to a quosure.

mtcars %>% 
  mutate_(.dots = exps) %>% 
  as_tibble()
#> # A tibble: 32 x 13
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb  zMPG   zHP
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4  0.15 -0.54
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4  0.15 -0.54
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1  0.45 -0.78
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1  0.22 -0.54
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2 -0.23  0.41
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1 -0.33 -0.61
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4 -0.96  1.43
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2  0.72 -1.24
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2  0.45 -0.75
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4 -0.15 -0.35
#> # ... with 22 more rows
```

### [*Jan. 4, 2021*] Hello from the future

Here is how you would go about this in 2021 in the tidy evaluation framework.
Use an expression-list or a quosure-list and splice them in with `!!!`.


```r
exprs <- rlang::exprs(
  zMPG = z_round(mpg), 
  zHP = z_round(hp)
)

# These are just expressions
exprs
#> $zMPG
#> z_round(mpg)
#> 
#> $zHP
#> z_round(hp)

mtcars %>% 
  mutate(!!! exprs) %>% 
  as_tibble()
#> # A tibble: 32 x 13
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb  zMPG   zHP
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4  0.15 -0.54
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4  0.15 -0.54
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1  0.45 -0.78
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1  0.22 -0.54
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2 -0.23  0.41
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1 -0.33 -0.61
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4 -0.96  1.43
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2  0.72 -1.24
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2  0.45 -0.75
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4 -0.15 -0.35
#> # ... with 22 more rows

quosures <- quos(
  zMPG = z_round(mpg), 
  zHP = z_round(hp)
)

# These are expression-environment pairs
quosures
#> <list_of<quosure>>
#> 
#> $zMPG
#> <quosure>
#> expr: ^z_round(mpg)
#> env:  00000000165A9EC8
#> 
#> $zHP
#> <quosure>
#> expr: ^z_round(hp)
#> env:  00000000165A9EC8

mtcars %>% 
  mutate(!!! quosures) %>% 
  as_tibble()
#> # A tibble: 32 x 13
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb  zMPG   zHP
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4  0.15 -0.54
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4  0.15 -0.54
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1  0.45 -0.78
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1  0.22 -0.54
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2 -0.23  0.41
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1 -0.33 -0.61
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4 -0.96  1.43
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2  0.72 -1.24
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2  0.45 -0.75
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4 -0.15 -0.35
#> # ... with 22 more rows
```

***

*Last knitted on 2021-01-04. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2016-03-23-setnames-dots.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info ---------------------------------------------------------------
    #>  setting  value                       
    #>  version  R version 4.0.3 (2020-10-10)
    #>  os       Windows 10 x64              
    #>  system   x86_64, mingw32             
    #>  ui       RTerm                       
    #>  language (EN)                        
    #>  collate  English_United States.1252  
    #>  ctype    English_United States.1252  
    #>  tz       America/Chicago             
    #>  date     2021-01-04                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package     * version date       lib source        
    #>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
    #>  cli           2.2.0   2020-11-20 [1] CRAN (R 4.0.3)
    #>  crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.2)
    #>  dplyr       * 1.0.2   2020-08-18 [1] CRAN (R 4.0.2)
    #>  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)
    #>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.2)
    #>  fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.2)
    #>  generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.3)
    #>  git2r         0.27.1  2020-05-03 [1] CRAN (R 4.0.2)
    #>  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    #>  knitr       * 1.30    2020-09-22 [1] CRAN (R 4.0.2)
    #>  lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.2)
    #>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
    #>  pillar        1.4.7   2020-11-20 [1] CRAN (R 4.0.3)
    #>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
    #>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
    #>  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.2)
    #>  rlang         0.4.9   2020-11-26 [1] CRAN (R 4.0.3)
    #>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
    #>  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)
    #>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    #>  tibble        3.0.4   2020-10-12 [1] CRAN (R 4.0.3)
    #>  tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.2)
    #>  utf8          1.1.4   2018-05-24 [1] CRAN (R 4.0.2)
    #>  vctrs         0.3.6   2020-12-17 [1] CRAN (R 4.0.3)
    #>  withr         2.3.0   2020-09-22 [1] CRAN (R 4.0.2)
    #>  xfun          0.19    2020-10-30 [1] CRAN (R 4.0.3)
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.3/library
    ```
