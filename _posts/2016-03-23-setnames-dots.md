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
#> Warning: `mutate_()` was deprecated in dplyr 0.7.0.
#> Please use `mutate()` instead.
#> See vignette('programming') for more help
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
#> # A tibble: 32 × 13
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
#> # … with 22 more rows
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
#> Error in `compat_lazy()`:
#> ! Can't convert a list to a quosure.

mtcars %>% 
  mutate_(.dots = exps) %>% 
  as_tibble()
#> # A tibble: 32 × 13
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
#> # … with 22 more rows
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
#> # A tibble: 32 × 13
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
#> # … with 22 more rows

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
#> env:  0x0000016b09228918
#> 
#> $zHP
#> <quosure>
#> expr: ^z_round(hp)
#> env:  0x0000016b09228918

mtcars %>% 
  mutate(!!! quosures) %>% 
  as_tibble()
#> # A tibble: 32 × 13
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
#> # … with 22 more rows
```





***

*Last knitted on 2022-05-26. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2016-03-23-setnames-dots.Rmd).*[^si] 

[^si]: 
    
    ```r
    .session_info
    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.2.0 (2022-04-22 ucrt)
    #>  os       Windows 10 x64 (build 22000)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language (EN)
    #>  collate  English_United States.utf8
    #>  ctype    English_United States.utf8
    #>  tz       America/Chicago
    #>  date     2022-05-26
    #>  pandoc   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package     * version date (UTC) lib source
    #>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.2.0)
    #>  cli           3.3.0   2022-04-25 [1] CRAN (R 4.2.0)
    #>  crayon        1.5.1   2022-03-26 [1] CRAN (R 4.2.0)
    #>  DBI           1.1.2   2021-12-20 [1] CRAN (R 4.2.0)
    #>  dplyr       * 1.0.9   2022-04-28 [1] CRAN (R 4.2.0)
    #>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.2.0)
    #>  evaluate      0.15    2022-02-18 [1] CRAN (R 4.2.0)
    #>  fansi         1.0.3   2022-03-24 [1] CRAN (R 4.2.0)
    #>  generics      0.1.2   2022-01-31 [1] CRAN (R 4.2.0)
    #>  git2r         0.30.1  2022-03-16 [1] CRAN (R 4.2.0)
    #>  glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
    #>  here          1.0.1   2020-12-13 [1] CRAN (R 4.2.0)
    #>  knitr       * 1.39    2022-04-26 [1] CRAN (R 4.2.0)
    #>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.2.0)
    #>  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
    #>  pillar        1.7.0   2022-02-01 [1] CRAN (R 4.2.0)
    #>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
    #>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.2.0)
    #>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
    #>  ragg          1.2.2   2022-02-21 [1] CRAN (R 4.2.0)
    #>  rlang         1.0.2   2022-03-04 [1] CRAN (R 4.2.0)
    #>  rprojroot     2.0.3   2022-04-02 [1] CRAN (R 4.2.0)
    #>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.2.0)
    #>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
    #>  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.2.0)
    #>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.2.0)
    #>  systemfonts   1.0.4   2022-02-11 [1] CRAN (R 4.2.0)
    #>  textshaping   0.3.6   2021-10-13 [1] CRAN (R 4.2.0)
    #>  tibble        3.1.7   2022-05-03 [1] CRAN (R 4.2.0)
    #>  tidyselect    1.1.2   2022-02-21 [1] CRAN (R 4.2.0)
    #>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.2.0)
    #>  vctrs         0.4.1   2022-04-13 [1] CRAN (R 4.2.0)
    #>  xfun          0.31    2022-05-10 [1] CRAN (R 4.2.0)
    #> 
    #>  [1] C:/Users/trist/AppData/Local/R/win-library/4.2
    #>  [2] C:/Program Files/R/R-4.2.0/library
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
    ```
