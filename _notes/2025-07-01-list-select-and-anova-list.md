---
title: "Selecting models from lists and running anova()"
date: 2025-07-01
tags: [r, programming, tidyselect, modeling]
---

I was making a notebook with a list of related models, like


``` r
m <- list(
  mpg_wt = lm(mpg ~ wt, mtcars),
  mpg_disp = lm(mpg ~ disp, mtcars),
  mpg_disp_wt = lm(mpg ~ disp + wt, mtcars),
  mpg_disp_wt_int = lm(mpg ~ disp * wt, mtcars),
  hp_wt = lm(hp ~ wt, mtcars),
  hp_disp = lm(hp ~ disp, mtcars),
  hp_disp_wt = lm(hp ~ disp + wt, mtcars),
  hp_disp_wt_int = lm(hp ~ disp * wt, mtcars)
)
```

I want to select a subset of models and `anova()` them, so I made a quick
tidyselect function and an S3 method method of lists with `anova()`:


``` r
library(tidyverse)
#> Warning: package 'tibble' was built under R version 4.5.2
#> Warning: package 'tidyr' was built under R version 4.5.2
#> Warning: package 'readr' was built under R version 4.5.2
#> Warning: package 'purrr' was built under R version 4.5.2
#> Warning: package 'dplyr' was built under R version 4.5.2
#> Warning: package 'stringr' was built under R version 4.5.2
#> Warning: package 'lubridate' was built under R version 4.5.2
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.0     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

list_select <- function(l, ...) {
  pos <- tidyselect::eval_select(c(...), l)
  rlang::set_names(l[pos], names(pos))
}

anova.list <- function(object) {
  do.call(anova, unname(object))
}

m |> 
  list_select(starts_with("mpg")) |> 
  list_select(matches("disp")) |> 
  anova()
#> Analysis of Variance Table
#> 
#> Model 1: mpg ~ disp
#> Model 2: mpg ~ disp + wt
#> Model 3: mpg ~ disp * wt
#>   Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
#> 1     30 317.16                                
#> 2     29 246.68  1    70.476 11.694 0.001942 **
#> 3     28 168.75  1    77.934 12.931 0.001227 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(m$mpg_disp, m$mpg_disp_wt, m$mpg_disp_wt_int)
#> Analysis of Variance Table
#> 
#> Model 1: mpg ~ disp
#> Model 2: mpg ~ disp + wt
#> Model 3: mpg ~ disp * wt
#>   Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
#> 1     30 317.16                                
#> 2     29 246.68  1    70.476 11.694 0.001942 **
#> 3     28 168.75  1    77.934 12.931 0.001227 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

`list_select()` is extremely basic. It's based on the examples in the
`tidyselect::eval_select()` documentation. 

For `list_select()`, the closest option available from purrr (the
tidyverse package for working on lists) is `purrr::keep_at()`:


``` r
m |> 
  purrr::keep_at(function(x) startsWith(x, "mpg")) |> 
  str(max.level = 1)
#> List of 4
#>  $ mpg_wt         :List of 12
#>   ..- attr(*, "class")= chr "lm"
#>  $ mpg_disp       :List of 12
#>   ..- attr(*, "class")= chr "lm"
#>  $ mpg_disp_wt    :List of 12
#>   ..- attr(*, "class")= chr "lm"
#>  $ mpg_disp_wt_int:List of 12
#>   ..- attr(*, "class")= chr "lm"
```

