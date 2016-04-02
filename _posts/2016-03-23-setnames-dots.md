---
layout: post
title: "Why is using list() critical for .dots = setNames() uses in dplyr?"
excerpt: "Why is using list() critical for .dots = setNames() uses in dplyr?"
tags: [link post]
link: https://stackoverflow.com/questions/36067533/why-is-using-list-critical-for-dots-setnames-uses-in-dplyr/36168162#36168162
share: false
---

I wrote [an answer](https://stackoverflow.com/questions/36067533/why-is-using-list-critical-for-dots-setnames-uses-in-dplyr/36168162#36168162) about why `setNames()` shows up sometimes in standard evaluation with dplyr. 

My explanation turned into a mini-tutorial on why those standard evaluation functions have a `.dots` argument. The basic idea is that the usual variadic argument `...` is a series of expressions that get evaluated inside of the dataframe. 

```r
library("dplyr")

# standardize and round
z_round <- . %>% scale %>% as.numeric %>% round(2)

# The two expressions defining zSL, zSW are the `...`
iris %>% 
  mutate_(zSL = ~ z_round(Sepal.Length), 
          zSW = ~ z_round(Sepal.Width)) %>%
  tbl_df
#> Source: local data frame [150 x 7]
#> 
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species   zSL   zSW
#>           (dbl)       (dbl)        (dbl)       (dbl)  (fctr) (dbl) (dbl)
#> 1           5.1         3.5          1.4         0.2  setosa -0.90  1.02
#> 2           4.9         3.0          1.4         0.2  setosa -1.14 -0.13
#> 3           4.7         3.2          1.3         0.2  setosa -1.38  0.33
#> 4           4.6         3.1          1.5         0.2  setosa -1.50  0.10
#> 5           5.0         3.6          1.4         0.2  setosa -1.02  1.25
#> 6           5.4         3.9          1.7         0.4  setosa -0.54  1.93
#> 7           4.6         3.4          1.4         0.3  setosa -1.50  0.79
#> 8           5.0         3.4          1.5         0.2  setosa -1.02  0.79
#> 9           4.4         2.9          1.4         0.2  setosa -1.74 -0.36
#> 10          4.9         3.1          1.5         0.1  setosa -1.14  0.10
```

If we programmatically assemble or manipulate those expressions before calling `mutate_`, we can't use that `...`, because we have a _list_ of expressions, not a series of individual expressions. We use the `.dots` argument instead.

```r
exps <- list(
  zSL = ~ z_round(Sepal.Length), 
  zSW = ~ z_round(Sepal.Width)
)

iris %>% mutate_(exps)
#> Error in UseMethod("as.lazy") : 
#>   no applicable method for 'as.lazy' applied to an object of class "list"

iris %>% mutate_(.dots = exps) %>% tbl_df
#> Source: local data frame [150 x 7]
#> 
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species   zSL   zSW
#>           (dbl)       (dbl)        (dbl)       (dbl)  (fctr) (dbl) (dbl)
#> 1           5.1         3.5          1.4         0.2  setosa -0.90  1.02
#> 2           4.9         3.0          1.4         0.2  setosa -1.14 -0.13
#> 3           4.7         3.2          1.3         0.2  setosa -1.38  0.33
#> 4           4.6         3.1          1.5         0.2  setosa -1.50  0.10
#> 5           5.0         3.6          1.4         0.2  setosa -1.02  1.25
#> 6           5.4         3.9          1.7         0.4  setosa -0.54  1.93
#> 7           4.6         3.4          1.4         0.3  setosa -1.50  0.79
#> 8           5.0         3.4          1.5         0.2  setosa -1.02  0.79
#> 9           4.4         2.9          1.4         0.2  setosa -1.74 -0.36
#> 10          4.9         3.1          1.5         0.1  setosa -1.14  0.10
#> ..          ...         ...          ...         ...     ...   ...   ...
```