---
title: "sweep() demo"
date: 2025-01-01
tags: [r]
---

`sweep(data, margin, stats, fun)` is a goofy function for adjusting cols
or rows. For example, you can subtract the mean and divide by the SD to
get *z*-scores:


``` r
d <- mtcars
d_scale <- d |> lapply(scale) |> as.data.frame()
d_sweep <- d |> 
  sweep(2, colMeans(d), `-`) |> 
  sweep(2, apply(d, 2, sd), `/`)
all(d_scale$mpg == d_sweep$mpg)
#> [1] TRUE
```
