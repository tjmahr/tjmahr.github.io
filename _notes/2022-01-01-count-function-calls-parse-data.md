---
title: "Counting function calls using getParseData()"
date: 2022-01-01
tags: [r, metaprogramming, parsing]
---

We can do data analysis on `getParseData()` result.


``` r
code <- quote({
  check_out <- my(fancy(code))
  here_is <- some(fancy(code))
})

parse_data <- getParseData(parse(text = deparse(code), keep.source = TRUE))
head(parse_data)
#>    line1 col1 line2 col2 id parent       token terminal      text
#> 55     1    1     4    1 55      0        expr    FALSE          
#> 1      1    1     1    1  1     55         '{'     TRUE         {
#> 25     2    5     2   32 25     55        expr    FALSE          
#> 3      2    5     2   13  3      5      SYMBOL     TRUE check_out
#> 5      2    5     2   13  5     25        expr    FALSE          
#> 4      2   15     2   16  4     25 LEFT_ASSIGN     TRUE        <-
```

Count tokens using results of `all.names()` and `all.vars()`.


``` r
names <- all.names(code)
vars <- all.vars(code)

# Just the non-variable tokens
names[!names %in% vars]
#> [1] "{"     "<-"    "my"    "fancy" "<-"    "some"  "fancy"
```
