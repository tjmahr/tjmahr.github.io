---
title: Think of `&&` as a stricter `&`
excerpt: A crash course on the *and*s and *or*s in R
tags:
  - r
share: true
header:
  overlay_image: "assets/images/2021-07-beach-1920.jpg"
  image_description: "A beach with curlicue tracks that look like ampersands"
  overlay_filter: rgba(10, 10, 10, 0.5)
  caption: "Photo credit: [**Theodora  Lee**](https://unsplash.com/photos/Wxzl17FhBJ0)"
---

In programming languages, we find logical operators for *and*
and *or*. In fact, Python uses the actual words `and` and `or`
for these operators.


```python
# Python via the reticulate package
x = True
y = False
x and y
#> False
x or y
#> True
```

In Javascript, we see `&&` for *and* and `||` for *or* instead.


```javascript
// Javascript via `engine = "node"` in knitr
let x = true;
let y = false;
console.log(x && y);
console.log(x || y);
// false
// true
```

In R, we have *two* versions of logical *and* (`&` and `&&`) and logical
*or* (`|` and `||`). What's going on? 

Documentation in [`help("Logic",
package = "base")`](https://rdrr.io/r/base/Logic.html) provides the
following:

> `&` and `&&` indicate logical AND and `|` and `||` indicate logical
> OR. The shorter form performs elementwise comparisons in much the same
> way as arithmetic operators. The longer form evaluates left to right
> examining only the first element of each vector. Evaluation proceeds
> only until the result is determined. The longer form is appropriate
> for programming control-flow and typically preferred in `if` clauses.

Let's unpack this paragraph


## The shorter operators are vectorized

The crucial difference is that the shorter versions (`&`, `|`) are
vectorized. Given two vectors, they will apply logical *and*/*or* on
pairs of elements from each vector. In the example below, `ttff[1]` is
*and*-ed with `tftf[1]`, `ttff[2]` is *and*-ed with `tftf[2]`, and so
on. This vectorization is a pretty important feature, for example, when
we are comparing columns in a dataframe in order to filter rows.


```r
# Returns something of length four
ttff <- c(TRUE,  TRUE, FALSE, FALSE)
tftf <- c(TRUE, FALSE,  TRUE, FALSE)
ttff & tftf
#> [1]  TRUE FALSE FALSE FALSE
ttff | tftf
#> [1]  TRUE  TRUE  TRUE FALSE
```

In contrast, `&&` and `||` only work on *scalars* (length-one values).
They return just one element. In this example, they look at `ttff[1]`
and `tftf[1]`.


```r
# Returns something of length one
ttff && tftf
#> [1] TRUE
ttff || tftf
#> [1] TRUE
```

To help remember the distinction, **think of the longer versions (`&&`,
`||`) as *stricter* forms of the logical operators**. They don't just care
about truthiness or falsiness, but they also care about length. The
extra *and*/*or* characters are there because the operators are
[extra](http://extra.urbanup.com/245251#.YN3RSfYxVDc.twitter), if you will.

Okay, that was the point of this post: to describe the difference
between the short and long operators and introduce the intuition that
the longer forms are stricter. The rest of this post will dig into some
other oddities and notes about *and* and *or*.

## Short circuit evaluation

You may have noticed a strange or unclear detail in the documentation.

> The longer form evaluates left to right examining only the first
> element of each vector. Evaluation proceeds only until the result is
> determined.

This part is describing the semantics of [short-circuit
evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation).
Here are two facts about *and* and *or*:

  - *x and y* is false when either *x* or *y* is false. Therefore, if
    *x* is false, we don't need to look at *y* at all.
  - *x or y* is true when either *x* or *y* is true. Therefore, if *x*
    is true, we don't need to look at *y* at all.

R will not evaluate the second operand for `&&` and `||` if it can learn
the answer from the first operand. Thus, short-circuit evaluation will
ignore the `stop()` calls in the examples below.


```r
FALSE && stop("this is an error")
#> [1] FALSE
TRUE || stop("this is an error")
#> [1] TRUE

# Short-circuiting doesn't apply. Need the second operand.
TRUE && stop("this is an error")
#> Error in eval(expr, envir, enclos): this is an error
FALSE || stop("this is an error")
#> Error in eval(expr, envir, enclos): this is an error
```

### The NULL-or-default pattern

Logically, the short-circuit evaluation for `||` is equivalent to a
particular kind of *if* statement:


```r
if_x_then_x_else_y <- function(x, y) {
  if (x) x else y 
}
if_x_then_x_else_y(TRUE, FALSE)
#> [1] TRUE
# short-circuited
if_x_then_x_else_y(TRUE, stop("this is an error"))
#> [1] TRUE
```

In languages that treat undefined values as falsy and defined values
as truthy, this `if (x) x else y` behavior is sometimes used as an idiom
to set a default, backup value for a variable. In the Javascript code below,
the undefined variable `name` is treated as falsy, the
string `"I don't know your name"` is treated as truthy, so the *or*
returns the second string. In other words, ["the first truthy value is
returned"](https://flexdinesh.github.io/short-circuit-assignment-in-javascript/).



```javascript
let name;
let fallback = name || "I don't know your name!";
console.log(name);
console.log(fallback);
// undefined
// I don't know your name!
```

(This pattern, incidentally, appears to have earned its own operator in
Javascript with the ["nullish coalescing operator"
`??`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing_operator).)

Why do I mention this programming idiom from Javascript? Because setting
a default for missing values is pretty useful, and this syntax is pretty
nice. The tidyverse provides [a null coalescing
operator](https://rlang.r-lib.org/reference/op-null-default.html),
inspired by Ruby's `||` operator.


```r
library(purrr)
1 %||% 2
#> [1] 1
NULL %||% 2
#> [1] 2

# Not exactly or-like. It just cares about NULL-ness.
FALSE %||% 2
#> [1] FALSE

# See the source code
`%||%`
#> function (x, y) 
#> {
#>     if (is_null(x)) 
#>         y
#>     else x
#> }
#> <bytecode: 0x00000000216f9fb0>
#> <environment: namespace:rlang>
```

## `if()` statements want the stricter operators

Recall the following from the documentation:

> The longer form is appropriate for programming control-flow and
> typically preferred in `if` clauses.

`if()` statements are not vectorized. (See
[`ifelse()`](https://rdrr.io/r/base/ifelse.html) instead.) `if()`
statements complain when they see a vector:


```r
if (ttff | tftf) {
  "We are going to get a warning."  
}
#> Warning in if (ttff | tftf) {: the condition has length > 1 and only the first
#> element will be used
#> [1] "We are going to get a warning."
```

The idea behind the documentation is that because `&` and `|` return
vectors and because `if()` only likes scalars, we should not use the
shorter forms in `if()` statements. They provide the wrong output for
`if()`. But that *does not mean* the following code with the stricter
*or* operator is correct.


```r
if (ttff || tftf) {
  "No warning but this code is not right."  
}
#> [1] "No warning but this code is not right."
```

Although the code here does not raise any warnings, it reduces all of
the information in `ttff` and `tftf` into just `ttff[1]` and
`tftf[1]`. Those values likely will not be appropriate for the
programming task at hand, so we should provide the scalars ourselves.


### `all()` and `any()` can apply *and* and *or* down a vector

Because I am talking about *and* and *or* and about creating logical
scalars, I want to advertise two particular functions that can reduce a
logical vector into a scalar. [`all()`](https://rdrr.io/r/base/all.html)
is `TRUE` when all of the elements are `TRUE`. For a vector, it would be
like replacing the commas in `c(TRUE, TRUE, FALSE)` with `&`s.
[`any()`](https://rdrr.io/r/base/any.html) provides the analogous
down-vector behavior for `|`. `any()` is `TRUE` when any of the elements
in the vector are `TRUE` (and not `NA`---more on that later).


```r
all(c(TRUE, TRUE, TRUE))
#> [1] TRUE
c(TRUE & TRUE & TRUE)
#> [1] TRUE

all(c(TRUE, FALSE, TRUE))
#> [1] FALSE
c(TRUE & FALSE & TRUE)
#> [1] FALSE

c(TRUE | FALSE | TRUE)
#> [1] TRUE

# The input can be scalars or vectors
all(TRUE, TRUE, TRUE, c(TRUE, FALSE))
#> [1] FALSE
any(FALSE, FALSE, FALSE, c(FALSE, TRUE))
#> [1] TRUE

# These appear not to short circuit
any(TRUE, stop("this is an error"))
#> Error in eval(expr, envir, enclos): this is an error
all(FALSE, stop("this is an error"))
#> Error in eval(expr, envir, enclos): this is an error
```

### We can make the strict operators even stricter

Recall the following unsettling example where just `ttff[1]` and
`tftf[1]` are considered in the `if()` statement. 


```r
if (ttff || tftf) {
  "No warning but this code is not right."  
}
#> [1] "No warning but this code is not right."
```

It would be nice to rule out this behavior outright and make this
behavior illegal. In fact, we can make our code stricter by setting the
system environment variable `_R_CHECK_LENGTH_1_LOGIC2_`. Once set, using
the strict forms on inputs longer than 1 will throw an error. 


```r
Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "TRUE")
ttff && tftf
#> Error in ttff && tftf: 'length(x) = 4 > 1' in coercion to 'logical(1)'
ttff || tftf
#> Error in ttff || tftf: 'length(x) = 4 > 1' in coercion to 'logical(1)'

# Default behavior
Sys.unsetenv("_R_CHECK_LENGTH_1_LOGIC2_")
ttff && tftf
#> [1] TRUE
ttff || tftf
#> [1] TRUE
```

A related environment variable is `_R_CHECK_LENGTH_1_CONDITION_` that
turn vectors inside of `if()` into errors instead of warnings.


```r
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "TRUE")
if (ttff | tftf) { 
  "This code will not even be seen."
}
#> Error in if (ttff | tftf) {: the condition has length > 1

# Default behavior
Sys.unsetenv("_R_CHECK_LENGTH_1_CONDITION_")
if (ttff | tftf) { 
  "We are going to get a warning."
}
#> Warning in if (ttff | tftf) {: the condition has length > 1 and only the first
#> element will be used
#> [1] "We are going to get a warning."
```

To make project code more robust, one might consider setting these
inside of [a .Renviron
file](https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf)
or using them with [dotenv](https://github.com/gaborcsardi/dotenv). (But
if I remember correctly, these checks apply to package code so you
get errors for legal-but-dodgy R code in other people's packages.)


## `NA`s infect other values

All of the above examples conveniently avoided `NA`s. These are missing
values that infect other logical values, turning them into `NA`s. For this
section, I want to briefly highlight some behaviors of `NA`s and some
functions that can help us work around them.

For *and*, an `NA` with any non-`FALSE` value is `NA`. For *or*, an `NA`
with any non-`TRUE` value is `NA`. That's a funny sentence, but it
reflects the case where we can infer the answer without seeing the `NA`
value. `TRUE | NA` (and `NA | TRUE`) returns `TRUE` because it would
return `TRUE` if the `NA` was actually a `TRUE` or `FALSE`. The same
holds for `FALSE & NA` (and `NA & FALSE`) returning `FALSE`. If we
un-missing-ed the `NA` into `TRUE` or `FALSE`, the statement would still
be `FALSE`.


```r
tfnn <- c(TRUE, FALSE, NA, NA)
nntf <- c(NA, NA, TRUE, FALSE)
tfnn & nntf
#> [1]    NA FALSE    NA FALSE
tfnn | nntf
#> [1] TRUE   NA TRUE   NA

# Infecting in all() and any()
TRUE && NA
#> [1] NA
FALSE || NA
#> [1] NA
all(TRUE, TRUE, NA)
#> [1] NA
any(FALSE, FALSE, NA)
#> [1] NA

# These return FALSE and TRUE because they would return TRUE and FALSE
# regardless of whether the NA was a TRUE or a FALSE.
FALSE && NA
#> [1] FALSE
TRUE || NA
#> [1] TRUE
all(NA, FALSE, FALSE)
#> [1] FALSE
any(TRUE, FALSE, NA)
#> [1] TRUE
```

This uhh complicates things, so how do I check if what I have is `TRUE`
or `FALSE`? [`isTRUE()` and
`isFALSE()`](https://rdrr.io/r/base/Logic.html) provide direct tests of
whether the input is the scalar `TRUE` or the scalar `FALSE`.


```r
c(isTRUE(TRUE), isTRUE(FALSE), isTRUE(NA))
#> [1]  TRUE FALSE FALSE
c(isFALSE(TRUE), isFALSE(FALSE), isFALSE(NA))
#> [1] FALSE  TRUE FALSE
```

The documentation notes that

> `if(isTRUE(cond))` may be preferable to `if(cond)` because of `NA`s

so `isTRUE()` is something we run into packaged code.

`isTRUE()` and `isFALSE()` are not vectorized, but we can check elements
in a vector are `TRUE` and only `TRUE` in a few different ways.


```r
# Make a new function
Vectorize(FUN = isTRUE)(tfnn)
#> [1]  TRUE FALSE FALSE FALSE

# Apply the function on a vector
vapply(X = tfnn, FUN = isTRUE, FUN.VALUE = logical(1))
#> [1]  TRUE FALSE FALSE FALSE

# Use table-lookup or set operations
tfnn %in% TRUE
#> [1]  TRUE FALSE FALSE FALSE
is.element(el = tfnn, set = TRUE)
#> [1]  TRUE FALSE FALSE FALSE
```

*** 

I think that's just about every useful thing I want to say about *and*
and *or* in R. But just remember, `&&` and `||` are longer operators because
they are stricter.






***

*Last knitted on 2021-11-16. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2021-07-01-think-of-stricter-logical-operators.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info  --------------------------------------------------------------
    #>  hash: wind chime, sheaf of rice, person gesturing NO: medium skin tone
    #> 
    #>  setting  value
    #>  version  R version 4.1.2 (2021-11-01)
    #>  os       Windows 10 x64 (build 22000)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language (EN)
    #>  collate  English_United States.1252
    #>  ctype    English_United States.1252
    #>  tz       America/Chicago
    #>  date     2021-11-16
    #>  pandoc   NA
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package     * version date (UTC) lib source
    #>  cli           3.1.0   2021-10-27 [1] CRAN (R 4.1.1)
    #>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.1.0)
    #>  git2r         0.28.0  2021-01-10 [1] CRAN (R 4.1.1)
    #>  here          1.0.1   2020-12-13 [1] CRAN (R 4.1.0)
    #>  jsonlite      1.7.2   2020-12-09 [1] CRAN (R 4.1.0)
    #>  knitr       * 1.36    2021-09-29 [1] CRAN (R 4.1.1)
    #>  lattice       0.20-45 2021-09-22 [2] CRAN (R 4.1.2)
    #>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.1.0)
    #>  Matrix        1.3-4   2021-06-01 [2] CRAN (R 4.1.2)
    #>  png           0.1-7   2013-12-03 [1] CRAN (R 4.1.0)
    #>  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.1.0)
    #>  ragg          1.2.0   2021-10-30 [1] CRAN (R 4.1.1)
    #>  Rcpp          1.0.7   2021-07-07 [1] CRAN (R 4.1.0)
    #>  reticulate    1.22    2021-09-17 [1] CRAN (R 4.1.1)
    #>  rlang         0.4.12  2021-10-18 [1] CRAN (R 4.1.1)
    #>  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.1.0)
    #>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.0)
    #>  sessioninfo   1.2.1   2021-11-02 [1] CRAN (R 4.1.2)
    #>  stringi       1.7.5   2021-10-04 [1] CRAN (R 4.1.1)
    #>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.1.0)
    #>  systemfonts   1.0.3   2021-10-13 [1] CRAN (R 4.1.1)
    #>  textshaping   0.3.6   2021-10-13 [1] CRAN (R 4.1.1)
    #>  xfun          0.27    2021-10-18 [1] CRAN (R 4.1.1)
    #> 
    #>  [1] C:/Users/trist/Documents/R/win-library/4.1
    #>  [2] C:/Program Files/R/R-4.1.2/library
    #> 
    #> - Python configuration -------------------------------------------------------
    #>  python:         C:/Python310/python.exe
    #>  libpython:      C:/Python310/python310.dll
    #>  pythonhome:     C:/Python310
    #>  version:        3.10.0 (tags/v3.10.0:b494f59, Oct  4 2021, 19:00:18) [MSC v.1929 64 bit (AMD64)]
    #>  Architecture:   64bit
    #>  numpy:           [NOT FOUND]
    #>  
    #>  NOTE: Python version was forced by RETICULATE_PYTHON
    #> 
    #> ------------------------------------------------------------------------------
    ```
