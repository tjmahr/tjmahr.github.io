---
title: "set_na_where(): a nonstandard evaluation use case"
excerpt: Bottling up magic spells
tags:
  - r
  - nonstandard evaluation
  - eyetracking
  - rlang
share: true
header:
  overlay_image: "assets/images/jovi-waqa-1280.jpg"
  caption: "Photo credit: [**Jovi Waqa**](https://unsplash.com/photos/gNJn5-C5enE)"
---




In this post, I describe a recent case where I used rlang's [tidy
evaluation](http://rlang.tidyverse.org/articles/tidy-evaluation.html)
system to do some data-cleaning. This example is not particularly
involved, but it demonstrates is a basic but powerful idea: That we can
capture the expressions that a user writes, pass them around as data,
and make some 💫 magic ✨ happen.
This technique in R is called [*nonstandard
evaluation*](http://adv-r.had.co.nz/Computing-on-the-language.html).

## Strange eyetracking data

Last week, I had to deal with a file with some eyetracking data from a
sequence-learning experiment. The eyetracker records the participant's gaze
location at a rate of 60 frames per second---except for this weird file which
wrote out ~80 frames each second. In this kind of data, we have one row per
eyetracking sample, and each sample records a timestamp and the gaze location
:eyes: on the computer screen at each timestamp. In this particular dataset, we
have _x_ and _y_ gaze coordinates in pixels (both eyes averaged together,
`GazeX` and `GazeY`) or in screen proportions (for each eye in the `EyeCoord`
columns.)


```r
library(dplyr)
library(ggplot2)
library(rlang)
# the data is bundled with an R package I wrote
# devtools::install_github("tjmahr/fillgaze")

df <- system.file("test-gaze.csv", package = "fillgaze") %>% 
  readr::read_csv() %>% 
  mutate(Time = Time - min(Time)) %>% 
  select(Time:REyeCoordY) %>% 
  round(3) %>% 
  mutate_at(vars(Time), round, 1) %>% 
  mutate_at(vars(GazeX, GazeY), round, 0)
df
#> # A tibble: 14,823 x 8
#>     Time Trial GazeX GazeY LEyeCoordX LEyeCoordY REyeCoordX REyeCoordY
#>    <dbl> <dbl> <dbl> <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1   0       1  1176   643      0.659      0.589      0.566      0.602
#>  2   3.5     1 -1920 -1080     -1         -1         -1         -1    
#>  3  20.2     1 -1920 -1080     -1         -1         -1         -1    
#>  4  36.8     1  1184   648      0.664      0.593      0.570      0.606
#>  5  40       1  1225   617      0.685      0.564      0.591      0.579
#>  6  56.7     1 -1920 -1080     -1         -1         -1         -1    
#>  7  73.4     1  1188   641      0.665      0.587      0.572      0.6  
#>  8  76.6     1  1204   621      0.674      0.568      0.580      0.582
#>  9  93.3     1 -1920 -1080     -1         -1         -1         -1    
#> 10 110.      1  1189   665      0.666      0.609      0.572      0.622
#> # ... with 14,813 more rows
```

In this particular eyetracking setup, offscreen looks are coded as negative gaze
coordinates, and what's extra weird here is that every second or third point is
incorrectly placed offscreen. We see that in the frequent -1920 values in
`GazeX`. Plotting the first few _x_ and _y_ pixel locations shows the 
pattern as well.



```r
p <- ggplot(head(df, 40)) + 
  aes(x = Time) + 
  geom_hline(yintercept = 0, size = 2, color = "white") + 
  geom_point(aes(y = GazeX, color = "GazeX")) +
  geom_point(aes(y = GazeY, color = "GazeY")) + 
  labs(
    x = "Time (ms)", 
    y = "Screen location (pixels)", 
    color = "Variable"
  )

p + 
  annotate(
    "text", x = 50, y = -200, 
    label = "offscreen", color = "grey20"
  ) + 
  annotate(
    "text", x = 50, y = 200, 
    label = "onscreen", color = "grey20"
  ) 
```

<img src="/figs/2017-08-15-set-na-where-nonstandard-evaluation-use-case/missing-data-plot-1.png" title="Offscreens looks occurred every two or three samples." alt="Offscreens looks occurred every two or three samples." width="80%" style="display: block; margin: auto;" />

It is physiologically impossible for a person's gaze to oscillate so quickly and
with such magnitude (the gaze is tracked on a large screen display), so
obviously something weird was going on with the experiment software.

This file motivated me to develop a [general purpose package for interpolating
missing data in eyetracking experiments](https://github.com/tjmahr/fillgaze).
This package was always something I wanted to do, and this file moved it from 
the _someday_ list to the _today_ list. 

## A function to recode values in many columns as `NA`

The first step in handling this problematic dataset is to convert the offscreen
values into actual missing (`NA`) values). Because we have several columns of
data, I wanted a succinct way to recode values in multiple columns into `NA`
values.

First, we sketch out the _code we want to write_ when we're done.


```r
set_na_where <- function(data, ...) {
  # do things
}

set_na_where(
  data = df,
  GazeX = GazeX < -500 | 2200 < GazeX,
  GazeY = GazeY < -200 | 1200 < GazeY
)
```

That is, after specifying the `data`, we list off an arbitrary number of column
names, and with each name, we provide a rule to determine whether a value in
that column is offscreen and should be set to `NA`. For example, we want every
value in `GazeX` where `GazeX < -500` or `2299 < GazeX` is `TRUE` to be replaced
with `NA`.

### Bottling up magic spells

Lines of computer code are magic spells: We say the incantations and things
happen around us. Put more formally, the code contains expressions that are
evaluated in an environment.


```r
hey <- "Hello!"
message(hey)
#> Hello!

exists("x")
#> [1] FALSE

x <- pi ^ 2
exists("x")
#> [1] TRUE

print(x)
#> [1] 9.869604

stop("what are you doing?")
#> Error in eval(expr, envir, enclos): what are you doing?
```

In our function signature, `function(data, ...)`, the expressions are collected
in the special "dots" argument (`...`). In normal circumstances, we can view the
contents of the dots by storing them in a list. Consider:


```r
hello_dots <- function(...) {
  str(list(...))
}
hello_dots(x = pi, y = 1:10, z = NA)
#> List of 3
#>  $ x: num 3.14
#>  $ y: int [1:10] 1 2 3 4 5 6 7 8 9 10
#>  $ z: logi NA
```

But we not passing in regular data, but expressions that need to be evaluated in
a particular location. Below the magic words are uttered and we get an error
because they mention things that do not exist in the current environment.


```r
hello_dots(GazeX = GazeX < -500 | 2200 < GazeX)
#> Error in str(list(...)): object 'GazeX' not found
```

What we need to do is prevent these words from being uttered until the time and
place are right. **Nonstandard evaluation is a way of bottling up magic spells
and changing how or where they are cast**---sometimes we even change the magic
words themselves. We bottle up or _capture_ the expressions given by the user by
quoting them. `quo()` quotes a single expression, and `quos()` (plural) will
quote a list of expressions. Below, we capture the expressions stored in the
dots :speech_balloon: and then make sure that their names match column names in
the dataframe.


```r
set_na_where <- function(data, ...) {
  dots <- quos(...)
  stopifnot(names(dots) %in% names(data), !anyDuplicated(names(dots)))
  
  dots
  # more to come
}

spells <- set_na_where(
  data = df,
  GazeX = GazeX < -500 | 2200 < GazeX, 
  GazeY = GazeY < -200 | 1200 < GazeY
)
spells
#> <list_of<quosure>>
#> 
#> $GazeX
#> <quosure>
#> expr: ^GazeX < -500 | 2200 < GazeX
#> env:  0000000016955328
#> 
#> $GazeY
#> <quosure>
#> expr: ^GazeY < -200 | 1200 < GazeY
#> env:  0000000016955328
```

I call these results `spells` because it just contains the expressions stored as
data. We can interrogate these results like data. We can query the names of the
stored data, and we can extract values (the quoted expressions).


```r
names(spells)
#> [1] "GazeX" "GazeY"
spells[[1]]
#> <quosure>
#> expr: ^GazeX < -500 | 2200 < GazeX
#> env:  0000000016955328
```

### Casting spells

We can cast a spell by evaluating an expression. To keep the incantation from
fizzling out, we specify that we want to evaluate the expression _inside_ of the
dataframe. The function `eval_tidy(expr, data)` lets us do just that: evaluate
an expression `expr` inside of some `data`.


```r
# Evaluate the first expression inside of the data
xs_to_set_na <- eval_tidy(spells[[1]], data = df)

# Just the first few bc there are 10000+ values
xs_to_set_na[1:20]
#>  [1] FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE
#> [13] FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE
```

In fact, we can evaluate them all at once with by applying `eval_tidy()` on each
listed expression.


```r
to_set_na <- lapply(spells, eval_tidy, data = df)
str(to_set_na)
#> List of 2
#>  $ GazeX: logi [1:14823] FALSE TRUE TRUE FALSE FALSE TRUE ...
#>  $ GazeY: logi [1:14823] FALSE TRUE TRUE FALSE FALSE TRUE ...
```

### Finishing touches

Now, the rest of the function is straightforward. Evaluate each `NA`-rule on 
the named columns, and then set each row where the rule is `TRUE` to `NA`.


```r
set_na_where <- function(data, ...) {
  dots <- quos(...)
  stopifnot(names(dots) %in% names(data), !anyDuplicated(names(dots)))
  
  set_to_na <- lapply(dots, eval_tidy, data = data)
  
  for (col in names(set_to_na)) {
    data[set_to_na[[col]], col] <- NA
  }
  
  data
}

results <- set_na_where(
  data = df,
  GazeX = GazeX < -500 | 2200 < GazeX, 
  GazeY = GazeY < -200 | 1200 < GazeY
)
results
#> # A tibble: 14,823 x 8
#>     Time Trial GazeX GazeY LEyeCoordX LEyeCoordY REyeCoordX REyeCoordY
#>    <dbl> <dbl> <dbl> <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1   0       1  1176   643      0.659      0.589      0.566      0.602
#>  2   3.5     1    NA    NA     -1         -1         -1         -1    
#>  3  20.2     1    NA    NA     -1         -1         -1         -1    
#>  4  36.8     1  1184   648      0.664      0.593      0.570      0.606
#>  5  40       1  1225   617      0.685      0.564      0.591      0.579
#>  6  56.7     1    NA    NA     -1         -1         -1         -1    
#>  7  73.4     1  1188   641      0.665      0.587      0.572      0.6  
#>  8  76.6     1  1204   621      0.674      0.568      0.580      0.582
#>  9  93.3     1    NA    NA     -1         -1         -1         -1    
#> 10 110.      1  1189   665      0.666      0.609      0.572      0.622
#> # ... with 14,813 more rows
```

Visually, we can see that the offscreen values are no longer plotted. Plus, we 
are told that our data now has missing values.


```r
# `plot %+% data`: replace the data in `plot` with `data`
p %+% head(results, 40)
#> Warning: Removed 15 rows containing missing values (geom_point).

#> Warning: Removed 15 rows containing missing values (geom_point).
```

<img src="/figs/2017-08-15-set-na-where-nonstandard-evaluation-use-case/no-offscreen-plots-1.png" title="Offscreens are no longer plotted." alt="Offscreens are no longer plotted." width="80%" style="display: block; margin: auto;" />

One of the quirks about some eyetracking data is that during a blink, sometimes
the device will record the _x_ location but not the _y_ location. (I think this
happens because blinks move vertically so the horizontal detail can still be
inferred in a half-closed eye.) This effect shows up in the data when there are
more `NA` values for the _y_ values than for the _x_ values:


```r
count_na <- function(data, ...) {
  subset <- select(data, ...)
  lapply(subset, function(xs) sum(is.na(xs)))
}

count_na(results, GazeX, GazeY)
#> $GazeX
#> [1] 2808
#> 
#> $GazeY
#> [1] 3064
```

We can equalize these counts by running the function a second time with new rules.


```r
df %>% 
  set_na_where(
    GazeX = GazeX < -500 | 2200 < GazeX, 
    GazeY = GazeY < -200 | 1200 < GazeY
  ) %>% 
  set_na_where(
    GazeX = is.na(GazeY), 
    GazeY = is.na(GazeX)
  ) %>% 
  count_na(GazeX, GazeY)
#> $GazeX
#> [1] 3069
#> 
#> $GazeY
#> [1] 3069
```

Alternatively, we can do this all at once by using the same `NA`-filtering rule
on `GazeX` and `GazeY`.


```r
df %>% 
  set_na_where(
    GazeX = GazeX < -500 | 2200 < GazeX | GazeY < -200 | 1200 < GazeY, 
    GazeY = GazeX < -500 | 2200 < GazeX | GazeY < -200 | 1200 < GazeY
  ) %>% 
  count_na(GazeX, GazeY)
#> $GazeX
#> [1] 3069
#> 
#> $GazeY
#> [1] 3069
```

These last examples, where we compare different rules, showcases how nonstandard
evaluation lets us write in a very succinct and convenient manner and quickly
iterate over possible rules. Works like magic, indeed.




***

*Last knitted on 2021-02-15. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2017-08-15-set-na-where-nonstandard-evaluation-use-case.Rmd).*[^si] 

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
    #>  date     2021-02-15                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package     * version    date       lib source                     
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.2)             
    #>  cli           2.3.0      2021-01-31 [1] CRAN (R 4.0.3)             
    #>  colorspace    2.0-0      2020-11-11 [1] CRAN (R 4.0.3)             
    #>  crayon        1.4.1      2021-02-08 [1] CRAN (R 4.0.3)             
    #>  DBI           1.1.1      2021-01-15 [1] CRAN (R 4.0.3)             
    #>  digest        0.6.27     2020-10-24 [1] CRAN (R 4.0.3)             
    #>  dplyr       * 1.0.4      2021-02-02 [1] CRAN (R 4.0.3)             
    #>  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.2)             
    #>  emo           0.0.0.9000 2020-07-06 [1] Github (hadley/emo@3f03b11)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.2)             
    #>  fansi         0.4.2      2021-01-15 [1] CRAN (R 4.0.3)             
    #>  farver        2.0.3      2020-01-16 [1] CRAN (R 4.0.2)             
    #>  generics      0.1.0      2020-10-31 [1] CRAN (R 4.0.3)             
    #>  ggplot2     * 3.3.3      2020-12-30 [1] CRAN (R 4.0.3)             
    #>  git2r         0.28.0     2021-01-10 [1] CRAN (R 4.0.3)             
    #>  glue          1.4.2      2020-08-27 [1] CRAN (R 4.0.2)             
    #>  gtable        0.3.0      2019-03-25 [1] CRAN (R 4.0.2)             
    #>  here          1.0.1      2020-12-13 [1] CRAN (R 4.0.3)             
    #>  highr         0.8        2019-03-20 [1] CRAN (R 4.0.2)             
    #>  hms           1.0.0      2021-01-13 [1] CRAN (R 4.0.3)             
    #>  knitr       * 1.31       2021-01-27 [1] CRAN (R 4.0.3)             
    #>  labeling      0.4.2      2020-10-20 [1] CRAN (R 4.0.2)             
    #>  lifecycle     1.0.0      2021-02-15 [1] CRAN (R 4.0.3)             
    #>  lubridate     1.7.9.2    2020-11-13 [1] CRAN (R 4.0.3)             
    #>  magrittr      2.0.1      2020-11-17 [1] CRAN (R 4.0.3)             
    #>  munsell       0.5.0      2018-06-12 [1] CRAN (R 4.0.2)             
    #>  pillar        1.4.7      2020-11-20 [1] CRAN (R 4.0.3)             
    #>  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.0.2)             
    #>  ps            1.5.0      2020-12-05 [1] CRAN (R 4.0.3)             
    #>  purrr         0.3.4      2020-04-17 [1] CRAN (R 4.0.2)             
    #>  R6            2.5.0      2020-10-28 [1] CRAN (R 4.0.2)             
    #>  ragg          0.4.1      2021-01-11 [1] CRAN (R 4.0.3)             
    #>  Rcpp          1.0.6      2021-01-15 [1] CRAN (R 4.0.3)             
    #>  readr         1.4.0      2020-10-05 [1] CRAN (R 4.0.2)             
    #>  rlang       * 0.4.10     2020-12-30 [1] CRAN (R 4.0.3)             
    #>  rprojroot     2.0.2      2020-11-15 [1] CRAN (R 4.0.3)             
    #>  rstudioapi    0.13       2020-11-12 [1] CRAN (R 4.0.3)             
    #>  scales        1.1.1      2020-05-11 [1] CRAN (R 4.0.2)             
    #>  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.2)             
    #>  stringi       1.5.3      2020-09-09 [1] CRAN (R 4.0.2)             
    #>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.2)             
    #>  systemfonts   1.0.0      2021-02-01 [1] CRAN (R 4.0.3)             
    #>  textshaping   0.2.1      2020-11-13 [1] CRAN (R 4.0.3)             
    #>  tibble        3.0.6      2021-01-29 [1] CRAN (R 4.0.3)             
    #>  tidyselect    1.1.0      2020-05-11 [1] CRAN (R 4.0.2)             
    #>  utf8          1.1.4      2018-05-24 [1] CRAN (R 4.0.2)             
    #>  vctrs         0.3.6      2020-12-17 [1] CRAN (R 4.0.3)             
    #>  withr         2.4.1      2021-01-26 [1] CRAN (R 4.0.3)             
    #>  xfun          0.20       2021-01-06 [1] CRAN (R 4.0.3)             
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.3/library
    ```
