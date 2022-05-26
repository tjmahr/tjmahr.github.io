---
title: A one-liner for generating random participant IDs
excerpt: Find a `match()` in your base R library
tags:
  - r
share: true
header:
  overlay_image: "assets/images/2021-10-matches.jpg"
  image_description: "A set of matchsticks on fire all in a row"
  overlay_filter: rgba(10, 10, 10, 0.5)
  caption: "Photo credit: [**Jamie Street**](https://unsplash.com/photos/6zXwP5xpbPE)"
---



On one of the Slacks I browse, someone asked how to de-identify a
column of participant IDs. The original dataset was a wait list, so
the ordering of IDs itself was a sensitive feature of the data and we
need to scramble the order of IDs produced.

For example, suppose we have the following *repeated measures* dataset.


```r
library(tidyverse)
data <- tibble::tribble(
  ~ participant, ~ timepoint, ~ score,
           "DB",           1,       7,
           "DB",           2,       8,
           "DB",           3,       8,
           "TW",           1,      NA,
           "TW",           2,       9,
           "CF",           1,       9,
           "CF",           2,       8,
           "JH",           1,      10,
           "JH",           2,      10,
           "JH",           3,      10
)
```

We want to map the `participant` identifiers onto some sort of
shuffled-up random IDs. Suggestions included hashing the IDs with
[digest](https://rdrr.io/pkg/digest/man/sha1.html):


```r
# This approach cryptographically compresses the input into a short
# "digest". (It is not a random ID.)
data %>% 
  mutate(
    participant = Vectorize(digest::sha1)(participant)
  )
#> # A tibble: 10 × 3
#>    participant                              timepoint score
#>    <chr>                                        <dbl> <dbl>
#>  1 ad61ec1247b2381922bec89483c3ce2fb67f98d9         1     7
#>  2 ad61ec1247b2381922bec89483c3ce2fb67f98d9         2     8
#>  3 ad61ec1247b2381922bec89483c3ce2fb67f98d9         3     8
#>  4 c080f9a87edc6d47f28185279fd8be068c566a37         1    NA
#>  5 c080f9a87edc6d47f28185279fd8be068c566a37         2     9
#>  6 1f9da22bf684761daec27326331c58b46502a25b         1     9
#>  7 1f9da22bf684761daec27326331c58b46502a25b         2     8
#>  8 627d211747438ae59690cea8f0a8d6adf666b974         1    10
#>  9 627d211747438ae59690cea8f0a8d6adf666b974         2    10
#> 10 627d211747438ae59690cea8f0a8d6adf666b974         3    10
```

But this approach seems like overkill, and hashing just transforms these
IDs. We want to be rid of them completely.

The [uuid](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html) package provides [another approach](https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)):


```r
data %>% 
  group_by(participant) %>% 
  mutate(
    id = uuid::UUIDgenerate(use.time = FALSE)
  ) %>% 
  ungroup() %>% 
  select(-participant, participant = id) %>% 
  relocate(participant)
#> # A tibble: 10 × 3
#>    participant                          timepoint score
#>    <chr>                                    <dbl> <dbl>
#>  1 6ccf779f-6cf6-44f4-94c7-f266e6224b5a         1     7
#>  2 6ccf779f-6cf6-44f4-94c7-f266e6224b5a         2     8
#>  3 6ccf779f-6cf6-44f4-94c7-f266e6224b5a         3     8
#>  4 389f2e47-9578-4841-b13a-3986d9f031d4         1    NA
#>  5 389f2e47-9578-4841-b13a-3986d9f031d4         2     9
#>  6 600c58ea-07c8-473e-b5a8-3cf56463c9dd         1     9
#>  7 600c58ea-07c8-473e-b5a8-3cf56463c9dd         2     8
#>  8 c9429d4b-7c27-4edb-b859-295ae4ba32a6         1    10
#>  9 c9429d4b-7c27-4edb-b859-295ae4ba32a6         2    10
#> 10 c9429d4b-7c27-4edb-b859-295ae4ba32a6         3    10
```

Again, these IDs seem excessive: Imagine plotting data with one participant 
per facet.

When I create blogposts for this site, I use a function to create a new
.Rmd file with the date and a [random adjective-animal
phrase](https://rdrr.io/pkg/ids/man/adjective_animal.html) for a
placeholder (e.g., `2021-06-28-mild-capybara.Rmd`). We could try that for
fun:


```r
data %>% 
  group_by(participant) %>% 
  mutate(
    id = ids::adjective_animal()
  ) %>% 
  ungroup() %>% 
  select(-participant, participant = id) %>% 
  relocate(participant)
#> # A tibble: 10 × 3
#>    participant              timepoint score
#>    <chr>                        <dbl> <dbl>
#>  1 chrysoprase_bushsqueaker         1     7
#>  2 chrysoprase_bushsqueaker         2     8
#>  3 chrysoprase_bushsqueaker         3     8
#>  4 hideous_cheetah                  1    NA
#>  5 hideous_cheetah                  2     9
#>  6 powdery_siamang                  1     9
#>  7 powdery_siamang                  2     8
#>  8 ducal_hornshark                  1    10
#>  9 ducal_hornshark                  2    10
#> 10 ducal_hornshark                  3    10
```

But that's too whimsical (and something like `hideous-cheetah` seems
disrespectful for human subjects).

One user suggested [`forcats::fct_anon()`](https://forcats.tidyverse.org/reference/fct_anon.html):


```r
data %>% 
  mutate(
    participant = participant %>% 
      as.factor() %>% 
      forcats::fct_anon(prefix = "p0")
    )
#> # A tibble: 10 × 3
#>    participant timepoint score
#>    <fct>           <dbl> <dbl>
#>  1 p04                 1     7
#>  2 p04                 2     8
#>  3 p04                 3     8
#>  4 p02                 1    NA
#>  5 p02                 2     9
#>  6 p03                 1     9
#>  7 p03                 2     8
#>  8 p01                 1    10
#>  9 p01                 2    10
#> 10 p01                 3    10
```

This approach works wonderfully. The only wrinkle is that it requires
converting our IDs to a factor in order to work.

## Call me the `match()`-maker



My approach is a nice combination of base R functions:


```r
data %>% 
  mutate(
    participant = match(participant, sample(unique(participant)))
  )
#> # A tibble: 10 × 3
#>    participant timepoint score
#>          <int>     <dbl> <dbl>
#>  1           3         1     7
#>  2           3         2     8
#>  3           3         3     8
#>  4           1         1    NA
#>  5           1         2     9
#>  6           2         1     9
#>  7           2         2     8
#>  8           4         1    10
#>  9           4         2    10
#> 10           4         3    10
```

[`match(x, table)`](https://rdrr.io/r/base/match.html) returns the first
positions of the `x` elements in some vector `table`. What is the
position in the alphabet of the letters L and Q and L again?


```r
match(c("L", "Q", "L"), LETTERS)
#> [1] 12 17 12
```

[`sample()`](https://rdrr.io/r/base/sample.html) shuffles the values in
the `table` so the order of elements is lost. The `unique()` is
optional. We could just `sample(data$participant)`. Then the first
position of one of the IDs might be a number larger than 4:


```r
shuffle <- sample(data$participant)
shuffle
#>  [1] "CF" "JH" "TW" "JH" "DB" "DB" "DB" "JH" "CF" "TW"

match(data$participant, shuffle)
#>  [1] 5 5 5 3 3 1 1 2 2 2
```

For more aesthetically pleasing names, and for names that will sort
correctly, we can zero-pad the results with
[`sprintf()`](https://rdrr.io/r/base/sprintf.html). I am mostly
including this step so that I have it written down somewhere for my own
reference.


```r
zero_pad <- function(xs, prefix = "", width = 0) {
  # use widest element if bigger than `width`
  width <- max(c(nchar(xs), width))
  sprintf(paste0(prefix, "%0", width, "d"), xs)    
}

data %>% 
  mutate(
    participant = match(participant, sample(unique(participant))),
    participant = zero_pad(participant, "p", 3)
  )
#> # A tibble: 10 × 3
#>    participant timepoint score
#>    <chr>           <dbl> <dbl>
#>  1 p003                1     7
#>  2 p003                2     8
#>  3 p003                3     8
#>  4 p004                1    NA
#>  5 p004                2     9
#>  6 p002                1     9
#>  7 p002                2     8
#>  8 p001                1    10
#>  9 p001                2    10
#> 10 p001                3    10
```


### Bonus: `match()` `%in%` disguise 

What happens when `match()` fails to find an `x` in the table? By
default, we get `NA`. But we can customize the results with the
`nomatch` argument.


```r
match(c("7", "A", "L"), LETTERS)
#> [1] NA  1 12
match(c("7", "A", "L"), LETTERS, nomatch = -99)
#> [1] -99   1  12
match(c("7", "A", "L"), LETTERS, nomatch = 0)
#> [1]  0  1 12
```

If we do something like this last example, then we can check whether an
element in `x` has a match by checking for numbers greater than 0.


```r
match(c("7", "A", "L"), LETTERS, nomatch = 0) > 0
#> [1] FALSE  TRUE  TRUE
```

And that is how the functions [`%in%`](https://rdrr.io/r/base/match.html) and [`is.element()`](https://rdrr.io/r/base/sets.html) are implemented
behind the scenes:


```r
c("7", "A", "L") %in% LETTERS
#> [1] FALSE  TRUE  TRUE

# The 0L means it's an integer number instead of floating point number
`%in%`
#> function (x, table) 
#> match(x, table, nomatch = 0L) > 0L
#> <bytecode: 0x0000023daa395730>
#> <environment: namespace:base>

is.element(c("7", "A", "L"), LETTERS)
#> [1] FALSE  TRUE  TRUE

is.element
#> function (el, set) 
#> match(as.vector(el), as.vector(set), 0L) > 0L
#> <bytecode: 0x0000023dad036de0>
#> <environment: namespace:base>
```








***

*Last knitted on 2022-05-26. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2021-10-12-one-liner-to-generate-ids.Rmd).*[^si] 

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
    #>  backports     1.4.1   2021-12-13 [1] CRAN (R 4.2.0)
    #>  broom         0.8.0   2022-04-13 [1] CRAN (R 4.2.0)
    #>  cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.2.0)
    #>  cli           3.3.0   2022-04-25 [1] CRAN (R 4.2.0)
    #>  colorspace    2.0-3   2022-02-21 [1] CRAN (R 4.2.0)
    #>  crayon        1.5.1   2022-03-26 [1] CRAN (R 4.2.0)
    #>  DBI           1.1.2   2021-12-20 [1] CRAN (R 4.2.0)
    #>  dbplyr        2.1.1   2021-04-06 [1] CRAN (R 4.2.0)
    #>  digest        0.6.29  2021-12-01 [1] CRAN (R 4.2.0)
    #>  dplyr       * 1.0.9   2022-04-28 [1] CRAN (R 4.2.0)
    #>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.2.0)
    #>  evaluate      0.15    2022-02-18 [1] CRAN (R 4.2.0)
    #>  fansi         1.0.3   2022-03-24 [1] CRAN (R 4.2.0)
    #>  forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.2.0)
    #>  fs            1.5.2   2021-12-08 [1] CRAN (R 4.2.0)
    #>  generics      0.1.2   2022-01-31 [1] CRAN (R 4.2.0)
    #>  ggplot2     * 3.3.6   2022-05-03 [1] CRAN (R 4.2.0)
    #>  git2r         0.30.1  2022-03-16 [1] CRAN (R 4.2.0)
    #>  glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
    #>  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.2.0)
    #>  haven         2.5.0   2022-04-15 [1] CRAN (R 4.2.0)
    #>  here          1.0.1   2020-12-13 [1] CRAN (R 4.2.0)
    #>  hms           1.1.1   2021-09-26 [1] CRAN (R 4.2.0)
    #>  httr          1.4.3   2022-05-04 [1] CRAN (R 4.2.0)
    #>  ids           1.0.1   2017-05-31 [1] CRAN (R 4.2.0)
    #>  jsonlite      1.8.0   2022-02-22 [1] CRAN (R 4.2.0)
    #>  knitr       * 1.39    2022-04-26 [1] CRAN (R 4.2.0)
    #>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.2.0)
    #>  lubridate     1.8.0   2021-10-07 [1] CRAN (R 4.2.0)
    #>  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
    #>  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.2.0)
    #>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.2.0)
    #>  pillar        1.7.0   2022-02-01 [1] CRAN (R 4.2.0)
    #>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
    #>  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.2.0)
    #>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
    #>  ragg          1.2.2   2022-02-21 [1] CRAN (R 4.2.0)
    #>  readr       * 2.1.2   2022-01-30 [1] CRAN (R 4.2.0)
    #>  readxl        1.4.0   2022-03-28 [1] CRAN (R 4.2.0)
    #>  reprex        2.0.1   2021-08-05 [1] CRAN (R 4.2.0)
    #>  rlang         1.0.2   2022-03-04 [1] CRAN (R 4.2.0)
    #>  rprojroot     2.0.3   2022-04-02 [1] CRAN (R 4.2.0)
    #>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.2.0)
    #>  rvest         1.0.2   2021-10-16 [1] CRAN (R 4.2.0)
    #>  scales        1.2.0   2022-04-13 [1] CRAN (R 4.2.0)
    #>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
    #>  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.2.0)
    #>  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.2.0)
    #>  systemfonts   1.0.4   2022-02-11 [1] CRAN (R 4.2.0)
    #>  textshaping   0.3.6   2021-10-13 [1] CRAN (R 4.2.0)
    #>  tibble      * 3.1.7   2022-05-03 [1] CRAN (R 4.2.0)
    #>  tidyr       * 1.2.0   2022-02-01 [1] CRAN (R 4.2.0)
    #>  tidyselect    1.1.2   2022-02-21 [1] CRAN (R 4.2.0)
    #>  tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.2.0)
    #>  tzdb          0.3.0   2022-03-28 [1] CRAN (R 4.2.0)
    #>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.2.0)
    #>  uuid          1.1-0   2022-04-19 [1] CRAN (R 4.2.0)
    #>  vctrs         0.4.1   2022-04-13 [1] CRAN (R 4.2.0)
    #>  withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
    #>  xfun          0.31    2022-05-10 [1] CRAN (R 4.2.0)
    #>  xml2          1.3.3   2021-11-30 [1] CRAN (R 4.2.0)
    #> 
    #>  [1] C:/Users/trist/AppData/Local/R/win-library/4.2
    #>  [2] C:/Program Files/R/R-4.2.0/library
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
    ```
