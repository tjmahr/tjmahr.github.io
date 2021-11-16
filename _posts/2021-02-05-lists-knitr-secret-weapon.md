---
title: Lists are my secret weapon for reporting stats with knitr
excerpt: Tidying and splitting model summaries for inline reporting
tags:
  - r
  - knitr
  - printy
  - mixed effects
  - purrr
header:
  overlay_image: "assets/images/2021-02-05-sitka.jpg"
  image_description: "Sitka spruce"
  overlay_filter: rgba(10, 50, 50, 0.4)
  caption: "Photo credit: [**Roland Tanglao**, CC-A](https://commons.wikimedia.org/wiki/File:Picea_sitchensis_Wild_Pacific_Trail,_Ucluelet_2.jpg)"
---





I am going to describe my favorite knitr trick: **Using lists to
simplify inline reporting**. *Trick* might not do it justice. I consider
this a *best practice* for working with knitr.


## Plug it in

[Inline reporting][inline] lets you insert R expressions inside of
markdown text. Those expressions are evaluated and their results are
plugged in as text. The following example shows a common use case:
Reporting descriptive statistics. The [`sitka` dataset][sitka]
describes the longitudinal growth of Sitka spruce trees in different
ozone conditions, so here are some lines we might report:

<!-- If I remove the backtick+r+single-quote-single-quote+backtick sequence, -->
<!-- the R code will run and be syntax highlighted. But there will be a blank -->
<!-- line before it. I can't figure out how to get rid of it. So we do it -->
<!-- this way. -->

````md
```{r}
library(magrittr)
data("sitka", package = "gamair")
n_trees <- length(levels(sitka$id.num))
n_conditions <- length(unique(sitka$ozone))
```

The dataset contains `r nrow(sitka)` tree size measurements 
from `r n_trees` trees grown in 
`r n_conditions` ozone treatment conditions.
````

which produces

````md
The dataset contains 1027 tree size measurements 
from 0 trees grown in 
2 ozone treatment conditions.
````

If we update the dataset, the numbers will update automatically when the
document is reknitted. It's just magical. Besides reporting statistics,
I routinely use inline reporting for package versions, dates, file
provenance, links to package documentation, and emoji. Here is an example
of each:

````md
```{r}
knitted_when <- format(Sys.Date())
knitted_where <- knitr::current_input()
knitted_with <- packageVersion("knitr")
knitted_doc_url <- downlit::autolink_url("knitr::knit()")
```

Reported prepared on `r knitted_when` from ``r knitted_where`` 
with knitr version `r knitted_with` `r emo::ji('smile')`. 
Read more about [`knitr::knit()`](`r knitted_doc_url`)`. 

::::

Reported prepared on 2021-11-16 from `2021-02-05-lists-knitr-secret-weapon.Rmd` 
with knitr version 1.36 😄.
Read more about [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html)`. 
````

## Are your variable names doing a list's job?

In this last example, I used prefixes in variable names to convey that
the data were related. `knitted_when`, `knitted_where` and
`knitted_with` are all facts about the knitting process. They are all
reported in the narrative text pretty close to each other. The prefix
informally bundles them together. The prefix also helps with writing our
code because we have to remember less. We can type `knitted_` and press
<kbd>Tab</kbd> and let autocompletion remind us which variables are
available.

<!-- This works interactively but not during knitting. -->


```r
# Simulate tab completion (what happens when we press Tab in after 
# `knitted_` in RStudio). The definition of `tab()` is not important.
"knitted_" %>% tab()
#> knitted_doc_url
#> knitted_when
#> knitted_where
#> knitted_with
```

But---and here is the key insight---what if we change that underscore
`_` into a dollar sign `$`, so to speak? That is, let's bundle everything
into a list and then report the results by accessing list elements.

````md
```{r}
knitted <- list(
  when = format(Sys.Date()),
  where = knitr::current_input(),
  with = packageVersion("knitr"),
  doc_url = downlit::autolink_url("knitr::knit()")
)
```

Reported prepared on `r knitted$when` from ``r knitted$where`` 
with knitr version `r knitted$with` `r emo::ji('happy')`. 
Read more about [`knitr::knit()`](`r knitted$doc_url`)`. 

::::

Reported prepared on 2021-11-16 from `2021-02-05-lists-knitr-secret-weapon.Rmd` 
with knitr version 1.36 😀. 
Read more about [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html)`. 
````

We have structured names, and we still retain our <kbd>Tab</kbd> completion:


```r
"knitted$" %>% tab()
#> knitted$when
#> knitted$where
#> knitted$with
#> knitted$doc_url
```

But we can also glance at our list to see everything about the knitting
process all at once.


```r
knitted
#> $when
#> [1] "2021-11-16"
#> 
#> $where
#> [1] "2021-02-05-lists-knitr-secret-weapon.Rmd"
#> 
#> $with
#> [1] '1.36'
#> 
#> $doc_url
#> [1] "https://rdrr.io/pkg/knitr/man/knit.html"
```

Basically, using a list formalizes the relationship we had implicitly
set out by using our naming convention, but so what? How does this help
inline reporting? Lists have all the nice benefits of using a naming
convention, plus one important feature: **We can create lists
programmatically**.


## Set up model results with `tidy()`

Let's say we model the growth of each tree in each ozone condition and
want to know how much steeper the growth rate is for the ozone treatment
condition. We fit a linear mixed model where we estimate the population
averages for the intercept and slope for each ozone condition, and we
use random effects to accord each tree its own intercept and slope.


```r
library(lme4)
#> Loading required package: Matrix
#> 
#> Attaching package: 'Matrix'
#> The following objects are masked from 'package:tidyr':
#> 
#>     expand, pack, unpack
# Rescale to get improve convergence
sitka$hund_days <- sitka$days / 100
m <- lmer(
  log.size ~ hund_days * ozone + (hund_days | id.num),
  sitka
) 
summary(m)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: log.size ~ hund_days * ozone + (hund_days | id.num)
#>    Data: sitka
#> 
#> REML criterion at convergence: 767
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.6836 -0.4723  0.0078  0.4237  2.5919 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev. Corr 
#>  id.num   (Intercept) 0.390928 0.62524       
#>           hund_days   0.002459 0.04959  -0.23
#>  Residual             0.082770 0.28770       
#> Number of obs: 1027, groups:  id.num, 79
#> 
#> Fixed effects:
#>                 Estimate Std. Error t value
#> (Intercept)      4.25491    0.13100  32.481
#> hund_days        0.33903    0.01278  26.528
#> ozone           -0.14101    0.15844  -0.890
#> hund_days:ozone -0.03611    0.01546  -2.336
#> 
#> Correlation of Fixed Effects:
#>             (Intr) hnd_dy ozone 
#> hund_days   -0.345              
#> ozone       -0.827  0.286       
#> hund_dys:zn  0.286 -0.827 -0.345
```

Our job is get to numbers from this summary view into prose. For this
example, we want to report that the two groups don't have a
statistically
[clear](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13159)
difference in their intercepts, as given by the `ozone` line in the
model summary. We also want to report that growth per 100 days is
statistically significantly slower in the ozone group `hund_days:ozone`.

First, we
[`tidy()`](https://rdrr.io/pkg/broom.mixed/man/lme4_tidiers.html) the
model summary using broom.mixed.


```r
library(tidyverse)
library(broom.mixed)
tidy(m, conf.int = TRUE) %>% 
  filter(effect == "fixed") 
#> # A tibble: 4 x 8
#>   effect group term            estimate std.error statistic conf.low conf.high
#>   <chr>  <chr> <chr>              <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 fixed  <NA>  (Intercept)       4.25      0.131     32.5     4.00     4.51   
#> 2 fixed  <NA>  hund_days         0.339     0.0128    26.5     0.314    0.364  
#> 3 fixed  <NA>  ozone            -0.141     0.158     -0.890  -0.452    0.170  
#> 4 fixed  <NA>  hund_days:ozone  -0.0361    0.0155    -2.34   -0.0664  -0.00581
```

We are also going to format the numbers. I have my own R package for
this job called [printy](https://github.com/tjmahr/printy). Below I use
it to round numbers---[`round()`](https://rdrr.io/r/base/Round.html)
drops 0s off the ends of rounded numbers whereas
`printy::fmt_fixed_digits()` keeps them. I also use it for formatting minus
signs.


```r
text_ready <- tidy(m, conf.int = TRUE) %>% 
  filter(effect == "fixed") %>% 
  mutate(
    # round the numbers
    across(
      c(estimate, conf.low, conf.high), 
      printy::fmt_fix_digits, 
      2
    ),
    se = printy::fmt_fix_digits(std.error, 3),
    # use a minus sign instead of a hyphen for negative numbers
    across(
      c(estimate, conf.low, conf.high), 
      printy::fmt_minus_sign
    ),
    ci = glue::glue("[{conf.low}, {conf.high}]")
  ) %>% 
  select(term, estimate, se, ci)
text_ready
#> # A tibble: 4 x 4
#>   term            estimate    se    ci                        
#>   <chr>           <chr>       <chr> <glue>                    
#> 1 (Intercept)     4.25        0.131 [4.00, 4.51]              
#> 2 hund_days       0.34        0.013 [0.31, 0.36]              
#> 3 ozone           &minus;0.14 0.158 [&minus;0.45, 0.17]       
#> 4 hund_days:ozone &minus;0.04 0.015 [&minus;0.07, &minus;0.01]
```



We could use dataframe functions to `filter()` down to the down the
terms and `pull()` the values and use a list:

````md
```{r}
stats <- list()
stats$b_intercept <- text_ready %>% 
  filter(term == "(Intercept)") %>% 
  pull(estimate)
```

The average log-size in the control condition was `r stats$b_intercept` units.

::::

The average log-size in the control condition was 4.25 units.
````

(The documentation for `sitka$log.size` doesn't say what units the 
data are in, so I'm sticking with "units" 🙃.)

## `split()` makes lists

A much better approach is to use
[`split()`](https://rdrr.io/r/base/split.html) to create a list using
the values in a dataframe column. To make the list easier for typing, I
use
[`janitor::make_clean_names()`](https://rdrr.io/pkg/janitor/man/make_clean_names.html)
to clean up the `term` value.


```r
stats <- text_ready %>% 
  mutate(term = janitor::make_clean_names(term)) %>%
  split(.$term)
```

Now we have a list of one-row dataframes:


```r
str(stats)
#> List of 4
#>  $ hund_days      : tibble [1 x 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ term    : chr "hund_days"
#>   ..$ estimate: chr "0.34"
#>   ..$ se      : chr "0.013"
#>   ..$ ci      : 'glue' chr "[0.31, 0.36]"
#>  $ hund_days_ozone: tibble [1 x 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ term    : chr "hund_days_ozone"
#>   ..$ estimate: chr "&minus;0.04"
#>   ..$ se      : chr "0.015"
#>   ..$ ci      : 'glue' chr "[&minus;0.07, &minus;0.01]"
#>  $ intercept      : tibble [1 x 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ term    : chr "intercept"
#>   ..$ estimate: chr "4.25"
#>   ..$ se      : chr "0.131"
#>   ..$ ci      : 'glue' chr "[4.00, 4.51]"
#>  $ ozone          : tibble [1 x 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ term    : chr "ozone"
#>   ..$ estimate: chr "&minus;0.14"
#>   ..$ se      : chr "0.158"
#>   ..$ ci      : 'glue' chr "[&minus;0.45, 0.17]"
```

And we have structured, autocomplete-friendly names too:


```r
"stats$" %>% tab()
#> stats$hund_days
#> stats$hund_days_ozone
#> stats$intercept
#> stats$ozone

"stats$ozone$" %>% tab()
#> stats$ozone$term
#> stats$ozone$estimate
#> stats$ozone$se
#> stats$ozone$ci
```

Now, we can write up our results with inline reporting:

````md
```{r}
stats <- text_ready %>% 
  mutate(term = janitor::make_clean_names(term)) %>%
  split(.$term)
```

The average log-size in the control condition was 
`r stats$intercept$estimate` units, 95% Wald CI `r stats$intercept$ci`. 
There was not a statistically clear difference between the 
ozone conditions for their intercepts (day-0 values), 
*B* = `r stats$ozone$estimate`, `r stats$ozone$ci`.
For the control group, the average growth rate was 
`r stats$hund_days$estimate` log-size units per 100 days, 
`r stats$hund_days$ci`. The growth rate for 
the ozone treatment group was significantly slower, 
*diff* = `r stats$hund_days_ozone$estimate`, 
`r stats$hund_days_ozone$ci`.

::::

The average log-size in the control condition was 
4.25 units, 95% Wald CI [4.00, 4.51]. 
There was not a statistically clear difference between the 
ozone conditions for their intercepts (day-0 values), 
*B* = &minus;0.14, [&minus;0.45, 0.17].
For the control group, the average growth rate was 
0.34 log-size units per 100 days, 
[0.31, 0.36]. The growth rate for the ozone treatment group was 
significantly slower, *diff* = &minus;0.04, 
[&minus;0.07, &minus;0.01].
````

Isn't that RMarkdown text just *a joy* to read? Everything so neatly named
and organized, and we got all of that for free by using `tidy()` and
`split()` to make a list. 

**Wrap it up, put a bow on it.** By the way, we can also make the
RMarkdown source even neater by using my [WrapRmd RStudio
plugin][wraprmd] which wraps the text so lines of text are all less
than a given width. Some other tools for rewrapping markdown text will
insert line breaks inside of spans of inline reporting and break them. I
have my RStudio set so that
<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>/</kbd> will rewrap
the RMarkdown text.
{: .notice--info}

## Splitting splits of splits

I am such a champion of this approach that I wrote my own split function
for splitting by multiple variables. In the
[`mtcars`](https://rdrr.io/r/datasets/mtcars.html) dataset,
suppose we want to report the mean mpg of 6- and 8-cylinder (`cyl`)
vehicles split by automatic versus manual (`am`) vehicles. We compute
the stats with some basic dplyring and we prepare names that work better
with `split()`.


```r
car_means <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(
    n = n(), 
    mean_mpg = mean(mpg), 
    .groups = "drop"
  ) %>% 
  # make names for spliting
  mutate(
    a = paste0("am_", am),
    c = paste0("cyl_", cyl),
  )
car_means
#> # A tibble: 6 x 6
#>     cyl    am     n mean_mpg a     c    
#>   <dbl> <dbl> <int>    <dbl> <chr> <chr>
#> 1     4     0     3     22.9 am_0  cyl_4
#> 2     4     1     8     28.1 am_1  cyl_4
#> 3     6     0     4     19.1 am_0  cyl_6
#> 4     6     1     3     20.6 am_1  cyl_6
#> 5     8     0    12     15.0 am_0  cyl_8
#> 6     8     1     2     15.4 am_1  cyl_8
```

Now enter `super_split()`:


```r
car_stats <- car_means %>% 
  printy::super_split(a, c)

# set `max.level` to not print individual tibble structures
str(car_stats, max.level = 3)
#> List of 2
#>  $ am_0:List of 3
#>   ..$ cyl_4: tibble [1 x 6] (S3: tbl_df/tbl/data.frame)
#>   ..$ cyl_6: tibble [1 x 6] (S3: tbl_df/tbl/data.frame)
#>   ..$ cyl_8: tibble [1 x 6] (S3: tbl_df/tbl/data.frame)
#>  $ am_1:List of 3
#>   ..$ cyl_4: tibble [1 x 6] (S3: tbl_df/tbl/data.frame)
#>   ..$ cyl_6: tibble [1 x 6] (S3: tbl_df/tbl/data.frame)
#>   ..$ cyl_8: tibble [1 x 6] (S3: tbl_df/tbl/data.frame)
```

Here, we have a list of lists of 1-row dataframes, and we can just use
`$` to drill down the lists during inline reporting.

````md
The average mpg of the `r car_stats$am_0$cyl_4$n` automatic, 
four-cylinder cars was `r car_stats$am_0$cyl_4$mean_mpg`.

::::

The average mpg of the 3 automatic, 
four-cylinder cars was 22.9.
````

For the curious, `super_split()` works behind the scenes by exploiting
two functions:

  - `split()` adds a level of depth to a list by splitting a list into
    sublists using a variable.
  - [`purrr::map_depth(.x, .depth,
    .f)`](https://purrr.tidyverse.org/reference/map_if.html)
    applies a function `.f` on the lists at a given `.depth`.

So the function walks through each variable and applies `split()` at
successively deeper depths. 


```r
super_split <- function(.data, ...) {
  dots <- rlang::enquos(...)
  for (var in seq_along(dots)) {
    var_name <- rlang::as_name(dots[[var]])
    .data <- purrr::map_depth(
      .x = .data,
      .depth = var - 1,
      .f = function(xs) split(xs, xs[var_name])
    )
  }
  .data
}
```

The first variable splits the list at depth 0, the second variable
splits the sublists at depth 1 (which were created in the prior split),
and so on. The business with
[`enquos(...)`](https://rlang.r-lib.org/reference/nse-defuse.html) is there
to let me refer to the variable names directly.





***

*Last knitted on 2021-11-16. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2021-02-05-lists-knitr-secret-weapon.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info  --------------------------------------------------------------
    #>  hash: selfie: medium-dark skin tone, fearful face, paintbrush
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
    #>  package     * version    date (UTC) lib source
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.1.0)
    #>  backports     1.3.0      2021-10-27 [1] CRAN (R 4.1.1)
    #>  boot          1.3-28     2021-05-03 [2] CRAN (R 4.1.2)
    #>  broom         0.7.10     2021-10-31 [1] CRAN (R 4.1.1)
    #>  broom.mixed * 0.2.7      2021-07-07 [1] CRAN (R 4.1.0)
    #>  cachem        1.0.6      2021-08-19 [1] CRAN (R 4.1.1)
    #>  cellranger    1.1.0      2016-07-27 [1] CRAN (R 4.1.0)
    #>  cli           3.1.0      2021-10-27 [1] CRAN (R 4.1.1)
    #>  colorspace    2.0-2      2021-06-24 [1] CRAN (R 4.1.0)
    #>  crayon        1.4.2      2021-10-29 [1] CRAN (R 4.1.1)
    #>  DBI           1.1.1      2021-01-15 [1] CRAN (R 4.1.0)
    #>  dbplyr        2.1.1      2021-04-06 [1] CRAN (R 4.1.0)
    #>  downlit       0.4.0      2021-10-29 [1] CRAN (R 4.1.1)
    #>  dplyr       * 1.0.7      2021-06-18 [1] CRAN (R 4.1.0)
    #>  ellipsis      0.3.2      2021-04-29 [1] CRAN (R 4.1.0)
    #>  emo           0.0.0.9000 2021-10-14 [1] Github (hadley/emo@3f03b11)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.1.0)
    #>  fansi         0.5.0      2021-05-25 [1] CRAN (R 4.1.0)
    #>  fastmap       1.1.0      2021-01-25 [1] CRAN (R 4.1.0)
    #>  forcats     * 0.5.1      2021-01-27 [1] CRAN (R 4.1.0)
    #>  fs            1.5.0      2020-07-31 [1] CRAN (R 4.1.0)
    #>  generics      0.1.1      2021-10-25 [1] CRAN (R 4.1.1)
    #>  ggplot2     * 3.3.5      2021-06-25 [1] CRAN (R 4.1.0)
    #>  git2r         0.28.0     2021-01-10 [1] CRAN (R 4.1.1)
    #>  glue          1.4.2      2020-08-27 [1] CRAN (R 4.1.1)
    #>  gtable        0.3.0      2019-03-25 [1] CRAN (R 4.1.0)
    #>  haven         2.4.3      2021-08-04 [1] CRAN (R 4.1.0)
    #>  here          1.0.1      2020-12-13 [1] CRAN (R 4.1.0)
    #>  hms           1.1.1      2021-09-26 [1] CRAN (R 4.1.1)
    #>  httr          1.4.2      2020-07-20 [1] CRAN (R 4.1.0)
    #>  janitor       2.1.0      2021-01-05 [1] CRAN (R 4.1.0)
    #>  jsonlite      1.7.2      2020-12-09 [1] CRAN (R 4.1.0)
    #>  knitr       * 1.36       2021-09-29 [1] CRAN (R 4.1.1)
    #>  lattice       0.20-45    2021-09-22 [2] CRAN (R 4.1.2)
    #>  lifecycle     1.0.1      2021-09-24 [1] CRAN (R 4.1.1)
    #>  lme4        * 1.1-27.1   2021-06-22 [1] CRAN (R 4.1.0)
    #>  lubridate     1.8.0      2021-10-07 [1] CRAN (R 4.1.1)
    #>  magrittr      2.0.1      2020-11-17 [1] CRAN (R 4.1.0)
    #>  MASS          7.3-54     2021-05-03 [2] CRAN (R 4.1.2)
    #>  Matrix      * 1.3-4      2021-06-01 [2] CRAN (R 4.1.2)
    #>  memoise       2.0.0      2021-01-26 [1] CRAN (R 4.1.0)
    #>  minqa         1.2.4      2014-10-09 [1] CRAN (R 4.1.0)
    #>  modelr        0.1.8      2020-05-19 [1] CRAN (R 4.1.0)
    #>  munsell       0.5.0      2018-06-12 [1] CRAN (R 4.1.0)
    #>  nlme          3.1-153    2021-09-07 [2] CRAN (R 4.1.2)
    #>  nloptr        1.2.2.2    2020-07-02 [1] CRAN (R 4.1.1)
    #>  pillar        1.6.4      2021-10-18 [1] CRAN (R 4.1.1)
    #>  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.1.0)
    #>  printy        0.0.0.9003 2021-10-14 [1] Github (tjmahr/printy@df0d96e)
    #>  purrr       * 0.3.4      2020-04-17 [1] CRAN (R 4.1.0)
    #>  R6            2.5.1      2021-08-19 [1] CRAN (R 4.1.1)
    #>  ragg          1.2.0      2021-10-30 [1] CRAN (R 4.1.1)
    #>  Rcpp          1.0.7      2021-07-07 [1] CRAN (R 4.1.0)
    #>  readr       * 2.0.2      2021-09-27 [1] CRAN (R 4.1.1)
    #>  readxl        1.3.1      2019-03-13 [1] CRAN (R 4.1.0)
    #>  reprex        2.0.1      2021-08-05 [1] CRAN (R 4.1.0)
    #>  rlang         0.4.12     2021-10-18 [1] CRAN (R 4.1.1)
    #>  rprojroot     2.0.2      2020-11-15 [1] CRAN (R 4.1.0)
    #>  rstudioapi    0.13       2020-11-12 [1] CRAN (R 4.1.0)
    #>  rvest         1.0.2      2021-10-16 [1] CRAN (R 4.1.1)
    #>  scales        1.1.1      2020-05-11 [1] CRAN (R 4.1.0)
    #>  sessioninfo   1.2.1      2021-11-02 [1] CRAN (R 4.1.2)
    #>  snakecase     0.11.0     2019-05-25 [1] CRAN (R 4.1.0)
    #>  stringi       1.7.5      2021-10-04 [1] CRAN (R 4.1.1)
    #>  stringr     * 1.4.0      2019-02-10 [1] CRAN (R 4.1.0)
    #>  systemfonts   1.0.3      2021-10-13 [1] CRAN (R 4.1.1)
    #>  textshaping   0.3.6      2021-10-13 [1] CRAN (R 4.1.1)
    #>  tibble      * 3.1.5      2021-09-30 [1] CRAN (R 4.1.1)
    #>  tidyr       * 1.1.4      2021-09-27 [1] CRAN (R 4.1.1)
    #>  tidyselect    1.1.1      2021-04-30 [1] CRAN (R 4.1.0)
    #>  tidyverse   * 1.3.1      2021-04-15 [1] CRAN (R 4.1.0)
    #>  tzdb          0.2.0      2021-10-27 [1] CRAN (R 4.1.1)
    #>  utf8          1.2.2      2021-07-24 [1] CRAN (R 4.1.0)
    #>  vctrs         0.3.8      2021-04-29 [1] CRAN (R 4.1.0)
    #>  withr         2.4.2      2021-04-18 [1] CRAN (R 4.1.0)
    #>  xfun          0.27       2021-10-18 [1] CRAN (R 4.1.1)
    #>  xml2          1.3.2      2020-04-23 [1] CRAN (R 4.1.0)
    #>  yaml          2.2.1      2020-02-01 [1] CRAN (R 4.1.0)
    #> 
    #>  [1] C:/Users/trist/Documents/R/win-library/4.1
    #>  [2] C:/Program Files/R/R-4.1.2/library
    #> 
    #> ------------------------------------------------------------------------------
    ```

[inline]: https://bookdown.org/yihui/rmarkdown-cookbook/r-code.html "Inline reporting page in a RMarkdown Cookbook"
[sitka]: https://rdrr.io/cran/gamair/man/sitka.html "Documentation of sitka dataset"
[wraprmd]: https://github.com/tjmahr/WrapRmd "WrapRmd GitHub page"
