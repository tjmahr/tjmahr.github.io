---
title: Keep your R scripts locally sourced
excerpt: A lesson from debugging `source()`
tags:
  - r
  - knitr
  - nonstandard evaluation
share: true
header:
  overlay_image: "assets/images/2021-08-local-farm.jpg"
  image_description: "A sign that says 'locally grown fresh from the farm'"
  overlay_filter: rgba(10, 10, 10, 0.5)
  caption: "Photo credit: [**Dave Phillips**](https://unsplash.com/photos/r0ZrCr7ZVl0)"
---



A few weeks ago, I had a *bad* debugging session. The code was just not
doing what I expected, and I went down a lot of deadends trying to fix
or simplify things. I could not get the problem to happen in a
reproducible example ([reprex](https://reprex.tidyverse.org/)) or
interactively (in RStudio). Eventually, the most minimal example of the
problem completely broke my mental model for how the code should work.

The problem had to do with names and what they mean. `select()` is a
function the lives in the MASS package and the dplyr package, and I
always intend for `select()` to point to
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
But sometimes a statistics package will load in MASS and overwrite
`select()` to point to
[`MASS::select()`](https://rdrr.io/pkg/MASS/man/lm.ridge.html). And in
this case, my attempts to use `select()` in a
[`source()`](https://rdrr.io/r/base/source.html)-ed file kept reverting
to `MASS::select()` instead of `dplyr::select()`. A tweet from the
session shows the minimal example and my wracked brain. (I will describe
the example in more detail below.)

<blockquote class="twitter-tweet" data-conversation="none" data-lang="en" data-dnt="true" data-theme="light">
<p lang="en" dir="ltr">i&#39;m dry heaving here wtf is going <a href="https://t.co/KIeRJT6kwY">pic.twitter.com/KIeRJT6kwY</a></p>

  <img src="/assets/images/2021-08-wtf-debugging.jpg" width="60%" alt="Code/output where I map `select` to `dplyr::select`, create a file with one function that prints the environment of `select`, print `select` (namespace:dplyr), call the function (namespace:MASS), and print `select` (namespace:dplyr)" />
  <br/>
  &mdash; tj mahr 🍍🍕 (@tjmahr) <a href="https://twitter.com/tjmahr/status/1417894498080800769?ref_src=twsrc%5Etfw">July 21, 2021</a>
</blockquote> 

Here's what happens:

1.  I explicitly assign `select` to `dplyr::select()`.
2.  I make a function `f()` that prints the environment of `select`
    (where the name/function is defined), store the function in a `.R`
    text file and `source()` in the text file. (`source()` runs the code
    in an R script.)
3.  I print the value of `select` and see that it is indeed from the
    dplyr environment.
4.  I call my function, and it says that `select` is actually in the
    MASS package.
5.  I check the value of `select`, and it reports the dplyr environment
    once again.
    

    
## A similar problem using functions

This problem only happened while knitting [one of my analysis
notebooks][notestar] (which was a clue). Right now, it's proving
difficult for me to write examples of this problem for this blogpost, so
I'm going to show the source 😉 of the problem using functions.

[notestar]: https://github.com/tjmahr/notestar "My notebook system"

First, let's set up things so that `select` belongs to the MASS package.
We are also going to use the [conflicted][conflicted] package which normally prevents
package name *conflicts* from happening. This part isn't necessary or
helpful; I just want to illustrate that this is not a simple name
conflict problem.

[conflicted]: https://conflicted.r-lib.org/ "conflicted: An Alternative Conflict Resolution Strategy"



```r
library(conflicted)
library(MASS)
environment(select)
#> <environment: namespace:MASS>
```

We are going to make a function that does what my original code example
tried to do:

  - set `select` to dplyr explicitly
  - `source()` in a file that gives the environment of `select`
  - return the environment of `select`, both using the `source()`-ed
    function and directly.


```r
source_in_my_code <- function(...) {
  # set dplyr select
  select <- dplyr::select
  
  # write a script to temporary file
  temp_script <- tempfile(fileext = ".R")
  my_code <- "
    f <- function() environment(select)
  "
  writeLines(my_code, temp_script)
  
  # run the script
  source(temp_script, ...)
  
  list(
    source_select_environment = f(),
    function_select_environment = environment(select)
  )
}


default_results <- source_in_my_code()
```

What do you think the `select` environment should be? dplyr, right?
That's what `select` means everywhere else inside of the function.
`source()` is just like dropping in some R code and running it, right?
That's what I thought.


```r
default_results
#> $source_select_environment
#> <environment: namespace:MASS>
#> 
#> $function_select_environment
#> <environment: namespace:dplyr>
```

No, it's the MASS environment. 😕




## Local and parent environments

In order to understand what's happening, let's first note that R works
by evaluating expressions in an environment. The environment defines the
values of names. If a name is not found in an environment, R searches
parent environment for the name (or the parent's parent, and so on).
This idea is [illustrated beautifully in *Advanced R* using
diagrams](https://adv-r.hadley.nz/environments.html#parents).

For an analogy, you might think of environments as looking up someone in
an office, a building directory, then an area directory:


<blockquote class="twitter-tweet" data-conversation="none" data-lang="en" data-dnt="true" data-theme="light">
<p lang="en" dir="ltr">I like the multi-company building analogy. If you want to call Jim, first you look in your company directory. If there isn’t a Jim there, you look in the all-building maintenance dir. If not there, you look in the city services dir. You don’t look in another company-specific dir
  </p>
  &mdash; Brenton Wiernik 🏳️‍🌈 (@bmwiernik) <a href="https://twitter.com/bmwiernik/status/1387164714451488772?ref_src=twsrc%5Etfw">April 27, 2021</a>
</blockquote> 

Here is small example showing a local function environment, its
parent environment and how a name will take different values depending on
the context.


```r
where_am_i <- "outside of the function"
where_are_you <- "outside of the function too"

where_is_everyone <- function() {
  where_am_i <- "inside of the function"
  list(
    where_am_i = where_am_i,
    where_are_you = where_are_you
  )
} 

where_am_i
#> [1] "outside of the function"
where_is_everyone()
#> $where_am_i
#> [1] "inside of the function"
#> 
#> $where_are_you
#> [1] "outside of the function too"
where_am_i
#> [1] "outside of the function"
```

Outside of the function, `where_am_i` is `"outside of the function"`,
but in the body of the function, it is defined to `"inside of the
function"`. The variable `where_are_you` is *only* defined `"out of the
function too"`, so the function has to search for the variable in its
parent environment.


<blockquote class="twitter-tweet" data-conversation="none" data-lang="en" data-dnt="true" data-theme="light">
<p lang="en" dir="ltr">&quot;parent&quot; environment suggests a family metaphor. if you cant find what a symbol means, ask a parent.</p>
  &mdash; tj mahr 🍍🍕 (@tjmahr) <a href="https://twitter.com/tjmahr/status/1387087953982328833?ref_src=twsrc%5Etfw">April 27, 2021</a>
</blockquote> 

## Locally sourced R code

Reading the [documentation to `source()`](https://rdrr.io/r/base/source.html), we find the solution to the
original problem:

> **Arguments**
>
> **`local`** \
> `TRUE`, `FALSE` or an environment, determining where the parsed
> expressions are evaluated. `FALSE` (the default) corresponds to the
> user's workspace (the global environment) and `TRUE` to the
> environment from which `source` is called.

By default, the code evaluated by `source()` runs in the global
environment--that is, "outside" of the body of the function. The code
*breaks out* of the function environment and runs at the higher
environment. 

My mental model for `source()` was completely wrong. `source()` is not
like dropping in the R code from a file and running it. It is more like
pausing everything that you're doing in your current context, backing
out to the highest level context, running that code, and then resuming
what you're doing. 

Fortunately, if we ask source to run locally (`local = TRUE`), `select`
has the same environment inside the function and in the code run using
`source()`.


```r
# I defined the function so it could pass arguments to source()
source_in_my_code(local = TRUE)
#> $source_select_environment
#> <environment: namespace:dplyr>
#> 
#> $function_select_environment
#> <environment: namespace:dplyr>
```

When we're using `source()` as one of the first few lines of an R
script, the default global environment for `source()` doesn't really
matter. But in contexts like the function example or code stored in a
custom knitr/RMarkdown setup (my original problem), this difference *is*
a problem. Therefore, in the future, I'm going to abide by the motto
*Keep it locally sourced*. This way fits my mental model for `source()`
as something that drops in R code and runs it in place.

And by the way, yes, even though I cited *Advanced R* above, I clearly
did not do all of the exercises:

> [20.2.4 Exercises](https://adv-r.hadley.nz/evaluation.html#exercises-61)
> 
> 1.  Carefully read the documentation for `source()`. What environment
>     does it use by default? What if you supply `local = TRUE`? How do
>     you provide a custom environment?







***

*Last knitted on 2022-05-25. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2021-08-16-keep-it-locally-sourced.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.2.0 RC (2022-04-21 r82226 ucrt)
    #>  os       Windows 10 x64 (build 22000)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language (EN)
    #>  collate  English_United States.utf8
    #>  ctype    English_United States.utf8
    #>  tz       America/Chicago
    #>  date     2022-05-25
    #>  pandoc   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package     * version    date (UTC) lib source
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.2.0)
    #>  cachem        1.0.6      2021-08-19 [1] CRAN (R 4.2.0)
    #>  cli           3.2.0      2022-02-14 [1] CRAN (R 4.2.0)
    #>  conflicted  * 1.1.0      2021-11-26 [1] CRAN (R 4.2.0)
    #>  crayon        1.5.1      2022-03-26 [1] CRAN (R 4.2.0)
    #>  DBI           1.1.2      2021-12-20 [1] CRAN (R 4.2.0)
    #>  dplyr         1.0.9      2022-04-28 [1] CRAN (R 4.2.0)
    #>  ellipsis      0.3.2      2021-04-29 [1] CRAN (R 4.2.0)
    #>  emo           0.0.0.9000 2022-05-25 [1] Github (hadley/emo@3f03b11)
    #>  evaluate      0.15       2022-02-18 [1] CRAN (R 4.2.0)
    #>  fansi         1.0.3      2022-03-24 [1] CRAN (R 4.2.0)
    #>  fastmap       1.1.0      2021-01-25 [1] CRAN (R 4.2.0)
    #>  generics      0.1.2      2022-01-31 [1] CRAN (R 4.2.0)
    #>  git2r         0.30.1     2022-03-16 [1] CRAN (R 4.2.0)
    #>  glue          1.6.2      2022-02-24 [1] CRAN (R 4.2.0)
    #>  here          1.0.1      2020-12-13 [1] CRAN (R 4.2.0)
    #>  knitr       * 1.39       2022-04-26 [1] CRAN (R 4.2.0)
    #>  lifecycle     1.0.1      2021-09-24 [1] CRAN (R 4.2.0)
    #>  lubridate     1.8.0      2021-10-07 [1] CRAN (R 4.2.0)
    #>  magrittr      2.0.3      2022-03-30 [1] CRAN (R 4.2.0)
    #>  MASS        * 7.3-56     2022-03-23 [2] CRAN (R 4.2.0)
    #>  memoise       2.0.1      2021-11-26 [1] CRAN (R 4.2.0)
    #>  pillar        1.7.0      2022-02-01 [1] CRAN (R 4.2.0)
    #>  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.2.0)
    #>  purrr         0.3.4      2020-04-17 [1] CRAN (R 4.2.0)
    #>  R6            2.5.1      2021-08-19 [1] CRAN (R 4.2.0)
    #>  ragg          1.2.2      2022-02-21 [1] CRAN (R 4.2.0)
    #>  rlang         1.0.2      2022-03-04 [1] CRAN (R 4.2.0)
    #>  rprojroot     2.0.3      2022-04-02 [1] CRAN (R 4.2.0)
    #>  rstudioapi    0.13       2020-11-12 [1] CRAN (R 4.2.0)
    #>  sessioninfo   1.2.2      2021-12-06 [1] CRAN (R 4.2.0)
    #>  stringi       1.7.6      2021-11-29 [1] CRAN (R 4.2.0)
    #>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.2.0)
    #>  systemfonts   1.0.4      2022-02-11 [1] CRAN (R 4.2.0)
    #>  textshaping   0.3.6      2021-10-13 [1] CRAN (R 4.2.0)
    #>  tibble        3.1.7      2022-05-03 [1] CRAN (R 4.2.0)
    #>  tidyselect    1.1.2      2022-02-21 [1] CRAN (R 4.2.0)
    #>  utf8          1.2.2      2021-07-24 [1] CRAN (R 4.2.0)
    #>  vctrs         0.4.1      2022-04-13 [1] CRAN (R 4.2.0)
    #>  xfun          0.31       2022-05-10 [1] CRAN (R 4.2.0)
    #> 
    #>  [1] C:/Users/Tristan/AppData/Local/R/win-library/4.2
    #>  [2] C:/Program Files/R/R-4.2.0rc/library
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
    ```
