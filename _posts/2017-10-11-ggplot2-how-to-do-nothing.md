---
title: "Simplifying ggplot2 code by doing nothing"
excerpt: "The ggplot2 version of multiplying by 1"
tags:
  - bayesplot
  - ggplot2
  - r
---

Recently, I joined the development team for
[bayesplot](http://mc-stan.org/bayesplot/), an R package by the Stan team for
plotting Bayesian models. Because visualizing Bayesian models in ggplot2 is a
[recurring](/visualizing-uncertainty-rstanarm/)
[topic](/plotting-partial-pooling-in-mixed-effects-models/)
[here](/bayesian-fisher-exact-test/), it was a natural fit. So from time to
time, I'll post about some programming techniques and new features we develop in
the bayesplot package. 

For this post, I describe one of strategies I have been
using to clean up and simplify some of the plotting code in the package: **avoid
if-branches by sometimes plotting nothing**.

## Warm-up example

Let's start with a non-plotting example. We consider a function
that takes a list of counts of people and returns the total number of people.
That sum, however, is controlled by some of the function's arguments:


```r
library(magrittr)
head_count <- function(data, staff = FALSE, faculty = FALSE) {
  total <- data[["students"]]
  
  if (staff) {
    total <- total + data[["staff"]]
  }
  
  if (faculty) {
    total <- total + data[["faculty"]]
  }
  
  total
}

data <- list(students = 10, staff = 3, faculty = 2)
head_count(data)
#> [1] 10
head_count(data, staff = TRUE)
#> [1] 13
head_count(data, faculty = TRUE)
#> [1] 12
```

The function as it's written works fine... but the if-branches get in the
way. The main job of the function is to assemble something (a sum) from
pieces of data, but that assembly is split across the if-branches. The `total`
is updated in-place twice (`total <- total + ...`). I would rather combine all
the data in one fell swoop.

One way to move the data-assembly out of these if-branches is to make _the data_
conditional. That is, instead of doing the addition in the `if` statements, we
do all the addition at once but just add 0 when a piece of data isn't
needed. This technique simplifies the function definition:


```r
head_count <- function(data, staff = FALSE, faculty = FALSE) {
  staff <- if (staff) data[["staff"]] else 0
  faculty <- if (faculty) data[["faculty"]] else 0
  
  data[["students"]] + staff + faculty
}

head_count(data)
#> [1] 10
head_count(data, staff = TRUE)
#> [1] 13
head_count(data, faculty = TRUE)
#> [1] 12
```

For operations where we combine things to get a new thing, there's often an
identity element: a value we can plug in as a placeholder value without changing
the results. Some examples:


```r
# multiplication
pi * 1
#> [1] 3.141593

# string concatenation
paste0("a", "")
#> [1] "a"

# combining things
c(1, 2, 3, c())
#> [1] 1 2 3

# data-frame binding
rbind(head(iris), list())
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

# magrittr pipes
f <- function(x, ...) x
1:2 %>% f()
#> [1] 1 2
```

Below, I demonstrate two examples of how I applied a similar strategy in
bayesplot. In ggplot2, we assemble plots by adding layers and options to a plot
by stringing together statements with `+`. To handle conditional plotting
elements and construct the plot all in one pass, I took advantage of ways to
adding nothing to a ggplot2 plot.[^monoids]

## Posterior interval plots

Here are some posterior samples for a Bayesian model, in this case the ["eight schools"](http://andrewgelman.com/2014/01/21/everything-need-know-bayesian-statistics-learned-eight-schools/) meta-analysis example. We want to create a plot that
shows the 90% and 50% uncertainty intervals for each parameter. 

We first use a helper function to clean up the data to prepare it for plotting.
(At the time of writing, `mcmc_intervals_data()` is not yet in the CRAN version
of bayesplot.)


```r
library(dplyr, warn.conflicts = FALSE)
library(bayesplot)
#> This is bayesplot version 1.8.0.9000
#> - Online documentation and vignettes at mc-stan.org/bayesplot
#> - bayesplot theme set to bayesplot::theme_default()
#>    * Does _not_ affect other ggplot2 plots
#>    * See ?bayesplot_theme_set for details on theme setting
library(ggplot2)
theme_set(theme_grey())

interval_data <- shinystan::eight_schools@posterior_sample %>% 
  mcmc_intervals_data(pars = "mu", regex_pars = "theta") %>% 
  mutate_if(is.numeric, round, 2) 

interval_data
#> # A tibble: 9 x 9
#>   parameter outer_width inner_width point_est    ll     l     m     h    hh
#> * <fct>           <dbl>       <dbl> <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mu                0.9         0.5 median    -0.66  4.74  8.11 11.5   16.7
#> 2 theta[1]          0.9         0.5 median    -0.4   6.63 11.2  17.0   28.8
#> 3 theta[2]          0.9         0.5 median    -3.16  3.63  7.95 12.2   19.1
#> 4 theta[3]          0.9         0.5 median    -8.86  1.29  6.33 11.0   18.4
#> 5 theta[4]          0.9         0.5 median    -3.69  3.47  7.84 12.0   19.2
#> 6 theta[5]          0.9         0.5 median    -6.84  0.47  5.1   8.99  14.6
#> 7 theta[6]          0.9         0.5 median    -6.53  1.76  6.27 10.4   16.7
#> 8 theta[7]          0.9         0.5 median     0.33  6.67 10.8  15.7   23.7
#> 9 theta[8]          0.9         0.5 median    -4.97  3.79  8.53 13.5   22.9
```

Below is the basic plot we want to
create. (Normally, in bayesplot, we would use
[`mcmc_intervals()`](http://mc-stan.org/bayesplot/reference/MCMC-intervals.html)
to make this plot, but for the sake of illustration, we will write a version of
it from scratch.


```r
ggplot(interval_data) + 
  aes_(y = ~ parameter, yend = ~ parameter) +
  geom_segment(aes_(x = ~ ll, xend = ~ hh), size = 1) + 
  geom_segment(aes_(x = ~ l, xend = ~ h), size = 2) + 
  scale_y_discrete(limits = rev(interval_data$parameter)) + 
  labs(x = NULL, y = NULL)
```

<img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/intervals-1-1.png" title="An interval plot showing the 90% and 50% intervals for eight schools and average value in the eight schools model." alt="An interval plot showing the 90% and 50% intervals for eight schools and average value in the eight schools model." width="80%" style="display: block; margin: auto;" />

**Wait, what's going on with the tildes?** We use `aes_(y = ~ parameter)`
because in an R package, writing `aes(y = parameter)` would raise an undefined
global variable warning :warning: during CRAN package checks. The code looks like it uses
a variable called `parameter`, but a variable with that name has not been defined in
the code yet. The formula form `~ parameter` "quotes" the variable name, so
it doesn't appear as a global variable.
{: .notice--info}

We are going to wrap this code in an R function along with two
annotation options:

1.  whether to draw point estimates (medians) over the intervals
2.  whether to draw a vertical reference line at *x* = 0

Here is how we might write the function with branching code.


```r
plot_intervals <- function(
  data, 
  draw_points = TRUE, 
  draw_ref_line = TRUE,
  line_position = 0
) {
  p <- ggplot(data) + 
    aes_(y = ~ parameter, yend = ~ parameter) 
  
  if (draw_ref_line) {
    p <- p + 
      geom_vline(xintercept = line_position, size = 2, color = "white")
  }
  
  p <- p +
    geom_segment(aes_(x = ~ ll, xend = ~ hh), size = 1) + 
    geom_segment(aes_(x = ~ l, xend = ~ h), size = 2)
  
  if (draw_points) {
    p <- p + geom_point(aes_(x = ~ m), size = 3)
  }
  
   p + 
     scale_y_discrete(limits = rev(data$parameter)) + 
     labs(x = NULL, y = NULL)
}

# Test the code
plot_intervals(interval_data) + ggtitle("Points and line")
plot_intervals(interval_data, draw_points = FALSE) + ggtitle("No points")
plot_intervals(interval_data, draw_ref_line = FALSE) + ggtitle("No line")
```

<img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-1-1.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" /><img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-1-2.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" /><img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-1-3.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" />

As we can see, the plot is built up incrementally throughout the function.
The plot object `p` is updated three times (the `p <- p + ...` parts). This
makes it more difficult to understand the function when reading it and increases
the chance that we might do something wrong when working on this code.

We can improve the readability by using empty or placeholder elements to move
the plot updates outside of the if-branches.

## Do nothing by plotting an empty dataframe

ggplot2 is built to work on dataframe columns, and if there is no data in a
column, it will quietly plot nothing. Therefore, one way to do nothing is to run
the normal `geom_` plotting function but on an empty dataframe. Here, we apply
that strategy for the point estimates by toggling between the original data or
zero-row alternative in the first line.


```r
plot_intervals <- function(
  data, 
  draw_points = TRUE, 
  draw_ref_line = TRUE,
  line_position = 0
) {
  maybe_points <- if (draw_points) data else data[0, ]
  
  p <- ggplot(data) + 
    aes_(y = ~ parameter, yend = ~ parameter) 
  
  if (draw_ref_line) {
    p <- p + 
      geom_vline(xintercept = line_position, size = 2, color = "white")
  }
  
  p +
    geom_segment(aes_(x = ~ ll, xend = ~ hh), size = 1) + 
    geom_segment(aes_(x = ~ l, xend = ~ h), size = 2) + 
    geom_point(aes_(x = ~ m), data = maybe_points, size = 3) + 
    scale_y_discrete(limits = rev(data$parameter)) + 
    labs(x = NULL, y = NULL)
}

# Test the code
plot_intervals(interval_data) + ggtitle("Points and line")
plot_intervals(interval_data, draw_points = FALSE) + ggtitle("No points")
```

<img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-2-1.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" /><img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-2-2.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" />

This is a marked improvement over the original, as most of the plot construction
happens at the end of the function.

## Do nothing with `geom_blank()`

Another way to do nothing is to add `geom_blank()`. In fact, this is what
ggplot2 does when we print a ggplot2 object without any geom layers.


```r
ggplot(interval_data) + 
  aes(x = m, y = parameter)
```

<img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/blank-1.png" title="Demo of how ggplot uses geom_blank() to print plots" alt="Demo of how ggplot uses geom_blank() to print plots" width="80%" style="display: block; margin: auto;" />

When we inspect the last plot that was displayed, we see `geom_blank()` is one
of the layers:


```r
last_plot()[["layers"]]
#> [[1]]
#> geom_blank: na.rm = FALSE
#> stat_identity: na.rm = FALSE
#> position_identity
```

As a first pass for technique, we toggle between the plotting functions
`geom_vline()` and `geom_blank()`. This version simplifies the plot
construction to a single stream of plot additions.


```r
plot_intervals <- function(
  data, 
  draw_points = TRUE, 
  draw_ref_line = TRUE,
  line_position = 0
) {
  maybe_points <- if (draw_points) data else data[0, ]
  geom_maybe_vline <- if (draw_ref_line) geom_vline else geom_blank
    
  ggplot(data) + 
    aes_(y = ~ parameter, yend = ~ parameter) +
    geom_maybe_vline(xintercept = line_position, size = 2, color = "white") +
    geom_segment(aes_(x = ~ ll, xend = ~ hh), size = 1) + 
    geom_segment(aes_(x = ~ l, xend = ~ h), size = 2) + 
    geom_point(aes_(x = ~ m), data = maybe_points, size = 3) + 
    scale_y_discrete(limits = rev(data$parameter)) + 
    labs(x = NULL, y = NULL)
}

# Test the code
plot_intervals(interval_data) + ggtitle("Points and line")
plot_intervals(interval_data, draw_ref_line = FALSE) + ggtitle("No line")
#> Warning: Ignoring unknown parameters: xintercept, size, colour
```

<img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-3-1.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" /><img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-3-2.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" />

But it also issues a warning when it toggles to `geom_blank()`. That might alarm
some users, so we need to fix that. For bayesplot, I wrote a helper called
`geom_ignore()` as a version of `geom_blank()` that ignores any input arguments.
The function receives any number of arguments in the `...` placeholder argument,
but the function does nothing with those dots. It just ignores them.


```r
geom_ignore <- function(...) {
  geom_blank(
    mapping = NULL, 
    data = NULL,
    show.legend = FALSE, 
    inherit.aes = FALSE
  )
}
```

For completeness, here is the final form using `geom_ignore()`.


```r
plot_intervals <- function(
  data, 
  draw_points = TRUE, 
  draw_ref_line = TRUE,
  line_position = 0
) {
  maybe_points <- if (draw_points) data else data[0, ]
  geom_maybe_vline <- if (draw_ref_line) geom_vline else geom_ignore
    
  ggplot(data) + 
    aes_(y = ~ parameter, yend = ~ parameter) +
    geom_maybe_vline(xintercept = line_position, size = 2, color = "white") +
    geom_segment(aes_(x = ~ ll, xend = ~ hh), size = 1) + 
    geom_segment(aes_(x = ~ l, xend = ~ h), size = 2) + 
    geom_point(aes_(x = ~ m), data = maybe_points, size = 3) + 
    scale_y_discrete(limits = rev(data$parameter)) + 
    labs(x = NULL, y = NULL)
}

# Test the code
plot_intervals(interval_data) + ggtitle("Points and line")
plot_intervals(interval_data, draw_ref_line = FALSE) + ggtitle("No line")
```

<img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-4-1.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" /><img src="/figs/2017-10-11-ggplot2-how-to-do-nothing/test-4-2.png" title="A test of the plot_intervals() function" alt="A test of the plot_intervals() function" width="50%" />

## A final note on `NULL` [_updated_] 

I should come clean and note that these blank layers are not really identity
elements. Unlike adding 0 to a number, using these techniques to add nothing to
a plot still updates the plot's data. For the plot object below, the first layer
says `geom_blank` and the last layer says `geom_point`. Visually, we have done
nothing, but internally, we left a trace behind.


```r
p <- plot_intervals(
  interval_data, 
  draw_points = FALSE, 
  draw_ref_line = FALSE
)

p[["layers"]][[1]]
#> geom_blank: na.rm = FALSE
#> stat_identity: na.rm = FALSE
#> position_identity

p[["layers"]][[4]]
#> mapping: x = ~m 
#> geom_point: na.rm = FALSE
#> stat_identity: na.rm = FALSE
#> position_identity
```

**Update** After some playing around, I discovered that adding `NULL` to
a ggplot doesn't change it (e.g., `p + NULL` does not leave a trace), so
it provides a stronger identity element for ggplot2 than invisible
layers. In the original version of this post, I said that I would not
use `NULL` in this way because it seemed like an edge case that might
break some day. But the creator of ggplot2, Hadley Wickham, [told me
that using `NULL` as an identity element is a deliberate design
feature](https://twitter.com/hadleywickham/status/918438633111203841).
🎉 Cool! Now, we can make `geom_ignore()` even
simpler.


```r
geom_ignore <- function(...) {
  NULL
}
```

[^monoids]: This technique of using an empty element with combiner functions was inspired by the concept of [monoids](https://en.wikipedia.org/wiki/Monoid) which come up in functional programming. Monoids have a strict formal definition, and ggplot2 are certainly not monoids because everything must be added onto an initial `ggplot()` object. 





***

*Last knitted on 2021-02-03. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2017-10-11-ggplot2-how-to-do-nothing.Rmd).*[^si] 

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
    #>  date     2021-02-03                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  ! package      * version    date       lib source                     
    #>    assertthat     0.2.1      2019-03-21 [1] CRAN (R 4.0.2)             
    #>    base64enc      0.1-3      2015-07-28 [1] CRAN (R 4.0.0)             
    #>    bayesplot    * 1.8.0.9000 2021-02-01 [1] local                      
    #>    callr          3.5.1      2020-10-13 [1] CRAN (R 4.0.3)             
    #>    cli            2.2.0      2020-11-20 [1] CRAN (R 4.0.3)             
    #>    codetools      0.2-18     2020-11-04 [1] CRAN (R 4.0.2)             
    #>    colorspace     2.0-0      2020-11-11 [1] CRAN (R 4.0.3)             
    #>    colourpicker   1.1.0      2020-09-14 [1] CRAN (R 4.0.2)             
    #>    crayon         1.4.0      2021-01-30 [1] CRAN (R 4.0.3)             
    #>    crosstalk      1.1.1      2021-01-12 [1] CRAN (R 4.0.3)             
    #>    curl           4.3        2019-12-02 [1] CRAN (R 4.0.2)             
    #>    DBI            1.1.1      2021-01-15 [1] CRAN (R 4.0.3)             
    #>    digest         0.6.27     2020-10-24 [1] CRAN (R 4.0.3)             
    #>    dplyr        * 1.0.3      2021-01-15 [1] CRAN (R 4.0.3)             
    #>    DT             0.17       2021-01-06 [1] CRAN (R 4.0.3)             
    #>    dygraphs       1.1.1.6    2018-07-11 [1] CRAN (R 4.0.2)             
    #>    ellipsis       0.3.1      2020-05-15 [1] CRAN (R 4.0.2)             
    #>    emo            0.0.0.9000 2020-07-06 [1] Github (hadley/emo@3f03b11)
    #>    evaluate       0.14       2019-05-28 [1] CRAN (R 4.0.2)             
    #>    fansi          0.4.2      2021-01-15 [1] CRAN (R 4.0.3)             
    #>    farver         2.0.3      2020-01-16 [1] CRAN (R 4.0.2)             
    #>    fastmap        1.1.0      2021-01-25 [1] CRAN (R 4.0.3)             
    #>    generics       0.1.0      2020-10-31 [1] CRAN (R 4.0.3)             
    #>    ggplot2      * 3.3.3      2020-12-30 [1] CRAN (R 4.0.3)             
    #>    ggridges       0.5.3      2021-01-08 [1] CRAN (R 4.0.3)             
    #>    git2r          0.28.0     2021-01-10 [1] CRAN (R 4.0.3)             
    #>    glue           1.4.2      2020-08-27 [1] CRAN (R 4.0.2)             
    #>    gridExtra      2.3        2017-09-09 [1] CRAN (R 4.0.2)             
    #>    gtable         0.3.0      2019-03-25 [1] CRAN (R 4.0.2)             
    #>    gtools         3.8.2      2020-03-31 [1] CRAN (R 4.0.0)             
    #>    here           1.0.1      2020-12-13 [1] CRAN (R 4.0.3)             
    #>    highr          0.8        2019-03-20 [1] CRAN (R 4.0.2)             
    #>    htmltools      0.5.1.1    2021-01-22 [1] CRAN (R 4.0.3)             
    #>    htmlwidgets    1.5.3      2020-12-10 [1] CRAN (R 4.0.3)             
    #>    httpuv         1.5.5      2021-01-13 [1] CRAN (R 4.0.3)             
    #>    igraph         1.2.6      2020-10-06 [1] CRAN (R 4.0.2)             
    #>    inline         0.3.17     2020-12-01 [1] CRAN (R 4.0.3)             
    #>    jsonlite       1.7.2      2020-12-09 [1] CRAN (R 4.0.3)             
    #>    knitr        * 1.31       2021-01-27 [1] CRAN (R 4.0.3)             
    #>    labeling       0.4.2      2020-10-20 [1] CRAN (R 4.0.2)             
    #>    later          1.1.0.1    2020-06-05 [1] CRAN (R 4.0.2)             
    #>    lattice        0.20-41    2020-04-02 [1] CRAN (R 4.0.2)             
    #>    lifecycle      0.2.0      2020-03-06 [1] CRAN (R 4.0.2)             
    #>    loo            2.4.1      2020-12-09 [1] CRAN (R 4.0.3)             
    #>    lubridate      1.7.9.2    2020-11-13 [1] CRAN (R 4.0.3)             
    #>    magrittr     * 2.0.1      2020-11-17 [1] CRAN (R 4.0.3)             
    #>    markdown       1.1        2019-08-07 [1] CRAN (R 4.0.2)             
    #>    matrixStats    0.57.0     2020-09-25 [1] CRAN (R 4.0.2)             
    #>    mime           0.9        2020-02-04 [1] CRAN (R 4.0.0)             
    #>    miniUI         0.1.1.1    2018-05-18 [1] CRAN (R 4.0.2)             
    #>    munsell        0.5.0      2018-06-12 [1] CRAN (R 4.0.2)             
    #>    pillar         1.4.7      2020-11-20 [1] CRAN (R 4.0.3)             
    #>    pkgbuild       1.2.0      2020-12-15 [1] CRAN (R 4.0.3)             
    #>    pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.0.2)             
    #>    plyr           1.8.6      2020-03-03 [1] CRAN (R 4.0.2)             
    #>    prettyunits    1.1.1      2020-01-24 [1] CRAN (R 4.0.2)             
    #>    processx       3.4.5      2020-11-30 [1] CRAN (R 4.0.3)             
    #>    promises       1.1.1      2020-06-09 [1] CRAN (R 4.0.2)             
    #>    ps             1.5.0      2020-12-05 [1] CRAN (R 4.0.3)             
    #>    purrr          0.3.4      2020-04-17 [1] CRAN (R 4.0.2)             
    #>    R6             2.5.0      2020-10-28 [1] CRAN (R 4.0.2)             
    #>    Rcpp           1.0.6      2021-01-15 [1] CRAN (R 4.0.3)             
    #>  D RcppParallel   5.0.2      2020-06-24 [1] CRAN (R 4.0.2)             
    #>    reshape2       1.4.4      2020-04-09 [1] CRAN (R 4.0.2)             
    #>    rlang          0.4.10     2020-12-30 [1] CRAN (R 4.0.3)             
    #>    rprojroot      2.0.2      2020-11-15 [1] CRAN (R 4.0.3)             
    #>    rsconnect      0.8.16     2019-12-13 [1] CRAN (R 4.0.2)             
    #>    rstan          2.21.2     2020-07-27 [1] CRAN (R 4.0.3)             
    #>    scales         1.1.1      2020-05-11 [1] CRAN (R 4.0.2)             
    #>    sessioninfo    1.1.1      2018-11-05 [1] CRAN (R 4.0.2)             
    #>    shiny          1.6.0      2021-01-25 [1] CRAN (R 4.0.3)             
    #>    shinyjs        2.0.0      2020-09-09 [1] CRAN (R 4.0.2)             
    #>    shinystan      2.5.0      2018-05-01 [1] CRAN (R 4.0.2)             
    #>    shinythemes    1.2.0      2021-01-25 [1] CRAN (R 4.0.3)             
    #>    StanHeaders    2.21.0-7   2020-12-17 [1] CRAN (R 4.0.3)             
    #>    stringi        1.5.3      2020-09-09 [1] CRAN (R 4.0.2)             
    #>    stringr        1.4.0      2019-02-10 [1] CRAN (R 4.0.2)             
    #>    threejs        0.3.3      2020-01-21 [1] CRAN (R 4.0.2)             
    #>    tibble         3.0.6      2021-01-29 [1] CRAN (R 4.0.3)             
    #>    tidyselect     1.1.0      2020-05-11 [1] CRAN (R 4.0.2)             
    #>    utf8           1.1.4      2018-05-24 [1] CRAN (R 4.0.2)             
    #>    V8             3.4.0      2020-11-04 [1] CRAN (R 4.0.3)             
    #>    vctrs          0.3.6      2020-12-17 [1] CRAN (R 4.0.3)             
    #>    withr          2.4.1      2021-01-26 [1] CRAN (R 4.0.3)             
    #>    xfun           0.20       2021-01-06 [1] CRAN (R 4.0.3)             
    #>    xtable         1.8-4      2019-04-21 [1] CRAN (R 4.0.2)             
    #>    xts            0.12.1     2020-09-09 [1] CRAN (R 4.0.2)             
    #>    zoo            1.8-8      2020-05-02 [1] CRAN (R 4.0.2)             
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.3/library
    #> 
    #>  D -- DLL MD5 mismatch, broken installation.
    ```
