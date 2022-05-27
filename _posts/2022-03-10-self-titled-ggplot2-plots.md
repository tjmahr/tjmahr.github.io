---
title: Self-documenting plots in ggplot2
excerpt: Including plotting code as an annotation on a plot
tags:
  - r
  - rlang
  - ggplot2
  - nonstandard evaluation
share: true
header:
  overlay_image: "assets/images/2022-03-neon.jpg"
  image_description: "A neon sign that says 'neon'"
  overlay_filter: rgba(10, 10, 10, 0.5)
  caption: "Photo credit: [**Slava Kuzminsk**](https://unsplash.com/photos/qnHOFl4VuFc)"
---




When I am showing off a plotting technique in
[ggplot2](https://ggplot2.tidyverse.org/), I sometimes like to include
the R code that produced the plot *as part of the plot*. Here is an
example I made to demonstrate the `debug` parameter in
[`element_text()`](https://rdrr.io/pkg/ggplot2/man/element.html):


```r
library(ggplot2)

self_document(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = "white") +
    labs(title = "A basic histogram") +
    theme(axis.title = element_text(debug = TRUE))
)
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/basic-example-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image. The plot theme includes yellow shading and points in the x and y axis titles." alt="A ggplot2 plot of a histogram with the plotting code above the image. The plot theme includes yellow shading and points in the x and y axis titles." width="80%" style="display: block; margin: auto;" />

Let's call these "self-documenting plots". If we're feeling nerdy, we
might also call them "qquines", although they are not true
[quines](https://en.wikipedia.org/wiki/Quine_%28computing%29).

In this post, we will build up a `self_document()` function from scratch. Here are
the problems we need to sort out:

- how to put plotting code above a title
- how to capture plotting code and convert it into text 


## Creating the code annotation

As a first step, let's just treat our plotting code as a string that 
is ready to use for annotation. 


```r
p_text <- 'ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 20, color = "white") +
  labs(title = "A basic histogram")'

p_plot <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 20, color = "white") +
  labs(title = "A basic histogram")
```

In order to have a titled plot along with this annotation, we need some
way to combine these two graphical objects together (the code and the
plot produced by ggplot2). I like the
[patchwork](https://patchwork.data-imaginist.com/articles/patchwork.html)
package for this job. Here we use
[`wrap_elements()`](https://patchwork.data-imaginist.com/reference/wrap_elements.html) to capture the plot into a
"patch" that patchwork can annotate.




```r
library(patchwork)
wrap_elements(p_plot) + 
  plot_annotation(title = p_text)
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/from-strings-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image. Here the title is in the default font." alt="A ggplot2 plot of a histogram with the plotting code above the image. Here the title is in the default font." width="50%" style="display: block; margin: auto;" />

Let's style this title to use a monospaced font. I use Windows and like
Consolas, so I will use that font.


```r
# Use default mono font if "Consolas" is not available
extrafont::loadfonts(device = "win", quiet = TRUE)
monofont <- ifelse(
  extrafont::choose_font("Consolas") == "", 
  "mono", 
  "Consolas"
)

title_theme <- theme(
  plot.title = element_text(
    family = monofont, hjust = 0, size = rel(.9), 
    margin = margin(0, 0, 5.5, 0, unit = "pt")
  )
)

wrap_elements(p_plot) + 
  plot_annotation(title = p_text, theme = title_theme)  
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/from-strings-consolas-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image. Here the title is in Consolas." alt="A ggplot2 plot of a histogram with the plotting code above the image. Here the title is in Consolas." width="50%" style="display: block; margin: auto;" />

One problem with this setup is that the plotting code has to be edited
in two places: the plot `p_plot` and the title `p_text`. As a result,
it's easy for these two pieces of code to fall out of sync with each
other, turning our self-documenting plot into a lying liar plot.

The solution is pretty easy: Tell R that `p_text` is code with
[`parse()`](https://rdrr.io/r/base/parse.html) and evaluate the code with
[`eval()`](https://rdrr.io/r/base/eval.html):


```r
wrap_elements(eval(parse(text = p_text))) + 
  plot_annotation(title = p_text, theme = title_theme)  
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/from-strings-consolas-eval-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image." alt="A ggplot2 plot of a histogram with the plotting code above the image." width="50%" style="display: block; margin: auto;" />

This *works*. It gets the job done. But we find ourselves in a clumsy
workflow, either having to edit R code inside of quotes or editing the
plot interactively and then having to wrap it in quotes. Let's do better.


## Capturing plotting code as a string

Time for some *nonstandard evaluation*. I will use the
[rlang](https://rlang.r-lib.org/) package, although in principle we
could use functions in base R to accomplish these goals.

First, we are going to use [`rlang::expr()`](https://rdrr.io/pkg/rlang/man/expr.html) to
capture/quote/[defuse](https://rlang.r-lib.org/reference/topic-defuse.html)
the R code as an expression. We can print the code as code, print it as
text, and use `eval()` to show the plot.



```r
p_code <- rlang::expr(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = "white") +
    labs(title = "A basic histogram")
)

# print the expressions
p_code
#> ggplot(mtcars, aes(x = mpg)) + geom_histogram(bins = 20, color = "white") + 
#>     labs(title = "A basic histogram")

# expression => text
rlang::expr_text(p_code)
#> [1] "ggplot(mtcars, aes(x = mpg)) + geom_histogram(bins = 20, color = \"white\") + \n    labs(title = \"A basic histogram\")"

eval(p_code)
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/from-code-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image." alt="A ggplot2 plot of a histogram with the plotting code above the image." width="50%" style="display: block; margin: auto;" />

Then, it should be straightforward to make the self-documenting plot, right?


```r
p_code <- rlang::expr(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = "white") +
    labs(title = "A basic histogram")
)

wrap_elements(eval(p_code)) + 
  plot_annotation(title = rlang::expr_text(p_code), theme = title_theme)  
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/from-code-eval-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image. In this case, the title is mostly on one line and some text is cut off from the image." alt="A ggplot2 plot of a histogram with the plotting code above the image. In this case, the title is mostly on one line and some text is cut off from the image." width="50%" style="display: block; margin: auto;" />

Hey, it reformatted the title! Indeed, in the process of capturing the
code, the code formatting was lost. To get something closer to the
source code we provided, we have to reformat the captured code before we
print it. 

The [styler](https://styler.r-lib.org/) package provides a suite of
functions for reformatting code. We can define our own coding
styles/formatting rules to customize how styler works. I like the styler
rules used by Garrick Aden-Buie in his
[grkstyle](https://github.com/gadenbuie/grkstyle) package, so I will use
`grkstyle::grk_style_text()` to reformat the code.


```r
p_code <- rlang::expr(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = "white") +
    labs(title = "A basic histogram")
)

wrap_elements(eval(p_code)) + 
  plot_annotation(
    title = rlang::expr_text(p_code) |> 
      grkstyle::grk_style_text() |> 
      # reformatting returns a vector of lines,
      # so we have to combine them
      paste0(collapse = "\n"), 
    theme = title_theme
  ) 
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/from-code-eval-style-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image." alt="A ggplot2 plot of a histogram with the plotting code above the image." width="50%" style="display: block; margin: auto;" />

## Putting it all together

When we write our `self_document()` function, the only change we have to
make is using [`rlang::enexpr()`](https://rdrr.io/pkg/rlang/man/defusing-advanced.html) instead `rlang::expr()`. The
en-variant is used when we want to *en*-quote exactly what the user
provided. Aside from that change, our `self_document()` function just bundles together all of the code we developed above:


```r
self_document <- function(expr) {
  monofont <- ifelse(
    extrafont::choose_font("Consolas") == "", 
    "mono", 
    "Consolas"
  )
  
  p <- rlang::enexpr(expr)
  title <- rlang::expr_text(p) |> 
    grkstyle::grk_style_text() |> 
    paste0(collapse = "\n")
  
  patchwork::wrap_elements(eval(p)) + 
    patchwork::plot_annotation(
      title = title, 
      theme = theme(
        plot.title = element_text(
          family = monofont, hjust = 0, size = rel(.9), 
          margin = margin(0, 0, 5.5, 0, unit = "pt")
        )
      )
    )
}
```

And let's confirm that it works. 


```r
library(ggplot2)
self_document(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = "white") +
    labs(title = "A basic histogram")
)
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/final-demo-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image." alt="A ggplot2 plot of a histogram with the plotting code above the image." width="50%" style="display: block; margin: auto;" />

Because we developed this function on top of rlang, we can do some tricks like 
injecting a variable's value when capturing the code. For example, here I 
use `!! color` to replace the `color` variable with the actual value.


```r
color <- "white"
self_document(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = !! color) +
    labs(title = "A basic histogram")
)
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/final-demo-inject-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image." alt="A ggplot2 plot of a histogram with the plotting code above the image." width="50%" style="display: block; margin: auto;" />

And if you are wondering, yes, we can `self_document()` a
`self_document()` plot.


```r
self_document(
  self_document(
    ggplot(mtcars, aes(x = mpg)) +
      geom_histogram(bins = 20, color = "white") +
      labs(title = "A basic histogram")
  )
)
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/final-demo-self-document-1.png" title="A self_document() plot of a plot of a histogram with the plotting code above the image. There are two sets of code on top of each other." alt="A self_document() plot of a plot of a histogram with the plotting code above the image. There are two sets of code on top of each other." width="50%" style="display: block; margin: auto;" />

## Alas, comments are lost

One downside of this approach is that helpful comments are lost.


```r
self_document(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = !! color) +
    # get rid of that grey
    theme_minimal() +
    labs(title = "A basic histogram")
)
```

<img src="/figs/2022-03-10-self-titled-ggplot2-plots/final-demo-no-comments-1.png" title="A ggplot2 plot of a histogram with the plotting code above the image." alt="A ggplot2 plot of a histogram with the plotting code above the image." width="50%" style="display: block; margin: auto;" />

I am not sure how to include comments. One place where comments are stored 
and printed is in function bodies:


```r
f <- function() {
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 20, color = !! color) +
  # get rid of that grey
  theme_minimal() +
  labs(title = "A basic histogram")
}

print(f, useSource = TRUE)
#> function() {
#> ggplot(mtcars, aes(x = mpg)) +
#>   geom_histogram(bins = 20, color = !! color) +
#>   # get rid of that grey
#>   theme_minimal() +
#>   labs(title = "A basic histogram")
#> }
#> <environment: 0x00000222d313b848>
```

I have no idea how to go about exploiting this feature for
self-documenting plots, however.





***

*Last knitted on 2022-05-27. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2022-03-10-self-titled-ggplot2-plots.Rmd).*[^si] 

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
    #>  date     2022-05-27
    #>  pandoc   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package     * version date (UTC) lib source
    #>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.2.0)
    #>  cachem        1.0.6   2021-08-19 [1] CRAN (R 4.2.0)
    #>  cli           3.3.0   2022-04-25 [1] CRAN (R 4.2.0)
    #>  colorspace    2.0-3   2022-02-21 [1] CRAN (R 4.2.0)
    #>  crayon        1.5.1   2022-03-26 [1] CRAN (R 4.2.0)
    #>  DBI           1.1.2   2021-12-20 [1] CRAN (R 4.2.0)
    #>  digest        0.6.29  2021-12-01 [1] CRAN (R 4.2.0)
    #>  downlit       0.4.0   2021-10-29 [1] CRAN (R 4.2.0)
    #>  dplyr         1.0.9   2022-04-28 [1] CRAN (R 4.2.0)
    #>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.2.0)
    #>  evaluate      0.15    2022-02-18 [1] CRAN (R 4.2.0)
    #>  extrafont     0.18    2022-04-12 [1] CRAN (R 4.2.0)
    #>  extrafontdb   1.0     2012-06-11 [1] CRAN (R 4.2.0)
    #>  fansi         1.0.3   2022-03-24 [1] CRAN (R 4.2.0)
    #>  farver        2.1.0   2021-02-28 [1] CRAN (R 4.2.0)
    #>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.2.0)
    #>  generics      0.1.2   2022-01-31 [1] CRAN (R 4.2.0)
    #>  ggplot2     * 3.3.6   2022-05-03 [1] CRAN (R 4.2.0)
    #>  git2r         0.30.1  2022-03-16 [1] CRAN (R 4.2.0)
    #>  glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
    #>  grkstyle      0.0.3   2022-05-25 [1] Github (gadenbuie/grkstyle@6a7011c)
    #>  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.2.0)
    #>  here          1.0.1   2020-12-13 [1] CRAN (R 4.2.0)
    #>  highr         0.9     2021-04-16 [1] CRAN (R 4.2.0)
    #>  knitr       * 1.39    2022-04-26 [1] CRAN (R 4.2.0)
    #>  labeling      0.4.2   2020-10-20 [1] CRAN (R 4.2.0)
    #>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.2.0)
    #>  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
    #>  memoise       2.0.1   2021-11-26 [1] CRAN (R 4.2.0)
    #>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.2.0)
    #>  patchwork   * 1.1.1   2020-12-17 [1] CRAN (R 4.2.0)
    #>  pillar        1.7.0   2022-02-01 [1] CRAN (R 4.2.0)
    #>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
    #>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.2.0)
    #>  R.cache       0.15.0  2021-04-30 [1] CRAN (R 4.2.0)
    #>  R.methodsS3   1.8.1   2020-08-26 [1] CRAN (R 4.2.0)
    #>  R.oo          1.24.0  2020-08-26 [1] CRAN (R 4.2.0)
    #>  R.utils       2.11.0  2021-09-26 [1] CRAN (R 4.2.0)
    #>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
    #>  ragg          1.2.2   2022-02-21 [1] CRAN (R 4.2.0)
    #>  rlang         1.0.2   2022-03-04 [1] CRAN (R 4.2.0)
    #>  rprojroot     2.0.3   2022-04-02 [1] CRAN (R 4.2.0)
    #>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.2.0)
    #>  Rttf2pt1      1.3.10  2022-02-07 [1] CRAN (R 4.2.0)
    #>  scales        1.2.0   2022-04-13 [1] CRAN (R 4.2.0)
    #>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
    #>  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.2.0)
    #>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.2.0)
    #>  styler        1.7.0   2022-03-13 [1] CRAN (R 4.2.0)
    #>  systemfonts   1.0.4   2022-02-11 [1] CRAN (R 4.2.0)
    #>  textshaping   0.3.6   2021-10-13 [1] CRAN (R 4.2.0)
    #>  tibble        3.1.7   2022-05-03 [1] CRAN (R 4.2.0)
    #>  tidyselect    1.1.2   2022-02-21 [1] CRAN (R 4.2.0)
    #>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.2.0)
    #>  vctrs         0.4.1   2022-04-13 [1] CRAN (R 4.2.0)
    #>  withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
    #>  xfun          0.31    2022-05-10 [1] CRAN (R 4.2.0)
    #>  yaml          2.3.5   2022-02-21 [1] CRAN (R 4.2.0)
    #> 
    #>  [1] C:/Users/Tristan/AppData/Local/R/win-library/4.2
    #>  [2] C:/Program Files/R/R-4.2.0/library
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
    ```
