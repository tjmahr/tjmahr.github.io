---
title: "10 PRINT mazes with ggplot2"
excerpt: "\\/\\/\\////\\\\/\\/\\/\\////\\/\\\\//\\/\\/\\\\\\//\\\\\\\\\\\\/\\"
tags:
  - ggplot2
  - r
---



There is a celebrated Commodore 64 program that randomly prints outs `/`
and `\` characters and fills the screen with neat-looking maze designs.
It is just one line of code, but there is 
[a whole book](https://10print.org/) written about it.

```
10 PRINT CHR$(205.5+RND(1)); : GOTO 10
```

{% include figure image_path="/assets/images/2018-05-print10.png" alt="Screenshots of 10 PRINT in action" caption="Screenshots of the 10 PRINT program in action. Images taken from the [_10 PRINT_ book](https://10print.org/)." %}

The basic idea, from my reading of the code, is that `CHR$(205)` is `\`,
`CHR$(206)` is `/`, and the program randomly selects between the two by
adding a random number to 205.5. Endlessly looping over this command
fills the screen with that pleasing maze pattern.

In R, we could replicate this functionality with by randomly sampling
the slashes:


```r
set.seed(20210215)

sample_n_slashes <- function(n) {
  sample(c("/", "\\"), size = n, replace = TRUE)
}

withr::with_options(
  list(width = 40), 
  cat(sample_n_slashes(800), sep = "", fill = TRUE)
)
#> ////\\///\//\\\/\\\\//\/////////\///////
#> /\\\////\\\\\//\\/\\\///\///\/////\///\/
#> /\\/\////////\/\\/\/////////\\\\//\\\\\/
#> \///\//\/\\/\//\\\\//\\///\\//\\\\\/\///
#> //\\\/\\\\///\\\/\/\\\\/\//\\\\\\/////\\
#> \\/\\\\\////\///\///\//\\\\\\\\\\/\\\\\/
#> /\\/\//////\\\//\/\\/\\\/////\\\\/\\\/\/
#> ////\//\\////\\/////\\/\//\/\/\/\/\///\/
#> \/\\\\///\\///\\/\\//\/\///////\//\////\
#> /\\\//\\////\\\/////\//////\//\//\\\\\//
#> /\\\/\/\\\\\\\/////\////\\\///\/\/\\////
#> \//\//\/\\////\///\//\////\\\\////\\/\\/
#> \/\/\/\\\\/\//\\//\///////////\\/\/\\\\\
#> \////\\/\\//\\\/\//\\//\/\\/\//\\\/\\\\\
#> \/\/\\////\/\//\/\///\\//////\/\/\\/\\\/
#> /\/\\/\\\//\\\/\/\//////\/\//\///\/\/\//
#> \/\/\///\/\//\\\/\/\//\/\/\///\\//\/\\/\
#> \\/\\\/////\////\/\\//\/\//\/\//////\/\\
#> \\\/\/\/\/\/\\//\\//\\//\\/\\/\\/\\\\/\/
#> //\\/\/\\//\/\\/\/\/\\\/\//\\//////\\\/\
```

where [`withr::with_options()`][with-options] lets us temporarily
change the print width and [`cat()`][cat] concatenates the slashes
and prints out the characters as text.

We can also make this much prettier by drawing the patterns using ggplot2. 

## Drawing line segments with ggplot2

Instead of writing out slashes, we will draw a grid of diagonal line
segments, some of which will be flipped at random. To draw a segment, we
need a starting *x*–*y* coordinate and an ending *x*–*y* coordinate.
[`geom_segment()`][geom-segment] will connect the two coordinates
with a line. Here's a small example where we draw four "slashes".


```r
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

data <- tibble::tribble(
  ~row, ~col, ~x_start, ~x_end, ~y_start, ~y_end,
     1,    1,        0,      1,        0,      1,
     1,    2,        1,      2,        1,      0, # flipped
     2,    1,        0,      1,        1,      2,
     2,    2,        1,      2,        1,      2
)
  
ggplot(data) + 
  aes(x = x_start, xend = x_end, y = y_start, yend = y_end) +
  geom_segment()
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/geom-segment-demo-1.png" title="A simple demo of geom_segment()" alt="A simple demo of geom_segment()" width="50%" style="display: block; margin: auto;" />

The programming task now is to make giant grid of these slashes. Let's
start with an observation: To draw two slashes, we needed three *x*
values: 0, 1, 2. The first two served as segment starts and the last two
as segment ends. The same idea applies to the *y* values. We can
generate a bunch of starts and ends by taking a sequence of steps and
removing the first and last elements.



```r
# We want a 20 by 20 grid
rows <- 20
cols <- 20

x_points <- seq(0, 1, length.out = cols + 1)
x_starts <- head(x_points, -1)
x_ends <- tail(x_points, -1)

y_points <- seq(0, 1, length.out = rows + 1)
y_starts <- head(y_points, -1)
y_ends <- tail(y_points, -1)
```

Each `x_starts`--`x_ends` pair is a column in the grid, and each
`y_starts`--`y_ends` is a row in the grid. To make a slash at each
row–column combination, we have to map out all the combinations of the
rows and columns. We can do this with `crossing()` which creates all
*crossed* combinations of values. (If it helps, you might think of
*crossed* like [crossed
experiments](https://en.wikipedia.org/wiki/Factorial_experiment) or the
[Cartesian cross
product](https://en.wikipedia.org/wiki/Cartesian_product) of sets.)


```r
grid <- tidyr::crossing(
  # columns
  tibble(x_start = x_starts, x_end = x_ends),
  # rows
  tibble(y_start = y_starts, y_end = y_ends)
) %>%
  # So values move left to right, bottom to top
  arrange(y_start, y_end)

# 400 rows because 20 rows x 20 columns
grid
#> # A tibble: 400 x 4
#>    x_start x_end y_start y_end
#>      <dbl> <dbl>   <dbl> <dbl>
#>  1    0     0.05       0  0.05
#>  2    0.05  0.1        0  0.05
#>  3    0.1   0.15       0  0.05
#>  4    0.15  0.2        0  0.05
#>  5    0.2   0.25       0  0.05
#>  6    0.25  0.3        0  0.05
#>  7    0.3   0.35       0  0.05
#>  8    0.35  0.4        0  0.05
#>  9    0.4   0.45       0  0.05
#> 10    0.45  0.5        0  0.05
#> # ... with 390 more rows
```

We can confirm that the segments in the grid fill out a plot. (I
randomly color the line segments to make individual ones visible.)


```r
ggplot(grid) +
  aes(
    x = x_start, 
    y = y_start, 
    xend = x_end, 
    yend = y_end, 
    color = runif(400)
  ) +
  geom_segment(size = 1) + 
  guides(color = FALSE)
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid-demo-1.png" title="A grid full of line segments, all pointing the same direction" alt="A grid full of line segments, all pointing the same direction" width="50%" style="display: block; margin: auto;" />

Finally, we need to flip slashes at random. A segment becomes flipped if
the `y_start` and `y_end` are swapped. In the code below, we flip the
slash in each row if a randomly drawn number between 0 and 1 is less
than .5. For style, we also use `theme_void()` to strip away the
plotting theme, leaving us with just the maze design.


```r
p_flip <- .5

grid <- grid %>%
  arrange(y_start, y_end) %>% 
  mutate(
    p_flip = p_flip,
    flip = runif(length(y_end)) <= p_flip,
    y_temp1 = y_start,
    y_temp2 = y_end,
    y_start = ifelse(flip, y_temp2, y_temp1),
    y_end = ifelse(flip, y_temp1, y_temp2)
  ) %>%
  select(x_start:y_end, p_flip)

ggplot(grid) +
  aes(x = x_start, y = y_start, xend = x_end, yend = y_end) +
  geom_segment(size = 1, color = "grey20") 

last_plot() + theme_void()
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid1-1.png" title="A maze with 50% flipping probability" alt="A maze with 50% flipping probability" width="50%" /><img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid1-2.png" title="A maze with 50% flipping probability" alt="A maze with 50% flipping probability" width="50%" />

Now, we wrap all these steps together into a pair of functions.


```r
make_10_print_data <- function(rows = 20, cols = 20, p_flip = .5) {
  x_points <- seq(0, 1, length.out = cols + 1)
  x_starts <- head(x_points, -1)
  x_ends <- tail(x_points, -1)
  
  y_points <- seq(0, 1, length.out = rows + 1)
  y_starts <- head(y_points, -1)
  y_ends <- tail(y_points, -1)

  grid <- tidyr::crossing(
    data.frame(x_start = x_starts, x_end = x_ends),
    data.frame(y_start = y_starts, y_end = y_ends)
  )

  grid %>%
    arrange(y_start, y_end) %>% 
    mutate(
      p_flip = p_flip,
      flip = runif(length(y_end)) <= p_flip,
      y_temp1 = y_start,
      y_temp2 = y_end,
      y_start = ifelse(flip, y_temp2, y_temp1),
      y_end = ifelse(flip, y_temp1, y_temp2)
    ) %>%
    select(x_start:y_end, p_flip)
}

draw_10_print <- function(rows = 20, cols = 20, p_flip = .5) {
  grid <- make_10_print_data(
    rows = rows, 
    cols = cols, 
    p_flip = p_flip
  )

  ggplot(grid) +
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end) +
    geom_segment(size = 1, color = "grey20") 
}
```


## Now the fun part: custom flipping probabilities

We can vary the probability of flipping the slashes. For example, we can use the
density of a normal distribution to make flipping more likely for central values
and less likely for more extreme values.


```r
xs <- seq(0, 1, length.out = 40)
p_flip <- dnorm(seq(-4, 4, length.out = 40))

ggplot(data.frame(x = xs, y = p_flip)) + 
  aes(x, y) + 
  geom_line() + 
  labs(
    x = "x position", 
    y = "p(flipping)",
    title = "normal density"
  )

# We repeat p_flip for each row of the grid
draw_10_print(rows = 40, cols = 40, p_flip = rep(p_flip, 40)) + 
  theme_void()
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid3-1.png" title="Density of the normal distribution" alt="Density of the normal distribution" width="50%" /><img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid3-2.png" title="A maze where flipping probability is based on the density of a normal curve" alt="A maze where flipping probability is based on the density of a normal curve" width="50%" />

We can use the cumulative density of the normal distribution so that
flipping becomes more likely as _x_ increases.


```r
xs <- seq(0, 1, length.out = 40)
p_flip <- pnorm(seq(-4, 4, length.out = 40))

ggplot(data.frame(x = xs, y = p_flip)) + 
  aes(x, y) + 
  geom_line() + 
  labs(
    x = "x position", 
    y = "p(flipping)", 
    title = "cumulative normal"
  )

draw_10_print(rows = 40, cols = 40, p_flip = rep(p_flip, 40)) + 
  theme_void()
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid4-1.png" title="Cumulative density of the normal distribution" alt="Cumulative density of the normal distribution" width="50%" /><img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid4-2.png" title="A maze where flipping probability is based on the cumulative density of a normal curve" alt="A maze where flipping probability is based on the cumulative density of a normal curve" width="50%" />

The Cauchy distribution is said to have "thicker" tails than the normal
distribution, so here it shows more flips on the left and right extremes.


```r
xs <- seq(0, 1, length.out = 40)
p_flip <- dcauchy(seq(-4, 4, length.out = 40))

ggplot(data.frame(x = xs, y = p_flip)) + 
  aes(x, y) + 
  geom_line() + 
  labs(
    x = "x position", 
    y = "p(flipping)",
    title = "Cauchy density"
  )

draw_10_print(rows = 40, cols = 40, p_flip = rep(p_flip, 40)) + 
  theme_void()
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid-cauchy-1.png" title="Density of the Cauchy distribution" alt="Density of the Cauchy distribution" width="50%" /><img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid-cauchy-2.png" title="A maze where flipping probability is based on the density of a Cauchy curve" alt="A maze where flipping probability is based on the density of a Cauchy curve" width="50%" />

The exponential distribution is a spike that quickly peters out. We can make a
probability "bowl" by splicing an exponential and a reversed exponential
together.


```r
# Use flipped exponential densities as probabilities
p_flip <- c(
  dexp(seq(0, 4, length.out = 20)),
  dexp(seq(4, 0, length.out = 20))
)

ggplot(data.frame(x = xs, y = p_flip)) + 
  aes(x, y) + 
  geom_line() + 
  labs(
    x = "x position", 
    y = "p(flipping)", 
    title = "exponential + flipped exponential"
  )

draw_10_print(rows = 40, cols = 40, p = rep(p_flip, 40)) +
  theme_void() 
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid5-1.png" title="Densities of two exponential curves spliced to form a bowl" alt="Densities of two exponential curves spliced to form a bowl" width="50%" /><img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid5-2.png" title="A maze where flipping probability is based on an exponential curve and a reversed exponential curve" alt="A maze where flipping probability is based on an exponential curve and a reversed exponential curve" width="50%" />

We might have the probabilities increase by 10% from row to row. In the code
below, I use a simple loop to boost some random probability values by 10% from
row to row. This gives us nice streaks in the grid as a column starts to flip
for every row.


```r
boost_probs <- function(p_flip, nrows, factor = 1.1) {
  output <- p_flip
  for (i in seq_len(nrows - 1)) {
    p_flip <- p_flip * factor
    output <- c(output, p_flip)
  }
  output
}

draw_10_print(
  cols = 40, 
  rows = 40, 
  p = boost_probs(runif(40), 40, 1.1)
) +
  theme_void()
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid6-1.png" title="A maze where flipping probability increases by 10% from row to row" alt="A maze where flipping probability increases by 10% from row to row" width="80%" style="display: block; margin: auto;" />

The probabilities can be anything we like. Here I compute the frequency of
English alphabet letters as they appear in *Pride and Prejudice* and based the
flipping probability on those values.


```r
char_counts <- janeaustenr::prideprejudice %>% 
  tolower() %>% 
  stringr::str_split("") %>% 
  unlist() %>% 
  table()

letter_counts <- char_counts[letters] %>% as.vector()
p_letter <- letter_counts / sum(letter_counts)

ggplot(data.frame(x = letters, y = p_letter)) + 
  aes(x, y, label = x) + 
  geom_text() +
  labs(
    x = NULL, 
    y = "p(letter)", 
    title = "letter frequencies in Pride and Prejudice"
  )
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/p-p-freqs-1.png" title="Letter frequencies in Pride and Prejudice" alt="Letter frequencies in Pride and Prejudice" width="80%" style="display: block; margin: auto;" />


```r
draw_10_print(cols = 26, rows = 80, p = rep(p_letter, 80)) +
  theme_void() 
```

<img src="/figs/2018-05-09-10-print-mazes-with-ggplot2/grid7-1.png" title="Maze where flipping frequency is based on letter frequencies in Pride and Prejudice" alt="Maze where flipping frequency is based on letter frequencies in Pride and Prejudice" width="80%" style="display: block; margin: auto;" />



***

*Last knitted on 2021-02-15. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2018-05-09-10-print-mazes-with-ggplot2.Rmd).*[^si] 

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
    #>  package     * version date       lib source        
    #>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
    #>  cli           2.3.0   2021-01-31 [1] CRAN (R 4.0.3)
    #>  colorspace    2.0-0   2020-11-11 [1] CRAN (R 4.0.3)
    #>  crayon        1.4.1   2021-02-08 [1] CRAN (R 4.0.3)
    #>  DBI           1.1.1   2021-01-15 [1] CRAN (R 4.0.3)
    #>  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.3)
    #>  dplyr       * 1.0.4   2021-02-02 [1] CRAN (R 4.0.3)
    #>  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)
    #>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.2)
    #>  fansi         0.4.2   2021-01-15 [1] CRAN (R 4.0.3)
    #>  farver        2.0.3   2020-01-16 [1] CRAN (R 4.0.2)
    #>  generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.3)
    #>  ggplot2     * 3.3.3   2020-12-30 [1] CRAN (R 4.0.3)
    #>  git2r         0.28.0  2021-01-10 [1] CRAN (R 4.0.3)
    #>  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    #>  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.2)
    #>  here          1.0.1   2020-12-13 [1] CRAN (R 4.0.3)
    #>  highr         0.8     2019-03-20 [1] CRAN (R 4.0.2)
    #>  janeaustenr   0.1.5   2017-06-10 [1] CRAN (R 4.0.2)
    #>  knitr       * 1.31    2021-01-27 [1] CRAN (R 4.0.3)
    #>  labeling      0.4.2   2020-10-20 [1] CRAN (R 4.0.2)
    #>  lifecycle     1.0.0   2021-02-15 [1] CRAN (R 4.0.3)
    #>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
    #>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.2)
    #>  pillar        1.4.7   2020-11-20 [1] CRAN (R 4.0.3)
    #>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
    #>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
    #>  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.2)
    #>  ragg          0.4.1   2021-01-11 [1] CRAN (R 4.0.3)
    #>  rlang         0.4.10  2020-12-30 [1] CRAN (R 4.0.3)
    #>  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.3)
    #>  scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.2)
    #>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
    #>  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)
    #>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    #>  systemfonts   1.0.0   2021-02-01 [1] CRAN (R 4.0.3)
    #>  textshaping   0.2.1   2020-11-13 [1] CRAN (R 4.0.3)
    #>  tibble        3.0.6   2021-01-29 [1] CRAN (R 4.0.3)
    #>  tidyr         1.1.2   2020-08-27 [1] CRAN (R 4.0.2)
    #>  tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.2)
    #>  utf8          1.1.4   2018-05-24 [1] CRAN (R 4.0.2)
    #>  vctrs         0.3.6   2020-12-17 [1] CRAN (R 4.0.3)
    #>  withr         2.4.1   2021-01-26 [1] CRAN (R 4.0.3)
    #>  xfun          0.20    2021-01-06 [1] CRAN (R 4.0.3)
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.3/library
    ```


[with-options]: https://withr.r-lib.org/reference/with_options.html 
    "withr documentation"

[cat]: https://rdrr.io/r/base/cat.html 
    "cat() documentation"

[geom-segment]: https://ggplot2.tidyverse.org/reference/geom_segment.html 
    "geom_segment() documentation"
