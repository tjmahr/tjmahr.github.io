---
title: Another mixed effects model visualization
excerpt: Creating a diagram to illustrate information borrowing (partial pooling)
tags:
  - bayesian
  - r
  - ggplot2
  - mixed effects
  - brms
  - tidybayes
---




Last week, I presented an analysis on the longitudinal development of
intelligibility in children with cerebral palsy---that is, how well do
strangers understand these children's speech from 2 to 8 years old. My
analysis used a Bayesian nonlinear mixed effects beta regression model.
If some [models are livestock and some are
pets](https://twitter.com/hadleywickham/status/742353541793120259?s=20),
this model is my dearest pet. I first started developing it a year ago,
and it took weeks of learning and problem-solving to get the first
version working correctly. I was excited to present results from it.

But here's the thing. I couldn't just formally describe my model. Not
for this audience. My talk was at the annual conference of the American
Speech-Language-Hearing Association: The main professional gathering for
speech-language pathologists, audiologists, and researchers in
communication sciences and disorders. The audience here is rightly more
concerned with clinical or research matters than the nuts and bolts of
my model.

Still, I wanted to convey the main ideas behind the model without
dumbing things down. I got to work making lots of educational diagrams.
Among them was the annotated logistic curve from [my earlier
post](/anatomy-of-a-logistic-growth-curve/) and the following figure,
used to illustrate information borrowing (or partial pooling) in mixed
effects models:

{% include figure image_path="/assets/images/2019-11-mixed-model-diagram.png" alt="Diagram showing four panels of observed data in the top row, a set of predictions for a new set of data in the middle row, and the four panels from the first row updated to included fitted estimates." caption="Diagram I used to illustrate how mixed effects models work." %}

I am pleased with this figure. Originally, I tried to convey the idea as
an interactive process: Individual-level data feed into the population
model and those feed back into the individual estimates. I had only two
sets of plots with labeled paths running back and forth between them; it
wasn't pretty. The final plot's "feed-forward" approach simplified
things a great deal. My only concern, in hindsight, is that I should
have oriented things to run left-to-right instead of top-to-bottom so I
could have filled a 16:9 widescreen slide better. (But this vertical
version probably looks great on your phone or tablet right now, so
whatever!)

In this post, I walk through how to produce a plot like this one from
scratch. I can't share the original model or the clinical data here, so
I will use the `sleepstudy` data from lme4, as in [my partial pooling
tutorial](/plotting-partial-pooling-in-mixed-effects-models/).




First, let's set up our data.


```r
library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)

# Convert to tibble for better printing. Convert factors to strings
sleepstudy <- lme4::sleepstudy %>%
  as_tibble() %>%
  mutate(Subject = as.character(Subject))

# Add two fake participants, as in the earlier partial pooling post
df_sleep <- bind_rows(
  sleepstudy,
  tibble(Reaction = c(286, 288), Days = 0:1, Subject = "374"),
  tibble(Reaction = 245, Days = 0, Subject = "373")
)

df_sleep
#> # A tibble: 183 x 3
#>    Reaction  Days Subject
#>       <dbl> <dbl> <chr>  
#>  1     250.     0 308    
#>  2     259.     1 308    
#>  3     251.     2 308    
#>  4     321.     3 308    
#>  5     357.     4 308    
#>  6     415.     5 308    
#>  7     382.     6 308    
#>  8     290.     7 308    
#>  9     431.     8 308    
#> 10     466.     9 308    
#> # ... with 173 more rows

# Select four participants to highlight
df_demo <- df_sleep %>%
  filter(Subject %in% c("335", "333", "350", "374"))
```

We fit a mixed model with default priors and a random-number seed for
reproducibility.


```r
b <- brm(
  Reaction ~ Days + (Days | Subject),
  data = df_sleep, 
  seed = 20191125
)
b
```





```
#>  Family: gaussian 
#>   Links: mu = identity; sigma = identity 
#> Formula: Reaction ~ Days + (Days | Subject) 
#>    Data: df_sleep (Number of observations: 183) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Group-Level Effects: 
#> ~Subject (Number of levels: 20) 
#>                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)          25.84      6.26    15.17    39.80 1.00     1822     2642
#> sd(Days)                6.55      1.50     4.19     9.99 1.00     1607     1965
#> cor(Intercept,Days)     0.09      0.29    -0.45     0.66 1.00      904     1618
#> 
#> Population-Level Effects: 
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept   252.49      6.77   239.10   266.00 1.00     2116     2502
#> Days         10.49      1.71     7.26    14.07 1.00     1410     2073
#> 
#> Family Specific Parameters: 
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma    25.81      1.55    23.01    29.07 1.00     3272     2891
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

## Top row: connect the dots

Let's create the top row. The core of the plot is straightforward: We
draw lines, draw points, facet by `Subject`, and set a reasonable
*y*-axis range for the plot (controlling the coordinates via
`coord_cartesian()`).


```r
col_data <- "#2F2E33"

p_first_pass <- ggplot(df_demo) +
  aes(x = Days, y = Reaction) +
  geom_line(aes(group = Subject), color = col_data) +
  geom_point(color = col_data, size = 2) +
  facet_wrap("Subject", nrow = 1) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Each participant contributes data") + 
  theme_grey(base_size = 14)

p_first_pass
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/top-row-take-1-1.png" title="First pass at the model. It shows points connected by lines split over four panels." alt="First pass at the model. It shows points connected by lines split over four panels." width="66%" style="display: block; margin: auto;" />

First, let's make one somewhat obscure tweak. Currently, the left edge
of the title is aligned with the plotting panel---that is, the window
with the grey background where the data are drawn. But in our final
ensemble, we are going to have three plots and the left edge of the
panel will not be in the same location across all three plots. We want
our titles to be aligned with each other from plot to plot, so we tell
ggplot2 to position the plot title using the left edge of the `"plot"`,
as opposed to the `"panel"`.


```r
p_first_pass_tweak <- p_first_pass + 
  theme(plot.title.position = "plot")
p_first_pass_tweak
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/top-row-take-1-tweak-1.png" title="The previous plot with its title moved to be aligned with the left edge of the image." alt="The previous plot with its title moved to be aligned with the left edge of the image." width="66%" style="display: block; margin: auto;" />

For the diagram, we have to remove the facet labels ("strips"), axis
titles, axis text, and axis ticks (those little lines that stick out of
the plot). We also should clean up the gridlines for the *x* axis. The
*x* unit is whole number `Days`, so putting a line ("break") at 2.5 days
is not meaningful.


```r
p_second_pass <- p_first_pass_tweak + 
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  labs(x = NULL, y = NULL) +
  theme(
    # Removing things from `theme()` is accomplished by setting them
    # `element_blank()`
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank()
  )
  
p_second_pass
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/top-row-take-2-1.png" title="The previous plot update to have all axis text and ticks removed and have the gridlines on the panel set to even numbers." alt="The previous plot update to have all axis text and ticks removed and have the gridlines on the panel set to even numbers." width="66%" style="display: block; margin: auto;" />

Finally, let's add the "(others)" label. Here, I use the `tag` label as
a sneaky way to add an annotation. Normally, tags are meant to label
individual plots in an ensemble display of multiple plots:


```r
ggplot() + 
  labs(title = "A tagged plot", tag = "A. ")
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/tag-demo-1.png" title="A diagram showing the placement a plot's title and tag. The tag is in the upper left corner of the plot." alt="A diagram showing the placement a plot's title and tag. The tag is in the upper left corner of the plot." width="33%" style="display: block; margin: auto;" />

But we will use that feature to place some text to the right side of the
plot. Sometimes, there's a correct way and there's the way that gives
you the image you can paste into your slides, and this tag trick is one
of two such shortcuts I used in my diagram.


```r
tag_others <- "   (others)   "

p_second_pass + 
  labs(tag = tag_others) + 
  theme(
    plot.tag.position = "right",
    plot.tag = element_text(size = rel(.9))
  )
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/top-row-1.png" title="The current plot updated to have text '(others)' added to the right of the plot to indicate the more panels exist but are omitted." alt="The current plot updated to have text '(others)' added to the right of the plot to indicate the more panels exist but are omitted." width="66%" style="display: block; margin: auto;" />

I have discovered that incrementally and didactically building up my
plots over multiple code chunks creates a hassle for Future Me when
copypasting plotting code, so the final, complete code is provided
below.


```r
col_data <- "#2F2E33"
tag_others <- "   (others)   "

p_top <- ggplot(df_demo) +
  aes(x = Days, y = Reaction) +
  geom_line(aes(group = Subject), color = col_data) +
  geom_point(color = col_data, size = 2) +
  facet_wrap("Subject", nrow = 1) +
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Each participant contributes data") +
  labs(x = NULL, y = NULL, tag = tag_others) + 
  theme_grey(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.tag.position = "right",
    plot.tag = element_text(size = rel(.9)),
    plot.title.position = "plot"
  )
```

## Bottom row: Beams of light

Let's skip to the bottom row because it uses the same coordinates,
points and theming as the top row. The plot visualizes the posterior
expectations (predicted means) as a median and 95% interval.
[tidybayes](https://github.com/mjskay/tidybayes) makes the process
straightforward with `add_epred_draws()` to add model fits onto a
dataframe and with `stat_lineribbon()` to plot a line and ribbon summary
of a posterior distribution. Here *epred* stands for \*e\*xpectations of
posterior \*pred\*ictions, so predicted means on the outcome scale.


```r
df_demo_fitted <- df_demo %>%
  # Create a dataframe with all possible combos of Subject and Days
  tidyr::expand(
    Subject,
    Days = seq(min(Days), max(Days), by = 1)
  ) %>%
  # Get the posterior predictions
  add_epred_draws(object = b)
```

Now we have 4,000 posterior samples for the fitted `Reaction` (`.epred`)
for each `Subject` for each day.


```r
df_demo_fitted
#> # A tibble: 160,000 x 7
#> # Groups:   Subject, Days, .row [40]
#>    Subject  Days  .row .chain .iteration .draw .epred
#>    <chr>   <dbl> <int>  <int>      <int> <int>  <dbl>
#>  1 333         0     1     NA         NA     1   272.
#>  2 333         0     1     NA         NA     2   264.
#>  3 333         0     1     NA         NA     3   266.
#>  4 333         0     1     NA         NA     4   286.
#>  5 333         0     1     NA         NA     5   269.
#>  6 333         0     1     NA         NA     6   270.
#>  7 333         0     1     NA         NA     7   265.
#>  8 333         0     1     NA         NA     8   283.
#>  9 333         0     1     NA         NA     9   246.
#> 10 333         0     1     NA         NA    10   269.
#> # ... with 159,990 more rows
```

Given these posterior fits, we call `stat_lineribbon()` to get the
median and 95% intervals. The plotting code is otherwise the same as the
last one, except for a different title and two lines that set the fill
color and hide the fill legend.


```r
col_data <- "#2F2E33"
tag_others <- "   (others)   "

p_bottom <- ggplot(df_demo_fitted) +
  aes(x = Days, y = .epred) +
  # .width is the interval width
  stat_lineribbon(alpha = .4, .width = .95) +
  geom_point(
    aes(y = Reaction), 
    data = df_demo, 
    size = 2, 
    color = col_data
  ) +
  facet_wrap("Subject", nrow = 1) +
  # Use the viridis scale on the ribbon fill
  scale_color_viridis_d(aesthetics = "fill") +
  # No legend
  guides(fill = "none") +
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle(
    "Individual trajectories \"borrow information\" from others"
  ) +
  labs(x = NULL, y = NULL, tag = tag_others) + 
  theme_grey(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.tag.position = "right",
    plot.tag = element_text(size = rel(.9)),
    plot.title.position = "plot"
  )

p_bottom
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/bottom-row-1.png" title="A plot with a showing the points from the earlier plot but showing the posterior median and 95% interval band." alt="A plot with a showing the points from the earlier plot but showing the posterior median and 95% interval band." width="66%" style="display: block; margin: auto;" />



## Middle row: Piles of ribbons

For the center population plot, we are going to use posterior predicted
means for a new (as yet unobserved) participant. This type of prediction
incorporates the uncertainty for the population average (i.e., the fixed
effects) and the population variation (i.e., the random effects).

We do this by defining a new participant and obtaining their posterior
fitted values. I like to use `"fake"` as the ID for the hypothetical
participant.


```r
df_population <- df_sleep %>%
  distinct(Days) %>%
  mutate(Subject = "fake") %>%
  add_epred_draws(b, allow_new_levels = TRUE)
```

Next, it's just a matter of using `stat_lineribbon()` again with many,
many `.width` values to recreate that center visualization.


```r
ggplot(df_population) +
  aes(x = Days, y = .epred) +
  stat_lineribbon(
    .width = c(.1, .25, .5, .6, .7, .8, .9, .95)
  ) +
  scale_x_continuous(
    "Days",
    breaks = seq(0, 9, by = 2),
    minor_breaks = NULL
  ) +
  coord_cartesian(ylim = c(200, 500)) +
  scale_y_continuous("Reaction Time") +
  scale_color_viridis_d(aesthetics = "fill") +
  guides(fill = "none") +
  ggtitle("Model estimates the population of participants") +
  theme_grey(base_size = 14) +
  theme(plot.title.position = "plot")
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/middle-attempt-1-1.png" title="A plot showing several differently colored probability bands to indicate which means are most probable in the population. The population median is marked with a distracting black line." alt="A plot showing several differently colored probability bands to indicate which means are most probable in the population. The population median is marked with a distracting black line." width="66%" style="display: block; margin: auto;" />

Actually, not quite. That median line ruins everything. It needs to go.

I'm too lazy to figure out how to get `stat_lineribbon()` to draw just
the ribbons. Instead, note that colors in ggplot2 can be 8-digit hex
codes, where the last two digits set the transparency for the color.


```r
ggplot() + 
  geom_vline(xintercept = 1, size = 4, color = "#000000FF") + 
  geom_vline(xintercept = 2, size = 4, color = "#000000CC") + 
  geom_vline(xintercept = 3, size = 4, color = "#000000AA") +  
  geom_vline(xintercept = 4, size = 4, color = "#00000077") +  
  geom_vline(xintercept = 5, size = 4, color = "#00000044") +  
  geom_vline(xintercept = 6, size = 4, color = "#00000011") + 
  ggtitle("Using 8-digit hex colors for transparency values") +
  theme(panel.grid = element_blank())
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/hex-8-demo-1.png" title="A diagram showing vertical lines that become increasingly transparent." alt="A diagram showing vertical lines that become increasingly transparent." width="50%" style="display: block; margin: auto;" />

This is the other shortcut I used in this diagram: I tell
`stat_lineribbon()` to use a color with `00` for the final 2 digits, so
that it draws the median as a completely transparent line.


```r
p_population <- ggplot(df_population) +
  aes(x = Days, y = .epred) +
  stat_lineribbon(
    # new part
    color = "#11111100",
    .width = c(.1, .25, .5, .6, .7, .8, .9, .95)
  ) +
  scale_x_continuous(
    "Days",
    breaks = seq(0, 9, by = 2),
    minor_breaks = NULL
  ) +
  coord_cartesian(ylim = c(200, 500)) +
  scale_y_continuous("Reaction Time") +
  scale_color_viridis_d(aesthetics = "fill") +
  guides(fill = "none") +
  ggtitle("Model estimates the population of participants") +
  theme_grey(base_size = 14) +
  theme(plot.title.position = "plot")
p_population
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/better-middle-1.png" title="The previous population plot but with the black line at the population median removed." alt="The previous population plot but with the black line at the population median removed." width="66%" style="display: block; margin: auto;" />

## Mooooooo: cowplot time

Finally, we use `plot_grid()` from cowplot[^cow] to put things
together. First, I don't want the middle population plot to be as wide
as the top and bottom rows, so I first create a `plot_grid()` containing
the center plot and an empty `NULL` spacer to its right. (I recommend
[this vignette](https://wilkelab.org/cowplot/articles/plot_grid.html)
for learning how to use `plot_grid()`.)


```r
p_middle <- plot_grid(
  p_population, 
  NULL, 
  nrow = 1, 
  rel_widths = c(3, .5)
)
```

Now, we just stack the three on top of each other in a single column:


```r
plot_grid(
  p_top,
  p_middle,
  p_bottom,
  ncol = 1,
  rel_heights = c(1, 2, 1)
)
```

<img src="/figs/2019-11-25-another-mixed-effects-model-visualization/final-diagram-1.png" title="The final assembled diagram." alt="The final assembled diagram." width="80%" style="display: block; margin: auto;" />

That's it! It's amazing how much work tidybayes and cowplot save us in
making these plots. In fact, without being aware of them or their
capabilities, I might have been discouraged from even trying to create
this diagram.





***

*Last knitted on 2021-11-19. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2019-11-25-another-mixed-effects-model-visualization.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info  --------------------------------------------------------------
    #>  hash: handshake: medium-dark skin tone, person running: medium-dark skin tone, mage: medium skin tone
    #> 
    #>  setting  value
    #>  version  R version 4.1.1 (2021-08-10)
    #>  os       Windows 10 x64 (build 22000)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language (EN)
    #>  collate  English_United States.1252
    #>  ctype    English_United States.1252
    #>  tz       America/Chicago
    #>  date     2021-11-19
    #>  pandoc   2.14.0.3 @ C:\\PROGRA~1\\RStudio\\bin\\pandoc\\pandoc.exe
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  ! package        * version  date (UTC) lib source
    #>    abind            1.4-5    2016-07-21 [1] CRAN (R 4.1.0)
    #>    arrayhelpers     1.1-0    2020-02-04 [1] CRAN (R 4.1.0)
    #>    assertthat       0.2.1    2019-03-21 [1] CRAN (R 4.1.0)
    #>    backports        1.3.0    2021-10-27 [1] CRAN (R 4.1.1)
    #>    base64enc        0.1-3    2015-07-28 [1] CRAN (R 4.1.0)
    #>    bayesplot        1.8.1    2021-06-14 [1] CRAN (R 4.1.0)
    #>    boot             1.3-28   2021-05-03 [1] CRAN (R 4.1.0)
    #>    bridgesampling   1.1-2    2021-04-16 [1] CRAN (R 4.1.0)
    #>    brms           * 2.16.1   2021-08-23 [1] CRAN (R 4.1.0)
    #>    Brobdingnag      1.2-6    2018-08-13 [1] CRAN (R 4.1.0)
    #>    broom            0.7.10   2021-10-31 [1] CRAN (R 4.1.1)
    #>    callr            3.7.0    2021-04-20 [1] CRAN (R 4.1.0)
    #>    cellranger       1.1.0    2016-07-27 [1] CRAN (R 4.1.0)
    #>    checkmate        2.0.0    2020-02-06 [1] CRAN (R 4.1.0)
    #>    cli              3.1.0    2021-10-27 [1] CRAN (R 4.1.1)
    #>    coda             0.19-4   2020-09-30 [1] CRAN (R 4.1.0)
    #>    codetools        0.2-18   2020-11-04 [2] CRAN (R 4.1.1)
    #>    colorspace       2.0-2    2021-06-24 [1] CRAN (R 4.1.0)
    #>    colourpicker     1.1.1    2021-10-04 [1] CRAN (R 4.1.1)
    #>    cowplot        * 1.1.1    2020-12-30 [1] CRAN (R 4.1.1)
    #>    crayon           1.4.2    2021-10-29 [1] CRAN (R 4.1.1)
    #>    crosstalk        1.2.0    2021-11-04 [1] CRAN (R 4.1.1)
    #>    curl             4.3.2    2021-06-23 [1] CRAN (R 4.1.0)
    #>    DBI              1.1.1    2021-01-15 [1] CRAN (R 4.1.0)
    #>    dbplyr           2.1.1    2021-04-06 [1] CRAN (R 4.1.0)
    #>    digest           0.6.28   2021-09-23 [1] CRAN (R 4.1.1)
    #>    distributional   0.2.2    2021-02-02 [1] CRAN (R 4.1.0)
    #>    dplyr          * 1.0.7    2021-06-18 [1] CRAN (R 4.1.0)
    #>    DT               0.20     2021-11-15 [1] CRAN (R 4.1.2)
    #>    dygraphs         1.1.1.6  2018-07-11 [1] CRAN (R 4.1.0)
    #>    ellipsis         0.3.2    2021-04-29 [1] CRAN (R 4.1.0)
    #>    emmeans          1.7.0    2021-09-29 [1] CRAN (R 4.1.1)
    #>    estimability     1.3      2018-02-11 [1] CRAN (R 4.1.0)
    #>    evaluate         0.14     2019-05-28 [1] CRAN (R 4.1.0)
    #>    fansi            0.5.0    2021-05-25 [1] CRAN (R 4.1.0)
    #>    farver           2.1.0    2021-02-28 [1] CRAN (R 4.1.0)
    #>    fastmap          1.1.0    2021-01-25 [1] CRAN (R 4.1.0)
    #>    forcats        * 0.5.1    2021-01-27 [1] CRAN (R 4.1.0)
    #>    fs               1.5.0    2020-07-31 [1] CRAN (R 4.1.0)
    #>    gamm4            0.2-6    2020-04-03 [1] CRAN (R 4.1.0)
    #>    generics         0.1.1    2021-10-25 [1] CRAN (R 4.1.1)
    #>    ggdist           3.0.0    2021-07-19 [1] CRAN (R 4.1.0)
    #>    ggplot2        * 3.3.5    2021-06-25 [1] CRAN (R 4.1.0)
    #>    ggridges         0.5.3    2021-01-08 [1] CRAN (R 4.1.0)
    #>    git2r            0.28.0   2021-01-10 [1] CRAN (R 4.1.0)
    #>    glue             1.4.2    2020-08-27 [1] CRAN (R 4.1.1)
    #>    gridExtra        2.3      2017-09-09 [1] CRAN (R 4.1.0)
    #>    gtable           0.3.0    2019-03-25 [1] CRAN (R 4.1.0)
    #>    gtools           3.9.2    2021-06-06 [1] CRAN (R 4.1.0)
    #>    haven            2.4.3    2021-08-04 [1] CRAN (R 4.1.0)
    #>    here             1.0.1    2020-12-13 [1] CRAN (R 4.1.0)
    #>    highr            0.9      2021-04-16 [1] CRAN (R 4.1.0)
    #>    hms              1.1.1    2021-09-26 [1] CRAN (R 4.1.1)
    #>    htmltools        0.5.2    2021-08-25 [1] CRAN (R 4.1.1)
    #>    htmlwidgets      1.5.4    2021-09-08 [1] CRAN (R 4.1.1)
    #>    httpuv           1.6.3    2021-09-09 [1] CRAN (R 4.1.1)
    #>    httr             1.4.2    2020-07-20 [1] CRAN (R 4.1.0)
    #>    igraph           1.2.7    2021-10-15 [1] CRAN (R 4.1.1)
    #>    inline           0.3.19   2021-05-31 [1] CRAN (R 4.1.0)
    #>    jsonlite         1.7.2    2020-12-09 [1] CRAN (R 4.1.0)
    #>    knitr          * 1.36     2021-09-29 [1] CRAN (R 4.1.1)
    #>    labeling         0.4.2    2020-10-20 [1] CRAN (R 4.1.0)
    #>    later            1.3.0    2021-08-18 [1] CRAN (R 4.1.1)
    #>    lattice          0.20-45  2021-09-22 [1] CRAN (R 4.1.1)
    #>    lifecycle        1.0.1    2021-09-24 [1] CRAN (R 4.1.1)
    #>    lme4             1.1-27.1 2021-06-22 [1] CRAN (R 4.1.0)
    #>    loo              2.4.1    2020-12-09 [1] CRAN (R 4.1.0)
    #>    lubridate        1.8.0    2021-10-07 [1] CRAN (R 4.1.1)
    #>    magrittr         2.0.1    2020-11-17 [1] CRAN (R 4.1.0)
    #>    markdown         1.1      2019-08-07 [1] CRAN (R 4.1.0)
    #>    MASS             7.3-54   2021-05-03 [1] CRAN (R 4.1.0)
    #>    Matrix           1.3-4    2021-06-01 [1] CRAN (R 4.1.0)
    #>    matrixStats      0.61.0   2021-09-17 [1] CRAN (R 4.1.1)
    #>    mgcv             1.8-38   2021-10-06 [1] CRAN (R 4.1.1)
    #>    mime             0.12     2021-09-28 [1] CRAN (R 4.1.1)
    #>    miniUI           0.1.1.1  2018-05-18 [1] CRAN (R 4.1.0)
    #>    minqa            1.2.4    2014-10-09 [1] CRAN (R 4.1.0)
    #>    modelr           0.1.8    2020-05-19 [1] CRAN (R 4.1.0)
    #>    munsell          0.5.0    2018-06-12 [1] CRAN (R 4.1.0)
    #>    mvtnorm          1.1-3    2021-10-08 [1] CRAN (R 4.1.1)
    #>    nlme             3.1-152  2021-02-04 [2] CRAN (R 4.1.1)
    #>    nloptr           1.2.2.3  2021-11-02 [1] CRAN (R 4.1.2)
    #>    pillar           1.6.4    2021-10-18 [1] CRAN (R 4.1.1)
    #>    pkgbuild         1.2.0    2020-12-15 [1] CRAN (R 4.1.0)
    #>    pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.1.0)
    #>    plyr             1.8.6    2020-03-03 [1] CRAN (R 4.1.0)
    #>    posterior        1.1.0    2021-09-09 [1] CRAN (R 4.1.1)
    #>    prettyunits      1.1.1    2020-01-24 [1] CRAN (R 4.1.0)
    #>    processx         3.5.2    2021-04-30 [1] CRAN (R 4.1.0)
    #>    projpred         2.0.2    2020-10-28 [1] CRAN (R 4.1.0)
    #>    promises         1.2.0.1  2021-02-11 [1] CRAN (R 4.1.0)
    #>    ps               1.6.0    2021-02-28 [1] CRAN (R 4.1.0)
    #>    purrr          * 0.3.4    2020-04-17 [1] CRAN (R 4.1.0)
    #>    R6               2.5.1    2021-08-19 [1] CRAN (R 4.1.0)
    #>    ragg             1.2.0    2021-10-30 [1] CRAN (R 4.1.1)
    #>    Rcpp           * 1.0.7    2021-07-07 [1] CRAN (R 4.1.0)
    #>  D RcppParallel     5.1.4    2021-05-04 [1] CRAN (R 4.1.0)
    #>    readr          * 2.1.0    2021-11-11 [1] CRAN (R 4.1.2)
    #>    readxl           1.3.1    2019-03-13 [1] CRAN (R 4.1.0)
    #>    reprex           2.0.1    2021-08-05 [1] CRAN (R 4.1.0)
    #>    reshape2         1.4.4    2020-04-09 [1] CRAN (R 4.1.0)
    #>    rlang            0.4.11   2021-04-30 [1] CRAN (R 4.1.1)
    #>    rprojroot        2.0.2    2020-11-15 [1] CRAN (R 4.1.0)
    #>    rsconnect        0.8.25   2021-11-19 [1] CRAN (R 4.1.1)
    #>    rstan            2.21.2   2020-07-27 [1] CRAN (R 4.1.0)
    #>    rstantools       2.1.1    2020-07-06 [1] CRAN (R 4.1.0)
    #>    rstudioapi       0.13     2020-11-12 [1] CRAN (R 4.1.0)
    #>    rvest            1.0.2    2021-10-16 [1] CRAN (R 4.1.1)
    #>    scales           1.1.1    2020-05-11 [1] CRAN (R 4.1.0)
    #>    sessioninfo      1.2.1    2021-11-02 [1] CRAN (R 4.1.1)
    #>    shiny            1.7.1    2021-10-02 [1] CRAN (R 4.1.1)
    #>    shinyjs          2.0.0    2020-09-09 [1] CRAN (R 4.1.0)
    #>    shinystan        2.5.0    2018-05-01 [1] CRAN (R 4.1.0)
    #>    shinythemes      1.2.0    2021-01-25 [1] CRAN (R 4.1.0)
    #>    StanHeaders      2.21.0-7 2020-12-17 [1] CRAN (R 4.1.0)
    #>    stringi          1.7.5    2021-10-04 [1] CRAN (R 4.1.1)
    #>    stringr        * 1.4.0    2019-02-10 [1] CRAN (R 4.1.0)
    #>    svUnit           1.0.6    2021-04-19 [1] CRAN (R 4.1.0)
    #>    systemfonts      1.0.3    2021-10-13 [1] CRAN (R 4.1.1)
    #>    tensorA          0.36.2   2020-11-19 [1] CRAN (R 4.1.0)
    #>    textshaping      0.3.6    2021-10-13 [1] CRAN (R 4.1.1)
    #>    threejs          0.3.3    2020-01-21 [1] CRAN (R 4.1.0)
    #>    tibble         * 3.1.5    2021-09-30 [1] CRAN (R 4.1.1)
    #>    tidybayes      * 3.0.1    2021-08-22 [1] CRAN (R 4.1.0)
    #>    tidyr          * 1.1.4    2021-09-27 [1] CRAN (R 4.1.1)
    #>    tidyselect       1.1.1    2021-04-30 [1] CRAN (R 4.1.0)
    #>    tidyverse      * 1.3.1    2021-04-15 [1] CRAN (R 4.1.0)
    #>    tzdb             0.2.0    2021-10-27 [1] CRAN (R 4.1.1)
    #>    utf8             1.2.2    2021-07-24 [1] CRAN (R 4.1.0)
    #>    V8               3.6.0    2021-11-10 [1] CRAN (R 4.1.2)
    #>    vctrs            0.3.8    2021-04-29 [1] CRAN (R 4.1.0)
    #>    viridisLite      0.4.0    2021-04-13 [1] CRAN (R 4.1.0)
    #>    withr            2.4.2    2021-04-18 [1] CRAN (R 4.1.0)
    #>    xfun             0.26     2021-09-14 [1] CRAN (R 4.1.1)
    #>    xml2             1.3.2    2020-04-23 [1] CRAN (R 4.1.0)
    #>    xtable           1.8-4    2019-04-21 [1] CRAN (R 4.1.0)
    #>    xts              0.12.1   2020-09-09 [1] CRAN (R 4.1.0)
    #>    zoo              1.8-9    2021-03-09 [1] CRAN (R 4.1.0)
    #> 
    #>  [1] C:/Users/Tristan/Documents/R/win-library/4.1
    #>  [2] C:/Program Files/R/R-4.1.1/library
    #> 
    #>  D -- DLL MD5 mismatch, broken installation.
    #> 
    #> ------------------------------------------------------------------------------
    ```

[^cow]: As I've mentioned elsewhere on this site, I grew up on a dairy farm 🐮, so I always read *cowplot* with an agricultural reading: These are plots of space that are being fenced off and gridded together, like one might see in [an aerial shot of farmland](https://pixels.com/featured/aerial-of-agricultural-fields-james-p-blair.html). But the package author is Claus O. Wilke, so it's probably just C.O.W.'s plotting package. *Probably.* I've never asked and don't intend to. I don't want my pastoral notions dispelled.
