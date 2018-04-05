---
title: Ridgelines in bayesplot 1.5.0
excerpt: "It's getting faster. Moving faster now. It's getting out of hand"
tags:
  - bayesplot
  - ggplot2
  - r
---

At the end of March, Jonah Gabry and I released
[bayesplot 1.5.0][bayesplot]. The major additions to the package were
visualizations using ridgelines and a new plot for PIT diagnostics from LOO
validation. I don't know what that LOO PIT thing is yet, so I'll talk about how
ridgelines have been incorporated into bayesplot instead.

![Unknown Pleasures cover](/assets/images/up-cover.jpg){: .align-right}

Ridgelines display the distributions of some variables---think of a series
of bell curves---but allow the curves to vary in height or overlap each other.
Several curves packed together look like a mountain ridge (hence the name). The
canonical example of this visualization are the cold, austere lines from the
[cover of Joy Division's *Unknown Pleasures*][up-cover]. 

In this post, I briefly describe where ridgelines appear in the package now. At
the end of the post, I show a more involved example of fitting a model and
plotting results from it.

## Behind the scenes of `mcmc_areas()`

I'm going to start with a less obvious use of ridgelines. `mcmc_areas()`
visualizes the posterior distribution of model parameters. It helps us see where
the peak values are, and occasionally, whether there is more than one peak. By
default, it will plot the complete range of the distribution, shade the
middle 50% of the distribution, and draw a line at the median.

For this release, I [rewrote the function][pull-request] so that it uses the
[ggridges][ggridges] package. I had noticed that the shapes it produced had
different areas. Here's an example of how the plots have changed.


```r
library(ggplot2)

# Temporarily install the previous version of bayesplot and 
# make an mcmc_areas() plot.
withr::with_temp_libpaths({
  versions::install.versions("bayesplot", "1.4.0", quiet = TRUE)
  
  example_data <- bayesplot::example_mcmc_draws()
  p_a <- bayesplot::mcmc_areas(example_data, pars = c("beta[1]", "beta[2]")) +
    ggtitle("Version 1.4.0") +
    bayesplot::theme_default()
})
#> package 'bayesplot' successfully unpacked and MD5 sums checked

library(bayesplot)
#> This is bayesplot version 1.5.0
#> - Plotting theme set to bayesplot::theme_default()
#> - Online documentation at mc-stan.org/bayesplot
p_b <- mcmc_areas(example_data, c("beta[1]", "beta[2]")) + 
  ggtitle("Version 1.5.0") +
  bayesplot::theme_default()

# Show the two together
cowplot::plot_grid(p_a, p_b)
```

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/version-differences-1.png" title="Comparison of mcmc_areas() in bayesplot 1.4.0 and 1.5.0" alt="Comparison of mcmc_areas() in bayesplot 1.4.0 and 1.5.0" width="80%" style="display: block; margin: auto;" />

In the plot on the left, the shapes have different areas. Eyeballing it, it
looks like `beta[2]` would fit inside `beta[1]`. Area is width times height.
These shapes have different widths because they cover different values---that
makes sense. Therefore, the problem is the height---namely, the fact that the
shapes have the same height. To make the areas more similar, we adjust the
heights, and that's what's happening in the plot on the right. The distribution
for `beta[2]` is concentrated to a more narrow range, and that feature manifests
in that shape's much higher peak.

### Three plots in one

This plot is built using the ggridges packages which handles drawing the shapes
at different heights. It took a bit of trickery, however, to do the annotation
for the shaded 50% interval and the median. Here is a high-level overview of
what's happening.

`mcmc_areas_data()` tidies the MCMC samples into a dataframe suitable for
plotting. It computes the density of the parameter values, and it returns the
densities over *three* intervals: 1) an outer interval, 2) an inner interval to
be shaded, and 3) a narrow "point" interval.


```r
library(dplyr, warn.conflicts = FALSE)

d <- mcmc_areas_data(example_data, c("beta[1]", "beta[2]")) %>% 
  mutate(
    interval = factor(interval, c("outer", "inner", "point")))

d
#> # A tibble: 4,238 x 5
#>    parameter interval interval_width     x density
#>    <fct>     <fct>             <dbl> <dbl>   <dbl>
#>  1 beta[1]   inner             0.500 0.136   0.986
#>  2 beta[1]   inner             0.500 0.137   0.988
#>  3 beta[1]   inner             0.500 0.137   0.989
#>  4 beta[1]   inner             0.500 0.138   0.990
#>  5 beta[1]   inner             0.500 0.138   0.991
#>  6 beta[1]   inner             0.500 0.139   0.992
#>  7 beta[1]   inner             0.500 0.139   0.993
#>  8 beta[1]   inner             0.500 0.139   0.994
#>  9 beta[1]   inner             0.500 0.140   0.995
#> 10 beta[1]   inner             0.500 0.140   0.997
#> # ... with 4,228 more rows
```

Internally, `mcmc_areas()` uses `ggridges::geom_density_ridges()` to draw
*three* different ridgeline plots.


```r
# Separate the full range so it can be drawn on each facet.
full_range <- d %>% 
  filter(interval == "outer") %>% 
  select(-interval)

p_base <- ggplot(d) + 
  aes(x = x, y = parameter, fill = interval, height = density) + 
  # For reference include the full-range. 
  # `stat = "identity"` means that ggridges should not scale the heights
  ggridges::geom_density_ridges(
    stat = "identity", data = full_range, color = "grey80",
    size = 1, fill = NA) + 
  ggridges::geom_density_ridges(stat = "identity") + 
  scale_fill_brewer(type = "seq") + 
  guides(fill = FALSE)

p_base + 
  facet_wrap("interval")
```

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/three-fold-1.png" title="The three layers behind an mcmc_areas() plot drawn separately" alt="The three layers behind an mcmc_areas() plot drawn separately" width="100%" style="display: block; margin: auto;" />

These intervals are layered to achieve the desired look. 

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/collapsed-1.png" title="The layers from the previous plot flattened together" alt="The layers from the previous plot flattened together" width="50%" style="display: block; margin: auto;" />

There's a bit more fussing to the actual function. For example, the width of the
intervals and whether a point is calculated are options that have to be handled.
The tips of the topmost peaks are not cut off in those plots as well. But that's
the basic idea.



## Overlapping ridgelines for related parameters

Plots with the classic, overlapping *Unknown Pleasures* look are supported by
`mcmc_areas_ridges()`. I like these plots for hierarchical models where the
parameters represent similar units. In fact, part of the reason Jonah approached
me about contributing to bayesplot was a [proof-of-concept demo][rpub-demo]
I wrote to visualize hierarchical effects using ridgeline plots, back when they
were popularly called "joyplots".

Below is the [eight schools][eight-schools] example where there is a
treatment effect for each school (thetas) and an overall average effect (mu).


```r
m <- shinystan::eight_schools@posterior_sample

mcmc_areas_ridges(m, pars = "mu", regex_pars = "theta") +
  ggplot2::ggtitle("Treatment effect on eight schools (Rubin, 1981)")
```

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/eight-schools-ridges-1.png" title="Overlapping ridgelines in a typical looking ridgeline plot" alt="Overlapping ridgelines in a typical looking ridgeline plot" width="80%" style="display: block; margin: auto;" />

If we really want to go for the *Unknown Pleasures* look, we can manually plot
the densities in a white-on-black theme. Most of the work here is padding zero
values onto the ridgelines so that they all have the same width.


```r
d <- mcmc_areas_ridges_data(m, pars = "mu", regex_pars = "theta") %>% 
  filter(interval == "outer") 

zeroes <- d %>%
  # Get range of each parameter
  group_by(parameter) %>% 
  summarise(pmin = min(x), pmax = max(x)) %>% 
  mutate(
    min = min(pmin), 
    max = max(pmax),
    # On each row, make a data-frame of zeroes from absolute minimum 
    # value to this parameter's minimum value
    lower_df = purrr::map2(
      min, pmin, 
      ~ data.frame(x = seq(.x, .y, length.out = 20), density = 0)),
    # Make a dataframe of zeroes from each parameter's max to the 
    # overall max
    upper_df = purrr::map2(
      pmax, max, 
      ~ data.frame(x = seq(.x, .y, length.out = 20), density = 0)),
    # Gather the zeroes together
    df = purrr::map2(lower_df, upper_df, bind_rows)) %>% 
  select(parameter, df) %>% 
  tidyr::unnest(df)

ggplot(bind_rows(d, zeroes)) + 
  aes(x = x, y = parameter, height = density) +
  ggridges::geom_density_ridges(
    stat = "identity", fill = NA, 
    color = "grey70", scale = 5) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "black"), 
    plot.margin = unit(c(.25, .25, .25, .25), units = "npc"),
    axis.ticks = element_blank())
```

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/unknown-1.png" title="The above plot rethemed to look like Unknown Pleausres" alt="The above plot rethemed to look like Unknown Pleausres" width="50%" style="display: block; margin: auto;" />

## Densities of different sampling chains

Finally, ridgelines appear in by-chain diagnostics using `mcmc_dens_chains()`.


```r
mcmc_dens_chains(m, pars = c("theta[1]", "theta[2]", "theta[3]"))
```

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/chain-ridges-1.png" title="A ridgeline plot where MCMC chains are drawn in different colors" alt="A ridgeline plot where MCMC chains are drawn in different colors" width="66%" style="display: block; margin: auto;" />

There really isn't much interesting to say about this plot, except that it
follows Mike DeCrescenzo's [early example][early-ridgeline-tweet] of using
ridgelines on Bayesian models.


## An example of fitting and plotting a model

The `cake` dataset in lme4 reports the angle of breakage of chocolate cakes
for 3 different recipes at 6 temperatures tested 15 times each (3 × 6 × 15 = 270
observations). The data are shown below, ignoring temperature details:


```r
set.seed(20180405)
cake <- lme4::cake

theme_set(theme_grey())

ggplot(cake) + 
  # aes(x = temp, y = angle, color = recipe) + 
  aes(x = replicate, y = angle, color = recipe) + 
  geom_text(aes(label = recipe), position = position_jitter(.15), size = 3) + 
  stat_summary(color = "grey10", size = .4) + 
  guides(color = FALSE, label = FALSE) +
  labs(
    x = "Replicate", 
    y = "Angle of breakage", 
    caption = "Letters: Recipe. Points: Replicate Mean ± SE")
#> No summary function supplied, defaulting to `mean_se()
```

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/cake-dataset-1.png" title="The cake dataset" alt="The cake dataset" width="80%" style="display: block; margin: auto;" />

It looks like the first few replications showed larger breakage angles compared
to later ones. From a modeling perspective, we would want to capture variation
due to replication. There could be variation due to recipe × replication
effects, so we will model the data at that level of detail.


```r
library(rstanarm)
#> Loading required package: Rcpp
#> Warning: package 'Rcpp' was built under R version 3.4.4
#> Loading required package: methods
#> rstanarm (Version 2.17.3, packaged: 2018-02-17 05:11:16 UTC)
#> - Do not expect the default priors to remain the same in future rstanarm versions.
#> Thus, R scripts should specify priors explicitly, even if they are just the defaults.
#> - For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores())
#> - Plotting theme set to bayesplot::theme_default().
m <- stan_glmer(
  angle ~ recipe * temp + (1 | recipe:replicate), 
  family = gaussian(), 
  data = cake,
  prior = normal(0, 1),
  seed = 20180405)
```

This models predicts the breakage angle using an average recipe, temperature,
and recipe × temperate effects and adjusting the prediction for each recipe ×
replication. 

Below, we use `mcmc_areas_ridges()` to plot the distributions of replication
effects for recipe A. The plotting functions in bayesplot take a set of MCMC
samples from a model. We can extract the samples using `as.matrix()`. We can
narrow the parameters to be plotted by naming them individually with `pars` or
by selecting all the parameters that match a regular expression pattern with
`regex_pars`.


```r
# Revert to the bayesplot default theme
theme_set(bayesplot::theme_default())

mcmc_areas_ridges(
  as.matrix(m), 
  regex_pars = "recipe:replicate:A") 
```

<img src="/figs//2018-04-03-ridgelines-in-bayesplot-1-5-0-release/cake-model-results-1.png" title="Replications effects for recipe A" alt="Replications effects for recipe A" width="60%" style="display: block; margin: auto;" />

The effects describe deviations from the recipe average. A faint line is drawn
at *x* = 0 represents the average value. Ridges to the left of the line are
below average and those to the right are above average. We can see that the
model captured how the first three replications had particular high breakage
angles.


[bayesplot]: http://mc-stan.org/bayesplot/index.html
  "bayesplot package website"

[up-cover]: https://blogs.scientificamerican.com/sa-visual/pop-culture-pulsar-origin-story-of-joy-division-s-unknown-pleasures-album-cover-video/ 
  "Pop Culture Pulsar: Origin Story of Joy Division’s Unknown Pleasures Album Cover"

[pull-request]: https://github.com/stan-dev/bayesplot/pull/115
  "My pull request where I refactored mcmc_areas()"

[ggridges]: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
  "ggridges package vignette"

[rpub-demo]: https://rpubs.com/tjmahr/joyplot
  "Demo of I wrote using ridgelines for a mixed effects model"

[eight-schools]: http://andrewgelman.com/2014/01/21/everything-need-know-bayesian-statistics-learned-eight-schools/
  "Blog post about the Eight Schools model"

[early-ridgeline-tweet]: https://twitter.com/mikedecr/status/886376620004700160?lang=en
  "Tweet with a plot containing ridgelines from a hierarchical model"

