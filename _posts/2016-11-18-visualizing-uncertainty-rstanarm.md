---
title: "RStanARM basics: visualizing uncertainty in linear regression"
excerpt: "Summarizing many, many lines of fit"
header:
  overlay_image: "sleeping-raccoon-1280.jpg"
  caption: "Photo credit: [**Lance Anderson**](https://unsplash.com/photos/QZwf5yNopUo)"
tags:
  - rstanarm
  - bayesian
  - r
---



As part of [my tutorial talk on RStanARM](/rstanarm-tutorial-slides/), I
presented some examples of how to visualize the uncertainty in Bayesian linear
regression models. This post is an expanded demonstration of the approaches I
presented in that tutorial.


Data: Does brain mass predict how much mammals sleep in a day?
-------------------------------------------------------------------------------

Let's use the [mammal sleep dataset from ggplot2][ggplot2-mammals]. This dataset
contains the number of hours spent sleeping per day for 83 different species of
mammals along with each species' brain mass (kg) and body mass (kg), among other
measures. Here's a first look at the data.


```r
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# Preview sorted by brain/body ratio. I chose this sorting so that humans would
# show up in the preview.
msleep %>% 
  select(name, sleep_total, brainwt, bodywt, everything()) %>% 
  arrange(desc(brainwt / bodywt))
#> # A tibble: 83 × 11
#>                              name sleep_total brainwt bodywt        genus
#>                             <chr>       <dbl>   <dbl>  <dbl>        <chr>
#> 1  Thirteen-lined ground squirrel        13.8 0.00400  0.101 Spermophilus
#> 2                      Owl monkey        17.0 0.01550  0.480        Aotus
#> 3       Lesser short-tailed shrew         9.1 0.00014  0.005    Cryptotis
#> 4                 Squirrel monkey         9.6 0.02000  0.743      Saimiri
#> 5                         Macaque        10.1 0.17900  6.800       Macaca
#> 6                Little brown bat        19.9 0.00025  0.010       Myotis
#> 7                          Galago         9.8 0.00500  0.200       Galago
#> 8                        Mole rat        10.6 0.00300  0.122       Spalax
#> 9                      Tree shrew         8.9 0.00250  0.104       Tupaia
#> 10                          Human         8.0 1.32000 62.000         Homo
#> # ... with 73 more rows, and 6 more variables: vore <chr>, order <chr>,
#> #   conservation <chr>, sleep_rem <dbl>, sleep_cycle <dbl>, awake <dbl>

ggplot(msleep) + 
  aes(x = brainwt, y = sleep_total) + 
  geom_point()
#> Warning: Removed 27 rows containing missing values (geom_point).
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/brain-sleep-1.png" title="Brain mass by sleep hours. This plot looks terrible because the masses span many orders of magnitudes." alt="Brain mass by sleep hours. This plot looks terrible because the masses span many orders of magnitudes." width="70%" style="display: block; margin: auto;" />

Hmmm, not very helpful! We should put our measures on a log-10 scale. Also, 27
of the species don't have brain mass data, so we'll exclude those rows for the
rest of this tutorial.


```r
msleep <- msleep %>% 
  filter(!is.na(brainwt)) %>% 
  mutate(log_brainwt = log10(brainwt), 
         log_bodywt = log10(bodywt), 
         log_sleep_total = log10(sleep_total))
```

Now, plot the log-transformed data. But let's also get a little fancy and label
the points for some example critters :cat: so that we can get some intuition
about the data in this scaling. (Plus, I wanted to try out the [annotation
tips][r4ds-labels] from the _R4DS_ book.)


```r
# Create a separate data-frame of species to highlight
ex_mammals <- c("Domestic cat", "Human", "Dog", "Cow", "Rabbit",
                "Big brown bat", "House mouse", "Horse", "Golden hamster")

# We will give some familiar species shorter names
renaming_rules <- c(
  "Domestic cat" = "Cat", 
  "Golden hamster" = "Hamster", 
  "House mouse" = "Mouse")

ex_points <- msleep %>% 
  filter(name %in% ex_mammals) %>% 
  mutate(name = stringr::str_replace_all(name, renaming_rules))

# Define these labels only once for all the plots
lab_lines <- list(
  brain_log = "Brain mass (kg., log-scaled)", 
  sleep_raw = "Sleep per day (hours)",
  sleep_log = "Sleep per day (log-hours)"
)

ggplot(msleep) + 
  aes(x = brainwt, y = sleep_total) + 
  geom_point(color = "grey40") +
  # Circles around highlighted points + labels
  geom_point(size = 3, shape = 1, color = "grey40", data = ex_points) +
  ggrepel::geom_text_repel(aes(label = name), data = ex_points) + 
  # Use log scaling on x-axis
  scale_x_log10(breaks = c(.001, .01, .1, 1)) + 
  labs(x = lab_lines$brain_log, y = lab_lines$sleep_raw)
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/log-brain-sleep-1.png" title="Brain mass by sleep hours, now with both on a log-10 scale. Some species have their data highlighted." alt="Brain mass by sleep hours, now with both on a log-10 scale. Some species have their data highlighted." width="70%" style="display: block; margin: auto;" />

As a child growing up on a dairy farm :cow:, it was remarkable to me how little 
I saw cows sleeping, compared to dogs or cats. Were they okay? Are they 
constantly tired and groggy? Maybe they are asleep when I'm asleep? Here, it
looks like they just don't need very much sleep.

Next, let's fit a classical regression model. We will use a log-scaled sleep
measure so that the regression line doesn't imply negative sleep (even though
brains never get _that_ large).


```r
m1_classical <- lm(log_sleep_total ~ log_brainwt, data = msleep) 
arm::display(m1_classical)
#> lm(formula = log_sleep_total ~ log_brainwt, data = msleep)
#>             coef.est coef.se
#> (Intercept)  0.74     0.04  
#> log_brainwt -0.13     0.02  
#> ---
#> n = 56, k = 2
#> residual sd = 0.17, R-Squared = 0.40
```



We can interpret the model in the usual way: A mammal with 1 kg (0 log-kg) 
of brain mass sleeps 10<sup>0.74</sup>&nbsp;= 5.5 hours per
day. A mammal with a tenth of that brain mass (-1 log-kg) sleeps 
10<sup>0.74&nbsp;+ 0.13</sup>&nbsp;= 7.4 hours.

We illustrate the regression results to show the predicted mean of _y_ and
its 95% confidence interval. This task is readily accomplished in ggplot2 using 
`stat_smooth()`. This function fits a model and plots the mean and CI for each 
aesthetic grouping of data[^1] in a plot.


```r
ggplot(msleep) + 
  aes(x = log_brainwt, y = log_sleep_total) + 
  geom_point() +
  stat_smooth(method = "lm", level = .95) + 
  scale_x_continuous(labels = function(x) 10 ^ x) +
  labs(x = lab_lines$brain_log, y = lab_lines$sleep_log)
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/log-brain-sleep-lm-fit-1.png" title="Brain mass by sleep hours, log-10 scale, plus the predicted mean and 95% CI from a linear regression." alt="Brain mass by sleep hours, log-10 scale, plus the predicted mean and 95% CI from a linear regression." width="70%" style="display: block; margin: auto;" />

This interval conveys some uncertainty in the estimate of the mean, but this 
interval has a [frequentist interpretation][interval-interp] which can be
unintuitive for this sort of data.

Now, for the point of this post: **What's the Bayesian version of this kind of
visualization**? Specifically, we want to illustrate:

* Predictions from a regression model
* Some uncertainty about those predictions
* Raw data used to train the model



Option 1: The pile-of-lines plot
-------------------------------------------------------------------------------

The regression line in the classical plot is just one particular line. It's the
line of best fit that satisfies a least-squares or maximum-likelihood objective. 
Our Bayesian model estimates an entire distribution of plausible
regression lines. The first way to visualize our uncertainty is to plot our
own line of best fit along with a sample of other lines from the posterior
distribution of the model.

First, we fit a model RStanARM using weakly informative priors.


```r
library("rstanarm")

m1 <- stan_glm(
  log_sleep_total ~ log_brainwt, 
  family = gaussian(), 
  data = msleep, 
  prior = normal(0, 3),
  prior_intercept = normal(0, 3))
```

We now have 4,000 credible regressions lines for our data.


```r
summary(m1)
#> stan_glm(formula = log_sleep_total ~ log_brainwt, family = gaussian(), 
#>     data = msleep, prior = normal(0, 3), prior_intercept = normal(0, 
#>         3))
#> 
#> Family: gaussian (identity)
#> Algorithm: sampling
#> Posterior sample size: 4000
#> Observations: 56
#> 
#> Estimates:
#>                 mean   sd   2.5%   25%   50%   75%   97.5%
#> (Intercept)    0.7    0.0  0.7    0.7   0.7   0.8   0.8   
#> log_brainwt   -0.1    0.0 -0.2   -0.1  -0.1  -0.1  -0.1   
#> sigma          0.2    0.0  0.1    0.2   0.2   0.2   0.2   
#> mean_PPD       1.0    0.0  0.9    0.9   1.0   1.0   1.0   
#> log-posterior 12.1    1.2  9.1   11.6  12.4  12.9  13.4   
#> 
#> Diagnostics:
#>               mcse Rhat n_eff
#> (Intercept)   0.0  1.0  3249 
#> log_brainwt   0.0  1.0  3247 
#> sigma         0.0  1.0  2823 
#> mean_PPD      0.0  1.0  3769 
#> log-posterior 0.0  1.0  2248 
#> 
#> For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).
```

For models fit by RStanARM, the generic coefficient function `coef()` returns 
the median parameter values.


```r
coef(m1)
#> (Intercept) log_brainwt 
#>   0.7358459  -0.1260126
coef(m1_classical)
#> (Intercept) log_brainwt 
#>   0.7363492  -0.1264049
```

We can see that the intercept and slope of the median line is pretty close to
the classical model's intercept and slope. The median line serves as the "point
estimate" for our model: If we had to summarize the modeled relationship using
just a single number for each parameter, we can use the medians.

One way to visualize our model therefore is to **plot our point-estimate line
plus a sample of the other credible lines from our model**. First, we create a 
data-frame with all 4,000 regression lines.


```r
# Coercing a model to a data-frame returns data-frame of posterior samples. 
# One row per sample.
fits <- m1 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)
fits
#> # A tibble: 4,000 × 2
#>    intercept log_brainwt
#>        <dbl>       <dbl>
#> 1  0.7378144 -0.13516523
#> 2  0.8177494 -0.09003485
#> 3  0.8074633 -0.09440248
#> 4  0.8545143 -0.11018825
#> 5  0.8061616 -0.10772447
#> 6  0.8422515 -0.09022916
#> 7  0.7968577 -0.11273502
#> 8  0.7679546 -0.12267055
#> 9  0.7370562 -0.12933438
#> 10 0.7192707 -0.12798695
#> # ... with 3,990 more rows
```

We now plot the 500 randomly sampled lines from our model with light, 
semi-transparent lines.


```r
# aesthetic controllers
n_draws <- 500
alpha_level <- .15
col_draw <- "grey60"
col_median <-  "#3366FF"

ggplot(msleep) + 
  aes(x = log_brainwt, y = log_sleep_total) + 
  # Plot a random sample of rows as gray semi-transparent lines
  geom_abline(aes(intercept = intercept, slope = log_brainwt), 
              data = sample_n(fits, n_draws), color = col_draw, 
              alpha = alpha_level) + 
  # Plot the median values in blue
  geom_abline(intercept = median(fits$intercept), 
              slope = median(fits$log_brainwt), 
              size = 1, color = col_median) +
  geom_point() + 
  scale_x_continuous(labels = function(x) 10 ^ x) +
  labs(x = lab_lines$brain_log, y = lab_lines$sleep_log)
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/pile-of-lines-plot-1.png" title="Brain mass by sleep hours, log-10 scale, plus the median regression line and 500 random regressions lines sampled from the posterior." alt="Brain mass by sleep hours, log-10 scale, plus the median regression line and 500 random regressions lines sampled from the posterior." width="70%" style="display: block; margin: auto;" />

Each of these light lines represents a credible prediction of the mean across 
the values of _x_. As these line pile up on top of each other, they create an 
uncertainty band around our line of best fit. More plausible lines are more 
likely to be sampled, so these lines overlap and create a uniform color around
the median line. As we move left or right, getting farther away from the mean of
_x_, the lines start to fan out and we see very faint individual lines for some
of the more extreme (yet still plausible) lines.

The advantage of this plot is that it is a direct visualization of posterior 
samples---one line per sample. It provides an estimate for the central tendency
in the data but it also converys uncertainty around that estimate. 

This approach has limitations, however. Lines for subgroups require a little 
more effort to undo interactions. Also, the regression lines span the whole _x_ 
axis which is not appropriate when subgroups only use a portion of the x-axis. 
(This limitation is solvable though.) Finally, I haven't found good defaults 
for the aesthetic options: The number of samples, the colors to use, and the 
transparency level. One can lose of lots and lots and lots time fiddling with
those knobs!



Option 2: Mean and its 95% interval
-------------------------------------------------------------------------------

Another option is a direct port of the `stat_smooth()` plot: Draw a line of
best fit and the 95% uncertainty interval around it.

To limit the amount of the _x_ axis used by the lines, we're going to create a 
sequence of 80 points along the range of the data. 


```r
x_rng <- range(msleep$log_brainwt) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 80)
new_data <- data_frame(
  observation = seq_along(x_steps), 
  log_brainwt = x_steps)
new_data
#> # A tibble: 80 × 2
#>    observation log_brainwt
#>          <int>       <dbl>
#> 1            1   -3.853872
#> 2            2   -3.795509
#> 3            3   -3.737146
#> 4            4   -3.678784
#> 5            5   -3.620421
#> 6            6   -3.562058
#> 7            7   -3.503695
#> 8            8   -3.445332
#> 9            9   -3.386970
#> 10          10   -3.328607
#> # ... with 70 more rows
```

The function `posterior_linpred()` returns the model-fitted means for a data-frame
of new data. I say _means_ because the function computes 80 predicted means for
each sample from the posterior. The result is 4000 x 80 matrix of fitted means.


```r
pred_lin <- posterior_linpred(m1, newdata = new_data)
dim(pred_lin)
#> [1] 4000   80
```

We are going to reduce this down to just a median and 95% interval around each 
point. I do some tidying to get the data into a long format (one row per fitted 
mean per posterior sample), and then do a table-join with the `observation` 
column included in `new_data`. I store these steps in a function because I
have to do them again later in this post.


```r
tidy_predictions <- function(mat_pred, df_data, obs_name = "observation",
                             prob_lwr = .025, prob_upr = .975) {
  # Get data-frame with one row per fitted value per posterior sample
  df_pred <- mat_pred %>% 
    as_data_frame %>% 
    setNames(seq_len(ncol(.))) %>% 
    tibble::rownames_to_column("posterior_sample") %>% 
    tidyr::gather_(obs_name, "fitted", setdiff(names(.), "posterior_sample"))
  df_pred
  
  # Helps with joining later
  class(df_pred[[obs_name]]) <- class(df_data[[obs_name]])
  
  # Summarise prediction interval for each observation
  df_pred %>% 
    group_by_(obs_name) %>% 
    summarise(median = median(fitted),
              lower = quantile(fitted, prob_lwr), 
              upper = quantile(fitted, prob_upr)) %>% 
    left_join(df_data, by = obs_name)
}

df_pred_lin <- tidy_predictions(pred_lin, new_data)
df_pred_lin
#> # A tibble: 80 × 5
#>    observation   median    lower    upper log_brainwt
#>          <int>    <dbl>    <dbl>    <dbl>       <dbl>
#> 1            1 1.221808 1.130714 1.314318   -3.853872
#> 2            2 1.214417 1.125008 1.305139   -3.795509
#> 3            3 1.206990 1.119488 1.296013   -3.737146
#> 4            4 1.199741 1.114178 1.286591   -3.678784
#> 5            5 1.192367 1.108308 1.277354   -3.620421
#> 6            6 1.184998 1.103334 1.267785   -3.562058
#> 7            7 1.177831 1.097672 1.258505   -3.503695
#> 8            8 1.170491 1.092854 1.249195   -3.445332
#> 9            9 1.163016 1.086950 1.240197   -3.386970
#> 10          10 1.155859 1.081630 1.231081   -3.328607
#> # ... with 70 more rows
```

We can do the line-plus-interval plot using `geom_ribbon()` for the uncertainty
band.


```r
p_linpread <- ggplot(msleep) + 
  aes(x = log_brainwt) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), data = df_pred_lin, 
              alpha = 0.4, fill = "grey60") + 
  geom_line(aes(y = median), data = df_pred_lin, colour = "#3366FF", size = 1) + 
  geom_point(aes(y = log_sleep_total)) + 
  scale_x_continuous(labels = function(x) 10 ^ x) +
  labs(x = lab_lines$brain_log, y = lab_lines$sleep_log)
p_linpread
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/posterior-linpred-plot-1.png" title="Brain mass by sleep hours, log-10 scale, plus the median and 95% uncertainty interval for the model-predicted mean." alt="Brain mass by sleep hours, log-10 scale, plus the median and 95% uncertainty interval for the model-predicted mean." width="70%" style="display: block; margin: auto;" />

This plot is just like the `stat_smooth()` plot, except the interval here is 
interpreted in terms of post-data probabilities: We're 95% certain---given the 
data, model and our prior information---that the "true" average sleep duration 
is contained in this interval. I put "true" in quotes because this is truth in
the "small world" of the model, to quote 
[_Statistical Rethinking_](http://xcelab.net/rm/statistical-rethinking/), not 
necessarily the real world.

Although the interpretation of the interval changes (compared to a classical 
confidence interval), its location barely changes at all. If we overlay a 
`stat_smooth()` layer onto this plot, we can see that two sets of intervals are 
virtually identical. With this much data and for this simple of a model, both
types of models can make very similar estimates.


```r
p_linpread + stat_smooth(aes(y = log_sleep_total), method = "lm")
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/posterior-linpred-plot-and-smooth-1.png" title="Previous line-plus-interval plot with the classical regression line and confidence interval overlaid." alt="Previous line-plus-interval plot with the classical regression line and confidence interval overlaid." width="70%" style="display: block; margin: auto;" />

The previous plot illustrates one limitation of this approach: Pragmatically
speaking, `stat_smooth()` basically does the same thing, and we're
not taking advantage of the affordances provided by our model. This is why 
RStanARM, in a kind of amusing way, disowns `posterior_linpred()` in its 
documentation:

> This function is occasionally convenient, but it should be used sparingly. 
> Inference and model checking should generally be carried out using the 
> posterior predictive distribution (see `posterior_predict`).

_Occasionally convenient._ :open_mouth: And elsewhere:

> See also: `posterior_predict` to draw from the posterior predictive 
> distribution of the outcome, which is almost always preferable.



Option 3: Mean and 95% interval for model-generated data
-------------------------------------------------------------------------------

The reason why `posterior_predict()` is preferable is that it uses more 
information from our model, namely the error term `sigma`. 
`poseterior_linpred()` predicts averages; `posterior_predict()` predicts new 
observations. This _posterior predictive checking_ helps us confirm whether our 
model---a story of how the data could have been generated---can produce new data
that resembles our data.

Here, we can use the function we defined earlier to get prediction intervals.


```r
# Still a matrix with one row per posterior draw and one column per observation
pred_post <- posterior_predict(m1, newdata = new_data)
dim(pred_post)
#> [1] 4000   80

df_pred_post <- tidy_predictions(pred_post, new_data)
df_pred_post
#> # A tibble: 80 × 5
#>    observation   median     lower    upper log_brainwt
#>          <int>    <dbl>     <dbl>    <dbl>       <dbl>
#> 1            1 1.222472 0.8646270 1.587308   -3.853872
#> 2            2 1.216009 0.8693197 1.559538   -3.795509
#> 3            3 1.207915 0.8509499 1.565226   -3.737146
#> 4            4 1.205512 0.8388800 1.566543   -3.678784
#> 5            5 1.199273 0.8318168 1.552506   -3.620421
#> 6            6 1.186385 0.8245860 1.535794   -3.562058
#> 7            7 1.181573 0.8333821 1.544027   -3.503695
#> 8            8 1.176840 0.8200045 1.517013   -3.445332
#> 9            9 1.167701 0.7943410 1.508184   -3.386970
#> 10          10 1.152479 0.8002372 1.516674   -3.328607
#> # ... with 70 more rows
```

And we can plot the interval in the same way.


```r
ggplot(msleep) + 
  aes(x = log_brainwt) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), data = df_pred_post, 
              alpha = 0.4, fill = "grey60") + 
  geom_line(aes(y = median), data = df_pred_post, colour = "#3366FF", size = 1) + 
  geom_point(aes(y = log_sleep_total)) + 
  scale_x_continuous(labels = function(x) 10 ^ x) +
  labs(x = lab_lines$brain_log, y = lab_lines$sleep_log)
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/posterior-predict-1.png" title="Brain mass by sleep hours, log-10 scale, plus the median and 95% interval for posterior predicted observations." alt="Brain mass by sleep hours, log-10 scale, plus the median and 95% interval for posterior predicted observations." width="70%" style="display: block; margin: auto;" />

First, we can appreciate that this interval is **much wider**. That's because the
interval doesn't summarize a particular statistic (like an average) but all of
the observations that can generated by our model. Okay, not _all_ of the
observations---just the 95% most probable observations.

Next, we can also appreciate that the line and the ribbon are **jagged due to 
simulation randomness**. Each prediction is a random number draw, and at each
value of _x_, we have 4000 such random draws. We computed a median and 95% 
interval at each _x_, but due to randomness from simulating new data, these
medians do not smoothly connect together in the plot. That's okay, because these
fluctuations are relatively small.

Finally, we can see that there are only two **points outside of the interval**. 
These appear to be the restless roe deer and the ever-sleepy giant armadillo.
These two represent the main outliers for our model because they fall slight
outside of the 95% prediction interval. In this way, the posterior predictive
interval can help us discover which data points are relative outliers for our
model.

(Maybe _outliers_ isn't the right word. It makes perfect sense that 2/56&nbsp;=
3.6% of the observations fall outside of the 95%
interval.)


```
#> # A tibble: 5 × 4
#>              name sleep_total log_sleep_total log_brainwt
#>             <chr>       <dbl>           <dbl>       <dbl>
#> 1        Roe deer         3.0       0.4771213  -1.0078885
#> 2            Goat         5.3       0.7242759  -0.9393022
#> 3             Dog        10.1       1.0043214  -1.1549020
#> 4    Patas monkey        10.9       1.0374265  -0.9393022
#> 5 Giant armadillo        18.1       1.2576786  -1.0915150
```

This posterior prediction plot does reveal a shortcoming of our model, when
plotted in a different manner.


```r
last_plot() + 
  geom_hline(yintercept = log10(24), color = "grey50") + 
  geom_label(x = 0, y = log10(24), label = "24 hours")
```

<img src="/figs//2016-11-18-visualizing-uncertainty-rstanarm/posterior-predict-24-hours-1.png" title="Previous plot updated to include a line indicating 24 hours. Same of the 95% interval goes above the line." alt="Previous plot updated to include a line indicating 24 hours. Same of the 95% interval goes above the line." width="70%" style="display: block; margin: auto;" />

One faulty consequence of how our model was specified is that it predicts that
some mammals sleep more than 24 hours per day---oh, what a life to live 
:sleeping:.

***

In the post, I covered three different ways to plot the results of an RStanARM 
model, while demonstrating some of the key functions for working with RStanARM 
models. Time well spent, I think.

This week, I learned about the under-development (as of November 2016) R package
[bayesplot](https://github.com/stan-dev/bayesplot) by the Stan team. The README
package shows off a lot of different ways to visualize posterior samples from a
model. I'll be sure to demo it on this data-set once it goes live.


[^1]: That is, if we map the plot's color aesthetic to a categorical variable in the data, `stat_smooth()` will fit a separate model for each color/category. I figured this out when I tried to write my own function `stat_smooth_stan()` based on [ggplot2's extensions vignette](https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html) and noticed that RStanARM was printing out MCMC sampling information for each color/category of the data. 



[ggplot2-mammals]: https://vincentarelbundock.github.io/Rdatasets/doc/ggplot2/msleep.html
[r4ds-labels]: r4ds.had.co.nz/graphics-for-communication.html#annotations
[interval-interp]: https://stats.stackexchange.com/questions/2272/whats-the-difference-between-a-confidence-interval-and-a-credible-interval
