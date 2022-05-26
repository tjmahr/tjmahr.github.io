---
title: "Random effects and penalized splines are the same thing"
excerpt: Weighted wiggles and smoothed categories
tags:
  - r
  - bayesian
  - mixed effects
  - brms
  - mgcv
  - splines
  - stan
  - math
share: true
header:
  overlay_image: "assets/images/2021-02-clothesline.jpg"
  image_description: "A clothesline"
  overlay_filter: rgba(10, 10, 10, 0.5)
  caption: "Photo credit: [**Félix Prado**](https://unsplash.com/photos/nbKaLT4cmRM)"
---





For a long time, I've been curious about something. It is a truth I've
seen casually dropped in textbooks, package documentation, and tweets:
**random effects and penalized smoothing splines are the same thing**. 
It sounds so profound and enlightened. What does it mean? How are they
the same? What deep statistical *gnosis* was I missing out on?

I have spent months, off and on, trying to understand this equivalence.
I can't give you the full mathematical treatment, but I have the gist of
it and I can point you to the equations. In this post, I will try to 
highlight the connections between the two.

Here are the main takeaways:

  - Mixed effects models (a.k.a. hierarchical models or multilevel
    models) use partial pooling to strike a balance between a grand
    population mean (complete pooling) and individual group means (no
    pooling).
  - Smoothing splines work by penalizing model coefficients to reduce
    the model degrees of freedom.
  - You can use the computational machinery of one framework to estimate
    the other.


<blockquote class="twitter-tweet" data-conversation="none" data-lang="en" data-dnt="true" data-theme="light">
  <p lang="en" dir="ltr">Sadly, I feel like my career has peaked with the creation of this meme <a href="https://t.co/5ilRFonsy7">pic.twitter.com/5ilRFonsy7</a></p>

  <img src="/assets/images/spider-smooth.jpg" alt="Spiderman (Penalized smooths) pointing at (and being pointed at) by Spiderman (Random effects)" />
  <br/>
  &mdash; Eric Pedersen (@ericJpedersen) <a href="https://twitter.com/ericJpedersen/status/1293508069016637440?ref_src=twsrc%5Etfw">August 12, 2020</a>
</blockquote> 




## Mixed model review

Let's review what these things means. Mixed effects models,
[apparently](/another-mixed-effects-model-visualization/) the [main
focus](/plotting-partial-pooling-in-mixed-effects-models/) of [this
blog](/iccbot-comes-online/) over the years, are used to estimate
"random" or "varying" effects. Here is the classic equation set up:

$$
\mathbf{y} = \mathbf{X\beta} + \mathbf{Zb} + \mathbf{\epsilon} \\
\mathbf{b} \sim \textsf{Normal}(0, \sigma_b) \\
\mathbf{\epsilon} \sim \textsf{Normal}(0, \sigma_y) \\
\mathbf{X}: \textrm{fixed effects model matrix} \\
\mathbf{Z}: \textrm{random effects model matrix} \\
\sigma_b, \sigma_y : \textrm{variance components} \\
\sigma_b : \textrm{where the magic happens} \\
$$

The magic here is the *σ*<sub>*b*</sub>, as it ties all of the
individual effects in **b** under a common distribution. If
*σ*<sub>*b*</sub> were replaced with a fixed number like 10, then all
of the effects in **b** would be independent and unaware of each other:
There would be *no pooling* of information between the groups. If we
remove it from the model---replace *σ*<sub>*b*</sub> with 0, so to
speak---then all the group variability is ignored, and there is
*complete pooling* of information into a single mean effect. With 
*σ*<sub>*b*</sub> all the groups can contribute information about the 
distribution of plausible effects, and so, there can be 
*partial pooling* of information between groups.

Consider the [`radon` dataset][radon] example from [Gelman and Hill
(2007)][arm-book]. Radon measurements were taken in Minnesota
counties. We would like to estimate the average radon measurement for
each county. We have a repeated measures situation, and some counties
have more observations than others. We use a Bayesian mixed effects
model with [brms](https://github.com/paul-buerkner/brms) to estimate a
population distribution of county estimates, and the county-level
estimates are randomly varying effects. They are drawn from a random
distribution, the scale of which we estimate from the data.



```r
library(tidyverse)
theme_set(theme_grey(base_size = 14))
library(brms)
radon <- rstanarm::radon

b_radon <- brm(
  log_radon ~ 1 + (1 | county), 
  radon, 
  family = gaussian, 
  file = "_caches/2021-02-26-radon", 
  backend = "cmdstanr"
)
b_radon
#>  Family: gaussian 
#>   Links: mu = identity; sigma = identity 
#> Formula: log_radon ~ 1 + (1 | county) 
#>    Data: radon (Number of observations: 919) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Group-Level Effects: 
#> ~county (Number of levels: 85) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.30      0.05     0.22     0.40 1.00     1516     2152
#> 
#> Population-Level Effects: 
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.35      0.05     1.25     1.44 1.00     3023     3004
#> 
#> Family Specific Parameters: 
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.77      0.02     0.73     0.80 1.00     6193     2831
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Here `sd(Intercept)` corresponds to *σ*<sub>*b*</sub>.

We can plot the observed county means alongside the model estimated
ones. First, I do some wrangling so that the difference between observed
means and estimated means are computed for use later on.


```r
radon_aug <- radon %>%
  # add ns and means
  group_by(county) %>% 
  mutate(
    observed_mean = mean(log_radon),
    county_n = n()
  ) %>% 
  ungroup() %>% 
  # add fitted values
  tidybayes::add_epred_draws(b_radon) %>% 
  mutate(
    observed_minus_model = observed_mean - .epred 
  ) %>% 
  # summarize fitted values
  ggdist::median_qi(.epred, observed_minus_model) 

radon_aug$type <- "mixed model estimates"
radon$type <- "observed means"

ggplot(radon_aug) + 
  aes(
    x = fct_rev(fct_infreq(county)), 
    y = log_radon, 
    color = type, 
    shape = type
  ) +
  stat_summary(data = radon, fun = mean, geom = "point") +
  geom_point(aes(y = .epred)) + 
  # Want to include y = 0 in the figure
  geom_blank(aes(y = 0)) +
  labs(
    x = "county (in increasing order by sample size)", 
    y = "log(radon)"
  ) +
  geom_hline(yintercept = fixef(b_radon)[1]) +
  scale_color_manual(values = c("blue", "grey40")) +
  labs(color = NULL, shape = NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "top", 
    legend.direction = "horizontal",
    legend.justification = "left",
  ) 
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/county-means-1.png" title="A plot showing log radon on the y axis and county on the x asis. There are two sets of overlapping points. There are the observed means in each country and the model estimated means. There is much less variability in the modeled means." alt="A plot showing log radon on the y axis and county on the x asis. There are two sets of overlapping points. There are the observed means in each country and the model estimated means. There is much less variability in the modeled means." width="80%" style="display: block; margin: auto;" />

We see a classic example of partial pooling. First note that model
estimates (blue circles) are less variable: None go above *y* = 2 and
only four go below *y* = 1. For counties with many observations (right
side), the estimated mean is hardly adjusted. There is less of a visual
gap between the observed mean and estimated mean. For counties with less
data (left side), the estimate is pulled towards the population mean
(`Intercept` in the summary above).

The following plot shows difference between the observed means and
the estimated means, subtracting the grey triangles from the blue squares
in the plot above.


```r
radon_aug %>% 
  ungroup() %>% 
  distinct(county, county_n, observed_minus_model) %>% 
  ggplot() + 
    aes(x = county_n, y = observed_minus_model) + 
    geom_point(alpha = .5) +
    labs(
      x = "Number of observations in county",
      y = "Observed mean - estimated mean"
    ) 
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/shrinkage-by-n-1.png" title="Plot with number of observations on the x axis and the difference between the observed and estimated means on the y axis. There is a smaller difference for counties with more data." alt="Plot with number of observations on the x axis and the difference between the observed and estimated means on the y axis. There is a smaller difference for counties with more data." width="66%" style="display: block; margin: auto;" />

The contention behind the *smooths = random effects* claim is that what we
just did is a case of *smoothing*. These random effects are, in a way, 
smoothed fixed effects.

> The function `random()` can be seen as a smoother for use with factors
> in `gamlss()`. It allows the fitted values for a factor predictor to
> be shrunk towards the overall mean [...]
> 
> — [GAMLSS documentation][gamlss-re] describing a random intercept as a smoother


## But what's smoothing?

Now let's walk through a generalized additive model in
[mgcv](https://cran.r-project.org/web/packages/mgcv/index.html) to
demonstrate a penalized smoothing spline. That was a mouth full, but
basically additive models are like the smoothing expansion pack for the
standard linear model. We're still doing regression, but we have some
new syntax and our models can do nonlinear relationships more easily
now.

I will walk through a basic example of how a spline's basis functions
are weighted to approximate a nonlinear trend, but this is not going to
be a full tutorial. Other people have made video introductions to
[additive models][gs-gam] or the [mgcv package][nr-gam]. I first
learned them from [a tutorial for linguists][ms-gam] and then from
[the mgcv textbook][mgcv-textbook], but there are [other resources
online][gam-resources].


We use the [`mcycle`][mcycle] dataset which gives the head
acceleration in a simulated motorcycle accident. We are going to fit a
model, plot the smooth from it, and then we are going to work through
what the model did.


```r
library(mgcv)

mcycle <- MASS::mcycle %>% 
  tibble::rowid_to_column()

# Fit the model
gam_20 <- gam(
  accel ~ 1 + s(times, bs = "cr", k = 20), 
  data = mcycle, 
  method = "REML"
)

mcycle$.fitted <- fitted(gam_20)

ggplot(mcycle) + 
  aes(x = times, y = accel) + 
  geom_point(alpha = .5) + 
  geom_line(aes(y = .fitted), color = "blue") + 
  labs(x = "time after impact [ms]", y = "acceleration [g]")
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/smooth-demo-1.png" title="Scattplot showing time on the x axis and acceleration on the y axis. The model fit is shown in blue. It makes two big turns down and then up." alt="Scattplot showing time on the x axis and acceleration on the y axis. The model fit is shown in blue. It makes two big turns down and then up." width="80%" style="display: block; margin: auto;" />

So what happened here? We will cover it visually.


### Splines are the sums of weighted wiggles

Let's look at the regression formula.


```r
formula(gam_20)
#> accel ~ 1 + s(times, bs = "cr", k = 20)
```

We told `gam()` to estimate `accel` using an intercept term and a smooth
term on the time predictor (`s(times, ...)`). Specifically, we created
our smooth using a cubic regression spline basis (`bs = "cr"`) with `k
= 20` - 1 curves. Our model is
estimating a function by adding up smaller components called *basis
functions*, and the space that defines those components is the *basis*.
These basis functions are weighted and summed together to produce a
smooth trend called a *spline*. The name *splines* is inspired by
drafting splines which are flexible strips of wood that can be weighted
and anchored in place to make a nice curve.


To reiterate, conceptually, we are decomposing the `times` predictor
into a bunch of individual wiggly lines (basis functions), and these are
weighted and summed together to approximate some nonlinear function. My
post on [orthogonal polynomials](/polypoly-package-released/)
illustrates the same principle but with polynomial basis functions.
Richard McElreath provides [a friendly 30-minute introduction
splines][rs-splines] in a Bayesian model in his Statistical
Rethinking course. One line I appreciate from his description is that
with splines, we replace a predictor variable, like `times`, with a set
of "synthetic" predictor variables.

{% include figure image_path="/assets/images/2021-02-spline.png" alt="An illustration of a drafting spline." caption="A drafting spline is a flexible strip of wood that is anchored at a few points so that one can create smooth curves. Illustration by [Pearson Scott Foresman](https://commons.wikimedia.org/wiki/File:Spline_(PSF).png#file)." %}{: style="max-width: 50%; display: block; margin: 2em auto;"}

An easy way to pull the wiggles is to use the model matrix. We have 20
columns for the intercept and 19 basis functions.


```r
model.matrix(gam_20) %>% 
  as_tibble() %>% 
  print(width = 72)
#> # A tibble: 133 × 20
#>    `(Intercept)` `s(times).1` `s(times).2` `s(times).3` `s(times).4`
#>            <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1             1      -0.143       -0.165      -0.154       -0.311  
#>  2             1      -0.0317      -0.193      -0.133       -0.293  
#>  3             1       0.292       -0.270      -0.0716      -0.242  
#>  4             1       0.492       -0.309      -0.0357      -0.208  
#>  5             1       0.669       -0.332      -0.00603     -0.175  
#>  6             1       0.933        0.0792     -0.0455      -0.0280 
#>  7             1       0.808        0.278      -0.0969      -0.0106 
#>  8             1       0.730        0.383      -0.122       -0.00387
#>  9             1       0.286        0.851      -0.182        0.00169
#> 10             1       0.123        0.957      -0.143       -0.0130 
#> # … with 123 more rows, and 15 more variables: `s(times).5` <dbl>,
#> #   `s(times).6` <dbl>, `s(times).7` <dbl>, `s(times).8` <dbl>,
#> #   `s(times).9` <dbl>, `s(times).10` <dbl>, `s(times).11` <dbl>,
#> #   `s(times).12` <dbl>, `s(times).13` <dbl>, `s(times).14` <dbl>,
#> #   `s(times).15` <dbl>, `s(times).16` <dbl>, `s(times).17` <dbl>,
#> #   `s(times).18` <dbl>, `s(times).19` <dbl>
```

To visualize the matrix, I am using a helper function from my personal R
package for plotting matrices in ggplot2. What we see is `times` on the
*x* axis and one line for the intercept and for each of the basis functions.


```r
# Helper function to plot the lines of a matrix
ggmatplot <- tjmisc::ggmatplot

# Helper function to label on a theme_grey() plot
annotate_grey <- tjmisc::annotate_label_grey

ggmatplot(cbind(mcycle$times, model.matrix(gam_20)), x_axis_column = 1) +
  annotate_grey("intercept", 0, 1.02, size = 5) +
  annotate_grey("individual\nbasis\nfunctions", 0, .16, size = 5) + 
  expand_limits(y = 1.2)  + 
  labs(x = "time [ms]", title = NULL)
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/matplot1-1.png" title="Plot of lines of the model matrix. There are several spiky lines peaking out from y = 0. These are labeled individual basis functions. There is a horizontal line at y = 1. This is labeled interceot." alt="Plot of lines of the model matrix. There are several spiky lines peaking out from y = 0. These are labeled individual basis functions. There is a horizontal line at y = 1. This is labeled interceot." width="80%" style="display: block; margin: auto;" />

Now we can weight these by multiplying by the model coefficients. Here
we use the `diag(coef())` trick to prevent the weighted predictors from
being summed together.


```r
weighted_coefs <- model.matrix(gam_20) %*% diag(coef(gam_20))

ggmatplot(cbind(mcycle$times, weighted_coefs), x_axis_column = 1) +
  annotate_grey("weighted intercept", 35, -40, size = 5) +
  annotate_grey("weighted basis functions", 0, 26, size = 5) +
  labs(x = "time [ms]", title = NULL)
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/matplot2-1.png" title="The spike from the above plot have been weight and there are two big rises and falls that match the shape of the data." alt="The spike from the above plot have been weight and there are two big rises and falls that match the shape of the data." width="80%" style="display: block; margin: auto;" />

We can see the two main inflections points in the dataset now. The basis
functions around 20 ms and 30 ms become very active in order to push the
spline away from 0 at those times.

If we sum the lines together, we get the regression line (the intercept
plus the smoothing spline).


```r
ggmatplot(
  cbind(mcycle$times, weighted_coefs), 
  x_axis_column = 1, 
  n_colors = 1
) + 
  stat_summary(
    aes(group = 1), 
    color = "maroon", 
    fun = sum, 
    geom = "line", 
    size = 1.5
  ) +
  annotate_grey("sum", 10, -70, size = 5, color = "maroon") +
  labs(x = "time [ms]", title = NULL)
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/matplot3-1.png" title="The individual lines from above have been summed together and their sum is highlighted and labelled sum." alt="The individual lines from above have been summed together and their sum is highlighted and labelled sum." width="80%" style="display: block; margin: auto;" />

Our plots so far demonstrate regression with basis functions, but smoothing
splines go one step further: They penalize wiggliness to prevent
overfitting. The idea is as follows: For the above demonstration, we
chose a 20-dimension spline basis (19 curves because 1 is removed for
identifiability, apparently). But where did that
number 20 come from? Thin air. What if we specified a dimension of 50? That's 50
predictors (1 intercept and 49 basis functions). Isn't it really easy to
overfit the data with this approach?

Well, let's look at a 50-dimension version.


```r
gam_50 <- gam(
  accel ~ s(times, bs = "cr", k = 50),
  data = mcycle, 
  method = "REML"
)

mcycle$.fitted50 <- fitted.values(gam_50)

ggplot(mcycle) + 
  aes(x = times, y = accel) + 
  geom_point(alpha = .5) +
  geom_line(aes(y = .fitted, color = "20"), size = 1) +
  geom_line(aes(y = .fitted50, color = "50"), size = 1) + 
  labs(
    x = "time after impact [ms]", 
    y = "acceleration [g]", 
    color = "Dimension"
  )
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/gam-comparison-1.png" title="Plot of the data with two smooths on it. They look nearly identical." alt="Plot of the data with two smooths on it. They look nearly identical." width="80%" style="display: block; margin: auto;" />

Huh, they hardly look any different. There is *no* overfitting. What's
going on? I already said it. **Wiggliness is being penalized.**


### Fit versus smoothness

Behind the scenes, the model is trying to balance two competing goals.
On the one hand we want to maximize the fit to the data. In linear
regression, this goal amounts to minimizing the sum of squared errors.
On the other hand, we want to minimize wiggliness (overfitting). In
penalized smoothing splines, this is done by first specifying a penalty
matrix that defines *wiggliness* for that spline basis. These two
features are pitted against each other in the following equation: 

$$
\begin{align*}
   \mathbf{\hat{β}} &= \operatorname{arg min}_\mathbf{β}\ \|\mathbf{y} − \mathbf{Xβ}\|^2 + \lambda\mathbf{β}^\intercal\mathbf{Sβ}  \\
   &\ \text{(want }\mathbf{\hat{β}} \text{ that minimizes fitting error and wiggliness)}\\
   \|\mathbf{y} − \mathbf{Xβ}\|^2 &: \text{sum of squared errors (minimize error to improve fit)} \\
   \lambda\beta^\intercal\mathbf{Sβ} &: \text{wiggliness penalty} \\
   \mathbf{Xβ} &: \text{spline basis times weights} \\
\mathbf{S} &: \text{penalty matrix (defines wiggliness for the spline)} \\
   \lambda &: \text{smoothness parameter (increase to make penalty stronger)} \\

\end{align*}
$$

Don't worry about the exact mathematics here: Just appreciate that error
is now paired with wiggliness, and wiggliness is controlled by a penalty
matrix **S** and a smoothness parameter *λ*. And yes, *wiggliness* is
the technical term. I based the equations from [Simon Wood's
slides][sw-slides], which use the phrase "fit-wiggliness tradeoff".

For our purposes, we won't worry too much about the penalty matrix. I'll
briefly describe it. For this model, wiggliness is defined by using the
second derivative of the estimated spline. The first derivative measures
the slope/steepness of the curve along *x*, and the second
derivatives measures how much the slope/steepness changes with *x*.
Thus, wiggliness is the change in slope, and the penalty matrix provides
penalties for each of the model coefficients related to this wiggliness.
The excellent [gratia][gratia] will plot the penalty matrix as a
heat map. *x* and *y* represent model coefficients (weights for the
basis functions), so along the main diagonal we see a penalty applied to
each coefficient. In the two off-diagonals, we see neighboring basis
functions have their weights jointly unpenalized or penalized.   


```r
gam_20 %>% 
  gratia::penalty() %>% 
  gratia::draw()
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/penalty-1.png" title="The heatmap described in the prose." alt="The heatmap described in the prose." width="80%" style="display: block; margin: auto;" />

The stiffest penalties are meted out to the 5th and 6th basis
functions, I think because these two basis functions cover the most rows
in the dataset, but I'm not 100% confident in that explanation.

To see the overfitting in action, we can disable the smoothing penalty
in the above spline-comparison plot by used fixed (`fx = TRUE`) regression
splines. Now, the model's main goal is to minimize the error, and
the 50-dimension spline basis gives the model many, many degrees of freedom.


```r
gam_20_fx <- gam(
  accel ~ s(times, bs = "cr", k = 20, fx = TRUE),
  data = mcycle, 
  method = "REML"
)

gam_50_fx <- gam(
  accel ~ s(times, bs = "cr", k = 50, fx = TRUE),
  data = mcycle, 
  method = "REML"
)

mcycle$.fitted20_fx <- fitted.values(gam_20_fx)
mcycle$.fitted50_fx <- fitted.values(gam_50_fx)

ggplot(mcycle) + 
  aes(x = times, y = accel) + 
  geom_point(alpha = .5) + 
  geom_line(aes(y = .fitted20_fx, color = "20"), size = 1) +
  geom_line(aes(y = .fitted50_fx, color = "50"), size = 1) + 
  labs(
    x = "time after impact [ms]", 
    y = "acceleration [g]", 
    color = "Dimension"
  )
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/fx-plot-1.png" title="The dataset plotted with two unpenalized smoothing splines. The 50-dimension spline is very wiggly." alt="The dataset plotted with two unpenalized smoothing splines. The 50-dimension spline is very wiggly." width="80%" style="display: block; margin: auto;" />

Thus, when we disable the penalty, the 50-dimension splines is free to
wiggle all over the place.


### How much smoothing happened: Effective degrees of freedom

The smoothing parameter, *λ*, is a hyperparameter. It controls the
spline coefficients (basis function weights), and it is estimated from
the data. We can set the *λ* manually and crank it up way, way up. In this case, the
model tries to find the least wiggly curve that decreases modeling
error: A straight line.


```r
gam_20_sp <- gam(
  accel ~ s(times, bs = "cr", k = 20, sp = 10000000),
  data = mcycle, 
  method = "REML"
)

mcycle$.fitted20_sp <- fitted.values(gam_20_sp)

ggplot(mcycle) + 
  aes(x = times, y = accel) + 
  geom_point(alpha = .5) +
  geom_line(aes(y = .fitted, color = "estimated"), size = 1) +
  geom_line(aes(y = .fitted20_fx, color = "no smoothing"), size = 1) +
  geom_line(aes(y = .fitted20_sp, color = "10,000,000"), size = 1) + 
  labs(
    x = "time after impact [ms]", 
    y = "acceleration [g]", 
    color = expression(lambda)
  ) 
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/lambda-plot-1.png" title="The data plotted with three smooths. One of them is a completely flat line. It has a penalty of 10000000." alt="The data plotted with three smooths. One of them is a completely flat line. It has a penalty of 10000000." width="80%" style="display: block; margin: auto;" />

We need some way to talk about how much smoothing took place. On the one
hand, we might treat each basis function as an independent predictor
that uses up a full degree of freedom in fitting the curve. On the other
hand, we might penalize the basis function weights so much that they
produce a straight line, and thus, the batch of predictors effectively
acts just like a single predictor variable would. That is, they are
effectively estimating a curve that has just 1-degree-of-freedom's worth
of action in it. And indeed this is how mgcv describes the smoothness of
the models: It reports the *effective (or estimated) degrees of freedom*
(EDFs) behind each smooth.

If we look at the model summary, we see that our 20-dimension basis
smooth has an EDF of 11.78 (see `edf` under `Approximate significance of
smooth terms`).


```r
summary(gam_20)
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> accel ~ 1 + s(times, bs = "cr", k = 20)
#> 
#> Parametric coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  -25.546      1.956  -13.06   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>            edf Ref.df     F p-value    
#> s(times) 11.78  13.97 33.93  <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> R-sq.(adj) =  0.782   Deviance explained = 80.1%
#> -REML = 616.01  Scale est. = 509.01    n = 133
```

Similarly, our 50-dimension basis smooth has only 12.81 effective
degrees of freedom, but the unpenalized version uses all 49 of its basis
function curves and uses 49 degrees of freedom.


```r
gam_50
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> accel ~ s(times, bs = "cr", k = 50)
#> 
#> Estimated degrees of freedom:
#> 12.8  total = 13.81 
#> 
#> REML score: 616.068

gam_50_fx
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> accel ~ s(times, bs = "cr", k = 50, fx = TRUE)
#> 
#> Estimated degrees of freedom:
#> 49  total = 50 
#> 
#> REML score: 403.4616
```



## The big trick: Turn λ into a random effect variance in a mixed model

Okay, so far, here's what we have:

  - A spline decomposes a predictor into a number of wiggly basis
    functions.
  - A penalized spline adds a penalty term to the model to reduce
    wiggliness.
  - This penalty shrinks model coefficients so that they use a smaller number
    of degrees of freedom used by the model.
  - The amount of smoothing is controlled by a hyperparameter *λ*.

We saw another hyperparameter earlier on in this post whose job was to
pull individual parameter estimates closer to 0: *σ*<sub>b</sub>. Both
of these hyperparameters are estimated from the data and perform
shrinkage on a batch of related coefficients (random effects or basis
function weights). 

So, here's the big thing... If you do a bunch of linear algebra (as in
[slide 7 here][smoothness-slides] or the Appendix in
[Wood, 2004][wood-2004]), you can express the smooth as a mixed
model:



$$
\begin{align*}
\mathbf{y} &= \mathbf{X}\mathbf{\beta} + \mathbf{Zb} + \mathbf{\epsilon} \\
\mathbf{b} &\sim \textsf{Normal}(0, \sigma/\lambda) \\
\mathbf{\epsilon} &\sim \textsf{Normal}(0,\sigma) \\
\mathbf{X}, \mathbf{Z} &: \textrm{matrices from transforming spline and penalty matrices} \\
\mathbf{X} &: \textrm{unpenalized (fixed) effect model matrix} \\
\mathbf{Z} &: \textrm{penalized (random) effects model matrix} \\
\lambda &: \textrm{smoothness parameter} \\
\sigma &: \textrm{residual variance component} \\
\end{align*}
$$

And right there, on the second line, we see the mixed effects magic
again: *σ*/*λ* = *σ*<sub>*b*</sub>: Model coefficients are related under
a common distribution so that they can share information with each
other. We can smuggle penalized smooths into the mixed effects
framework.




<blockquote class="twitter-tweet" data-conversation="none" data-dnt="true"><p lang="en" dir="ltr">random effects and splines are _the same_ thing. See also <a href="https://t.co/LgZTzZimH0">https://t.co/LgZTzZimH0</a></p>&mdash; DavidLawrenceMiller (@millerdl) <a href="https://twitter.com/millerdl/status/846719376338407424?ref_src=twsrc%5Etfw">March 28, 2017</a></blockquote> 



### So... you can turn splines into mixed models?



Yes. What this means is that we can use nlme or
[lme4](https://cran.r-project.org/web/packages/lme4/index.html) to
estimate a smooth as a mixed effects model. mgcv provides this feature
in its aptly named [`smooth2random()`][smooth2random] function.
Let's walk through what happens to our spline basis along the way.

First, let's set up our cubic regression spline basis, manually outside
of a regression formula. 


```r
basis_cr <- smoothCon(
  s(times, k = 20, bs = "cr"), 
  data = mcycle, 
  absorb.cons = TRUE
)

p1 <- ggmatplot(basis_cr[[1]]["X"][[1]]) + 
  labs(title = "original spline basis") 

p2 <- gratia::penalty(basis_cr[[1]]) %>%
  gratia::draw() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = .5)
  ) + 
  guides(fill = "none") +
  labs(title = "penalty matrix")

# for combining plots together
library(patchwork)

p1 + p2 
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/mixed-matrix-raw-1.png" title="Left: A matrix plot with one line per column. This looks like the bases plotted above, with 1 line per column and they form a nice series of spiky lines. Right: The penalty matrix shown earlier." alt="Left: A matrix plot with one line per column. This looks like the bases plotted above, with 1 line per column and they form a nice series of spiky lines. Right: The penalty matrix shown earlier." width="100%" style="display: block; margin: auto;" />

But we are going to reparameterize model so that the penalty matrix is
just a diagonal of 1s using `diagonal.penalty = TRUE`. Now, our basis is
no longer a row of pointy curves. 


```r
# Construct a smoothing basis outside of a model
basis_cr_diag <- smoothCon(
  s(times, k = 20, bs = "cr"), 
  data = mcycle, 
  absorb.cons = TRUE, 
  diagonal.penalty = TRUE
)

p1b <- ggmatplot(basis_cr_diag[[1]]["X"][[1]]) + 
  labs(title = "natural parameterization")

p2b <- gratia::penalty(basis_cr_diag[[1]]) %>%
  gratia::draw() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = .5)
  ) + 
  guides(fill = "none") +
  labs(title = "penalty matrix")

p1b + p2b 
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/mixed-matrix-diag-1.png" title="Left: A matrix plot with one line per column. Unlike the other ones, the lines here are not nice and bumpy. Right: The penalty matrix. It's just a diagonal of 1s except for the last element on the diagonal." alt="Left: A matrix plot with one line per column. Unlike the other ones, the lines here are not nice and bumpy. Right: The penalty matrix. It's just a diagonal of 1s except for the last element on the diagonal." width="100%" style="display: block; margin: auto;" />

If my reading of Wood's textbook is correct, this is the "natural"
parameterization of a single-variable smoothing spline. (The source code
for `smoothCon()` in mgcv also calls an internal function called
`nat.param()` so I think I am the right track here.) 

Take a look at F19 x F19 in the above plot. It snuck past me at first,
but this basis function is not penalized. In a mixed effect model, an
unpenalized parameter is a fixed effect. Thus, when `smooth2random()`
creates a one-column fixed effects matrix and stores the remaining curves
in a random effects matrix, we find that the unpenalized column is the
fixed effects column.


```r
re <- smooth2random(basis_cr_diag[[1]], "", type = 2)

# Confirm that the fixed effect column and the 
# unpenalized column are the same
unpenalized_column <- basis_cr_diag[[1]]["X"][[1]][, 19, drop = FALSE]
all(unpenalized_column == re$Xf)
#> [1] TRUE

p1c <- ggmatplot(re$Xf) + 
  labs(title = "fixed effects matrix") +
  expand_limits(y = c(-30, 80))

p2c <- ggmatplot(re$rand$Xr) + 
  labs(title = "random effects matrix") +
  expand_limits(y = c(-30, 80))

p1c + p2c
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/mixed-matrix-1.png" title="Left: A single rising curve plotted as the fixed effects matrix. Right: 18 wiggly curves from the previous figure plotted as the random effects matrix." alt="Left: A single rising curve plotted as the fixed effects matrix. Right: 18 wiggly curves from the previous figure plotted as the random effects matrix." width="100%" style="display: block; margin: auto;" />

I still don't understand the transformation (that takes place from the
original basis matrix) very well myself, and I probably won't be
converting smoothing bases into mixed effects model matrices and fitting
them with lme4 anytime soon.
[gamm4](https://cran.r-project.org/web/packages/gamm4/index.html) fits
its smooths with lme4, but by the looks of it, it's [not simply a
matter of calling of `lmer()`][gamm4-code]. Still, this mixed model
reparameterization is useful to know about because of the following
fact.


### This point seems obscure, but it is what brms uses!

What happens when we fit a smooth in brms? We are fitting a mixed
effects model.


```r
b_gam_20 <- brm(
  accel ~ s(times, bs = "cr", k = 20),
  data = mcycle, 
  family = gaussian,
  file = "_caches/2021-02-26-mcycle20", 
  backend = "cmdstanr"
)
summary(b_gam_20)
#>  Family: gaussian 
#>   Links: mu = identity; sigma = identity 
#> Formula: accel ~ s(times, bs = "cr", k = 20) 
#>    Data: mcycle (Number of observations: 133) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Smooth Terms: 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(stimes_1)     4.95      1.23     3.16     7.86 1.00      732     1457
#> 
#> Population-Level Effects: 
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept   -25.50      1.95   -29.30   -21.60 1.00     6191     2871
#> stimes_1      1.64      0.34     0.96     2.31 1.00     4455     2834
#> 
#> Family Specific Parameters: 
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma    22.76      1.48    20.03    25.86 1.00     5487     3015
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

You see `sds(times_1)`? That's the variance of smooth weights. You see
`stimes_1`? That the single fixed effects term (`re$Xf`) in the code
above. I'm pretty sure about this because I learned `smooth2random()`
from studying the brms source code.

Indeed, if we look at the actual Stan code used to fit the model, we see
a mixed effects model. **I suggest you only skim this code.**


```r
b_gam_20$model
```

```
// generated with brms 2.17.0
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  // data for splines
  int Ks; // number of linear effects
  matrix[N, Ks] Xs; // design matrix for the linear effects
  // data for spline s(times, bs = "cr", k = 20)
  int nb_1; // number of bases
  array[nb_1] int knots_1; // number of knots
  // basis function matrices
  matrix[N, knots_1[1]] Zs_1_1;
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  
}
parameters {
  real Intercept; // temporary intercept for centered predictors
  vector[Ks] bs; // spline coefficients
  // parameters for spline s(times, bs = "cr", k = 20)
  // standarized spline coefficients
  vector[knots_1[1]] zs_1_1;
  real<lower=0> sds_1_1; // standard deviations of spline coefficients
  real<lower=0> sigma; // dispersion parameter
}
transformed parameters {
  // actual spline coefficients
  vector[knots_1[1]] s_1_1;
  real lprior = 0; // prior contributions to the log posterior
  // compute actual spline coefficients
  s_1_1 = sds_1_1 * zs_1_1;
  lprior += student_t_lpdf(Intercept | 3, -13.3, 35.6);
  lprior += student_t_lpdf(sds_1_1 | 3, 0, 35.6)
            - 1 * student_t_lccdf(0 | 3, 0, 35.6);
  lprior += student_t_lpdf(sigma | 3, 0, 35.6)
            - 1 * student_t_lccdf(0 | 3, 0, 35.6);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + rep_vector(0.0, N) + Xs * bs + Zs_1_1 * s_1_1;
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zs_1_1);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}

```

Okay, that's a lot. But let me highlight and translate the key part of it.

$$
\begin{align*}
  
   \mathbf{S} &: \texttt{s_1_1} & \texttt{// actual spline coefficients}\\
   \mathbf{\sigma} &:  \texttt{sds_1_1} & \texttt{// standard deviations of spline coefficients}\\
   \mathbf{Z} &:  \texttt{zs_1_1} & \texttt{// standardized spline coefficients}\\
   \mathbf{S} &= \sigma \mathbf{Z} & \texttt{s_1_1 = sds_1_1 * zs_1_1}; \\
   \mathbf{Z} &\sim \textsf{Normal}(0, 1) & \texttt{target += std_normal_lpdf(zs_1_1);}\\
   \mathbf{\sigma} &\sim \textsf{StudentT}(3, 0, 35.6) & \texttt{lprior += student_t_lpdf(sds_1_1 | 3, 0, 35.6) ...}\\

\end{align*}
$$

The spline coefficients start as *z* scores, drawn from normal
distribution with mean 0 and a standard deviation of 1. They are then
scaled by *σ*, a scaling factor. This scaling factor acts as the
hyperparameter that is learned from the data. This formulation of a
mixed model is call the *noncentered parameterization*. It's one of
those topics you eventually run into [in the Stanverse][stan-ncp].
Richard McElreath does a [friendly tour of it][rm-mmm] as a matter
of algebra. For a deeper dive, Michael Betancourt [covers the
topic][mb-ncp] in the context of debugging degenerate posteriors.

We can see completely analogous code in the Bayesian radon model above.
I'll just show the relevant part:



```r
b_radon$model
```

```
...
parameters {
  ...
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  array[M_1] vector[N_1] z_1; // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1; // actual group-level effects
  r_1_1 = sd_1[1] * z_1[1];
  real lprior = 0; // prior contributions to the log posterior
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
          - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  ...
  target += std_normal_lpdf(z_1[1]);
}
```







## Simple random effects are category smoothers

One consequence of this relationship is that you can walk this relation
backwards: You can fit a simple random effects using a basis matrix and
penalty matrix. Indeed, mgcv provides a [random effects (`"re"`)
smoother][mgcv-re] basis so we can estimate our mixed model from
above using a smooth.


```r
gam_radon <- gam(
  log_radon ~ 1 + s(county, bs = "re"), 
  data = radon,
  method = "REML"
)
```

In this case, the basis matrix is just a single indicator variable for
county.


```r
ggmatplot(model.matrix(gam_radon)[, -1])
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/unnamed-chunk-13-1.png" title="A series of 85 spikes." alt="A series of 85 spikes." width="80%" style="display: block; margin: auto;" />

(Yikes, that one stings the eyes!)

The penalty matrix is a diagonal as each county effect is
equally penalized.


```r
gam_radon %>% 
  gratia::penalty() %>% 
  gratia::draw() + 
  theme(axis.text = element_blank())
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/unnamed-chunk-14-1.png" title="A penalty matrix but only the diagonal is active." alt="A penalty matrix but only the diagonal is active." width="80%" style="display: block; margin: auto;" />


mgcv knows about how *σ*/*λ* = *σ*<sub>b</sub> and it will provide the
random effect variance estimate for us with [`gam.vcomp()`][gam-vcomp] (variance
components):


```r
gam.vcomp(gam_radon) 
#> 
#> Standard deviations and 0.95 confidence intervals:
#> 
#>             std.dev     lower     upper
#> s(county) 0.2976671 0.2202676 0.4022639
#> scale     0.7660696 0.7304109 0.8034692
#> 
#> Rank: 2/2
```

Which closely matches the estimates from the Bayesian model above:


```r
b_radon %>% 
  posterior_summary(
    variable = c("sd_county__Intercept", "sigma")
  ) %>% 
  round(3)
#>                      Estimate Est.Error  Q2.5 Q97.5
#> sd_county__Intercept    0.302     0.047 0.216 0.399
#> sigma                   0.767     0.018 0.733 0.804
```

What I think is the coolest feature of random intercepts as
smooths is what the effective degrees of freedom tells us:


```r
nlevels(radon$county)
#> [1] 85
summary(gam_radon)
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> log_radon ~ 1 + s(county, bs = "re")
#> 
#> Parametric coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)    1.350      0.047   28.72   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>             edf Ref.df     F p-value    
#> s(county) 39.57     84 1.573  <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> R-sq.(adj) =  0.126   Deviance explained = 16.4%
#> -REML = 1092.4  Scale est. = 0.58686   n = 919
```

The data have 85 counties, but there is effectively 39.5 counties worth
of parameters here. For what it's worth, we find a similar number when
we ask brms to use [the LOO method][loo] to estimate the effective number of
parameters from its model. 


```r
loo(b_radon)
#> 
#> Computed from 4000 by 919 log-likelihood matrix
#> 
#>          Estimate   SE
#> elpd_loo  -1084.0 28.8
#> p_loo        44.5  4.1
#> looic      2167.9 57.5
#> ------
#> Monte Carlo SE of elpd_loo is 0.1.
#> 
#> Pareto k diagnostic values:
#>                          Count Pct.    Min. n_eff
#> (-Inf, 0.5]   (good)     916   99.7%   702       
#>  (0.5, 0.7]   (ok)         3    0.3%   198       
#>    (0.7, 1]   (bad)        0    0.0%   <NA>      
#>    (1, Inf)   (very bad)   0    0.0%   <NA>      
#> 
#> All Pareto k estimates are ok (k < 0.7).
#> See help('pareto-k-diagnostic') for details.
```

Here `p_loo` is the effective number of parameters from a leave-one-out
cross-validation method. It's around 45. Our mgcv model has around 41.6
parameters (39.6 from the smooth plus intercept and sigma). These two
parameter estimating methods are <strike>unrelated (as far as I
know)</strike> related ([thanks Avi Vehtari][avi-tweet]), and they
both seem to telling us something similar about how much information in
`county` variable we have after partially pooling down the model
parameters.

For a deeper dive on random effects, Gavin Simpson recently wrote about
[how to use mgcv for random effects][gs-re], so I encourage readers
to look at that post. 


## Okay, why are you telling me this?

The purpose of this post was to demystify the connection between
penalized smooths and mixed effects models. Formally, it all boils down
to a hyperparameter that penalizes model coefficients (or pools
information) so that the model uses a smaller number of effective
parameters. This connection enriches my understanding of the two kinds
of models. A by-county random intercept is a category smoother. We can
estimate a smooth by putting a prior on the variance of the basis
function weights (provided that we incorporate the penalty matrix
somehow). It's wonderful.

Random effects are funny things. We are first taught them in a repeated
measures framework as a way to handle a problem in the data
(non-independence in clustered units). But they are so much more. To use
a Richard McElreath phrase, they provide an "adaptive regularizing
prior": Something that constrains our estimates to reduce overfitting
but is actively learned from the data. Hodge and Clayton (2011) outline
this problem between repeated-measures random effects and pooling/smoothing
random effects in their paper [Random Effects Old and
New][old-and-new], and I think it's worth remembering this
distinction when we use the phrase "random effects".






***

*Last knitted on 2022-05-26. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2021-02-26-random-effects-penalized-splines-same-thing.Rmd).*[^si] 

[^si]: 
    
    ```r
    .session_info
    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting         value
    #>  version         R version 4.2.0 (2022-04-22 ucrt)
    #>  os              Windows 10 x64 (build 22000)
    #>  system          x86_64, mingw32
    #>  ui              RTerm
    #>  language        (EN)
    #>  collate         English_United States.utf8
    #>  ctype           English_United States.utf8
    #>  tz              America/Chicago
    #>  date            2022-05-26
    #>  pandoc          NA
    #>  stan (rstan)    2.21.0
    #>  stan (cmdstanr) 2.29.2
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package        * version    date (UTC) lib source
    #>    abind            1.4-5      2016-07-21 [1] CRAN (R 4.2.0)
    #>    arrayhelpers     1.1-0      2020-02-04 [1] CRAN (R 4.2.0)
    #>    assertthat       0.2.1      2019-03-21 [1] CRAN (R 4.2.0)
    #>    backports        1.4.1      2021-12-13 [1] CRAN (R 4.2.0)
    #>    base64enc        0.1-3      2015-07-28 [1] CRAN (R 4.2.0)
    #>    bayesplot        1.9.0      2022-03-10 [1] CRAN (R 4.2.0)
    #>    boot             1.3-28     2021-05-03 [2] CRAN (R 4.2.0)
    #>    bridgesampling   1.1-2      2021-04-16 [1] CRAN (R 4.2.0)
    #>    brms           * 2.17.0     2022-04-13 [1] CRAN (R 4.2.0)
    #>    Brobdingnag      1.2-7      2022-02-03 [1] CRAN (R 4.2.0)
    #>    broom            0.8.0      2022-04-13 [1] CRAN (R 4.2.0)
    #>    callr            3.7.0      2021-04-20 [1] CRAN (R 4.2.0)
    #>    cellranger       1.1.0      2016-07-27 [1] CRAN (R 4.2.0)
    #>    checkmate        2.1.0      2022-04-21 [1] CRAN (R 4.2.0)
    #>    cli              3.3.0      2022-04-25 [1] CRAN (R 4.2.0)
    #>    cmdstanr         0.5.1.9000 2022-04-18 [1] Github (stan-dev/cmdstanr@cc261b9)
    #>    coda             0.19-4     2020-09-30 [1] CRAN (R 4.2.0)
    #>    codetools        0.2-18     2020-11-04 [2] CRAN (R 4.2.0)
    #>    colorspace       2.0-3      2022-02-21 [1] CRAN (R 4.2.0)
    #>    colourpicker     1.1.1      2021-10-04 [1] CRAN (R 4.2.0)
    #>    crayon           1.5.1      2022-03-26 [1] CRAN (R 4.2.0)
    #>    crosstalk        1.2.0      2021-11-04 [1] CRAN (R 4.2.0)
    #>    DBI              1.1.2      2021-12-20 [1] CRAN (R 4.2.0)
    #>    dbplyr           2.1.1      2021-04-06 [1] CRAN (R 4.2.0)
    #>    digest           0.6.29     2021-12-01 [1] CRAN (R 4.2.0)
    #>    distributional   0.3.0      2022-01-05 [1] CRAN (R 4.2.0)
    #>    dplyr          * 1.0.9      2022-04-28 [1] CRAN (R 4.2.0)
    #>    DT               0.23       2022-05-10 [1] CRAN (R 4.2.0)
    #>    dygraphs         1.1.1.6    2018-07-11 [1] CRAN (R 4.2.0)
    #>    ellipsis         0.3.2      2021-04-29 [1] CRAN (R 4.2.0)
    #>    emmeans          1.7.4-1    2022-05-15 [1] CRAN (R 4.2.0)
    #>    estimability     1.3        2018-02-11 [1] CRAN (R 4.2.0)
    #>    evaluate         0.15       2022-02-18 [1] CRAN (R 4.2.0)
    #>    fansi            1.0.3      2022-03-24 [1] CRAN (R 4.2.0)
    #>    farver           2.1.0      2021-02-28 [1] CRAN (R 4.2.0)
    #>    fastmap          1.1.0      2021-01-25 [1] CRAN (R 4.2.0)
    #>    forcats        * 0.5.1      2021-01-27 [1] CRAN (R 4.2.0)
    #>    fs               1.5.2      2021-12-08 [1] CRAN (R 4.2.0)
    #>    generics         0.1.2      2022-01-31 [1] CRAN (R 4.2.0)
    #>    ggdist           3.1.1      2022-02-27 [1] CRAN (R 4.2.0)
    #>    ggplot2        * 3.3.6      2022-05-03 [1] CRAN (R 4.2.0)
    #>    ggridges         0.5.3      2021-01-08 [1] CRAN (R 4.2.0)
    #>    git2r            0.30.1     2022-03-16 [1] CRAN (R 4.2.0)
    #>    glue             1.6.2      2022-02-24 [1] CRAN (R 4.2.0)
    #>    gratia           0.7.3      2022-05-09 [1] CRAN (R 4.2.0)
    #>    gridExtra        2.3        2017-09-09 [1] CRAN (R 4.2.0)
    #>    gtable           0.3.0      2019-03-25 [1] CRAN (R 4.2.0)
    #>    gtools           3.9.2.1    2022-05-23 [1] CRAN (R 4.2.0)
    #>    haven            2.5.0      2022-04-15 [1] CRAN (R 4.2.0)
    #>    here             1.0.1      2020-12-13 [1] CRAN (R 4.2.0)
    #>    highr            0.9        2021-04-16 [1] CRAN (R 4.2.0)
    #>    hms              1.1.1      2021-09-26 [1] CRAN (R 4.2.0)
    #>    htmltools        0.5.2      2021-08-25 [1] CRAN (R 4.2.0)
    #>    htmlwidgets      1.5.4      2021-09-08 [1] CRAN (R 4.2.0)
    #>    httpuv           1.6.5      2022-01-05 [1] CRAN (R 4.2.0)
    #>    httr             1.4.3      2022-05-04 [1] CRAN (R 4.2.0)
    #>    igraph           1.3.1      2022-04-20 [1] CRAN (R 4.2.0)
    #>    inline           0.3.19     2021-05-31 [1] CRAN (R 4.2.0)
    #>    jsonlite         1.8.0      2022-02-22 [1] CRAN (R 4.2.0)
    #>    knitr          * 1.39       2022-04-26 [1] CRAN (R 4.2.0)
    #>    labeling         0.4.2      2020-10-20 [1] CRAN (R 4.2.0)
    #>    later            1.3.0      2021-08-18 [1] CRAN (R 4.2.0)
    #>    lattice          0.20-45    2021-09-22 [2] CRAN (R 4.2.0)
    #>    lifecycle        1.0.1      2021-09-24 [1] CRAN (R 4.2.0)
    #>    lme4             1.1-29     2022-04-07 [1] CRAN (R 4.2.0)
    #>    loo              2.5.1      2022-03-24 [1] CRAN (R 4.2.0)
    #>    lubridate        1.8.0      2021-10-07 [1] CRAN (R 4.2.0)
    #>    magrittr         2.0.3      2022-03-30 [1] CRAN (R 4.2.0)
    #>    markdown         1.1        2019-08-07 [1] CRAN (R 4.2.0)
    #>    MASS             7.3-56     2022-03-23 [2] CRAN (R 4.2.0)
    #>    Matrix           1.4-1      2022-03-23 [2] CRAN (R 4.2.0)
    #>    matrixStats      0.62.0     2022-04-19 [1] CRAN (R 4.2.0)
    #>    mgcv           * 1.8-40     2022-03-29 [2] CRAN (R 4.2.0)
    #>    mime             0.12       2021-09-28 [1] CRAN (R 4.2.0)
    #>    miniUI           0.1.1.1    2018-05-18 [1] CRAN (R 4.2.0)
    #>    minqa            1.2.4      2014-10-09 [1] CRAN (R 4.2.0)
    #>    modelr           0.1.8      2020-05-19 [1] CRAN (R 4.2.0)
    #>    multcomp         1.4-19     2022-04-26 [1] CRAN (R 4.2.0)
    #>    munsell          0.5.0      2018-06-12 [1] CRAN (R 4.2.0)
    #>    mvnfast          0.2.7      2021-05-20 [1] CRAN (R 4.2.0)
    #>    mvtnorm          1.1-3      2021-10-08 [1] CRAN (R 4.2.0)
    #>    nlme           * 3.1-157    2022-03-25 [2] CRAN (R 4.2.0)
    #>    nloptr           2.0.2      2022-05-19 [1] CRAN (R 4.2.0)
    #>    patchwork      * 1.1.1      2020-12-17 [1] CRAN (R 4.2.0)
    #>    pillar           1.7.0      2022-02-01 [1] CRAN (R 4.2.0)
    #>    pkgbuild         1.3.1      2021-12-20 [1] CRAN (R 4.2.0)
    #>    pkgconfig        2.0.3      2019-09-22 [1] CRAN (R 4.2.0)
    #>    plyr             1.8.7      2022-03-24 [1] CRAN (R 4.2.0)
    #>    posterior        1.2.1      2022-03-07 [1] CRAN (R 4.2.0)
    #>    prettyunits      1.1.1      2020-01-24 [1] CRAN (R 4.2.0)
    #>    processx         3.5.3      2022-03-25 [1] CRAN (R 4.2.0)
    #>    promises         1.2.0.1    2021-02-11 [1] CRAN (R 4.2.0)
    #>    ps               1.7.0      2022-04-23 [1] CRAN (R 4.2.0)
    #>    purrr          * 0.3.4      2020-04-17 [1] CRAN (R 4.2.0)
    #>    R6               2.5.1      2021-08-19 [1] CRAN (R 4.2.0)
    #>    ragg             1.2.2      2022-02-21 [1] CRAN (R 4.2.0)
    #>    Rcpp           * 1.0.8.3    2022-03-17 [1] CRAN (R 4.2.0)
    #>  D RcppParallel     5.1.5      2022-01-05 [1] CRAN (R 4.2.0)
    #>    readr          * 2.1.2      2022-01-30 [1] CRAN (R 4.2.0)
    #>    readxl           1.4.0      2022-03-28 [1] CRAN (R 4.2.0)
    #>    reprex           2.0.1      2021-08-05 [1] CRAN (R 4.2.0)
    #>    reshape2         1.4.4      2020-04-09 [1] CRAN (R 4.2.0)
    #>    rlang            1.0.2      2022-03-04 [1] CRAN (R 4.2.0)
    #>    rprojroot        2.0.3      2022-04-02 [1] CRAN (R 4.2.0)
    #>    rstan            2.21.5     2022-04-11 [1] CRAN (R 4.2.0)
    #>    rstanarm         2.21.3     2022-04-09 [1] CRAN (R 4.2.0)
    #>    rstantools       2.2.0      2022-04-08 [1] CRAN (R 4.2.0)
    #>    rstudioapi       0.13       2020-11-12 [1] CRAN (R 4.2.0)
    #>    rvest            1.0.2      2021-10-16 [1] CRAN (R 4.2.0)
    #>    sandwich         3.0-1      2021-05-18 [1] CRAN (R 4.2.0)
    #>    scales           1.2.0      2022-04-13 [1] CRAN (R 4.2.0)
    #>    sessioninfo      1.2.2      2021-12-06 [1] CRAN (R 4.2.0)
    #>    shiny            1.7.1      2021-10-02 [1] CRAN (R 4.2.0)
    #>    shinyjs          2.1.0      2021-12-23 [1] CRAN (R 4.2.0)
    #>    shinystan        2.6.0      2022-03-03 [1] CRAN (R 4.2.0)
    #>    shinythemes      1.2.0      2021-01-25 [1] CRAN (R 4.2.0)
    #>    StanHeaders      2.21.0-7   2020-12-17 [1] CRAN (R 4.2.0)
    #>    stringi          1.7.6      2021-11-29 [1] CRAN (R 4.2.0)
    #>    stringr        * 1.4.0      2019-02-10 [1] CRAN (R 4.2.0)
    #>    survival         3.3-1      2022-03-03 [2] CRAN (R 4.2.0)
    #>    svUnit           1.0.6      2021-04-19 [1] CRAN (R 4.2.0)
    #>    systemfonts      1.0.4      2022-02-11 [1] CRAN (R 4.2.0)
    #>    tensorA          0.36.2     2020-11-19 [1] CRAN (R 4.2.0)
    #>    textshaping      0.3.6      2021-10-13 [1] CRAN (R 4.2.0)
    #>    TH.data          1.1-1      2022-04-26 [1] CRAN (R 4.2.0)
    #>    threejs          0.3.3      2020-01-21 [1] CRAN (R 4.2.0)
    #>    tibble         * 3.1.7      2022-05-03 [1] CRAN (R 4.2.0)
    #>    tidybayes        3.0.2      2022-01-05 [1] CRAN (R 4.2.0)
    #>    tidyr          * 1.2.0      2022-02-01 [1] CRAN (R 4.2.0)
    #>    tidyselect       1.1.2      2022-02-21 [1] CRAN (R 4.2.0)
    #>    tidyverse      * 1.3.1      2021-04-15 [1] CRAN (R 4.2.0)
    #>    tjmisc           0.0.0.9000 2022-03-01 [1] Github (tjmahr/tjmisc@6724405)
    #>    tzdb             0.3.0      2022-03-28 [1] CRAN (R 4.2.0)
    #>    utf8             1.2.2      2021-07-24 [1] CRAN (R 4.2.0)
    #>    vctrs            0.4.1      2022-04-13 [1] CRAN (R 4.2.0)
    #>    withr            2.5.0      2022-03-03 [1] CRAN (R 4.2.0)
    #>    xfun             0.31       2022-05-10 [1] CRAN (R 4.2.0)
    #>    xml2             1.3.3      2021-11-30 [1] CRAN (R 4.2.0)
    #>    xtable           1.8-4      2019-04-21 [1] CRAN (R 4.2.0)
    #>    xts              0.12.1     2020-09-09 [1] CRAN (R 4.2.0)
    #>    zoo              1.8-10     2022-04-15 [1] CRAN (R 4.2.0)
    #> 
    #>  [1] C:/Users/trist/AppData/Local/R/win-library/4.2
    #>  [2] C:/Program Files/R/R-4.2.0/library
    #> 
    #>  D ── DLL MD5 mismatch, broken installation.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
    ```



[sw-slides]: https://www.maths.ed.ac.uk/~swood34/mgcv/tampere/basis-penalty.pdf "PDF of Simon Wood's slides on Basis Penalty Smoothers"

[smoothness-slides]: https://www.maths.ed.ac.uk/~swood34/mgcv/tampere/smoothness.pdf "Simon Wood's smoothness selection slides"

[nr-gam]: https://youtu.be/q4_t8jXcQgc "Noam Ross - Nonlinear Models in R: The Wonderful World of mgcv"

[gs-gam]: https://youtu.be/Zxokd_Eqrcg?t=506 "Dr. Gavin Simpson - Learning When, Where, and by How Much, Things Change [Remote]"

[ms-gam]: https://arxiv.org/abs/1703.05339 "Generalised additive mixed models for dynamic analysis in linguistics: a practical introduction"

[gam-resources]: https://github.com/noamross/gam-resources "Resources for Learning About and Using GAMs in R"

[mgcv-textbook]: https://amzn.to/37PLa8W "An Amazon Affliate link to Simon Wood's GAM textbook" 

[radon]: https://mc-stan.org/rstanarm/reference/rstanarm-datasets.html "Documentation on the radon dataset"

[mcycle]: https://rdrr.io/pkg/MASS/man/mcycle.html "Documentation on the mcycle dataset"

[smooth2random]: https://rdrr.io/pkg/mgcv/man/smooth2random.html "Documentation on smooth2random"

[gam-vcomp]: https://rdrr.io/pkg/mgcv/man/gam.vcomp.html "Documentation on gam.vcomp"

[rs-splines]: https://youtu.be/ENxTrFf9a7c?t=2226 "Statistical Rethinking Winter 2019 Lecture 04"

[arm-book]: https://amzn.to/3aVa9tB "An Amazon Affliate link to Gelman and Hill" 
 
[gs-re]: https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/ "Using random effects in GAMs with mgcv"

[gratia]: https://gavinsimpson.github.io/gratia/ "The gratia R package"

[stan-ncp]: https://mc-stan.org/docs/2_26/stan-users-guide/reparameterization-section.html#hierarchical-models-and-the-non-centered-parameterization "The Stan manual on reparameterization"

[rm-mmm]: https://elevanth.org/blog/2017/09/07/metamorphosis-multilevel-model/ "Metamorphosis and the Multilevel Model"

[mb-ncp]: https://betanalpha.github.io/assets/case_studies/hierarchical_modeling.html#24_Normal_Hierarchical_Models "Hierarchical Modeling"

[old-and-new]: http://www.biostat.umn.edu/~hodges/PubH8492/Hodges-ClaytonREONsubToStatSci.pdf "Random effects old and new"

[loo]: https://mc-stan.org/loo/reference/loo.html "Efficient approximate leave-one-out cross-validation (LOO)"

[wiki-spline]: https://commons.wikimedia.org/wiki/File:Spline_(PSF).png "File:Spline (PSF).png on Wikimedia"

[wood-2004]: https://www.tandfonline.com/doi/abs/10.1198/016214504000000980 "Stable and Efficient Multiple Smoothing Parameter Estimation for Generalized Additive Models"

[gamm4-code]: https://github.com/cran/gamm4/blob/8c89289807ee7092229b13d7129b3eca33101a89/R/gamm4.r "Source code from the gamm4 package"

[gamlss-re]: https://rdrr.io/cran/gamlss/man/random.html "random: Specify a random intercept model in a GAMLSS formula"

[mgcv-re]: https://rdrr.io/cran/mgcv/man/random.effects.html "random.effects: Random effects in GAMs"

[avi-tweet]: https://twitter.com/avehtari/status/1366085190074961921?s=20 "@avehtari [Feb 28]: btw. the two different effective sample size estimates are related (and asymptotically equal given some conditions)"
