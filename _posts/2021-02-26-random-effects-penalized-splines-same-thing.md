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

  - Mixed effects models use partial pooling to strike a balance between
    a grand mean (complete pooling) and individual means (no pooling).
  - Smoothing splines work by penalizing model coefficients to reduce the
    model degrees of freedom.
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
There would be no pooling of information between the groups.

Consider the [`radon` dataset][radon] example from [Gelman and Hill
(2007)][arm-book]. Radon measurements were taken in Minnesota
counties. We would like to estimate the average radon measurement for
each county. We have a repeated measures situation, and some counties
have more observations than others. We use a mixed effects model to
estimate a population distribution of county estimates, and the
county-level estimates are randomly varying effects. They are drawn from
a random distribution, the scale of which we estimate from the data.



```r
library(tidyverse)
theme_set(theme_grey(base_size = 14))
library(brms)
radon <- rstanarm::radon

b_radon <- brm(
  log_radon ~ 1 + (1 | county), 
  radon, 
  family = gaussian, 
  file = "radon"
)
b_radon
#>  Family: gaussian 
#>   Links: mu = identity; sigma = identity 
#> Formula: log_radon ~ 1 + (1 | county) 
#>    Data: radon (Number of observations: 919) 
#> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup samples = 4000
#> 
#> Group-Level Effects: 
#> ~county (Number of levels: 85) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.30      0.05     0.22     0.40 1.00     1782     2894
#> 
#> Population-Level Effects: 
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.35      0.05     1.26     1.45 1.00     2749     3198
#> 
#> Family Specific Parameters: 
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.77      0.02     0.73     0.80 1.00     7374     3105
#> 
#> Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
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
  tidybayes::add_fitted_draws(b_radon) %>% 
  mutate(
    observed_minus_model = observed_mean - .value 
  ) %>% 
  # summarize fitted values
  ggdist::median_qi(.value, observed_minus_model) 

radon_aug$type <- "mixed model estimates"
radon$type <- "observed means"

ggplot(radon_aug) + 
  aes(x = fct_infreq(county), y = log_radon) +
  stat_summary(
    aes(color = type, shape = type),
    data = radon,
    fun = mean,
    geom = "point"
  ) +
  geom_point(
    aes(y = .value, color = type, shape = type)
  ) + 
  # want to include 0 in the figure
  geom_blank(aes(y = 0)) +
  labs(
    x = "county (in decreasing order by sample size)", 
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

We see a classic example of partial pooling. For counties with many
observations, the estimate mean is hardly adjusted. For counties with
less data, the estimate is pulled towards the population mean
(`Intercept` in the summary above).

The following plot shows difference between the observed means and
the estimated means, subtracting the grey triangles from the blue squares
in the plot above.


```r
ggplot(radon_aug) + 
  aes(x = county_n, y = observed_minus_model) + 
  geom_point() +
  labs(
    x = "Number of observations in county",
    y = "Observed mean - estimated mean"
  ) 
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/shrinkage-by-n-1.png" title="Plot with number of observations on the x axis and the difference between the observed and estimated means on the y axis. There is a smaller difference for counties with more data." alt="Plot with number of observations on the x axis and the difference between the observed and estimated means on the y axis. There is a smaller difference for counties with more data." width="66%" style="display: block; margin: auto;" />



The contention behind the *smooths = random effects* claim is that what we
just did is a case of *smoothing*. These random effects are, in a way, 
smoothed fixed effects.




## But what's smoothing?

Now let's walk through a generalized additive model to demonstrate a
penalized smoothing spline. That was a mouth full, but basically
additive models are like the smoothing expansion pack for the
standard linear model. We're still doing regression, but we have some
new syntax and our models can do nonlinear relationships more easily
now.

I will walk through a basic example of how a spline's basis functions
are weighted to approximate a nonlinear trend, but this is not going to
be a full tutorial. Other people have made video introductions to
[additive models][gs-gam] or the [mgcv package][nr-gam]. I first
learned them from [a tutorial for linguists][ms-gam] and then from
[the MGCV textbook][mgcv-textbook], but there are [other resources
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

An easy way to pull the wiggles is to use the model matrix. We have 20 columns for the intercept and 19 basis functions.


```r
model.matrix(gam_20) %>% 
  tibble::trunc_mat(width = 72)
#> # Description: dbl[,20] [133 x 20]
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
#> # ... with 123 more rows, and 15 more variables: `s(times).5` <dbl>,
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
penalized smoothing splines, this is done by first specifying a penalty matrix that defines
*wiggliness* for that spline basis. These two features are pitted
against each other in the following equation: 

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

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/unnamed-chunk-5-1.png" title="The dataset plotted with two unpenalized smoothing splines. The 50-dimension spline is very wiggly." alt="The dataset plotted with two unpenalized smoothing splines. The 50-dimension spline is very wiggly." width="80%" style="display: block; margin: auto;" />

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
  geom_point() +
  geom_line(aes(y = .fitted, color = "estimated"), size = 1) +
  geom_line(aes(y = .fitted20_fx, color = "no smoothing"), size = 1) +
  geom_line(aes(y = .fitted20_sp, color = "10,000,000"), size = 1) + 
  labs(
    x = "time after impact [ms]", 
    y = "acceleration [g]", 
    color = "lambda"
  )
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/unnamed-chunk-6-1.png" title="The data plotted with three smooths. One of them is a completely flat line. It has a penalty of 10000000." alt="The data plotted with three smooths. One of them is a completely flat line. It has a penalty of 10000000." width="80%" style="display: block; margin: auto;" />

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
[slide 7 here][smoothness-slides]), you can express the smooth as a
mixed model:


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
again: *σ*/*λ* = *σ*<sub>*b*</sub>: Model coefficients are related under a
common distribution so that they can share information with each other.
We can smuggle penalized smooths into the mixed effects framework.




<blockquote class="twitter-tweet" data-conversation="none" data-dnt="true"><p lang="en" dir="ltr">random effects and splines are _the same_ thing. See also <a href="https://t.co/LgZTzZimH0">https://t.co/LgZTzZimH0</a></p>&mdash; DavidLawrenceMiller (@millerdl) <a href="https://twitter.com/millerdl/status/846719376338407424?ref_src=twsrc%5Etfw">March 28, 2017</a></blockquote> 



### So... you can turn splines into mixed models?

Yes. What this means is that we can use nlme or lme4 to estimate a smooth as
mixed effects model. mgcv provides this feature in its aptly named
[`smooth2random()`][smooth2random] function.

Below you can see the extent of the internal transformation needed to
convert our nice wiggly cubic regression basis into matrices for the mixed effects
framework.


```r
# Construct a smoothing basis outside of a model
sm_raw <- smoothCon(
  s(times, k = 20, bs = "cr"), 
  data = mcycle, 
  absorb.cons = TRUE, 
  diagonal.penalty = TRUE
)

re <- smooth2random(sm_raw[[1]], "", type = 2)

# 1 fixed effect and 18 random effect columns
mixed_matrix <- cbind(re$Xf, re$rand$Xr)
ggmatplot(mixed_matrix) + 
  labs(title = NULL)
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/mixed_matrix-1.png" title="A matrix plot with one line per column. Unlike the other ones, the lines here are not nice and bumpy." alt="A matrix plot with one line per column. Unlike the other ones, the lines here are not nice and bumpy." width="80%" style="display: block; margin: auto;" />





I don't understand it very well myself, and I probably won't be
converting smoothing bases into mixed effects model matrices and fitting
them with lme4 anytime soon, but it's useful to know about this idea
because of the following fact.


### This point seems obscure, but it is what brms uses!

What happens when we fit a smooth in brms? We are fitting a mixed
effects model.


```r
b_gam_20 <- brm(
  accel ~ s(times, bs = "cr", k = 20),
  data = mcycle, 
  family = gaussian,
  file = "b-gam-20"
)
summary(b_gam_20)
#>  Family: gaussian 
#>   Links: mu = identity; sigma = identity 
#> Formula: accel ~ s(times, bs = "cr", k = 20) 
#>    Data: mcycle (Number of observations: 133) 
#> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup samples = 4000
#> 
#> Smooth Terms: 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(stimes_1)     5.00      1.14     3.21     7.66 1.00     1095     1579
#> 
#> Population-Level Effects: 
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept   -25.50      2.02   -29.39   -21.45 1.00     5454     2867
#> stimes_1      1.65      0.34     0.98     2.30 1.00     5156     3033
#> 
#> Family Specific Parameters: 
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma    22.77      1.45    20.20    25.81 1.00     4851     3277
#> 
#> Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

You see `sds(times_1)`? That's the variance of smooth weights. You see
`stimes_1`? That the singe fixed effects term (`re$Xf`) in the code
above. I'm pretty sure about this because I learned `smooth2random()`
from studying the brms source code.

Indeed, if we look at the actual Stan code used to fit the model, we see
a mixed effects model. **I suggest you only skim this code.**


```r
b_gam_20$model
#> // generated with brms 2.14.4
#> functions {
#> }
#> data {
#>   int<lower=1> N;  // total number of observations
#>   vector[N] Y;  // response variable
#>   // data for splines
#>   int Ks;  // number of linear effects
#>   matrix[N, Ks] Xs;  // design matrix for the linear effects
#>   // data for spline s(times,bs="cr",k=20)
#>   int nb_1;  // number of bases
#>   int knots_1[nb_1];  // number of knots
#>   // basis function matrices
#>   matrix[N, knots_1[1]] Zs_1_1;
#>   int prior_only;  // should the likelihood be ignored?
#> }
#> transformed data {
#> }
#> parameters {
#>   real Intercept;  // temporary intercept for centered predictors
#>   vector[Ks] bs;  // spline coefficients
#>   // parameters for spline s(times,bs="cr",k=20)
#>   // standarized spline coefficients
#>   vector[knots_1[1]] zs_1_1;
#>   real<lower=0> sds_1_1;  // standard deviations of spline coefficients
#>   real<lower=0> sigma;  // residual SD
#> }
#> transformed parameters {
#>   // actual spline coefficients
#>   vector[knots_1[1]] s_1_1;
#>   // compute actual spline coefficients
#>   s_1_1 = sds_1_1 * zs_1_1;
#> }
#> model {
#>   // likelihood including all constants
#>   if (!prior_only) {
#>     // initialize linear predictor term
#>     vector[N] mu = Intercept + rep_vector(0.0, N) + Xs * bs + Zs_1_1 * s_1_1;
#>     target += normal_lpdf(Y | mu, sigma);
#>   }
#>   // priors including all constants
#>   target += student_t_lpdf(Intercept | 3, -13.3, 35.6);
#>   target += student_t_lpdf(sds_1_1 | 3, 0, 35.6)
#>     - 1 * student_t_lccdf(0 | 3, 0, 35.6);
#>   target += std_normal_lpdf(zs_1_1);
#>   target += student_t_lpdf(sigma | 3, 0, 35.6)
#>     - 1 * student_t_lccdf(0 | 3, 0, 35.6);
#> }
#> generated quantities {
#>   // actual population-level intercept
#>   real b_Intercept = Intercept;
#> }
```

Okay, that's a lot. But let me highlight and translate the key part of it.

$$
\begin{align*}
  
   \mathbf{S} &: \texttt{s_1_1} & \texttt{// actual spline coefficients}\\
   \mathbf{\sigma} &:  \texttt{sds_1_1} & \texttt{// standard deviations of spline coefficients}\\
   \mathbf{Z} &:  \texttt{zs_1_1} & \texttt{// standarized spline coefficients}\\
   \mathbf{S} &= \sigma \cdot \mathbf{Z} & \texttt{s_1_1 = sds_1_1 * zs_1_1}; \\
   \mathbf{Z} &\sim \textsf{Normal}(0, 1) & \texttt{target += std_normal_lpdf(zs_1_1);}\\
   \mathbf{\sigma} &\sim \textsf{StudentT}(3, 0, 35.6) & \texttt{target += student_t_lpdf(sds_1_1 | 3, 0, 35.6) ...}\\

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
#> ...
#> parameters {
#>   ...
#>   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#>   vector[N_1] z_1[M_1];  // standardized group-level effects
#> }
#> transformed parameters {
#>   vector[N_1] r_1_1;  // actual group-level effects
#>   r_1_1 = (sd_1[1] * (z_1[1]));
#> }
#> model {
#>   ...
#>   target += student_t_lpdf(sd_1 | 3, 0, 2.5)
#>     - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#>   target += std_normal_lpdf(z_1[1]);
#> }
```






## Simple random effects are category smoothers

One consequence of this relationship is that you can walk this relation
backwards: You can fit a simple random effects using a basis matrix and
penalty matrix. Indeed, mgcv provides a [random effects (`"re"`)
smoother](https://rdrr.io/cran/mgcv/man/random.effects.html) basis so we
can estimate our mixed model from above using a smooth.


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

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/unnamed-chunk-15-1.png" title="A series are 85 spikes." alt="A series are 85 spikes." width="80%" style="display: block; margin: auto;" />

(Yikes, that one stings the eyes!)

The penalty matrix is a diagonal as each county effect is
equally penalized.


```r
gam_radon %>% 
  gratia::penalty() %>% 
  gratia::draw() + 
  theme(axis.text = element_blank())
```

<img src="/figs/2021-02-26-random-effects-penalized-splines-same-thing/unnamed-chunk-16-1.png" title="A penalty matrix but only the diagonal is active." alt="A penalty matrix but only the diagonal is active." width="80%" style="display: block; margin: auto;" />


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
  posterior_summary(pars = c("sd_county__Intercept", "sigma")) %>% 
  round(3)
#>                      Estimate Est.Error  Q2.5 Q97.5
#> sd_county__Intercept    0.305     0.046 0.220 0.400
#> sigma                   0.767     0.019 0.731 0.805
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
#> elpd_loo  -1083.8 28.7
#> p_loo        44.9  4.0
#> looic      2167.7 57.5
#> ------
#> Monte Carlo SE of elpd_loo is 0.1.
#> 
#> Pareto k diagnostic values:
#>                          Count Pct.    Min. n_eff
#> (-Inf, 0.5]   (good)     917   99.8%   398       
#>  (0.5, 0.7]   (ok)         2    0.2%   227       
#>    (0.7, 1]   (bad)        0    0.0%   <NA>      
#>    (1, Inf)   (very bad)   0    0.0%   <NA>      
#> 
#> All Pareto k estimates are ok (k < 0.7).
#> See help('pareto-k-diagnostic') for details.
```

Here `p_loo` is the effective number of parameters from a leave-one-out
cross-validation method. It's around 45. Our mgcv model has around 41.6
parameters (39.6 from the smooth plus intercept and sigma). These two
parameter estimating methods are unrelated (as far as I know), but they
both seem to telling us something similar about how much information in
`county` variable we have after partially pooling down the model
parameters.

For a deeper dive on random effects, Gavin Simpson recently wrote about [how to use mgcv for random
effects][gs-re], so I encourage readers to look at that post. 


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

*Last knitted on 2021-02-26. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2021-02-26-random-effects-penalized-splines-same-thing.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info ---------------------------------------------------------------
    #>  setting  value                       
    #>  version  R version 4.0.4 (2021-02-15)
    #>  os       Windows 10 x64              
    #>  system   x86_64, mingw32             
    #>  ui       RTerm                       
    #>  language (EN)                        
    #>  collate  English_United States.1252  
    #>  ctype    English_United States.1252  
    #>  tz       America/Chicago             
    #>  date     2021-02-26                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  ! package        * version    date       lib
    #>    abind            1.4-5      2016-07-21 [1]
    #>    arrayhelpers     1.1-0      2020-02-04 [1]
    #>    assertthat       0.2.1      2019-03-21 [1]
    #>    backports        1.2.1      2020-12-09 [1]
    #>    base64enc        0.1-3      2015-07-28 [1]
    #>    bayesplot        1.8.0.9000 2021-02-01 [1]
    #>    boot             1.3-27     2021-02-12 [1]
    #>    bridgesampling   1.0-0      2020-02-26 [1]
    #>    brms           * 2.14.4     2020-11-03 [1]
    #>    Brobdingnag      1.2-6      2018-08-13 [1]
    #>    broom            0.7.5      2021-02-19 [1]
    #>    callr            3.5.1      2020-10-13 [1]
    #>    cellranger       1.1.0      2016-07-27 [1]
    #>    cli              2.3.1      2021-02-23 [1]
    #>    coda             0.19-4     2020-09-30 [1]
    #>    codetools        0.2-18     2020-11-04 [1]
    #>    colorspace       2.0-0      2020-11-11 [1]
    #>    colourpicker     1.1.0      2020-09-14 [1]
    #>    crayon           1.4.1      2021-02-08 [1]
    #>    crosstalk        1.1.1      2021-01-12 [1]
    #>    curl             4.3        2019-12-02 [1]
    #>    DBI              1.1.1      2021-01-15 [1]
    #>    dbplyr           2.1.0      2021-02-03 [1]
    #>    digest           0.6.27     2020-10-24 [1]
    #>    distributional   0.2.2      2021-02-02 [1]
    #>    dplyr          * 1.0.4      2021-02-02 [1]
    #>    DT               0.17       2021-01-06 [1]
    #>    dygraphs         1.1.1.6    2018-07-11 [1]
    #>    ellipsis         0.3.1      2020-05-15 [1]
    #>    emmeans          1.5.4      2021-02-03 [1]
    #>    estimability     1.3        2018-02-11 [1]
    #>    evaluate         0.14       2019-05-28 [1]
    #>    fansi            0.4.2      2021-01-15 [1]
    #>    farver           2.0.3      2020-01-16 [1]
    #>    fastmap          1.1.0      2021-01-25 [1]
    #>    forcats        * 0.5.1      2021-01-27 [1]
    #>    fs               1.5.0      2020-07-31 [1]
    #>    gamm4            0.2-6      2020-04-03 [1]
    #>    generics         0.1.0      2020-10-31 [1]
    #>    ggdist           2.4.0      2021-01-04 [1]
    #>    ggplot2        * 3.3.3      2020-12-30 [1]
    #>    ggridges         0.5.3      2021-01-08 [1]
    #>    git2r            0.28.0     2021-01-10 [1]
    #>    glue             1.4.2      2020-08-27 [1]
    #>    gratia           0.5.1.9002 2021-02-16 [1]
    #>    gridExtra        2.3        2017-09-09 [1]
    #>    gtable           0.3.0      2019-03-25 [1]
    #>    gtools           3.8.2      2020-03-31 [1]
    #>    haven            2.3.1      2020-06-01 [1]
    #>    here             1.0.1      2020-12-13 [1]
    #>    highr            0.8        2019-03-20 [1]
    #>    hms              1.0.0      2021-01-13 [1]
    #>    htmltools        0.5.1.1    2021-01-22 [1]
    #>    htmlwidgets      1.5.3      2020-12-10 [1]
    #>    httpuv           1.5.5      2021-01-13 [1]
    #>    httr             1.4.2      2020-07-20 [1]
    #>    igraph           1.2.6      2020-10-06 [1]
    #>    inline           0.3.17     2020-12-01 [1]
    #>    jsonlite         1.7.2      2020-12-09 [1]
    #>    knitr          * 1.31       2021-01-27 [1]
    #>    labeling         0.4.2      2020-10-20 [1]
    #>    later            1.1.0.1    2020-06-05 [1]
    #>    lattice          0.20-41    2020-04-02 [1]
    #>    lifecycle        1.0.0      2021-02-15 [1]
    #>    lme4             1.1-26     2020-12-01 [1]
    #>    loo              2.4.1      2020-12-09 [1]
    #>    lubridate        1.7.9.2    2020-11-13 [1]
    #>    magrittr         2.0.1      2020-11-17 [1]
    #>    markdown         1.1        2019-08-07 [1]
    #>    MASS             7.3-53.1   2021-02-12 [1]
    #>    Matrix           1.2-18     2019-11-27 [1]
    #>    matrixStats      0.58.0     2021-01-29 [1]
    #>    mgcv           * 1.8-33     2020-08-27 [1]
    #>    mime             0.10       2021-02-13 [1]
    #>    miniUI           0.1.1.1    2018-05-18 [1]
    #>    minqa            1.2.4      2014-10-09 [1]
    #>    modelr           0.1.8      2020-05-19 [1]
    #>    multcomp         1.4-16     2021-02-08 [1]
    #>    munsell          0.5.0      2018-06-12 [1]
    #>    mvnfast          0.2.5.1    2020-10-14 [1]
    #>    mvtnorm          1.1-1      2020-06-09 [1]
    #>    nlme           * 3.1-152    2021-02-04 [1]
    #>    nloptr           1.2.2.2    2020-07-02 [1]
    #>    patchwork        1.1.1      2020-12-17 [1]
    #>    pillar           1.5.0      2021-02-22 [1]
    #>    pkgbuild         1.2.0      2020-12-15 [1]
    #>    pkgconfig        2.0.3      2019-09-22 [1]
    #>    plyr             1.8.6      2020-03-03 [1]
    #>    prettyunits      1.1.1      2020-01-24 [1]
    #>    processx         3.4.5      2020-11-30 [1]
    #>    projpred         2.0.2      2020-10-28 [1]
    #>    promises         1.2.0.1    2021-02-11 [1]
    #>    ps               1.5.0      2020-12-05 [1]
    #>    purrr          * 0.3.4      2020-04-17 [1]
    #>    R6               2.5.0      2020-10-28 [1]
    #>    ragg             1.1.0      2021-02-15 [1]
    #>    Rcpp           * 1.0.6      2021-01-15 [1]
    #>  D RcppParallel     5.0.2      2020-06-24 [1]
    #>    readr          * 1.4.0      2020-10-05 [1]
    #>    readxl           1.3.1      2019-03-13 [1]
    #>    reprex           1.0.0      2021-01-27 [1]
    #>    reshape2         1.4.4      2020-04-09 [1]
    #>    rlang            0.4.10     2020-12-30 [1]
    #>    rprojroot        2.0.2      2020-11-15 [1]
    #>    rsconnect        0.8.16     2019-12-13 [1]
    #>    rstan            2.21.2     2020-07-27 [1]
    #>    rstanarm         2.21.1     2020-07-20 [1]
    #>    rstantools       2.1.1      2020-07-06 [1]
    #>    rstudioapi       0.13       2020-11-12 [1]
    #>    rvest            0.3.6      2020-07-25 [1]
    #>    sandwich         3.0-0      2020-10-02 [1]
    #>    scales           1.1.1      2020-05-11 [1]
    #>    sessioninfo      1.1.1      2018-11-05 [1]
    #>    shiny            1.6.0      2021-01-25 [1]
    #>    shinyjs          2.0.0      2020-09-09 [1]
    #>    shinystan        2.5.0      2018-05-01 [1]
    #>    shinythemes      1.2.0      2021-01-25 [1]
    #>    StanHeaders      2.21.0-7   2020-12-17 [1]
    #>    statmod          1.4.35     2020-10-19 [1]
    #>    stringi          1.5.3      2020-09-09 [1]
    #>    stringr        * 1.4.0      2019-02-10 [1]
    #>    survival         3.2-7      2020-09-28 [1]
    #>    svUnit           1.0.3      2020-04-20 [1]
    #>    systemfonts      1.0.1      2021-02-09 [1]
    #>    textshaping      0.3.0      2021-02-10 [1]
    #>    TH.data          1.0-10     2019-01-21 [1]
    #>    threejs          0.3.3      2020-01-21 [1]
    #>    tibble         * 3.0.6      2021-01-29 [1]
    #>    tidybayes        2.3.1      2020-11-02 [1]
    #>    tidyr          * 1.1.2      2020-08-27 [1]
    #>    tidyselect       1.1.0      2020-05-11 [1]
    #>    tidyverse      * 1.3.0      2019-11-21 [1]
    #>    tjmisc           0.0.0.9000 2021-02-25 [1]
    #>    utf8             1.1.4      2018-05-24 [1]
    #>    V8               3.4.0      2020-11-04 [1]
    #>    vctrs            0.3.6      2020-12-17 [1]
    #>    withr            2.4.1      2021-01-26 [1]
    #>    xfun             0.21       2021-02-10 [1]
    #>    xml2             1.3.2      2020-04-23 [1]
    #>    xtable           1.8-4      2019-04-21 [1]
    #>    xts              0.12.1     2020-09-09 [1]
    #>    zoo              1.8-8      2020-05-02 [1]
    #>  source                              
    #>  CRAN (R 4.0.0)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.0)                      
    #>  local                               
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.0)                      
    #>  CRAN (R 4.0.4)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.4)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.0)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  Github (gavinsimpson/gratia@32daf2f)
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.0)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.4)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.4)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.0)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.4)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.4)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  local                               
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.3)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #>  CRAN (R 4.0.2)                      
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.4/library
    #> 
    #>  D -- DLL MD5 mismatch, broken installation.
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

