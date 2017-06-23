---
title: Plotting partial pooling in mixed-effects models
excerpt: There are a lot of bilabial sounds in that title
tags:
  - rstanarm
  - bayesian
  - r
  - mixed effects
---



In this post, I demonstrate a few techniques for plotting information from a
relatively simple mixed-effects model fit in R. These plots can help us develop 
intuitions about what these models are doing and what "partial pooling" means.

## The `sleepstudy` dataset

For these examples, I'm going to use the `sleepstudy` dataset from the lme4
package. The outcome measure is reaction time, the predictor measure is days of
sleep deprivation, and these measurements are nested within participants&mdash;we
have 10 observations per participant. I am also going to add two fake
participants with incomplete data to illustrate partial pooling.


```r
library(lme4)
library(dplyr)
library(tibble)

# Convert to tibble for better printing. Convert factors to strings
sleepstudy <- sleepstudy %>% 
  as_tibble() %>% 
  mutate(Subject = as.character(Subject))

# Add two fake participants
df_sleep <- bind_rows(
  sleepstudy,
  data_frame(Reaction = c(286, 288), Days = 0:1, Subject = "374"),
  data_frame(Reaction = 245, Days = 0, Subject = "373"))

df_sleep
#> # A tibble: 183 x 3
#>    Reaction  Days Subject
#>       <dbl> <dbl>   <chr>
#>  1 249.5600     0     308
#>  2 258.7047     1     308
#>  3 250.8006     2     308
#>  4 321.4398     3     308
#>  5 356.8519     4     308
#>  6 414.6901     5     308
#>  7 382.2038     6     308
#>  8 290.1486     7     308
#>  9 430.5853     8     308
#> 10 466.3535     9     308
#> # ... with 173 more rows
```

We can visualize all the data in ggplot2 by using `facet_wrap()` to create
subplots for each participant and `stat_smooth()` to create a regression line
in each subplot.


```r
library(ggplot2)

xlab <- "Days of sleep deprivation"
ylab <- "Average reaction time (ms)"

ggplot(df_sleep) + 
  aes(x = Days, y = Reaction) + 
  stat_smooth(method = "lm", se = FALSE) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("Subject") +
  labs(x = xlab, y = ylab) + 
  # We also need to help the x-axis, so it doesn't 
  # create gridlines/ticks on 2.5 days
  scale_x_continuous(breaks = 0:4 * 2)
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/facet-plot-1.png" title="Trellis plot of reaction time by days of sleep deprivation." alt="Trellis plot of reaction time by days of sleep deprivation." width="80%" style="display: block; margin: auto;" />

By the way, ggplot2 doesn't draw the regressions lines outside of the range of
the data unless we set `fullrange = TRUE`. That's a helpful feature for 374!


## Complete pooling and no pooling models

Each one of these panels plotted above shows an independently estimated
regression line. This approach to fitting a separate line for each participant
is sometimes called the **no pooling** model because none of the information
from different participants is combined or _pooled_ together.

We fit a separate line for each cluster of data, unaware
that any of the other participants exist. The `lmList()` function in `lme4`
automates this process.


```r
df_no_pooling <- lmList(Reaction ~ Days | Subject, df_sleep) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("Subject") %>% 
  rename(Intercept = `(Intercept)`, Slope_Days = Days) %>% 
  add_column(Model = "No pooling") %>% 
  # Remove the participant who only had one data-point
  filter(Subject != "373")

head(df_no_pooling)
#>   Subject Intercept Slope_Days      Model
#> 1     308  244.1927  21.764702 No pooling
#> 2     309  205.0549   2.261785 No pooling
#> 3     310  203.4842   6.114899 No pooling
#> 4     330  289.6851   3.008073 No pooling
#> 5     331  285.7390   5.266019 No pooling
#> 6     332  264.2516   9.566768 No pooling
```

In contrast, we might consider a **complete pooling** model where all the
information from the participants is combined together. We fit a single line for
the combined data set, unaware that the data came from different participants.


```r
# Fit a model on all the data pooled together
m_pooled <- lm(Reaction ~ Days, df_sleep) 

# Repeat the intercept and slope terms for each participant
df_pooled <- data_frame(
  Model = "Complete pooling",
  Subject = unique(df_sleep$Subject),
  Intercept = coef(m_pooled)[1], 
  Slope_Days = coef(m_pooled)[2])

head(df_pooled)
#> # A tibble: 6 x 4
#>              Model Subject Intercept Slope_Days
#>              <chr>   <chr>     <dbl>      <dbl>
#> 1 Complete pooling     308  252.3207   10.32766
#> 2 Complete pooling     309  252.3207   10.32766
#> 3 Complete pooling     310  252.3207   10.32766
#> 4 Complete pooling     330  252.3207   10.32766
#> 5 Complete pooling     331  252.3207   10.32766
#> 6 Complete pooling     332  252.3207   10.32766
```

We can compare these two approaches. Instead of calculating the regression lines
with `stat_smooth()`, we can use  `geom_abline()` to draw the lines from our 
dataframe of intercept and slope parameters.


```r
# Join the raw data so we can use plot the points and the lines.
df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
  left_join(df_sleep, by = "Subject")

p_model_comparison <- ggplot(df_models) + 
  aes(x = Days, y = Reaction) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_Days, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("Subject") +
  labs(x = xlab, y = ylab) + 
  scale_x_continuous(breaks = 0:4 * 2) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top")

p_model_comparison
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/pooling-vs-no-pooling-1.png" title="Same trellis plot as above but with two regression lines per subplot to compare the two models." alt="Same trellis plot as above but with two regression lines per subplot to compare the two models." width="80%" style="display: block; margin: auto;" />

If we stare at this plot, a few things become apparent. The complete pooling 
model estimates a single line, and we see that same line drawn on every facet. 
One advantage is that the model can make a guess about the line for 373 who only
has one observation. That model looks pretty terrible elsewhere&mdash;309, 310,
etc.&mdash;because nobody is perfectly average. In contrast, the no pooling model can
follow the data, fitting the sharp trend upwards in 308 and even capturing the
negative slope in 335.

(Here's a fun question: Which approach has the better guess for 374's line?)

The no pooling model cannot make a guess about 373. In [_Statistical
Rethinking_](http://xcelab.net/rm/statistical-rethinking/), McElreath says these 
models have amnesia :hushed::

> Many statistical models also have anterograde amnesia. As the models move from
one cluster&mdash;individual, group, location&mdash;in the data to another, estimating
parameters for each cluster, they forget everything about the previous clusters.
They behave this way, because the assumptions force them to. Any of the models
from previous chapters that used dummy variables to handle categories
are programmed for amnesia. These models implicitly assume that nothing learned
about any one category informs estimates for the other categories&mdash;the parameters
are independent of one another and learn from completely separate portions of
the data. This would be like forgetting you had ever been in a café, each time 
you go to a new café. Cafés do differ, but they are also alike.

Once the no pooling model draws the line for 372, and it completely forgets
everything it has seen and moves on to 373. It has to skip 373 because it cannot
estimate a line from a single point, and it moves on.



## Improving estimates with a mixed-effects model

We can do better with mixed-effects models. In these models, we pool information
from all the lines together to improve our estimates of each individual line.
This approach is sometimes called **partial pooling**. In particular, after
seeing the 18 trend lines for the participants with complete data, we can make
an informed guess about the trend lines for the two participants with incomplete
data.

We can fit a classical mixed-effects model with the lme4 package:


```r
m <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), df_sleep)
arm::display(m)
#> lmer(formula = Reaction ~ 1 + Days + (1 + Days | Subject), data = df_sleep)
#>             coef.est coef.se
#> (Intercept) 252.54     6.43 
#> Days         10.45     1.54 
#> 
#> Error terms:
#>  Groups   Name        Std.Dev. Corr 
#>  Subject  (Intercept) 24.14         
#>           Days         5.92    0.07 
#>  Residual             25.48         
#> ---
#> number of obs: 183, groups: Subject, 20
#> AIC = 1783.4, DIC = 1787.8
#> deviance = 1779.6
```

The first two `coef.est` items are the "fixed effects" estimates; they reflect
the average intercept and slope parameters. For this example, the values are 
practically the same as the complete-pooling estimates. This model assumes that 
each participant's individual intercept and slope parameters are deviations 
from this average, and these random deviations drawn from a distribution of 
possible intercept and slope parameters. These are "randomly varying" or "random
effects". The information in the `Error terms` area describes the distribution 
of the effects. Because we have both fixed and random effects, 
we have a "mixed-effects" model. Hence the name.

To visualize these estimates, we extract each participant's intercept and slope
using `coef()`.


```r
# Make a dataframe with the fitted effects
df_partial_pooling <- coef(m)[["Subject"]] %>% 
  as_tibble() %>% 
  rownames_to_column("Subject") %>% 
  rename(Intercept = `(Intercept)`, Slope_Days = Days) %>% 
  mutate(Model = "Partial pooling")

head(df_partial_pooling)
#> # A tibble: 6 x 4
#>   Subject Intercept Slope_Days           Model
#>     <chr>     <dbl>      <dbl>           <chr>
#> 1     308  253.9478  19.626420 Partial pooling
#> 2     309  211.7334   1.731866 Partial pooling
#> 3     310  213.1585   4.906097 Partial pooling
#> 4     330  275.1422   5.643646 Partial pooling
#> 5     331  273.7283   7.386313 Partial pooling
#> 6     332  260.6503  10.163271 Partial pooling
```

Update the previous plot with a dataframe of all three models' estimates.


```r
df_models <- bind_rows(df_pooled, df_no_pooling, df_partial_pooling) %>% 
  left_join(df_sleep, by = "Subject")

# Replace the data-set of the last plot
p_model_comparison %+% df_models
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/partial-pooling-vs-others-1.png" title="Update of previous plot with partially pooled regression lines added." alt="Update of previous plot with partially pooled regression lines added." width="80%" style="display: block; margin: auto;" />

Most of the time, the no pooling and partial pooling lines are on top of each
other. But when the two differ, it's because the partial pooling model's line is
pulled slightly towards the no-pooling line.

We can appreciate the differences by zooming in on some participants.


```r
df_zoom <- df_models %>% 
  filter(Subject %in% c("335", "350", "373", "374"))

p_model_comparison %+% df_zoom
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/zoomed-in-partial-pooling-1.png" title="Trellis plot of four participants to highlight the fine differences among the regression lines." alt="Trellis plot of four participants to highlight the fine differences among the regression lines." width="80%" style="display: block; margin: auto;" />

The negative line for 335 from the no pooling model gets a flatter slope in the 
partial pooling model. The model knows that negative trends are rather unlikely,
so the it hedges its bets and pulls that line towards the group average. 
Something similar happens with 350 where a sharp slope is slightly attenuated. 
For the participants with incomplete data, the partial pooling model is much
more like the complete pooling model. The complete pooling and the partial
pooling lines are basically parallel&mdash;i.e, they have the same slope. That's a
reasonable guess given so little information.


## It's shrinkage

The partial pooling model pulls more extreme estimates towards an overall 
average. We can visualize this effect by plotting a scatterplot of intercept and
slope parameters from each model and connecting estimates for the same
participant.


```r
# Also visualize the point for the fixed effects
df_fixef <- data_frame(
  Model = "Partial pooling (average)",
  Intercept = fixef(m)[1],
  Slope_Days = fixef(m)[2])

# Complete pooling / fixed effects are center of gravity in the plot
df_gravity <- df_pooled %>% 
  distinct(Model, Intercept, Slope_Days) %>% 
  bind_rows(df_fixef)
df_gravity
#> # A tibble: 2 x 3
#>                       Model Intercept Slope_Days
#>                       <chr>     <dbl>      <dbl>
#> 1          Complete pooling  252.3207   10.32766
#> 2 Partial pooling (average)  252.5426   10.45212

df_pulled <- bind_rows(df_no_pooling, df_partial_pooling)

ggplot(df_pulled) + 
  aes(x = Intercept, y = Slope_Days, color = Model) + 
  geom_point(size = 2) + 
  geom_point(data = df_gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = Subject, color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = Subject, color = NULL), 
    data = df_no_pooling) + 
  # Don't forget 373
  ggrepel::geom_text_repel(
    aes(label = Subject, color = NULL), 
    data = filter(df_partial_pooling, Subject == "373")) + 
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/shrinkage-plot-1.png" title="Scatterplot of the model parameters showing how estimates from the no pooling model are pulled towards the completely pooled value." alt="Scatterplot of the model parameters showing how estimates from the no pooling model are pulled towards the completely pooled value." width="80%" style="display: block; margin: auto;" />

The average intercept and slope act like a center of gravity, pulling values
parameter estimates towards it. Hmm, maybe gravity is not quite the right
analogy, because the pull is greater for more extreme values. The lines near
that center point are very short; they get adjusted very little. The
lines in general get longer as we move away from the complete pooling
estimate. The fewer the observations in a cluster (here, participants), the more
information is borrowed from other clusters, and the greater the pull towards
the average estimate. Participant 373 had one observation, so their slope
estimate is the average. Likewise, 374 had only two observations, so they get
pulled the farthest and receive a slope estimate near the overall average.

This effect is sometimes called _shrinkage_, because more extreme values
shrinkage are pulled towards a more reasonable, more average value. In the lme4
book, Douglas Bates provides an alternative to _shrinkage_:

> The term "shrinkage" may have negative connotations. John Tukey preferred to
refer to the process as the estimates for individual subjects "borrowing
strength" from each other. This is a fundamental difference in the models
underlying mixed-effects models versus strictly fixed effects models. In a
mixed-effects model we assume that the levels of a grouping factor are a
selection from a population and, as a result, can be expected to share
characteristics to some degree. Consequently, the predictions from a
mixed-effects model are attenuated relative to those from strictly fixed-effects
models.

Shrinkage, borrowing strength :muscle: ... Another term would also be
_regularization_ if we think about how the model avoids overfitting by the
taming extreme estimates.

**This feature is why I use mixed effects models in my work.** If I have a 
speech discrimination experiment and I want to describe a child's speech 
perception ability, I am going to use the partially pooled, shrunken, 
strength-borrowing, regularized, model-derived estimate of their ability, 
because it uses more information. It's that simple to me. Other disciplines
might highlight other reasons to use these models, but for me, it's partially
pooling information that's the most attractive feature.



## A topographic map of parameters

For the next visualization, we are going to visualize the distribution of 
randomly varying effects. Honestly, I am partly including it just so that I
have a working ggplot2 version of how to make this plot online. It's not a 
routine visualization, but it reveals a little more about where estimates are 
being pulled towards.

I already remarked that the model estimates a distribution of intercept and 
slope effects. We know where the center of that distribution is: It's the fixed 
effects estimate, the center of gravity in the last plot. What the model also 
needs to estimate is the variability/spread of values around that center. Also,
intercepts and slopes might be correlated: Maybe the effect of an additional day
on reaction time is diminished for participants who are slower to respond in
general. So, the model also estimates the correlation of those effects too.

Imagine that the last plot is a landscape, and fixed effects point is the peak 
of a hill. What were are going to do is draw a topographic map with contour 
lines to show different elevation regions on that hill.

First, we need to extract the covariance matrix estimated by the model.


```r
# Extract the matrix
cov_mat <- VarCorr(m)[["Subject"]]

# Strip off some details so that just the useful part is printed
attr(cov_mat, "stddev") <- NULL
attr(cov_mat, "correlation") <- NULL
cov_mat
#>             (Intercept)     Days
#> (Intercept)   582.69656  9.89797
#> Days            9.89797 35.03298
```

The `ellipse()` function takes a covariance matrix, a center value, and
quantile/confidence level and returns the points from an oval around the center
at the given confidence level. We create five ellipses for different 
quantile levels.


```r
library(ellipse)

# Helper function to make a data-frame of ellipse points that 
# includes the level as a column
make_ellipse <- function(cov_mat, center, level) {
  ellipse(cov_mat, centre = center, level = level) %>%
    as.data.frame() %>%
    add_column(level = level) %>% 
    as_tibble()
}

center <- fixef(m)
levels <- c(.1, .3, .5, .7, .9)

# Create an ellipse dataframe for each of the levels defined 
# above and combine them
df_ellipse <- levels %>%
  purrr::map_df(~ make_ellipse(cov_mat, center, level = .x)) %>% 
  rename(Intercept = `(Intercept)`, Slope_Days = Days)

df_ellipse
#> # A tibble: 500 x 3
#>    Intercept Slope_Days level
#>        <dbl>      <dbl> <dbl>
#>  1  260.6448   12.43878   0.1
#>  2  260.1491   12.55233   0.1
#>  3  259.6227   12.65743   0.1
#>  4  259.0678   12.75365   0.1
#>  5  258.4867   12.84060   0.1
#>  6  257.8816   12.91793   0.1
#>  7  257.2550   12.98534   0.1
#>  8  256.6094   13.04254   0.1
#>  9  255.9475   13.08931   0.1
#> 10  255.2718   13.12547   0.1
#> # ... with 490 more rows
```

Then we add them onto our previous plot.


```r
ggplot(df_pulled) + 
  aes(x = Intercept, y = Slope_Days, color = Model) + 
  # Draw contour lines from the distribution of effects
  geom_path(aes(group = level, color = NULL), data = df_ellipse, 
            linetype = "dashed", color = "grey40") + 
  geom_point(data = df_gravity, size = 5) + 
  geom_point(size = 2) + 
  geom_path(aes(group = Subject, color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  theme(legend.position = "bottom") + 
  ggtitle("Topographic map of regression parameters") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/topgraphic-map-1-1.png" title="The scatterplot of shrinkage of regression parameters updated with contour lines to show different confidence regions." alt="The scatterplot of shrinkage of regression parameters updated with contour lines to show different confidence regions." width="80%" style="display: block; margin: auto;" />

The ellipses provide a little more information about where the estimates are 
being pulled. Even if some of the points are not being pulled directly towards 
the center of gravity, nearlly all of them are being pulled into a higher
confidence region.

There are a few tweaks we might consider for this plot. I don't think the ovals
need to be contained in the plot. The points are more important, and the
plotting boundaries should be set with respect to the points. We can redefine
the limits by using `coord_cartesian()`.


```r
last_plot() +
  coord_cartesian(
    xlim = range(df_pulled$Intercept), 
    ylim = range(df_pulled$Slope_Days),
    expand = TRUE) 
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/topographic-map-2-1.png" title="Tweak of the above plot to cut off some of the ellipses so the focus is on the data." alt="Tweak of the above plot to cut off some of the ellipses so the focus is on the data." width="80%" style="display: block; margin: auto;" />

To go all out :sunglasses:, let's also label the contours with the confidence
levels. I see that the lower left area is relatively free of points, so I can
place the labels there. I filter down to just the ellipse points in the bottom
25% of _x_ and _y_ values. That will keep points in that lower left quadrant.
Then I find the (_x_, _y_) point with the farthest distance from the center as the
location for my label. I make it sound so easy but it took a lot of trial and
error (including an an attempt to use cosines).


```r
# Euclidean distance
contour_dist <- function(xs, ys, center_x, center_y) {
  x_diff <- (center_x - xs) ^ 2
  y_diff <- (center_y - ys) ^ 2
  sqrt(x_diff + y_diff)
}

# Find the point to label in each ellipse.
df_label_locations <- df_ellipse %>% 
  group_by(level) %>%
  filter(Intercept < quantile(Intercept, .25), 
         Slope_Days < quantile(Slope_Days, .25)) %>% 
  # Compute distance from center.
  mutate(dist = contour_dist(Intercept, Slope_Days, 
                             fixef(m)[1], fixef(m)[2])) %>% 
  # Keep smallest values.
  top_n(-1, wt = dist) %>% 
  ungroup()

# Tweak the last plot one more time!
last_plot() +
  geom_text(aes(label = level, color = NULL), data = df_label_locations, 
            nudge_x = .5, nudge_y = .8, size = 3.5, color = "grey40")
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/topographic-map-3-1.png" title="Final variant of the above plot with the confidence regions labelled." alt="Final variant of the above plot with the confidence regions labelled." width="80%" style="display: block; margin: auto;" />

Are you feeling satisfied? I feel satisfied.


## Bonus: Plotting lines from a Bayesian mixed effects model

This last part is more of a code demo than a walkthough. I call myself a
Bayesian. Visualizing uncertainty is [one of my things
here](/visualizing-uncertainty-rstanarm/), so I would be remiss if I didn't also
demo how to do some plots using posterior samples. 

Conceptually, the classical model above estimated a single set of partially 
pooled regression lines. With the Bayesian model, we can sample from a posterior
distribution of partially pooled regression lines. Instead of one line for each 
participant, there's an entire distribution of them for each participant. This
distribution lets us quantify our uncertainty about each part of our model.

First, we fit the model in RStanARM with weakly informative priors. 


```r
library(rstanarm)
#> Loading required package: Rcpp
#> rstanarm (Version 2.15.3, packaged: 2017-04-29 06:18:44 UTC)
#> - Do not expect the default priors to remain the same in future rstanarm versions.
#> Thus, R scripts should specify priors explicitly, even if they are just the defaults.
#> - For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores())
```


```r
b <- stan_glmer(
  Reaction ~ Days + (Days | Subject),
  family = gaussian(),
  data = df_sleep,
  prior = normal(0, 2),
  prior_intercept = normal(0, 5),
  prior_covariance = decov(regularization = 2),
  prior_aux = cauchy(0, 1))
```

We get a similar overview as `arm::display()` when we print the model.


```r
b
#> stan_glmer
#>  family:  gaussian [identity]
#>  formula: Reaction ~ Days + (Days | Subject)
#> ------
#> 
#> Estimates:
#>             Median MAD_SD
#> (Intercept) 252.4    6.1 
#> Days         10.4    1.7 
#> sigma        25.7    1.5 
#> 
#> Error terms:
#>  Groups   Name        Std.Dev. Corr
#>  Subject  (Intercept) 24           
#>           Days         7       0.07
#>  Residual             26           
#> Num. levels: Subject 20 
#> 
#> Sample avg. posterior predictive 
#> distribution of y (X = xbar):
#>          Median MAD_SD
#> mean_PPD 297.9    2.7 
#> 
#> ------
#> For info on the priors used see help('prior_summary.stanreg').
```

We have posterior distribution of values now! That means instead of one "center 
of gravity" point, we have 4,000 plausible points for our central value. The 
center of our former contour plot has its own contour plot. That's Bayes for 
you. We can plot that easily with `stat_density_2d()`. We set the coordinate
limits to be the same as the last plot, just so that we don't exaggerate the
uncertainty around the central point by drawing a gigantic contour surface.


```r
# Get a dataframe: One row per posterior sample
df_posterior <- b %>% 
  as.data.frame() %>% 
  as_tibble()

ggplot(df_posterior) + 
  aes(x = `(Intercept)`, y = `Days`) + 
  # Calculate the density
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  ggtitle("Where's the average intercept and slope?") + 
  xlab("Estimate for average intercept") + 
  ylab("Estimate for average slope") +
  # Use the same coordinate limits as last plot
  coord_cartesian(
    xlim = range(df_pulled$Intercept), 
    ylim = range(df_pulled$Slope_Days),
    expand = TRUE) + 
  guides(fill = "none")
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/posterior-of-central-point-1.png" title="Contour map of the posterior values of the average intercept and slope values." alt="Contour map of the posterior values of the average intercept and slope values." width="80%" style="display: block; margin: auto;" />

For each participant, we have 4,000 partially-pooled regression lines too, so we
can visualize our uncertainty for each participant's individual regression line.

Let's finish by drawing a sample of those lines for a faceted plot. We have to
do a bunch of data wrangling to get a dataframe with one row per subject per 
posterior sample.


```r
# For each sample, add the average intercept and average slope values to each
# participant's deviation from that average. These yields the intercept and
# slope parameters for each participant.
df_effects <- df_posterior %>%
  # Find all the columns with the pattern "b[(Intercept". Add the column
  # df_posterior$`(Intercept)` to each of those columns.
  mutate_at(
    .vars = vars(matches("b\\[\\(Intercept")), 
    .funs = funs(. + df_posterior$`(Intercept)`)) %>%
  # Again for slope
  mutate_at(
    .vars = vars(matches("b\\[Day")), 
    .funs = funs(. + df_posterior$Days))

# Convert to a long format
df_long_effects <- df_effects %>%
  select(matches("b\\[")) %>%
  rowid_to_column("draw") %>%
  tidyr::gather(Parameter, Value, -draw)

# Extract the effect type and subject number from each parameter name
df_long_effects$Type <- df_long_effects$Parameter %>%
  stringr::str_detect("Intercept") %>%
  ifelse(., "Intercept", "Slope_Day")

df_long_effects$Subject <- df_long_effects$Parameter %>%
  stringr::str_extract("\\d\\d\\d")

df_long_effects <- df_long_effects %>% 
  select(draw, Subject, Effect = Type, Value)

# Finally!
df_long_effects
#> # A tibble: 160,000 x 4
#>     draw Subject    Effect    Value
#>    <int>   <chr>     <chr>    <dbl>
#>  1     1     308 Intercept 236.5760
#>  2     2     308 Intercept 263.6090
#>  3     3     308 Intercept 237.3153
#>  4     4     308 Intercept 247.9472
#>  5     5     308 Intercept 265.3939
#>  6     6     308 Intercept 241.7141
#>  7     7     308 Intercept 255.3133
#>  8     8     308 Intercept 261.2169
#>  9     9     308 Intercept 236.9531
#> 10    10     308 Intercept 264.2734
#> # ... with 159,990 more rows
```

Now that we have the data in the right shape, we are going randomly choose 50
posterior samples and plot those lines alongside the observed data.


```r
df_samples <- df_long_effects %>%
  filter(draw %in% sample(1:4000, size = 50)) %>%
  tidyr::spread(Effect, Value)
df_samples
#> # A tibble: 1,000 x 4
#>     draw Subject Intercept Slope_Day
#>  * <int>   <chr>     <dbl>     <dbl>
#>  1    15     308  266.8775 15.109710
#>  2    15     309  204.5003  4.056596
#>  3    15     310  223.4707  5.505083
#>  4    15     330  255.6470  7.120754
#>  5    15     331  279.8910  6.648727
#>  6    15     332  270.2003  8.059170
#>  7    15     333  283.5603  5.315121
#>  8    15     334  258.0755 11.155002
#>  9    15     335  243.8579  3.091841
#> 10    15     337  284.9078 19.905983
#> # ... with 990 more rows

ggplot(df_sleep) +
  aes(x = Days, y = Reaction) +
  geom_abline(aes(intercept = Intercept, slope = Slope_Day), 
              data = df_samples, color = "#3366FF", alpha = .1) +
  geom_point() +
  facet_wrap("Subject") + 
  scale_x_continuous(breaks = 0:4 * 2) + 
  labs(x = xlab, y = ylab) 
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/posterior-of-indvidual-lines-1.png" title="Final trellis plot updated to show 50 regression lines for each participant. The lines fan out for the two participants with incomplete data." alt="Final trellis plot updated to show 50 regression lines for each participant. The lines fan out for the two participants with incomplete data." width="80%" style="display: block; margin: auto;" />

For the participants with complete data, the lines pile up and form a narrow 
band, indicating a low degree of uncertainty. In the final two panels, however,
we only have limited data, and the sample of lines fan out and cover many
different plausible trajectories.

The uncertainty is more dramatic if we draw a contour plot for each
participant&mdash;basically, drawing each participants' mostly likely locations in
the landscape of parameter values.


```r
ggplot(df_long_effects %>% tidyr::spread(Effect, Value)) + 
  aes(x = Intercept, y = Slope_Day) + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  facet_wrap("Subject") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") +
  theme(legend.position = "bottom") +
  guides(fill = "none")
```

<img src="/figs//2017-06-22-plotting-partial-pooling-in-mixed-effects-models/posterior-of-indvidual-parameters-1.png" title="Density contour plots for each participant to visualize the larger uncertainty in the participants with incomplete data." alt="Density contour plots for each participant to visualize the larger uncertainty in the participants with incomplete data." width="80%" style="display: block; margin: auto;" />

For 373 and 374, the contour regions/ink-splats are very tall: A lot of slope
values are plausible. The region for 374 is more off center and slightly narrow
than that of 373: That extra data point matters.

***

Funnily enough, this post started as a quick write-up of a [demo I 
wrote](http://rpubs.com/tjmahr/ggplot2-lme4-facet-plot), but it kind of spiraled
out of control. I hope this write-up helps students and users understand
mixed-effects models at a more intuitive level.

I had formally learned about these models twice in graduate school. In 
psychology, we were told to use them if we wanted to make inferences about a 
larger population of subjects or stimulus items. In educational psychology, we 
were told to use them to capture the sources of variances in a nested data-set:
Kids nested in classrooms nested in schools, etc.
It wasn't until I taught myself Bayesian stats that I learned about third reason
to use them: They pool information across different units, providing regularized
model estimates. I find this rationale most intuitive. The [Gelman and Hill
book](http://amzn.to/2rVRZmw) and [_Statistical
Rethinking_](http://amzn.to/2ty0C3T) both discuss the partial pooling
description of these models. (Ooooh, as I added the _Rethinking_ link, I just
noticed that I created a ggplot2 version of the plot from the cover of that
book. :satisfied:)
