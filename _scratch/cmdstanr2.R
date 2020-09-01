library(cmdstanr)

d <- nlme::Rail
d <- rstanarm::radon
d$Rail <- d$county
# d$travel <- round(d$log_radon * 100) + 40
d$travel <- d$log_radon * 10


data <- list(
  N = nrow(d),
  y = d$travel,
  L = length(unique(d$Rail)),
  ll = as.numeric(d$Rail)
)



m <- "
data {
  int<lower=0> N;               // n data
  int<lower=0> y[N];            // data
  int<lower=1> L;               // groups
  int<lower=1,upper=L> ll[N];   // group membership
}

parameters {
  real<lower=0> sigma_y;
  real<lower=0> sigma_l;
  real grand_mean;
  real group_effects[L];
}

model {

  sigma_l ~ normal(0, 50);
  sigma_y ~ normal(0, 50);
  grand_mean ~ normal(0, 100);

  group_effects ~ normal(0, sigma_l);

  for (n in 1:N) {
    y[n] ~ normal(grand_mean + group_effects[ll[n]], sigma_y);
  }
}
"

m_smooth <- "
data {
  int<lower=0> N;               // n data
  // int<lower=0> y[N];            // data
  real y[N];            // data
  int<lower=1> L;               // groups
  int<lower=1,upper=L> ll[N];   // group membership
}

parameters {
  real<lower=0> sigma_y;
  real<lower=0> lambda;
  real grand_mean;
  real group_effects[L];
}

transformed parameters {
  real<lower=0> sigma_l = sqrt(square(sigma_y) / lambda);
}

model {
  lambda ~ exponential(.1);
  sigma_y ~ normal(0, 50);
  grand_mean ~ normal(0, 100);
  group_effects ~ normal(0, sigma_y / sqrt(lambda));

  for (n in 1:N) {
    y[n] ~ normal(grand_mean + group_effects[ll[n]], sigma_y);
  }
}

generated quantities {
  real<lower=0> l_check = square(sigma_y) / square(sigma_l);
}
"

hist(rexp(1000, .1))

mod_smooth <- cmdstan_model(write_stan_tempfile(m_smooth))

fit_smooth <- mod_smooth$sample(
  data = data,
  seed = 123,
  chains = 4,
  parallel_chains = 2,
  refresh = 500
)

fit_smooth$summary("sigma_l")
#
# fit_smooth2 <- mod_smooth$optimize(
#   data = data,
#   # seed = 123,
#   algorithm = "lbfgs",
#   # chains = 4,
#   # parallel_chains = 2,
#   refresh = 500
# )
# fit_smooth2
library(mgcv)
gam <- gam(
  travel ~ 1 + s(Rail, bs = "re"),
  method = "ML",
  data = d
)
summary(gam)
gam$sp


#>    s(Rail) // when d = Rail
#> 0.03158409

gam.vcomp(gam)
gam.vcomp(gam, rescale = FALSE)

(7.660732 ^ 2) / (2.92 ^ 2)


fit_smooth$summary()
draws <- posterior::as_draws_df(fit_smooth$draws(c("lambda", "sigma_l", "lp__")))
hist(draws$lambda)

ggplot(draws) +
  aes(x = lambda, y = sigma_l ) +
  geom_point() +
  geom_hex(bins = 50) +
  geom_vline(xintercept = gam$sp, color = "orange")

ggplot(draws) +
  aes(x = lambda) +
  geom_histogram(binwidth = .5) +
  geom_vline(xintercept = gam$sp, color = "orange") +
  NULL
  # annotate("text", x = .045, y = 550, label = "mgcv estimate", hjust = 0, color = "orange")



fit_smooth$summary("lambda")
#> # A tibble: 1 x 10
#>   variable   mean median     sd    mad     q5   q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 lambda   0.0631 0.0486 0.0573 0.0336 0.0122 0.160  1.00     807.    1036.

tjmisc::ggpreview()



# https://stackoverflow.com/questions/38644943/mgcv-how-to-return-estimated-smoothing-parameter
library(mgcv)
gam <- gam(travel ~ 1 + s(Rail, bs = "re"), method = "ML", data = Rail)
(gam.vcomp(gam)[2] ^ 2) / (gam.vcomp(gam)[1] ^ 2)

lme <- lme4::lmer(travel ~ 1 + (1 |Rail), REML = FALSE, data = Rail)
summary(lme)
16.17 / 511.86

(gam.vcomp(gam)[2] ^ 2) / (gam.vcomp(gam)[1] ^ 2)


gam$sp

library(tidyverse)
l_h <- gam[["smooth"]][[1]][["S.scale"]]
l_h

gam$gcv.ubre


gam$scale
gam$sig2

gam.check(gam)

summary(gam)
coef(gam)












mod <- cmdstan_model(write_stan_tempfile(m))

fit <- mod$sample(
  data = data,
  seed = 123,
  chains = 4,
  parallel_chains = 2,
  refresh = 500
)

fit

lme <- lme4::lmer(travel ~ 1 + (1 | Rail), data = Rail)
VarCorr(lme)
fit$summary(c("sigma_l", "sigma_y"))

ranef(lme)[["Rail"]]
fit$summary("group_effects")

fixef(lme)
fit$summary("grand_mean")

m_big <- "
data {
  int<lower=0> N;               // n data
  int<lower=0> y[N];            // data
  int<lower=1> L;               // groups
  int<lower=1,upper=L> ll[N];   // group membership
}
transformed data {
  real group_counts[L];

  group_counts = rep_array(0.0, L);

  for (i in 1:N) {
    group_counts[ll[i]] += ll[i];
  }

  real hm = mean(group_counts);

}
parameters {
  real<lower=0> sigma_y;
  real<lower=0> sigma_l;
  real grand_mean;
}
transformed parameters {
  real var_between = square(sigma_l);
  real var_within = square(sigma_y);
  real var_total = var_within + var_between;
}
model {
  matrix[N,N] bigma;
  vector[N] e;
  vector[N] zeroes;

  zeroes = rep_vector(0.0, N);
  bigma = rep_matrix(0.0, N, N);

  sigma_l ~ normal(0, 50);
  sigma_y ~ normal(0, 50);
  grand_mean ~ normal(0, 100);

  for (i in 1:N) {
    for (j in 1:N) {
      if (ll[i] == ll[j]) {
        if (i == j) {
          bigma[i,j] = var_total;
        } else {
          // If you divide everything by var_total this becomes
          // var_between / var_total which is the proportion of
          // of variance explained by between-group variance which
          // is the ICC
          bigma[i,j] = var_between;
        }
      }
    }
  }

  e = to_vector(y) - grand_mean;
  e ~ multi_normal(zeroes, bigma);
}
generated quantities {
  // how can we recover the group means?
  real<lower = 0, upper = 1> icc;
  icc = var_between / var_total;
  real mean_1 = mean((to_vector(y) - grand_mean)[1:3]);
  real mean_2 = mean((to_vector(y) - grand_mean)[4:6]);

  // trying to do effective sample size
  real deff = 1 + (hm - 1) * icc;
}
"

big_mod <- cmdstan_model(write_stan_tempfile(m_big))


fit_big <- big_mod$sample(
  data = data,
  seed = 123,
  chains = 4,
  parallel_chains = 2,
  refresh = 500
)

fit_big$summary()
fit_big$draws("deff")


#
# library(brms)
# library(rstan)
# mbo <- brm(travel ~ 1 + (1 | Rail), data = nlme::Rail)
# mbo
fit
fit_big

ranef(lme4::lmer(travel ~ 1 + (1 | Rail), nlme::Rail))
lme4::VarCorr(lme4::lmer(travel ~ 1 + (1 | Rail), nlme::Rail))
performance::icc(lme4::lmer(travel ~ 1 + (1 | Rail), nlme::Rail))



# we found a correspondence between smoothing penalty and variances in a random
# intercept model. now let's do the same between smoothing model.


d2 <- MASS::mcycle
plot(d2$times, d2$accel)
library(brms)
bm <- brm(accel ~ s(times, bs = "cr"), data = d2, backend = "cmdstanr")
bm

bm$model
conditional_smooths(bm)
conditional_effects(bm)

gg <- gam(accel ~ times + s(times, bs = "cr"), data = d2, method = "ML")
summary(gg)

# wait do i want raw or rescaled?
bm
gam.vcomp(gg, rescale = FALSE)
gam.vcomp(gg, rescale = TRUE)

draws <- posterior_samples(bm, pars = c("sds_stimes_1", "sigma"))

# scale check
ggplot(draws) +
  aes(x = sigma) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 22.31167, color = "orange") +
  NULL

# lambda? check
ggplot(draws) +
  aes(x = sigma^2 / sds_stimes_1^2) +
  geom_histogram(binwidth = .2) +
  geom_vline(xintercept = gg$sp, color = "orange") +
  NULL


ggplot(draws) +
  aes(x = sigma) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 8.40925, color = "orange") +
  NULL
  # annotate("text", x = .045, y = 550, label = "mgcv estimate", hjust = 0, color = "orange")

ggplot(draws) +
  aes(x = lambda, y = sigma_l ) +
  geom_point() +
  geom_hex(bins = 50) + geom_vline(xintercept = .03, color = "orange")

# https://stackoverflow.com/questions/51658946/gam-with-gp-smoother-how-to-retrieve-the-variogram-parameters


vcr <- gam.vcomp(gg, rescale = FALSE)
vcr[2] ^ 2 / vcr[1] ^ 2

vct <- gam.vcomp(gg, rescale = TRUE)
vct[2] ^ 2 / vct[1] ^ 2


??gamlss::s

# scaling <- gam$smooth[[1]][["S.scale"]]
# smoothing <- gam$sp
# scaling * smoothing
