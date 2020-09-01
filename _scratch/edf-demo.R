library(lme4)
library(mgcv)
library(brms)

m <- lmer(Reaction ~ 1 + (1 | Subject), sleepstudy)
hats <- hatvalues(m)
sum(hats) - 1  # minus intercept
#> [1] 14.74114

g <- gam(
  Reaction ~ 1 + s(Subject, bs = "re"),
  data = sleepstudy
)
summary(g)[["s.table"]]
#>                 edf Ref.df        F      p-value
#> s(Subject) 14.74113     17 6.525906 6.220283e-15

b <- brm(
  Reaction ~ 1 + (1 | Subject),
  sleepstudy,
  backend = "cmdstanr"
)
b
loo(b)
#>          Estimate   SE
#> elpd_loo   -947.6 10.8
#> p_loo        16.7  2.0  # edf plus intercept and sigma?
#> looic      1895.2 21.6

