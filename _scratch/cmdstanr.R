# https://rpubs.com/samuelkn/CovarianceStructuresInR

library(cmdstanr)
library(posterior)
# # library(bayesplot)
# # color_scheme_set("brightblue")
# #
# # cmdstan_path()
# # cmdstan_version()
#
# file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
# mod <- cmdstan_model(file)
#
#
# file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
# mod <- cmdstan_model(file)
#

d <- nlme::Rail

m2 <- "
data {
  int<lower=0> N;               // n data
  int<lower=0> y[N];            // data
  int<lower=1> L;               // groups
  int<lower=1,upper=L> ll[N];   // group membership
}

parameters {
  real<lower=0> sigma_y;
  real<lower=0> sigma_l;

  real mu[L];
}

model {

  sigma_l ~ cauchy(0, 20);
  sigma_y ~ cauchy(0, 20);

  for (l in 1:L) {
    mu[l] ~ normal(0, sigma_l);
  }

  for (n in 1:N) {
    y[n] ~ normal(mu[ll[n]], sigma_y);
  }
}
"
# try using an intercept term too?
# no result
m2_big <- "
data {
  int<lower=0> N;               // n data
  int<lower=0> y[N];            // data
  int<lower=1> L;               // groups
  int<lower=1,upper=L> ll[N];   // group membership
}

parameters {
  real<lower=0> sigma_y;
  real<lower=0> sigma_l;
  real mu[L];
}

model {
  matrix[N,N] bigma;
  vector[N] bigmu;
  vector[N] e;
  vector[N] zeroes;

  sigma_l ~ cauchy(0, 20);
  sigma_y ~ cauchy(0, 20);

  for (i in 1:N) {
    for (j in 1:N) {
      if (ll[i] == ll[j]) {
        if (i == j) {
          bigma[i,j] = sigma_l ^ 2 + sigma_y ^ 2;
        } else {
          bigma[i,j] = sigma_l ^ 2;
        }
      } else {
        bigma[i,j] = 0.0;
      }
    }
  }

  for (n in 1:N) {
    bigmu[n] = mu[ll[n]];
    zeroes[n] = 0;
  }

  e = to_vector(y) - bigmu;
  e ~ multi_normal(zeroes, bigma);
}
"

mod2_big <- cmdstan_model(write_stan_tempfile(m2_big))
rstan::stan_model(write_stan_tempfile(m2_big))




dl2 <- list(
  N = nrow(d),
  y = d$travel,
  L = length(unique(d$Rail)),
  ll = as.numeric(d$Rail)
)
[[85.3482,67.4657,67.4657,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [67.4657,85.3482,67.4657,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [67.4657,67.4657,85.3482,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,85.3482,67.4657,67.4657,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,67.4657,85.3482,67.4657,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,67.4657,67.4657,85.3482,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,85.3482,67.4657,67.4657,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,67.4657,85.3482,67.4657,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,67.4657,67.4657,85.3482,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,85.3482,67.4657,67.4657,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,67.4657,85.3482,67.4657,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,67.4657,67.4657,85.3482,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,85.3482,67.4657,67.4657,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,67.4657,85.3482,67.4657,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,67.4657,67.4657,85.3482,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,85.3482,67.4657,67.4657],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,67.4657,85.3482,67.4657],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,67.4657,67.4657,85.3482]]

sigma_y <- 4
sigma_l <- 70
N = nrow(d)
bigma <- matrix(rep(1:N, N), nrow = N, ncol = N)
ll <- dl2$ll

for (i in 1:N) {
  for (j in 1:N) {
    bigma[i,j] = 0.0;
    if (i == j) {
      bigma[i,j] = bigma[i,j] + sigma_y ^ 2;
    }
    if (ll[i] == ll[j]) {
      bigma[i,j] = bigma[i,j] + sigma_l ^ 2;
    }
  }
}



# d$Rail
# outer(d$Rail, d$Rail)

x <- posterior::as_draws(fit2$draws())

subset_draws(x, variable = "bigma")

posterior::


dl2 <- list(
  N = nrow(d),
  y = d$travel,
  L = length(unique(d$Rail)),
  ll = as.numeric(d$Rail)
)


fit2 <- mod2_big$sample(
  data = dl2,
  # seed = 123,
  chains = 4,
  parallel_chains = 2,
  refresh = 500,
  adapt_delta = .95,
  max_treedepth = 20
)


fit2

fit

s <- lme4::lmer(travel ~ 0 + (1 | Rail), nlme::Rail)
coef(s)
lme4::VarCorr(s)

library(lmeSplines)
data(smSplineEx1)
# variable `all' for top level grouping
smSplineEx1$all <- rep(1,nrow(smSplineEx1))
# setup spline Z-matrix
smSplineEx1$Zt <- smspline(~ time, data=smSplineEx1)
fit1s <- lme(y ~ time, data=smSplineEx1,
             random=list(all=pdIdent(~Zt - 1)))

summary(reStruct(pdIdent(~Zt - 1, data = smSplineEx1)))

smSplineEx1




library(nlme)
?pdIdent

d
p <- pdBlocked(1, ~factor(Rail), data = d)
p

summary(lme(travel ~ 0, d, ~ 1 | Rail))
# r <- lme(travel ~ 0, d, list(Rail = pdIdent(~ 1)), weights = varIdent( ~ Rail))



?kronecker
# simple scalar multiplication
( M <- matrix(1:6, ncol = 2) )
kronecker(4, M)
# Block diagonal matrix:
kronecker(diag(1, 3), M)

x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:8; names(y) <- paste(y,":", sep = "")
outer(y, x, "^")

outer(month.abb, 1999:2003, FUN = "paste")

## three way multiplication table:
x %o% x %o% y[1:3]

# ask for dimnames

fred <- matrix(1:12, 3, 4, dimnames = list(LETTERS[1:3], LETTERS[4:7]))
bill <- c("happy" = 100, "sad" = 1000)
kronecker(fred, bill, make.dimnames = TRUE)

bill <- outer(bill, c("cat" = 3, "dog" = 4))
kronecker(fred, bill, make.dimnames = TRUE)

q <- lme(travel ~ 0, d, list(Rail = pdIdent(~ 1)))
summary(q)
VarCorr(q)

d$set = "all"

# Same variance for all levels
r <- lme(travel ~ 0, d, random = list(set = pdIdent(~ Rail - 1)))
VarCorr(r)

# lmeStruct(): lmeStruct(reStruct, corStruct, varStruct)

a <- reStruct(list(Rail = pdIdent(~ 1)), data = d)

corSt
lmeStruct(a)


r
vcov(r)

reStruct(list(Rail = pdIdent(~ 1)), data = d)

as.matrix(reStruct(p))

pd1 <- pdBlocked(list(diag(1:2), diag(c(0.1, 0.2, 0.3))),
                 nam = list(c("A","B"), c("a1", "a2", "a3")))
pd1
pd1 <- pdCompSymm(diag(4) + 1, nam = c("A","B","C", "D"))
pd1

str(p)
reStruct(pdIdent(~Rail, data = d))
pdM(pdIdent(~Rail, data = d))

rs1 <- reStruct(pdSymm(diag(3), ~age+Sex, data = Orthodont))
pdMatrix(rs1)



# fit_o <- mod2$optimize(
#   data = dl2,
#   init = list(list(`mu[1]` = 100, `mu[2]` = 100, `mu[3]` = 100, sigma_y = 100, sigma_l = 100)),
#   algorithm = "bfgs",
#   seed = 123,
#   iter = 200000
# )
#
# fit_o


fit
coef(lme4::lmer(travel ~ 0 + (1 | Rail), nlme::Rail))
# names correspond to the data block in the Stan program

data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 2,
  refresh = 500
)


fit$summary()
fit$cmdstan_summary()
fit$sampler_diagnostics()

# install_cmdstan(cores = 4, overwrite = TRUE)
# rebuild_cmdstan(cores = 4)

cmdstanr:::build_cmdstan
cmdstanr:::make_cmd

stanfit <- rstan::read_stan_csv(fit$output_files())



xxxm3 <- "
data {
  int<lower=0> N;               // n data
  int<lower=0> y[N];            // data
  int<lower=1> L;               // groups
  int<lower=1,upper=L> ll[N];   // group membership
}

parameters {
  real<lower=0> sigma_y;
  real<lower=0> sigma_l;

  vector[L] mu;
}
transformed parameters {
  // vector e[L + 1];
  // e = append_row[mu, sigma_y];

  vector[L] e1;
  vector[N] e2;
  vector[L + N] long_mu;
  vector[L + N] zeros;
  cov_matrix[L + N] m;

  e1 = e1 + sigma_l;
  e2 = e2 + sigma_y;

  m = diag_matrix(append_row(e1, e2));

  for (i in 1:L) {
    long_mu[i] = mu[i];
    zeros[i] = 0;
  }
  long_mu[L + 1] = 0.0;
  zeros[L + 1] = 0;

}

model {

  sigma_l ~ normal(0, 50);
  sigma_y ~ normal(0, 50);
  long_mu ~ multi_normal(long_mu, m);
}
"
