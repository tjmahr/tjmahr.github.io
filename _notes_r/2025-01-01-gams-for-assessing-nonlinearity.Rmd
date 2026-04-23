---
title: "Use GAMs for assessing nonlinearity of effects"
date: 2025-01-01
tags: [gam, interactions, nonlinearity, moderation, methodology]
---

A [Data Colada post](https://datacolada.org/121) describes a problem and
one proposed solution: how to assess a moderator's interaction in a
model like `y ~ x + z + x:z`. One paper suggests binning the covariate
into thirds and fitting separate regressions on the bins. Uri Simonsohn
observes that this approach is biased when the `y ~ x` or `y ~ z`
relationship is nonlinear.

> The third problem is that if x and z in that x·z interaction are
> correlated, and either x or z impacts y non-linearly, the estimate of
> interaction term, d, in y=a+bx+cz+dxz is biased, and the binning
> estimator from the Political Analysis paper is also biased, possibly
> by the same amount.
> 
> Most notably, one is likely to find false-positive interactions, and
> marginal effects of the wrong sign.

The author instead advocates for using a "GAM simple slope" and point to
[their article
(DOI: 10.1177/2515245923120778)](https://urisohn.com/sohn_files/papers/interacting.pdf). 

The thing that I like most about the article from skimming it is that it
describes curves that are linear and ceiling out as "canopy" shaped.
