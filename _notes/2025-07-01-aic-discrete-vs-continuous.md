---
title: "Don’t compare discrete and continuous models with AIC"
date: 2025-07-01
tags: [statistics, model-comparison, aic]
---

Don't use AIC to compare a discrete and a continuous model [says
Thomas Lumley][tl-aic]. "The loose way to talk about the problem is that the
continuous model’s loglikelihood comes from a probability density function while
the discrete model’s loglikelihood comes from a probability mass function, and
these are just two different things. You can’t divide one by the other."

[tl-aic]: https://notstatschat.rbind.io/2025/07/22/aic-and-combined-discrete-continuous-models/ "AIC and combined discrete/continuous models"
