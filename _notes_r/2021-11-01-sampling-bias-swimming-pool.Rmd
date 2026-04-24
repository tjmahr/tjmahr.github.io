---
title: "Sampling bias: the swimming pool analogy"
date: 2021-11-01
tags: [statistics, sampling, intuition]
---

Jorbs on his Slay the Spire stream
commented problems with big data. He used [a nice
analogy](https://www.youtube.com/watch?v=kHS-tFnJKqA&t=5712s) for
sampling bias. Imagine you have a swimming pool and we can't see the
bottom of it. We want to measure the average depth of it. No amount of
big data will accurately estimate the depth with all the samples come
from the shallow end of the pool. But if you smartly sample the pool
with a grid of points, then you can come up with an accurate estimate
from relatively few points. This example is nice because it can
illustrate accuracy versus precision.

The analogy also funnily resembles an analogy used for Hamilitonian
Monte Carlo: a puck skating around a smooth curved surface. If the pool
is the posterior distribution and the depth is the negative log
posterior density, then we want to explore that pool too. 
