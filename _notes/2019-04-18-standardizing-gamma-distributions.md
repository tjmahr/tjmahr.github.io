---
title: "Standardizing gamma distributions via scaling"
date: 2019-04-18
tags: [statistics, distributions, modeling]
---

A gamma times a positive non-zero constant is still a gamma. Paul and I
used this fact to standardize results from different models onto a
single "z" scale. Here's a made-up example.

Simulate some gamma-distributed data.

<!--

``` r
library(patchwork)
#> Warning: package 'patchwork' was built under R version 4.6.1
set.seed(20211122)
par(mar = c(4, 4, 1, 1))

y <- rgamma(n = 100, shape = 20)
y2 <- rgamma(n = 100, shape = 200)
wrap_elements(full = ~ hist(y)) + 
  wrap_elements(full = ~ hist(y2))
```

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-04-18-standardizing-gamma-distributions/gamma-hists-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>
-->
<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://patchwork.data-imaginist.com'>patchwork</a></span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Warning: package 'patchwork' was built under R version 4.6.1</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/Random.html'>set.seed</a></span><span class='o'>(</span><span class='m'>20211122</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/graphics/par.html'>par</a></span><span class='o'>(</span>mar <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='m'>4</span>, <span class='m'>4</span>, <span class='m'>1</span>, <span class='m'>1</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>y</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/GammaDist.html'>rgamma</a></span><span class='o'>(</span>n <span class='o'>=</span> <span class='m'>100</span>, shape <span class='o'>=</span> <span class='m'>20</span><span class='o'>)</span></span>
<span><span class='nv'>y2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/GammaDist.html'>rgamma</a></span><span class='o'>(</span>n <span class='o'>=</span> <span class='m'>100</span>, shape <span class='o'>=</span> <span class='m'>200</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span>full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'><a href='https://rdrr.io/r/graphics/hist.html'>hist</a></span><span class='o'>(</span><span class='nv'>y</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>  <span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span>full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'><a href='https://rdrr.io/r/graphics/hist.html'>hist</a></span><span class='o'>(</span><span class='nv'>y2</span><span class='o'>)</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-04-18-standardizing-gamma-distributions/gamma-hists-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

Fit a GLM

<!--

``` r
get_shape <- function(model) { 
  1 / summary(model)[["dispersion"]]
}

m <- glm(y ~ 1, Gamma(link = "identity"))
m2 <- glm(y2 ~ 1, Gamma(link = "identity"))

get_shape(m)
#> [1] 21.09197
get_shape(m2)
#> [1] 178.6259
```
-->
<pre class='chroma'>
<span><span class='nv'>get_shape</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>model</span><span class='o'>)</span> <span class='o'>{</span> </span>
<span>  <span class='m'>1</span> <span class='o'>/</span> <span class='nf'><a href='https://rdrr.io/r/base/summary.html'>summary</a></span><span class='o'>(</span><span class='nv'>model</span><span class='o'>)</span><span class='o'>[[</span><span class='s'>"dispersion"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/glm.html'>glm</a></span><span class='o'>(</span><span class='nv'>y</span> <span class='o'>~</span> <span class='m'>1</span>, <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>Gamma</a></span><span class='o'>(</span>link <span class='o'>=</span> <span class='s'>"identity"</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='nv'>m2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/glm.html'>glm</a></span><span class='o'>(</span><span class='nv'>y2</span> <span class='o'>~</span> <span class='m'>1</span>, <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>Gamma</a></span><span class='o'>(</span>link <span class='o'>=</span> <span class='s'>"identity"</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 21.09197</span></span>
<span><span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m2</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 178.6259</span></span></pre>

The two `z` values have similar scales.

<!--

``` r
par(mar = c(4, 4, 1, 1))
r <- residuals(m)
r2 <- residuals(m2)

z <- y / fitted(m)
z2 <- y2 / fitted(m2)

p1 <- wrap_elements(
  full = ~ car::qqPlot(y, distribution = "gamma", shape = get_shape(m))
)
p2 <- wrap_elements(
  full = ~ car::qqPlot(y2, distribution = "gamma", shape = get_shape(m2))
)
p1 + p2
```

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-04-18-standardizing-gamma-distributions/z-gams-qqs-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

``` r

p3 <- wrap_elements(
  full = ~ car::qqPlot(
    z, 
    distribution = "gamma", 
    shape = get_shape(m), 
    scale = 1 / get_shape(m))
)  
p4 <- wrap_elements(
  full = ~ car::qqPlot(
    z2, 
    distribution = "gamma", 
    shape = get_shape(m2), 
    scale = 1 / get_shape(m2))
)
p3 + p4
```

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-04-18-standardizing-gamma-distributions/z-gams-qqs-2.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/graphics/par.html'>par</a></span><span class='o'>(</span>mar <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='m'>4</span>, <span class='m'>4</span>, <span class='m'>1</span>, <span class='m'>1</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='nv'>r</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>residuals</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span></span>
<span><span class='nv'>r2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>residuals</a></span><span class='o'>(</span><span class='nv'>m2</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>z</span> <span class='o'>&lt;-</span> <span class='nv'>y</span> <span class='o'>/</span> <span class='nf'><a href='https://rdrr.io/r/stats/fitted.values.html'>fitted</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span></span>
<span><span class='nv'>z2</span> <span class='o'>&lt;-</span> <span class='nv'>y2</span> <span class='o'>/</span> <span class='nf'><a href='https://rdrr.io/r/stats/fitted.values.html'>fitted</a></span><span class='o'>(</span><span class='nv'>m2</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>p1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span></span>
<span>  full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'>car</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/car/man/qqPlot.html'>qqPlot</a></span><span class='o'>(</span><span class='nv'>y</span>, distribution <span class='o'>=</span> <span class='s'>"gamma"</span>, shape <span class='o'>=</span> <span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>)</span></span>
<span><span class='nv'>p2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span></span>
<span>  full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'>car</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/car/man/qqPlot.html'>qqPlot</a></span><span class='o'>(</span><span class='nv'>y2</span>, distribution <span class='o'>=</span> <span class='s'>"gamma"</span>, shape <span class='o'>=</span> <span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m2</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>)</span></span>
<span><span class='nv'>p1</span> <span class='o'>+</span> <span class='nv'>p2</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-04-18-standardizing-gamma-distributions/z-gams-qqs-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

<pre class='chroma'>
<span></span>
<span><span class='nv'>p3</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span></span>
<span>  full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'>car</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/car/man/qqPlot.html'>qqPlot</a></span><span class='o'>(</span></span>
<span>    <span class='nv'>z</span>, </span>
<span>    distribution <span class='o'>=</span> <span class='s'>"gamma"</span>, </span>
<span>    shape <span class='o'>=</span> <span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span>, </span>
<span>    scale <span class='o'>=</span> <span class='m'>1</span> <span class='o'>/</span> <span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>)</span>  </span>
<span><span class='nv'>p4</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span></span>
<span>  full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'>car</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/car/man/qqPlot.html'>qqPlot</a></span><span class='o'>(</span></span>
<span>    <span class='nv'>z2</span>, </span>
<span>    distribution <span class='o'>=</span> <span class='s'>"gamma"</span>, </span>
<span>    shape <span class='o'>=</span> <span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m2</span><span class='o'>)</span>, </span>
<span>    scale <span class='o'>=</span> <span class='m'>1</span> <span class='o'>/</span> <span class='nf'>get_shape</span><span class='o'>(</span><span class='nv'>m2</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>)</span></span>
<span><span class='nv'>p3</span> <span class='o'>+</span> <span class='nv'>p4</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-04-18-standardizing-gamma-distributions/z-gams-qqs-2.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>
