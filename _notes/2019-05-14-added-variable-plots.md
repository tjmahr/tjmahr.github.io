---
title: "Added variable plots (partial regression plots)"
date: 2019-05-14
tags: [statistics, regression, visualization]
---

Here's what "added variable" plots are about.

  - Let `m = y ~ x1 + ...` be the full regression model
  - Let `m_y1 = y ~ ...` (all of the predictors except x1)
  - Let `m_x1 = x1 ~ ...` (regress x1 on those same predictors)
  - Let `m_partial = resid(m_y1) ~ resid(m_x1)`
  - The plot of `resid(m_x1)` versus `resid(m_y1)` is the added variable plot.
    It shows "X1 | others)" and "y | others"
  - Residuals in the final `m_partial` model will be same as residuals from full
    model `m`
  - Coefficient of `x1` in final `m_partial` model will be same as coefficient
    from full model `m`.

AV plots by hand and by `car::avPlot()`

<!--

``` r
par(mar = c(4, 4, 1, 1))

m <- lm(mpg ~ wt + hp + am, mtcars)
m_y1 <- lm(mpg ~ hp + am, mtcars)
m_x1 <- lm(wt ~ hp + am, mtcars)
m_partial <- lm(resid(m_y1) ~ resid(m_x1))

library(patchwork)
#> Warning: package 'patchwork' was built under R version 4.6.1
p1 <- wrap_elements(
  full = ~ plot(resid(m_x1), resid(m_y1))
) 
p2 <- wrap_elements(
  full = ~ car::avPlot(m, "wt")
)

p1 + p2
```

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-05-14-added-variable-plots/av-plots-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

``` r
all.equal(residuals(m_partial), residuals(m))
#> [1] TRUE

coef(m)["wt"]
#>        wt 
#> -2.878575
coef(m_partial)[2]
#> resid(m_x1) 
#>   -2.878575
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/graphics/par.html'>par</a></span><span class='o'>(</span>mar <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='m'>4</span>, <span class='m'>4</span>, <span class='m'>1</span>, <span class='m'>1</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/lm.html'>lm</a></span><span class='o'>(</span><span class='nv'>mpg</span> <span class='o'>~</span> <span class='nv'>wt</span> <span class='o'>+</span> <span class='nv'>hp</span> <span class='o'>+</span> <span class='nv'>am</span>, <span class='nv'>mtcars</span><span class='o'>)</span></span>
<span><span class='nv'>m_y1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/lm.html'>lm</a></span><span class='o'>(</span><span class='nv'>mpg</span> <span class='o'>~</span> <span class='nv'>hp</span> <span class='o'>+</span> <span class='nv'>am</span>, <span class='nv'>mtcars</span><span class='o'>)</span></span>
<span><span class='nv'>m_x1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/lm.html'>lm</a></span><span class='o'>(</span><span class='nv'>wt</span> <span class='o'>~</span> <span class='nv'>hp</span> <span class='o'>+</span> <span class='nv'>am</span>, <span class='nv'>mtcars</span><span class='o'>)</span></span>
<span><span class='nv'>m_partial</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/lm.html'>lm</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>resid</a></span><span class='o'>(</span><span class='nv'>m_y1</span><span class='o'>)</span> <span class='o'>~</span> <span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>resid</a></span><span class='o'>(</span><span class='nv'>m_x1</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://patchwork.data-imaginist.com'>patchwork</a></span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Warning: package 'patchwork' was built under R version 4.6.1</span></span>
<span><span class='nv'>p1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span></span>
<span>  full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'><a href='https://rdrr.io/r/graphics/plot.default.html'>plot</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>resid</a></span><span class='o'>(</span><span class='nv'>m_x1</span><span class='o'>)</span>, <span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>resid</a></span><span class='o'>(</span><span class='nv'>m_y1</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>)</span> </span>
<span><span class='nv'>p2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_elements.html'>wrap_elements</a></span><span class='o'>(</span></span>
<span>  full <span class='o'>=</span> <span class='o'>~</span> <span class='nf'>car</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/car/man/avPlots.html'>avPlot</a></span><span class='o'>(</span><span class='nv'>m</span>, <span class='s'>"wt"</span><span class='o'>)</span></span>
<span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>p1</span> <span class='o'>+</span> <span class='nv'>p2</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-05-14-added-variable-plots/av-plots-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/base/all.equal.html'>all.equal</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>residuals</a></span><span class='o'>(</span><span class='nv'>m_partial</span><span class='o'>)</span>, <span class='nf'><a href='https://rdrr.io/r/stats/residuals.html'>residuals</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/stats/coef.html'>coef</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span><span class='o'>[</span><span class='s'>"wt"</span><span class='o'>]</span></span>
<span><span class='c'>#&gt;        wt </span></span>
<span><span class='c'>#&gt; -2.878575</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/stats/coef.html'>coef</a></span><span class='o'>(</span><span class='nv'>m_partial</span><span class='o'>)</span><span class='o'>[</span><span class='m'>2</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; resid(m_x1) </span></span>
<span><span class='c'>#&gt;   -2.878575</span></span></pre>

What about interactions?
