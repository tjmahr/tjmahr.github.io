---
title: "Controlling geom_lineribbon() legend order and aesthetics"
date: 2019-08-26
tags: [r, ggdist, ggplot2, visualization]
---



[This note dates back to 2019 but I've updated it to run with
ggplot2 4.0.0 in 2026.]

<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://tidyverse.tidyverse.org'>tidyverse</a></span><span class='o'>)</span></span>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://mjskay.github.io/ggdist/'>ggdist</a></span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>model</span> <span class='o'>&lt;-</span> <span class='nf'>rstanarm</span><span class='nf'>::</span><span class='nf'><a href='https://mc-stan.org/rstanarm/reference/stan_glm.html'>stan_glm</a></span><span class='o'>(</span><span class='nv'>Sepal.Length</span> <span class='o'>~</span> <span class='nv'>Sepal.Width</span>, <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>gaussian</a></span><span class='o'>(</span><span class='o'>)</span>, <span class='nv'>iris</span><span class='o'>)</span></span>
<span><span class='nv'>colors</span> <span class='o'>&lt;-</span> <span class='nf'>colorspace</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/colorspace/man/hcl_palettes.html'>sequential_hcl</a></span><span class='o'>(</span><span class='m'>5</span>, palette <span class='o'>=</span> <span class='s'>"Purples"</span><span class='o'>)</span></span></pre>

Here is the basic lineribbon output but we mapped the type of point
estimate to the color of the line.

<pre class='chroma'>
<span><span class='nv'>data_stats</span> <span class='o'>&lt;-</span> <span class='nv'>iris</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>tidyr</span><span class='nf'>::</span><span class='nf'><a href='https://tidyr.tidyverse.org/reference/expand.html'>expand</a></span><span class='o'>(</span></span>
<span>    Sepal.Width <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/Extremes.html'>min</a></span><span class='o'>(</span><span class='nv'>Sepal.Width</span><span class='o'>)</span>, <span class='nf'><a href='https://rdrr.io/r/base/Extremes.html'>max</a></span><span class='o'>(</span><span class='nv'>Sepal.Width</span><span class='o'>)</span>, length.out <span class='o'>=</span> <span class='m'>100</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>tidybayes</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html'>add_epred_draws</a></span><span class='o'>(</span><span class='nv'>model</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>ggdist</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/ggdist/reference/point_interval.html'>median_qi</a></span><span class='o'>(</span>.width <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='m'>.95</span>, <span class='m'>.8</span>, <span class='m'>.5</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>data_stats</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>x <span class='o'>=</span> <span class='nv'>Sepal.Width</span>, y <span class='o'>=</span> <span class='nv'>.epred</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>  <span class='nf'>ggdist</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/ggdist/reference/geom_lineribbon.html'>geom_lineribbon</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>fill <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/factor.html'>factor</a></span><span class='o'>(</span><span class='nv'>.width</span><span class='o'>)</span>, color <span class='o'>=</span> <span class='nv'>.point</span>, ymin <span class='o'>=</span> <span class='nv'>.lower</span>, ymax <span class='o'>=</span> <span class='nv'>.upper</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-08-26-ggdist-lineribbon-legend-trick/unnamed-chunk-3-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

We can integrate the legend and tweak the legend to make the output
nicer. We have to set the fill for the last square, and I use the
RBG+alpha format to make it transparent.

<pre class='chroma'>
<span></span>
<span><span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>data_stats</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>x <span class='o'>=</span> <span class='nv'>Sepal.Width</span>, y <span class='o'>=</span> <span class='nv'>.epred</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>  <span class='nf'>ggdist</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/ggdist/reference/geom_lineribbon.html'>geom_lineribbon</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>fill <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/factor.html'>factor</a></span><span class='o'>(</span><span class='nv'>.width</span><span class='o'>)</span>, color <span class='o'>=</span> <span class='nv'>.point</span>, ymin <span class='o'>=</span> <span class='nv'>.lower</span>, ymax <span class='o'>=</span> <span class='nv'>.upper</span><span class='o'>)</span>,</span>
<span>  <span class='o'>)</span>  <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/scale_manual.html'>scale_color_manual</a></span><span class='o'>(</span></span>
<span>    aesthetics <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>"fill"</span>, <span class='s'>"color"</span><span class='o'>)</span>,</span>
<span>    breaks <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>"0.5"</span>, <span class='s'>"0.8"</span>, <span class='s'>"0.95"</span>, <span class='s'>"median"</span><span class='o'>)</span>,</span>
<span>    values <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='nv'>colors</span><span class='o'>[</span><span class='m'>2</span><span class='o'>:</span><span class='m'>4</span><span class='o'>]</span>, <span class='nv'>colors</span><span class='o'>[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/guides.html'>guides</a></span><span class='o'>(</span></span>
<span>    fill <span class='o'>=</span> <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/guide_legend.html'>guide_legend</a></span><span class='o'>(</span></span>
<span>      title <span class='o'>=</span> <span class='s'>"Posterior intervals"</span>,</span>
<span>      override.aes <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span></span>
<span>        fill <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='nv'>colors</span><span class='o'>[</span><span class='m'>2</span><span class='o'>:</span><span class='m'>4</span><span class='o'>]</span>, <span class='s'>"#FFFFFFFF"</span><span class='o'>)</span></span>
<span>      <span class='o'>)</span></span>
<span>    <span class='o'>)</span>,</span>
<span>    color <span class='o'>=</span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/guide_legend.html'>guide_legend</a></span><span class='o'>(</span>title <span class='o'>=</span> <span class='s'>"Posterior intervals"</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span> </span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggtheme.html'>theme_bw</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/theme.html'>theme</a></span><span class='o'>(</span>legend.position <span class='o'>=</span> <span class='s'>"top"</span>, legend.justification <span class='o'>=</span> <span class='s'>"left"</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2019-08-26-ggdist-lineribbon-legend-trick/unnamed-chunk-4-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>
