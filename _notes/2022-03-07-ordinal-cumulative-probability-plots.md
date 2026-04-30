---
title: "Visualizing ordinal model probabilities with ribbons"
date: 2022-03-07
tags: [r, ordinal, ggplot2, visualization]
---




Here is a ggplot2 recipe for plotting cumulative
probabilities from ordinal regression models.

*** 

We have `rating` scores of various wines (1--5 point scale), and during the
production, temperature (`temp`) and skin `contact` were experimentally
controlled.

<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://tidyverse.tidyverse.org'>tidyverse</a></span><span class='o'>)</span></span>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://github.com/runehaubo/ordinal'>ordinal</a></span><span class='o'>)</span></span>
<span><span class='nv'>wine</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://tibble.tidyverse.org/reference/as_tibble.html'>as_tibble</a></span><span class='o'>(</span><span class='nv'>wine</span><span class='o'>)</span></span>
<span><span class='nv'>wine</span></span>
<span><span class='c'>#&gt; # A tibble: 72 × 6</span></span>
<span><span class='c'>#&gt;    response rating temp  contact bottle judge</span></span>
<span><span class='c'>#&gt;       &lt;dbl&gt; &lt;ord&gt;  &lt;fct&gt; &lt;fct&gt;   &lt;fct&gt;  &lt;fct&gt;</span></span>
<span><span class='c'>#&gt;  1       36 2      cold  no      1      1    </span></span>
<span><span class='c'>#&gt;  2       48 3      cold  no      2      1    </span></span>
<span><span class='c'>#&gt;  3       47 3      cold  yes     3      1    </span></span>
<span><span class='c'>#&gt;  4       67 4      cold  yes     4      1    </span></span>
<span><span class='c'>#&gt;  5       77 4      warm  no      5      1    </span></span>
<span><span class='c'>#&gt;  6       60 4      warm  no      6      1    </span></span>
<span><span class='c'>#&gt;  7       83 5      warm  yes     7      1    </span></span>
<span><span class='c'>#&gt;  8       90 5      warm  yes     8      1    </span></span>
<span><span class='c'>#&gt;  9       17 1      cold  no      1      2    </span></span>
<span><span class='c'>#&gt; 10       22 2      cold  no      2      2    </span></span>
<span><span class='c'>#&gt; # ℹ 62 more rows</span></span>
<span></span>
<span><span class='nv'>model</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/pkg/ordinal/man/clm.html'>clm</a></span><span class='o'>(</span><span class='nv'>rating</span> <span class='o'>~</span> <span class='nv'>temp</span> <span class='o'>*</span> <span class='nv'>contact</span>, data <span class='o'>=</span> <span class='nv'>wine</span><span class='o'>)</span></span>
<span><span class='nv'>pred_grid</span> <span class='o'>&lt;-</span> <span class='nv'>wine</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/distinct.html'>distinct</a></span><span class='o'>(</span><span class='nv'>contact</span>, <span class='nv'>temp</span><span class='o'>)</span></span></pre>

`predict.clm(type = "cum.prob")` predictions provide two sets of
cumulative probabilities P(rating) ≤ rating<sub>*k*</sub> and
P(rating) \< rating<sub>*k*</sub>. The first set yields a probability
of 1 for the highest rating, and the second set have probability 0 for
the first rating.

<pre class='chroma'>
<span><span class='nv'>pred_grid</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/stats/predict.html'>predict</a></span><span class='o'>(</span><span class='nv'>model</span>, newdata <span class='o'>=</span> <span class='nv'>_</span>, type <span class='o'>=</span> <span class='s'>"cum.prob"</span><span class='o'>)</span> </span>
<span><span class='c'>#&gt; $cprob1</span></span>
<span><span class='c'>#&gt;             1          2         3         4 5</span></span>
<span><span class='c'>#&gt; 1 0.196035088 0.75833150 0.9669806 0.9929102 1</span></span>
<span><span class='c'>#&gt; 2 0.059595928 0.44919920 0.8838723 0.9732608 1</span></span>
<span><span class='c'>#&gt; 3 0.023374759 0.23547849 0.7419059 0.9321882 1</span></span>
<span><span class='c'>#&gt; 4 0.004323076 0.05291811 0.3427392 0.7137748 1</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; $cprob2</span></span>
<span><span class='c'>#&gt;   1           2          3         4         5</span></span>
<span><span class='c'>#&gt; 1 0 0.196035088 0.75833150 0.9669806 0.9929102</span></span>
<span><span class='c'>#&gt; 2 0 0.059595928 0.44919920 0.8838723 0.9732608</span></span>
<span><span class='c'>#&gt; 3 0 0.023374759 0.23547849 0.7419059 0.9321882</span></span>
<span><span class='c'>#&gt; 4 0 0.004323076 0.05291811 0.3427392 0.7137748</span></span></pre>

Each rating's probability is the difference between these two sets of
cumulative probabilities, so ultimately we will visualize them directly
as a ribbon between the two cumulative probabilities.

Get a long dataframe of probabilities.

<pre class='chroma'>
<span><span class='nv'>preds</span> <span class='o'>&lt;-</span> <span class='nv'>pred_grid</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/stats/predict.html'>predict</a></span><span class='o'>(</span><span class='nv'>model</span>, newdata <span class='o'>=</span> <span class='nv'>_</span>, type <span class='o'>=</span> <span class='s'>"cum.prob"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='nv'>as.data.frame</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/bind_rows.html'>bind_rows</a></span><span class='o'>(</span>.id <span class='o'>=</span> <span class='s'>"prob_type"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/cbind.html'>cbind</a></span><span class='o'>(</span><span class='nv'>pred_grid</span>, x <span class='o'>=</span> <span class='nv'>_</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/rename.html'>rename_with</a></span><span class='o'>(</span><span class='nf'>stringr</span><span class='nf'>::</span><span class='nv'><a href='https://stringr.tidyverse.org/reference/str_remove.html'>str_remove</a></span>, pattern <span class='o'>=</span> <span class='s'>"x[.]"</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>tidyr</span><span class='nf'>::</span><span class='nf'><a href='https://tidyr.tidyverse.org/reference/pivot_longer.html'>pivot_longer</a></span><span class='o'>(</span></span>
<span>    cols <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='o'>-</span><span class='nv'>prob_type</span>, <span class='o'>-</span><span class='nv'>temp</span>, <span class='o'>-</span><span class='nv'>contact</span><span class='o'>)</span>,</span>
<span>    names_to <span class='o'>=</span> <span class='s'>"rating"</span>,</span>
<span>    values_to <span class='o'>=</span> <span class='s'>"prob"</span></span>
<span>  <span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>tidyr</span><span class='nf'>::</span><span class='nf'><a href='https://tidyr.tidyverse.org/reference/pivot_wider.html'>pivot_wider</a></span><span class='o'>(</span></span>
<span>    names_from <span class='o'>=</span> <span class='nv'>prob_type</span>,</span>
<span>    values_from <span class='o'>=</span> <span class='nv'>prob</span></span>
<span>  <span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/rename.html'>rename</a></span><span class='o'>(</span>prob_lte <span class='o'>=</span> <span class='nv'>cprob1</span>, prob_lt <span class='o'>=</span> <span class='nv'>cprob2</span><span class='o'>)</span></span>
<span><span class='nv'>preds</span></span>
<span><span class='c'>#&gt; # A tibble: 20 × 5</span></span>
<span><span class='c'>#&gt;    contact temp  rating prob_lte prob_lt</span></span>
<span><span class='c'>#&gt;    &lt;fct&gt;   &lt;fct&gt; &lt;chr&gt;     &lt;dbl&gt;   &lt;dbl&gt;</span></span>
<span><span class='c'>#&gt;  1 no      cold  1       0.196   0      </span></span>
<span><span class='c'>#&gt;  2 no      cold  2       0.758   0.196  </span></span>
<span><span class='c'>#&gt;  3 no      cold  3       0.967   0.758  </span></span>
<span><span class='c'>#&gt;  4 no      cold  4       0.993   0.967  </span></span>
<span><span class='c'>#&gt;  5 no      cold  5       1       0.993  </span></span>
<span><span class='c'>#&gt;  6 yes     cold  1       0.0596  0      </span></span>
<span><span class='c'>#&gt;  7 yes     cold  2       0.449   0.0596 </span></span>
<span><span class='c'>#&gt;  8 yes     cold  3       0.884   0.449  </span></span>
<span><span class='c'>#&gt;  9 yes     cold  4       0.973   0.884  </span></span>
<span><span class='c'>#&gt; 10 yes     cold  5       1       0.973  </span></span>
<span><span class='c'>#&gt; 11 no      warm  1       0.0234  0      </span></span>
<span><span class='c'>#&gt; 12 no      warm  2       0.235   0.0234 </span></span>
<span><span class='c'>#&gt; 13 no      warm  3       0.742   0.235  </span></span>
<span><span class='c'>#&gt; 14 no      warm  4       0.932   0.742  </span></span>
<span><span class='c'>#&gt; 15 no      warm  5       1       0.932  </span></span>
<span><span class='c'>#&gt; 16 yes     warm  1       0.00432 0      </span></span>
<span><span class='c'>#&gt; 17 yes     warm  2       0.0529  0.00432</span></span>
<span><span class='c'>#&gt; 18 yes     warm  3       0.343   0.0529 </span></span>
<span><span class='c'>#&gt; 19 yes     warm  4       0.714   0.343  </span></span>
<span><span class='c'>#&gt; 20 yes     warm  5       1       0.714</span></span></pre>

The less-than probabilities (`prob_lt`) provide thresholds between
rating levels.

<pre class='chroma'>
<span><span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>preds</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>x <span class='o'>=</span> <span class='nv'>temp</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_path.html'>geom_line</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>group <span class='o'>=</span> <span class='nv'>rating</span>, y <span class='o'>=</span> <span class='nv'>prob_lt</span><span class='o'>)</span>,</span>
<span>    <span class='c'># don't draw solid line at y = 0</span></span>
<span>    data <span class='o'>=</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>x</span><span class='o'>)</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>x</span>, <span class='nv'>prob_lt</span> <span class='o'>!=</span> <span class='m'>0</span><span class='o'>)</span>,</span>
<span>    linetype <span class='o'>=</span> <span class='s'>"dashed"</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/facet_wrap.html'>facet_wrap</a></span><span class='o'>(</span><span class='s'>"contact"</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2022-03-07-ordinal-cumulative-probability-plots/thresholds-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

Next, we might fill in the areas. `geom_ribbon()` only likes continuous
*x* axes, so we note:

<pre class='chroma'>
<span><span class='nv'>preds</span><span class='o'>$</span><span class='nv'>temp_num</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/numeric.html'>as.numeric</a></span><span class='o'>(</span><span class='nv'>preds</span><span class='o'>$</span><span class='nv'>temp</span><span class='o'>)</span></span>
<span><span class='nv'>preds</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/distinct.html'>distinct</a></span><span class='o'>(</span><span class='nv'>temp_num</span>, <span class='nv'>temp</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 2 × 2</span></span>
<span><span class='c'>#&gt;   temp_num temp </span></span>
<span><span class='c'>#&gt;      &lt;dbl&gt; &lt;fct&gt;</span></span>
<span><span class='c'>#&gt; 1        1 cold </span></span>
<span><span class='c'>#&gt; 2        2 warm</span></span></pre>

Here the two columns of cumulative probabilities pay off because one
provides the bottom of the ribbon and the other provides the top of the
ribbon.

<pre class='chroma'>
<span><span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>preds</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>x <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/numeric.html'>as.numeric</a></span><span class='o'>(</span><span class='nv'>temp</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_ribbon.html'>geom_ribbon</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span></span>
<span>      fill <span class='o'>=</span> <span class='nv'>rating</span>, ymin <span class='o'>=</span> <span class='nv'>prob_lt</span>, ymax <span class='o'>=</span> <span class='nv'>prob_lte</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_path.html'>geom_line</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>group <span class='o'>=</span> <span class='nv'>rating</span>, y <span class='o'>=</span> <span class='nv'>prob_lt</span><span class='o'>)</span>,</span>
<span>    <span class='c'># don't draw solid line at y = 0</span></span>
<span>    data <span class='o'>=</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>x</span><span class='o'>)</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>x</span>, <span class='nv'>prob_lt</span> <span class='o'>!=</span> <span class='m'>0</span><span class='o'>)</span>,</span>
<span>    linetype <span class='o'>=</span> <span class='s'>"dashed"</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/facet_wrap.html'>facet_wrap</a></span><span class='o'>(</span><span class='s'>"contact"</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/scale_continuous.html'>scale_x_continuous</a></span><span class='o'>(</span></span>
<span>    breaks <span class='o'>=</span> <span class='m'>1</span><span class='o'>:</span><span class='m'>2</span>, labels <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>"cold"</span>, <span class='s'>"warm"</span><span class='o'>)</span>,</span>
<span>    expand <span class='o'>=</span> <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/expansion.html'>expansion</a></span><span class='o'>(</span>mult <span class='o'>=</span> <span class='m'>.25</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/scale_viridis.html'>scale_color_viridis_d</a></span><span class='o'>(</span>option <span class='o'>=</span> <span class='s'>"E"</span>, aesthetics <span class='o'>=</span> <span class='s'>"fill"</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2022-03-07-ordinal-cumulative-probability-plots/fill-area-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>


<pre class='chroma'>
<span><span class='nf'><a href='https://ggplot2.tidyverse.org/reference/get_last_plot.html'>last_plot</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/stat_summary.html'>stat_summary</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span></span>
<span>      x <span class='o'>=</span> <span class='m'>1</span> <span class='o'>+</span> <span class='nf'><a href='https://rdrr.io/r/base/numeric.html'>as.numeric</a></span><span class='o'>(</span><span class='nv'>rating</span><span class='o'>)</span> <span class='o'>/</span> <span class='m'>5</span> <span class='o'>-</span> <span class='m'>.05</span>,</span>
<span>      label <span class='o'>=</span> <span class='nv'>rating</span>,</span>
<span>      color <span class='o'>=</span> <span class='nv'>rating</span>,</span>
<span>      y <span class='o'>=</span> <span class='o'>(</span><span class='nv'>prob_lte</span> <span class='o'>+</span> <span class='nv'>prob_lt</span><span class='o'>)</span> <span class='o'>/</span> <span class='m'>2</span></span>
<span>    <span class='o'>)</span>,</span>
<span>    fun <span class='o'>=</span> <span class='nv'>median</span>,</span>
<span>    geom <span class='o'>=</span> <span class='s'>"text"</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/scale_manual.html'>scale_color_manual</a></span><span class='o'>(</span></span>
<span>    values <span class='o'>=</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/pal_viridis.html'>viridis_pal</a></span><span class='o'>(</span>option <span class='o'>=</span> <span class='s'>"E"</span><span class='o'>)</span><span class='o'>(</span><span class='m'>2</span><span class='o'>)</span><span class='o'>[</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='m'>2</span>,<span class='m'>2</span>,<span class='m'>2</span>,<span class='m'>1</span>,<span class='m'>1</span><span class='o'>)</span><span class='o'>]</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/guides.html'>guides</a></span><span class='o'>(</span>color <span class='o'>=</span> <span class='s'>"none"</span>, fill <span class='o'>=</span> <span class='s'>"none"</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/labs.html'>labs</a></span><span class='o'>(</span>x <span class='o'>=</span> <span class='s'>"temperature"</span>, y <span class='o'>=</span> <span class='s'>"cumulative probability"</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2022-03-07-ordinal-cumulative-probability-plots/fill-label-areas-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

### Bayesian version

I once prepared a Bayesian version of the idea. Here is that recipe:

<pre class='chroma'>
<span><span class='nv'>b_model</span> <span class='o'>&lt;-</span> <span class='nf'>brms</span><span class='nf'>::</span><span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>  <span class='nv'>rating</span> <span class='o'>~</span> <span class='nv'>temp</span> <span class='o'>*</span> <span class='nv'>contact</span>, </span>
<span>  data <span class='o'>=</span> <span class='nv'>wine</span>,</span>
<span>  family <span class='o'>=</span> <span class='nf'>brms</span><span class='nf'>::</span><span class='nf'><a href='https://paulbuerkner.com/brms/reference/brmsfamily.html'>cumulative</a></span><span class='o'>(</span><span class='o'>)</span>, </span>
<span>  backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>  chains <span class='o'>=</span> <span class='m'>1</span>, </span>
<span>  file <span class='o'>=</span> <span class='s'>"_caches/2022-03-07-ordinal"</span></span>
<span><span class='o'>)</span></span></pre>

Predictions comes as `.category` probabilities, so we sum them to
compute two cumulative probabilities. But we also compute posterior
medians to get the thresholds.

<pre class='chroma'>
<span><span class='nv'>data_rvars</span> <span class='o'>&lt;-</span> <span class='nv'>pred_grid</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>tidybayes</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html'>add_epred_draws</a></span><span class='o'>(</span><span class='nv'>b_model</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>tidybayes</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/tidybayes/reference/nest_rvars.html'>nest_rvars</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>group_by</a></span><span class='o'>(</span><span class='nv'>contact</span>, <span class='nv'>temp</span>, <span class='nv'>.row</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate.html'>mutate</a></span><span class='o'>(</span></span>
<span>    prob_lte <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/cumsum.html'>cumsum</a></span><span class='o'>(</span><span class='nv'>.epred</span><span class='o'>)</span>,</span>
<span>    <span class='c'># subtracting 1 from 1 to make a 0 rvar</span></span>
<span>    prob_lt <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='nv'>prob_lte</span><span class='o'>[</span><span class='m'>5</span><span class='o'>]</span> <span class='o'>-</span> <span class='m'>1</span>, <span class='nv'>prob_lte</span><span class='o'>[</span><span class='m'>1</span><span class='o'>:</span><span class='m'>4</span><span class='o'>]</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required namespace: rstan</span></span>
<span></span>
<span><span class='nv'>data_boundaries</span> <span class='o'>&lt;-</span> <span class='nv'>data_rvars</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>group_by</a></span><span class='o'>(</span><span class='nv'>contact</span>, <span class='nv'>temp</span>, <span class='nv'>.category</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>ggdist</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/ggdist/reference/point_interval.html'>median_qi</a></span><span class='o'>(</span><span class='nv'>prob_lte</span>, <span class='nv'>prob_lt</span><span class='o'>)</span></span></pre>

Here is the trick. Hint at the uncertainty by overlaying 100 ribbons:

<pre class='chroma'>
<span><span class='nv'>data_draws_100</span> <span class='o'>&lt;-</span> <span class='nv'>data_rvars</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>tidybayes</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/tidybayes/reference/nest_rvars.html'>unnest_rvars</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>.draw</span> <span class='o'>&lt;=</span> <span class='m'>100</span><span class='o'>)</span></span>
<span>  </span>
<span><span class='c'># create 100 geom_area() layers</span></span>
<span><span class='nv'>layers</span> <span class='o'>&lt;-</span> <span class='nv'>data_draws_100</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/split.html'>split</a></span><span class='o'>(</span><span class='o'>~</span> <span class='nv'>.draw</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='kr'>function</span><span class='o'>(</span><span class='nv'>x</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_ribbon.html'>geom_ribbon</a></span><span class='o'>(</span></span>
<span>      <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>fill <span class='o'>=</span> <span class='nv'>.category</span>, ymin <span class='o'>=</span> <span class='nv'>prob_lt</span>, ymax <span class='o'>=</span> <span class='nv'>prob_lte</span><span class='o'>)</span>,</span>
<span>      alpha <span class='o'>=</span> <span class='m'>.01</span>,</span>
<span>      data <span class='o'>=</span> <span class='nv'>x</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>data_draws_100</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>x <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/numeric.html'>as.numeric</a></span><span class='o'>(</span><span class='nv'>temp</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nv'>layers</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_path.html'>geom_line</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span>y <span class='o'>=</span> <span class='nv'>prob_lte</span>, group <span class='o'>=</span> <span class='nv'>.category</span><span class='o'>)</span>,</span>
<span>    data <span class='o'>=</span> <span class='nv'>data_boundaries</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>.category</span> <span class='o'>!=</span> <span class='m'>5</span><span class='o'>)</span>,</span>
<span>    linetype <span class='o'>=</span> <span class='s'>"dashed"</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/stat_summary.html'>stat_summary</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span></span>
<span>      x <span class='o'>=</span> <span class='m'>1</span> <span class='o'>+</span> <span class='nf'><a href='https://rdrr.io/r/base/numeric.html'>as.numeric</a></span><span class='o'>(</span><span class='nv'>.category</span><span class='o'>)</span> <span class='o'>/</span> <span class='m'>5</span> <span class='o'>-</span> <span class='m'>.05</span>,</span>
<span>      label <span class='o'>=</span> <span class='nv'>.category</span>,</span>
<span>      color <span class='o'>=</span> <span class='nv'>.category</span>,</span>
<span>      y <span class='o'>=</span> <span class='o'>(</span><span class='nv'>prob_lte</span> <span class='o'>+</span> <span class='nv'>prob_lt</span><span class='o'>)</span> <span class='o'>/</span> <span class='m'>2</span></span>
<span>    <span class='o'>)</span>,</span>
<span>    data <span class='o'>=</span> <span class='nv'>data_boundaries</span>,</span>
<span>    fun <span class='o'>=</span> <span class='nv'>median</span>,</span>
<span>    geom <span class='o'>=</span> <span class='s'>"text"</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/facet_wrap.html'>facet_wrap</a></span><span class='o'>(</span><span class='s'>"contact"</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/scale_continuous.html'>scale_x_continuous</a></span><span class='o'>(</span></span>
<span>    breaks <span class='o'>=</span> <span class='m'>1</span><span class='o'>:</span><span class='m'>2</span>, labels <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>"cold"</span>, <span class='s'>"warm"</span><span class='o'>)</span>,</span>
<span>    expand <span class='o'>=</span> <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/expansion.html'>expansion</a></span><span class='o'>(</span>mult <span class='o'>=</span> <span class='m'>.25</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/scale_viridis.html'>scale_color_viridis_d</a></span><span class='o'>(</span>option <span class='o'>=</span> <span class='s'>"E"</span>, aesthetics <span class='o'>=</span> <span class='s'>"fill"</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/scale_manual.html'>scale_color_manual</a></span><span class='o'>(</span></span>
<span>    values <span class='o'>=</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/pal_viridis.html'>viridis_pal</a></span><span class='o'>(</span>option <span class='o'>=</span> <span class='s'>"E"</span><span class='o'>)</span><span class='o'>(</span><span class='m'>2</span><span class='o'>)</span><span class='o'>[</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='m'>2</span>,<span class='m'>2</span>,<span class='m'>2</span>,<span class='m'>1</span>,<span class='m'>1</span><span class='o'>)</span><span class='o'>]</span></span>
<span>  <span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/guides.html'>guides</a></span><span class='o'>(</span>color <span class='o'>=</span> <span class='s'>"none"</span>, fill <span class='o'>=</span> <span class='s'>"none"</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>  <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/labs.html'>labs</a></span><span class='o'>(</span>x <span class='o'>=</span> <span class='s'>"temperature"</span>, y <span class='o'>=</span> <span class='s'>"cumulative probability"</span><span class='o'>)</span> </span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2022-03-07-ordinal-cumulative-probability-plots/unnamed-chunk-8-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>


