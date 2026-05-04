---
title: "ggplot2 loops and delayed evaluation in aes()"
date: 2026-05-04
tags: [r, ggplot2, tidy-eval]
---


This note solves a problem faced by Josiah Parry (relevant [gist] and
[Bluesky post]) Generating a series of plots with ggplot2 and a
for-loop can lead to unexpected results, as in the following example:

<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://ggplot2.tidyverse.org'>ggplot2</a></span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/matrix.html'>as.matrix</a></span><span class='o'>(</span><span class='nv'>penguins</span><span class='o'>[</span>,<span class='m'>3</span><span class='o'>:</span><span class='m'>6</span><span class='o'>]</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>all_plots</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='kr'>for</span> <span class='o'>(</span><span class='nv'>i</span> <span class='kr'>in</span> <span class='m'>1</span><span class='o'>:</span><span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>ncol</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>gg</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>penguins</span>, <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span><span class='nv'>bill_len</span>, <span class='nv'>m</span><span class='o'>[</span>, <span class='nv'>i</span><span class='o'>]</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_point.html'>geom_point</a></span><span class='o'>(</span>na.rm <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/labs.html'>labs</a></span><span class='o'>(</span>title <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"Column %i"</span>, <span class='nv'>i</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  <span class='nv'>all_plots</span><span class='o'>[[</span><span class='nv'>i</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nv'>gg</span></span>
<span><span class='o'>}</span></span>
<span><span class='nf'>patchwork</span><span class='nf'>::</span><span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_plots.html'>wrap_plots</a></span><span class='o'>(</span><span class='nv'>all_plots</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2026-05-04-ggplot2-for-loops/bad figure-1.png" alt="All panels show the same data because the loop variable `i` is evaluated too late." width="80%" />
<p class="caption">All panels show the same data because the loop variable `i` is evaluated too late.</p>
</div>

The data in every subplot is the same, but that's wrong. The problem is
that the `i` in `aes(bill_len, m[, i])` is not evaluated until the value
is needed and that doest not occur until after the loop when `i` has its
final value. So the data for `m[, 4]` is shown in every panel. 

<pre class='chroma'>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, i]`</span></span>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>2</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, i]`</span></span>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>3</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, i]`</span></span>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>4</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, i]`</span></span></pre>

What we need to do is force evaluation of something so that `i` can no
longer point to the same value in each subplot.

(Note also that we could avoid this whole thing by using `lapply()` or
something but pretend that our hands are tied here.)

One option is to **force evaluation of `i` with `!!`** so the column
index is inserted into the expression immediately.

<pre class='chroma'>
<span><span class='nv'>all_plots</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='kr'>for</span> <span class='o'>(</span><span class='nv'>i</span> <span class='kr'>in</span> <span class='m'>1</span><span class='o'>:</span><span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>ncol</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>gg</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>penguins</span>, <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span><span class='nv'>bill_len</span>, <span class='nv'>m</span><span class='o'>[</span>, <span class='o'>!</span><span class='o'>!</span> <span class='nv'>i</span><span class='o'>]</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_point.html'>geom_point</a></span><span class='o'>(</span>na.rm <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/labs.html'>labs</a></span><span class='o'>(</span>title <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"Column %i"</span>, <span class='nv'>i</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  <span class='nv'>all_plots</span><span class='o'>[[</span><span class='nv'>i</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nv'>gg</span></span>
<span><span class='o'>}</span></span>
<span><span class='nf'>patchwork</span><span class='nf'>::</span><span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_plots.html'>wrap_plots</a></span><span class='o'>(</span><span class='nv'>all_plots</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2026-05-04-ggplot2-for-loops/good figure eval-1.png" alt="Each panel now uses the correct column after forcing evaluation with `!!`." width="80%" />
<p class="caption">Each panel now uses the correct column after forcing evaluation with `!!`.</p>
</div>

The `y` value still comes from `m[, ]`, but the column index is now fixed.

<pre class='chroma'>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, 1L]`</span></span>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>2</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, 2L]`</span></span>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>3</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, 3L]`</span></span>
<span><span class='nv'>all_plots</span><span class='o'>[[</span><span class='m'>4</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"layers"</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>[[</span><span class='s'>"computed_mapping"</span><span class='o'>]</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; Aesthetic mapping: </span></span>
<span><span class='c'>#&gt; * `x` -&gt; `bill_len`</span></span>
<span><span class='c'>#&gt; * `y` -&gt; `m[, 4L]`</span></span></pre>

We could also **assemble and store the plot immediately**:

<pre class='chroma'>
<span><span class='nv'>all_plots</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='kr'>for</span> <span class='o'>(</span><span class='nv'>i</span> <span class='kr'>in</span> <span class='m'>1</span><span class='o'>:</span><span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>ncol</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>gg</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot</a></span><span class='o'>(</span><span class='nv'>penguins</span>, <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/aes.html'>aes</a></span><span class='o'>(</span><span class='nv'>bill_len</span>, <span class='nv'>m</span><span class='o'>[</span>, <span class='nv'>i</span><span class='o'>]</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>+</span> </span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/geom_point.html'>geom_point</a></span><span class='o'>(</span>na.rm <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span> <span class='o'>+</span></span>
<span>    <span class='nf'><a href='https://ggplot2.tidyverse.org/reference/labs.html'>labs</a></span><span class='o'>(</span>title <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"Column %i"</span>, <span class='nv'>i</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  <span class='nv'>all_plots</span><span class='o'>[[</span><span class='nv'>i</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nf'>patchwork</span><span class='nf'>::</span><span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_ggplot_grob.html'>wrap_ggplot_grob</a></span><span class='o'>(</span><span class='nf'><a href='https://ggplot2.tidyverse.org/reference/ggplotGrob.html'>ggplotGrob</a></span><span class='o'>(</span><span class='nv'>gg</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span><span class='nf'>patchwork</span><span class='nf'>::</span><span class='nf'><a href='https://patchwork.data-imaginist.com/reference/wrap_plots.html'>wrap_plots</a></span><span class='o'>(</span><span class='nv'>all_plots</span><span class='o'>)</span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2026-05-04-ggplot2-for-loops/good figure grob-1.png" alt="Forcing early evaluation by converting plots to grobs also yields correct panels." width="80%" />
<p class="caption">Forcing early evaluation by converting plots to grobs also yields correct panels.</p>
</div>

`ggplotGrob()` converts the plot into a **graphical objects** ("grob")
table to be drawn into by... whatever comes next in the plotting
pipeline. (I never have to work past this point.) patchwork can handle
these, but `wrap_ggplot_grob()` does some more work on grob tables that
come from ggplot2.

This is a subtle problem. I asked ChatGPT to proofread this note and it offered 
a nonsolution:

<div class="figure" style="text-align: center">
<img src="/./assets/images/2026-05-04-chatgpt-blows-it.png" alt="Screenshot of ChatGPT suggesting to use `y &lt;- m[, i]` on each loop iteration." width="80%" />
<p class="caption">Screenshot of ChatGPT suggesting to use `y <- m[, i]` on each loop iteration.</p>
</div>


[gist]: https://gist.github.com/JosiahParry/a013c715dc719abf075a0cd1193f5aed#file-ggplots-in-for-loops-dont-work-r "GitHub Gist showing the problem"

[Bluesky post]: https://bsky.app/profile/josiah.rs/post/3ml23p2u2es2p "Yet again bamboozled by ggplot2's lazy eval...This is my white whale in R 🐋 [...]"

