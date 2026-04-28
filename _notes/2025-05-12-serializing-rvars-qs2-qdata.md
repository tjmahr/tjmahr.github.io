---
title: "Serializing rvars with qs2's qdata"
date: 2025-05-12
tags: [r, posterior, rvar, tidybayes, serialization]
---

I was surprised by how large a tibble with `posterior::rvar()` columns became
when saved with `qs2::qs_save()`. The issue seems to be attributes/pointers on
the `rvar` objects. Removing the pointers by converting to a non-rvar format or 
using `qs2::qd_save()` will result in smaller file sizes.

***

Let's make rvars of 1000 draws each for 100 rows x 2 columns of data.

<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/base/Random.html'>set.seed</a></span><span class='o'>(</span><span class='m'>20250512</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>f</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>n</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>replicate</a></span><span class='o'>(</span>n <span class='o'>=</span> <span class='nv'>n</span>, <span class='nf'>posterior</span><span class='nf'>::</span><span class='nf'><a href='https://mc-stan.org/posterior/reference/rvar.html'>rvar</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='o'>(</span><span class='m'>1000</span><span class='o'>)</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/do.call.html'>do.call</a></span><span class='o'>(</span>what <span class='o'>=</span> <span class='nv'>c</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'>tibble</span><span class='nf'>::</span><span class='nf'><a href='https://tibble.tidyverse.org/reference/tibble.html'>tibble</a></span><span class='o'>(</span></span>
<span>  a <span class='o'>=</span> <span class='m'>1</span><span class='o'>:</span><span class='m'>100</span>,</span>
<span>  b <span class='o'>=</span> <span class='nf'>f</span><span class='o'>(</span><span class='m'>100</span><span class='o'>)</span>,</span>
<span>  c <span class='o'>=</span> <span class='nf'>f</span><span class='o'>(</span><span class='m'>100</span><span class='o'>)</span></span>
<span><span class='o'>)</span></span>
<span><span class='nv'>data</span></span>
<span><span class='c'>#&gt; # A tibble: 100 × 3</span></span>
<span><span class='c'>#&gt;        a               b                c</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;      &lt;rvar[1d]&gt;       &lt;rvar[1d]&gt;</span></span>
<span><span class='c'>#&gt;  1     1   0.0014 ± 1.02  -0.00354 ± 0.99</span></span>
<span><span class='c'>#&gt;  2     2   0.0479 ± 1.00  -0.06457 ± 1.01</span></span>
<span><span class='c'>#&gt;  3     3  -0.0602 ± 0.96   0.00022 ± 0.98</span></span>
<span><span class='c'>#&gt;  4     4   0.0068 ± 0.98   0.01593 ± 1.01</span></span>
<span><span class='c'>#&gt;  5     5  -0.0101 ± 1.00  -0.02121 ± 0.99</span></span>
<span><span class='c'>#&gt;  6     6  -0.0049 ± 0.95  -0.04301 ± 0.97</span></span>
<span><span class='c'>#&gt;  7     7   0.0078 ± 0.97   0.00162 ± 1.03</span></span>
<span><span class='c'>#&gt;  8     8  -0.0044 ± 0.96   0.03011 ± 0.99</span></span>
<span><span class='c'>#&gt;  9     9   0.0276 ± 1.00  -0.00378 ± 1.01</span></span>
<span><span class='c'>#&gt; 10    10  -0.0421 ± 1.05   0.08015 ± 0.99</span></span>
<span><span class='c'>#&gt; # ℹ 90 more rows</span></span></pre>

Let's save it with `save()` and `qs2::qs_save()`, which is meant to
serialize the R object's data like `save()` does but in a faster and
smarter way.

<pre class='chroma'>
<span><span class='nv'>t1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/save.html'>save</a></span><span class='o'>(</span><span class='nv'>data</span>, file <span class='o'>=</span> <span class='nv'>t1</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='nv'>t1</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/label_bytes.html'>label_bytes</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "155 MB"</span></span>
<span></span>
<span><span class='nv'>t2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nf'>qs2</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/qs2/man/qs_save.html'>qs_save</a></span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>t2</span>, nthreads <span class='o'>=</span> <span class='m'>8</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='nv'>t2</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/label_bytes.html'>label_bytes</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "110 MB"</span></span></pre>

If we unnest the data so that there is one row per draw, we get much
better sizes.

<pre class='chroma'>
<span><span class='nv'>t3</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nv'>data_long</span> <span class='o'>&lt;-</span> <span class='nf'>tidybayes</span><span class='nf'>::</span><span class='nf'><a href='https://mjskay.github.io/tidybayes/reference/nest_rvars.html'>unnest_rvars</a></span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span></span>
<span><span class='nv'>data_long</span></span>
<span><span class='c'>#&gt; # A tibble: 100,000 × 6</span></span>
<span><span class='c'>#&gt; # Groups:   a [100]</span></span>
<span><span class='c'>#&gt;        a      b      c .chain .iteration .draw</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;  &lt;dbl&gt;  &lt;dbl&gt;  &lt;int&gt;      &lt;int&gt; &lt;int&gt;</span></span>
<span><span class='c'>#&gt;  1     1 -0.584 -0.135      1          1     1</span></span>
<span><span class='c'>#&gt;  2     1 -1.78  -0.955      1          2     2</span></span>
<span><span class='c'>#&gt;  3     1 -0.222 -0.534      1          3     3</span></span>
<span><span class='c'>#&gt;  4     1 -0.560  1.13       1          4     4</span></span>
<span><span class='c'>#&gt;  5     1 -0.754 -1.81       1          5     5</span></span>
<span><span class='c'>#&gt;  6     1  0.436  1.24       1          6     6</span></span>
<span><span class='c'>#&gt;  7     1 -0.241 -1.58       1          7     7</span></span>
<span><span class='c'>#&gt;  8     1 -1.68  -1.84       1          8     8</span></span>
<span><span class='c'>#&gt;  9     1 -1.83   0.591      1          9     9</span></span>
<span><span class='c'>#&gt; 10     1 -0.470 -0.260      1         10    10</span></span>
<span><span class='c'>#&gt; # ℹ 99,990 more rows</span></span>
<span></span>
<span><span class='nf'>qs2</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/qs2/man/qs_save.html'>qs_save</a></span><span class='o'>(</span><span class='nv'>data_long</span>, <span class='nv'>t3</span>, nthreads <span class='o'>=</span> <span class='m'>8</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='nv'>t3</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/label_bytes.html'>label_bytes</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "1 MB"</span></span></pre>

<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='nv'>t2</span><span class='o'>)</span> <span class='o'>/</span> <span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='nv'>t3</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 78.05878</span></span></pre>

What is the bloat here? It is [cached
data](https://github.com/r-lib/vctrs/issues/1411) about the rvars. Here
is the data for one rvar:

<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/utils/str.html'>str</a></span><span class='o'>(</span><span class='nv'>data</span><span class='o'>$</span><span class='nv'>b</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/class.html'>unclass</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;  list()</span></span>
<span><span class='c'>#&gt;  - attr(*, "draws")= num [1:1000, 1:100] -0.584 -1.776 -0.222 -0.56 -0.754 ...</span></span>
<span><span class='c'>#&gt;   ..- attr(*, "dimnames")=List of 2</span></span>
<span><span class='c'>#&gt;   .. ..$ : chr [1:1000] "1" "2" "3" "4" ...</span></span>
<span><span class='c'>#&gt;   .. ..$ : NULL</span></span>
<span><span class='c'>#&gt;  - attr(*, "nchains")= int 1</span></span>
<span><span class='c'>#&gt;  - attr(*, "cache")=&lt;environment: 0x00000159652bf370&gt;</span></span></pre>

See the environment pointer?

Let's zap the caches and try again.

<pre class='chroma'>
<span><span class='nv'>data2</span> <span class='o'>&lt;-</span> <span class='nv'>data</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/attr.html'>attr</a></span><span class='o'>(</span><span class='nv'>data2</span><span class='o'>$</span><span class='nv'>c</span>, <span class='s'>"cache"</span><span class='o'>)</span> <span class='o'>&lt;-</span> <span class='kc'>NULL</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/attr.html'>attr</a></span><span class='o'>(</span><span class='nv'>data2</span><span class='o'>$</span><span class='nv'>b</span>, <span class='s'>"cache"</span><span class='o'>)</span> <span class='o'>&lt;-</span> <span class='kc'>NULL</span></span>
<span></span>
<span><span class='nv'>t4</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nf'>qs2</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/qs2/man/qs_save.html'>qs_save</a></span><span class='o'>(</span><span class='nv'>data2</span>, <span class='nv'>t4</span>, nthreads <span class='o'>=</span> <span class='m'>8</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='nv'>t4</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/label_bytes.html'>label_bytes</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "1 MB"</span></span></pre>

That does reduce the size to near the unnested file size. qs2 provides
the `qd_` functions for a [stricter kind of
serialization](https://github.com/qsbase/qs2#the-qdata-format) that will
replace pointers with `NULL`, so we don't have to mess with the guts of
the rvars:

<pre class='chroma'>
<span><span class='nv'>t5</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nf'>qs2</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/qs2/man/qd_save.html'>qd_save</a></span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>t5</span>, nthreads <span class='o'>=</span> <span class='m'>8</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Warning in qs2::qd_save(data, t5, nthreads = 8): Attributes of type environment</span></span>
<span><span class='c'>#&gt; are not supported in qdata format</span></span>
<span><span class='c'>#&gt; Warning in qs2::qd_save(data, t5, nthreads = 8): Attributes of type environment</span></span>
<span><span class='c'>#&gt; are not supported in qdata format</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='nv'>t5</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/label_bytes.html'>label_bytes</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "2 MB"</span></span></pre>

Importantly, the data seems to come back fine:

<pre class='chroma'>
<span><span class='nv'>data3</span> <span class='o'>&lt;-</span> <span class='nf'>qs2</span><span class='nf'>::</span><span class='nf'><a href='https://rdrr.io/pkg/qs2/man/qd_read.html'>qd_read</a></span><span class='o'>(</span><span class='nv'>t5</span><span class='o'>)</span></span>
<span><span class='nv'>data3</span></span>
<span><span class='c'>#&gt; # A tibble: 100 × 3</span></span>
<span><span class='c'>#&gt;        a               b                c</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;      &lt;rvar[1d]&gt;       &lt;rvar[1d]&gt;</span></span>
<span><span class='c'>#&gt;  1     1   0.0014 ± 1.02  -0.00354 ± 0.99</span></span>
<span><span class='c'>#&gt;  2     2   0.0479 ± 1.00  -0.06457 ± 1.01</span></span>
<span><span class='c'>#&gt;  3     3  -0.0602 ± 0.96   0.00022 ± 0.98</span></span>
<span><span class='c'>#&gt;  4     4   0.0068 ± 0.98   0.01593 ± 1.01</span></span>
<span><span class='c'>#&gt;  5     5  -0.0101 ± 1.00  -0.02121 ± 0.99</span></span>
<span><span class='c'>#&gt;  6     6  -0.0049 ± 0.95  -0.04301 ± 0.97</span></span>
<span><span class='c'>#&gt;  7     7   0.0078 ± 0.97   0.00162 ± 1.03</span></span>
<span><span class='c'>#&gt;  8     8  -0.0044 ± 0.96   0.03011 ± 0.99</span></span>
<span><span class='c'>#&gt;  9     9   0.0276 ± 1.00  -0.00378 ± 1.01</span></span>
<span><span class='c'>#&gt; 10    10  -0.0421 ± 1.05   0.08015 ± 0.99</span></span>
<span><span class='c'>#&gt; # ℹ 90 more rows</span></span>
<span></span>
<span><span class='nv'>data3</span><span class='o'>$</span><span class='nv'>b</span> <span class='o'>==</span> <span class='nv'>data2</span><span class='o'>$</span><span class='nv'>b</span></span>
<span><span class='c'>#&gt; rvar&lt;1000&gt;[100] mean ± sd:</span></span>
<span><span class='c'>#&gt;   [1] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [11] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [21] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [31] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [41] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [51] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [61] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [71] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [81] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0 </span></span>
<span><span class='c'>#&gt;  [91] 1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0  1 ± 0</span></span></pre>

