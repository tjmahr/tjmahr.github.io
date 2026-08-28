---
title: "Computing and filtering pairwise differences in R and duckdb"
date: 2026-08-28
tags: [r, dplyr, duckdb]
---



We have listeners transcribe children's speech, and to filter out
potentially unreliable listeners, we will select the first 2 listeners
that are within 10 percentage points (.10 on proportion units) of each
other.

<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://dplyr.tidyverse.org'>dplyr</a></span><span class='o'>)</span></span>
<span><span class='nf'>withr</span><span class='nf'>::</span><span class='nf'><a href='https://withr.r-lib.org/reference/with_seed.html'>local_seed</a></span><span class='o'>(</span><span class='m'>20260828</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'>tibble</span><span class='nf'>::</span><span class='nf'><a href='https://tibble.tidyverse.org/reference/tibble.html'>tibble</a></span><span class='o'>(</span></span>
<span>  id <span class='o'>=</span> <span class='m'>1</span><span class='o'>:</span><span class='m'>20</span>,</span>
<span>  mu <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='o'>(</span><span class='m'>20</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/stats/Logistic.html'>plogis</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>  phi <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/Round.html'>round</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/stats/GammaDist.html'>rgamma</a></span><span class='o'>(</span><span class='m'>20</span>, shape <span class='o'>=</span> <span class='m'>40</span><span class='o'>)</span><span class='o'>)</span>,</span>
<span>  a <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/Round.html'>floor</a></span><span class='o'>(</span><span class='nv'>mu</span> <span class='o'>*</span> <span class='nv'>phi</span><span class='o'>)</span>,</span>
<span>  b <span class='o'>=</span> <span class='nv'>phi</span> <span class='o'>-</span> <span class='nv'>a</span>,</span>
<span>  y <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/funprog.html'>Map</a></span><span class='o'>(</span><span class='nv'>rbeta</span>, <span class='m'>4</span>, <span class='nv'>a</span>, <span class='nv'>b</span><span class='o'>)</span></span>
<span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>tidyr</span><span class='nf'>::</span><span class='nf'><a href='https://tidyr.tidyverse.org/reference/unnest.html'>unnest</a></span><span class='o'>(</span><span class='nv'>y</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>group_by</a></span><span class='o'>(</span><span class='nv'>id</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate.html'>mutate</a></span><span class='o'>(</span>listener_num <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_along</a></span><span class='o'>(</span><span class='nv'>y</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>ungroup</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>id</span>, <span class='nv'>listener_num</span>, <span class='nv'>y</span><span class='o'>)</span></span>
<span><span class='nv'>data</span></span>
<span><span class='c'>#&gt; # A tibble: 80 × 3</span></span>
<span><span class='c'>#&gt;       id listener_num     y</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;        &lt;int&gt; &lt;dbl&gt;</span></span>
<span><span class='c'>#&gt;  1     1            1 0.226</span></span>
<span><span class='c'>#&gt;  2     1            2 0.214</span></span>
<span><span class='c'>#&gt;  3     1            3 0.132</span></span>
<span><span class='c'>#&gt;  4     1            4 0.168</span></span>
<span><span class='c'>#&gt;  5     2            1 0.911</span></span>
<span><span class='c'>#&gt;  6     2            2 0.888</span></span>
<span><span class='c'>#&gt;  7     2            3 0.891</span></span>
<span><span class='c'>#&gt;  8     2            4 0.819</span></span>
<span><span class='c'>#&gt;  9     3            1 0.147</span></span>
<span><span class='c'>#&gt; 10     3            2 0.217</span></span>
<span><span class='c'>#&gt; # ℹ 70 more rows</span></span></pre>

Let's work through the problem for a single vector of observations.
`outer()` will populate an outer product matrix from each combination of
values from two vectors. Here is a visualization of how elements from
vectors are paired off:

<pre class='chroma'>
<span><span class='nv'>x1</span> <span class='o'>&lt;-</span> <span class='nv'>letters</span><span class='o'>[</span><span class='m'>1</span><span class='o'>:</span><span class='m'>5</span><span class='o'>]</span></span>
<span><span class='nv'>x2</span> <span class='o'>&lt;-</span> <span class='m'>1</span><span class='o'>:</span><span class='m'>5</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/outer.html'>outer</a></span><span class='o'>(</span><span class='nv'>x1</span>, <span class='nv'>x2</span>, FUN <span class='o'>=</span> <span class='nv'>paste0</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;      [,1] [,2] [,3] [,4] [,5]</span></span>
<span><span class='c'>#&gt; [1,] "a1" "a2" "a3" "a4" "a5"</span></span>
<span><span class='c'>#&gt; [2,] "b1" "b2" "b3" "b4" "b5"</span></span>
<span><span class='c'>#&gt; [3,] "c1" "c2" "c3" "c4" "c5"</span></span>
<span><span class='c'>#&gt; [4,] "d1" "d2" "d3" "d4" "d5"</span></span>
<span><span class='c'>#&gt; [5,] "e1" "e2" "e3" "e4" "e5"</span></span></pre>

For pairwise differences, here are the steps in a very base R style:

- Compute pairwise differences into matrix
- Keep just the lower triangle of the matrix
- Get the row and column indices of pairs that meet the criteria
- Find the first such pair

Row and column indices correspond to data collection order (row 1 and
column 1 are listener 1). So, the *first such pair* is the first one with the
smallest maximum row/column index. For example, if pairs 1-4 and 2-3
both satisfy the criteria, 2-3 has to be first because 4 would not have
been collected yet by the time 2-3 satisfied the criteria.

<pre class='chroma'>
<span><span class='nv'>limit</span> <span class='o'>&lt;-</span> <span class='m'>.1</span></span>
<span><span class='nv'>xs</span> <span class='o'>&lt;-</span> <span class='nv'>data</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>id</span> <span class='o'>==</span> <span class='m'>1</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/pull.html'>pull</a></span><span class='o'>(</span><span class='nv'>y</span><span class='o'>)</span></span>
<span><span class='nv'>xs</span></span>
<span><span class='c'>#&gt; [1] 0.2264930 0.2141150 0.1319205 0.1681183</span></span>
<span></span>
<span><span class='nv'>diffs</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/outer.html'>outer</a></span><span class='o'>(</span><span class='nv'>xs</span>, <span class='nv'>xs</span>, FUN <span class='o'>=</span> <span class='s'>"-"</span><span class='o'>)</span></span>
<span><span class='nv'>diffs</span></span>
<span><span class='c'>#&gt;             [,1]        [,2]       [,3]        [,4]</span></span>
<span><span class='c'>#&gt; [1,]  0.00000000  0.01237806 0.09457258  0.05837471</span></span>
<span><span class='c'>#&gt; [2,] -0.01237806  0.00000000 0.08219452  0.04599665</span></span>
<span><span class='c'>#&gt; [3,] -0.09457258 -0.08219452 0.00000000 -0.03619787</span></span>
<span><span class='c'>#&gt; [4,] -0.05837471 -0.04599665 0.03619787  0.00000000</span></span>
<span></span>
<span><span class='nv'>pairs</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/which.html'>which</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/MathFun.html'>abs</a></span><span class='o'>(</span><span class='nv'>diffs</span><span class='o'>)</span> <span class='o'>&lt;=</span> <span class='nv'>limit</span>, arr.ind <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span></span>
<span><span class='c'># Keep the lower triangle of the matrix</span></span>
<span><span class='nv'>pairs</span> <span class='o'>&lt;-</span> <span class='nv'>pairs</span><span class='o'>[</span><span class='nv'>pairs</span><span class='o'>[</span>, <span class='s'>"col"</span><span class='o'>]</span> <span class='o'>&lt;</span> <span class='nv'>pairs</span><span class='o'>[</span>, <span class='s'>"row"</span><span class='o'>]</span>, <span class='m'>1</span><span class='o'>:</span><span class='m'>2</span>, drop <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>]</span></span>
<span><span class='nv'>pairs</span></span>
<span><span class='c'>#&gt;      row col</span></span>
<span><span class='c'>#&gt; [1,]   2   1</span></span>
<span><span class='c'>#&gt; [2,]   3   1</span></span>
<span><span class='c'>#&gt; [3,]   4   1</span></span>
<span><span class='c'>#&gt; [4,]   3   2</span></span>
<span><span class='c'>#&gt; [5,]   4   2</span></span>
<span><span class='c'>#&gt; [6,]   4   3</span></span>
<span></span>
<span><span class='c'># Bc we have a lower triangle matrix, row index &gt; col index, </span></span>
<span><span class='c'># so keep smallest row index</span></span>
<span><span class='nv'>row_pair</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/which.min.html'>which.min</a></span><span class='o'>(</span><span class='nv'>pairs</span><span class='o'>[</span>, <span class='s'>"row"</span><span class='o'>]</span><span class='o'>)</span></span>
<span><span class='nv'>pairs</span><span class='o'>[</span><span class='nv'>row_pair</span>, <span class='o'>]</span></span>
<span><span class='c'>#&gt; row col </span></span>
<span><span class='c'>#&gt;   2   1</span></span>
<span></span>
<span><span class='nv'>pair</span> <span class='o'>&lt;-</span> <span class='nv'>pairs</span><span class='o'>[</span><span class='nv'>row_pair</span>, , drop <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>]</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/unname.html'>unname</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nv'>pair</span></span>
<span><span class='c'>#&gt; [1] 2 1</span></span></pre>

Wrapping these steps into a function, we get:

<pre class='chroma'>
<span><span class='nv'>find_first_consistent_pair</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>xs</span>, <span class='nv'>limit</span> <span class='o'>=</span> <span class='m'>.1</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>diffs</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/outer.html'>outer</a></span><span class='o'>(</span><span class='nv'>xs</span>, <span class='nv'>xs</span>, FUN <span class='o'>=</span> <span class='s'>"-"</span><span class='o'>)</span></span>
<span>  <span class='nv'>pairs</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/which.html'>which</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/MathFun.html'>abs</a></span><span class='o'>(</span><span class='nv'>diffs</span><span class='o'>)</span> <span class='o'>&lt;=</span> <span class='nv'>limit</span>, arr.ind <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span></span>
<span>  <span class='c'># Keep the lower triangle of the matrix</span></span>
<span>  <span class='nv'>pairs</span> <span class='o'>&lt;-</span> <span class='nv'>pairs</span><span class='o'>[</span><span class='nv'>pairs</span><span class='o'>[</span>, <span class='s'>"col"</span><span class='o'>]</span> <span class='o'>&lt;</span> <span class='nv'>pairs</span><span class='o'>[</span>, <span class='s'>"row"</span><span class='o'>]</span>, <span class='m'>1</span><span class='o'>:</span><span class='m'>2</span>, drop <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>]</span></span>
<span>  </span>
<span>  <span class='kr'>if</span> <span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>nrow</a></span><span class='o'>(</span><span class='nv'>pairs</span><span class='o'>)</span> <span class='o'>==</span> <span class='m'>0</span><span class='o'>)</span> <span class='kr'><a href='https://rdrr.io/r/base/function.html'>return</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='kc'>NA_integer_</span>, <span class='kc'>NA_integer_</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  </span>
<span>  <span class='nv'>row_pair</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/which.min.html'>which.min</a></span><span class='o'>(</span><span class='nv'>pairs</span><span class='o'>[</span>, <span class='s'>"row"</span><span class='o'>]</span><span class='o'>)</span></span>
<span>  <span class='nv'>pair</span> <span class='o'>&lt;-</span> <span class='nv'>pairs</span><span class='o'>[</span><span class='nv'>row_pair</span>, , drop <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>]</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/unname.html'>unname</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>  <span class='nv'>pair</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nv'>data_with_pairs</span> <span class='o'>&lt;-</span> <span class='nv'>data</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>group_by</a></span><span class='o'>(</span><span class='nv'>id</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate.html'>mutate</a></span><span class='o'>(</span></span>
<span>    in_pair <span class='o'>=</span> <span class='nv'>listener_num</span> <span class='o'><a href='https://rdrr.io/r/base/match.html'>%in%</a></span> <span class='nf'>find_first_consistent_pair</span><span class='o'>(</span><span class='nv'>y</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>ungroup</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nv'>data_with_pairs</span></span>
<span><span class='c'>#&gt; # A tibble: 80 × 4</span></span>
<span><span class='c'>#&gt;       id listener_num     y in_pair</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;        &lt;int&gt; &lt;dbl&gt; &lt;lgl&gt;  </span></span>
<span><span class='c'>#&gt;  1     1            1 0.226 TRUE   </span></span>
<span><span class='c'>#&gt;  2     1            2 0.214 TRUE   </span></span>
<span><span class='c'>#&gt;  3     1            3 0.132 FALSE  </span></span>
<span><span class='c'>#&gt;  4     1            4 0.168 FALSE  </span></span>
<span><span class='c'>#&gt;  5     2            1 0.911 TRUE   </span></span>
<span><span class='c'>#&gt;  6     2            2 0.888 TRUE   </span></span>
<span><span class='c'>#&gt;  7     2            3 0.891 FALSE  </span></span>
<span><span class='c'>#&gt;  8     2            4 0.819 FALSE  </span></span>
<span><span class='c'>#&gt;  9     3            1 0.147 TRUE   </span></span>
<span><span class='c'>#&gt; 10     3            2 0.217 TRUE   </span></span>
<span><span class='c'>#&gt; # ℹ 70 more rows</span></span></pre>

(Aside: I know I have couple of other base R approaches sitting around
my computer. I can't remember where I stashed them though. The point of
this note, in fact, is to put this bit of code somewhere more permanent than
a random R file on my machine.)

### Database version

In production, I have these intelligibility values computed from a duckdb 
database so I would like to make this computation using R code that can be 
converted to the duckdb dialect of SQL. 

First, let's spin up a duckdb database.

<pre class='chroma'>
<span><span class='nv'>db</span> <span class='o'>&lt;-</span> <span class='nf'>withr</span><span class='nf'>::</span><span class='nf'><a href='https://withr.r-lib.org/reference/with_db_connection.html'>local_db_connection</a></span><span class='o'>(</span><span class='nf'>DBI</span><span class='nf'>::</span><span class='nf'><a href='https://dbi.r-dbi.org/reference/dbConnect.html'>dbConnect</a></span><span class='o'>(</span><span class='nf'>duckdb</span><span class='nf'>::</span><span class='nf'><a href='https://r.duckdb.org/reference/duckdb.html'>duckdb</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; duckdb is storing downloaded extensions and secrets under ~/.duckdb:</span></span>
<span><span class='c'>#&gt; ℹ C:\Users\Tristan/.duckdb</span></span>
<span><span class='c'>#&gt; This persists across sessions and is shared with the DuckDB CLI and other clients.</span></span>
<span><span class='c'>#&gt; ℹ Run duckdb(shared_home = FALSE) to use a temporary directory instead.</span></span>
<span><span class='c'>#&gt; ℹ See ?duckdb_storage for details and alternatives.</span></span>
<span><span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/copy_to.html'>copy_to</a></span><span class='o'>(</span><span class='nv'>db</span>, <span class='nv'>data</span>, <span class='s'>"data"</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/tbl.html'>tbl</a></span><span class='o'>(</span><span class='nv'>db</span>, <span class='s'>"data"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A query:  ?? x 3</span></span>
<span><span class='c'>#&gt; # Database: DuckDB 1.5.5 [Tristan@Windows 10 x64:R 4.6.0/:memory:]</span></span>
<span><span class='c'>#&gt;       id listener_num     y</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;        &lt;int&gt; &lt;dbl&gt;</span></span>
<span><span class='c'>#&gt;  1     1            1 0.226</span></span>
<span><span class='c'>#&gt;  2     1            2 0.214</span></span>
<span><span class='c'>#&gt;  3     1            3 0.132</span></span>
<span><span class='c'>#&gt;  4     1            4 0.168</span></span>
<span><span class='c'>#&gt;  5     2            1 0.911</span></span>
<span><span class='c'>#&gt;  6     2            2 0.888</span></span>
<span><span class='c'>#&gt;  7     2            3 0.891</span></span>
<span><span class='c'>#&gt;  8     2            4 0.819</span></span>
<span><span class='c'>#&gt;  9     3            1 0.147</span></span>
<span><span class='c'>#&gt; 10     3            2 0.217</span></span>
<span><span class='c'>#&gt; # ℹ more rows</span></span></pre>

To do the outer product type of pairing, we do a self-join on the
tables, but we can tweak the joining criteria to keep just the
lower-triangle of pairs. I create a `self_left_join()` function for 
friendly `|>` piping. 

<pre class='chroma'>
<span><span class='nv'>self_left_join</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>x</span>, <span class='nv'>...</span><span class='o'>)</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate-joins.html'>left_join</a></span><span class='o'>(</span><span class='nv'>x</span>, <span class='nv'>x</span>, <span class='nv'>...</span><span class='o'>)</span></span>
<span><span class='nv'>group_vars</span> <span class='o'>&lt;-</span> <span class='nf'>rlang</span><span class='nf'>::</span><span class='nf'><a href='https://rlang.r-lib.org/reference/sym.html'>syms</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>"id"</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>tbl_pairs</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/tbl.html'>tbl</a></span><span class='o'>(</span><span class='nv'>db</span>, <span class='s'>"data"</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>self_left_join</span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://dplyr.tidyverse.org/reference/join_by.html'>join_by</a></span><span class='o'>(</span></span>
<span>      <span class='c'># want A:B comparison. ordering removes redundant A:A and B:A comparisons</span></span>
<span>      <span class='o'>!</span><span class='o'>!</span><span class='o'>!</span> <span class='nv'>group_vars</span>, <span class='nv'>x</span><span class='o'>$</span><span class='nv'>listener_num</span> <span class='o'>&lt;</span> <span class='nv'>y</span><span class='o'>$</span><span class='nv'>listener_num</span>,</span>
<span>    <span class='o'>)</span>,</span>
<span>    suffix <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>"_left"</span>, <span class='s'>"_right"</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span></span>
<span><span class='nv'>tbl_pairs</span></span>
<span><span class='c'>#&gt; # A query:  ?? x 5</span></span>
<span><span class='c'>#&gt; # Database: DuckDB 1.5.5 [Tristan@Windows 10 x64:R 4.6.0/:memory:]</span></span>
<span><span class='c'>#&gt;       id listener_num_left y_left listener_num_right y_right</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;             &lt;int&gt;  &lt;dbl&gt;              &lt;int&gt;   &lt;dbl&gt;</span></span>
<span><span class='c'>#&gt;  1     1                 1  0.226                  4   0.168</span></span>
<span><span class='c'>#&gt;  2     1                 2  0.214                  4   0.168</span></span>
<span><span class='c'>#&gt;  3     1                 3  0.132                  4   0.168</span></span>
<span><span class='c'>#&gt;  4     2                 1  0.911                  4   0.819</span></span>
<span><span class='c'>#&gt;  5     2                 2  0.888                  4   0.819</span></span>
<span><span class='c'>#&gt;  6     2                 3  0.891                  4   0.819</span></span>
<span><span class='c'>#&gt;  7     3                 1  0.147                  4   0.209</span></span>
<span><span class='c'>#&gt;  8     3                 2  0.217                  4   0.209</span></span>
<span><span class='c'>#&gt;  9     3                 3  0.208                  4   0.209</span></span>
<span><span class='c'>#&gt; 10     4                 1  0.232                  4   0.236</span></span>
<span><span class='c'>#&gt; # ℹ more rows</span></span></pre>

Because `listener_num_left < listener_num_right` as a result of the table join, 
the *first such pair* is the first one with the smallest `listener_num_right`:

<pre class='chroma'>
<span><span class='nv'>limit</span> <span class='o'>&lt;-</span> <span class='m'>.1</span></span>
<span></span>
<span><span class='nv'>tbl_selected_pairs</span> <span class='o'>&lt;-</span> <span class='nv'>tbl_pairs</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>group_by</a></span><span class='o'>(</span><span class='o'>!</span><span class='o'>!</span><span class='o'>!</span> <span class='nv'>group_vars</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate.html'>mutate</a></span><span class='o'>(</span>diffs <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/MathFun.html'>abs</a></span><span class='o'>(</span><span class='nv'>y_left</span> <span class='o'>-</span> <span class='nv'>y_right</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>diffs</span> <span class='o'>&lt;=</span> <span class='nv'>limit</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>listener_num_right</span> <span class='o'>==</span> <span class='nf'><a href='https://rdrr.io/r/base/Extremes.html'>min</a></span><span class='o'>(</span><span class='nv'>listener_num_right</span>, na.rm <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>listener_num_left</span> <span class='o'>==</span> <span class='nf'><a href='https://rdrr.io/r/base/Extremes.html'>min</a></span><span class='o'>(</span><span class='nv'>listener_num_left</span>, na.rm <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span></span>
<span>    <span class='o'>!</span><span class='o'>!</span><span class='o'>!</span> <span class='nv'>group_vars</span>, <span class='nv'>listener_num_left</span>, <span class='nv'>listener_num_right</span></span>
<span>  <span class='o'>)</span> </span>
<span><span class='nv'>tbl_selected_pairs</span></span>
<span><span class='c'>#&gt; # A query:  ?? x 3</span></span>
<span><span class='c'>#&gt; # Database: DuckDB 1.5.5 [Tristan@Windows 10 x64:R 4.6.0/:memory:]</span></span>
<span><span class='c'>#&gt; # Groups:   id</span></span>
<span><span class='c'>#&gt;       id listener_num_left listener_num_right</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;             &lt;int&gt;              &lt;int&gt;</span></span>
<span><span class='c'>#&gt;  1     1                 1                  2</span></span>
<span><span class='c'>#&gt;  2     7                 1                  3</span></span>
<span><span class='c'>#&gt;  3    11                 2                  3</span></span>
<span><span class='c'>#&gt;  4    16                 1                  2</span></span>
<span><span class='c'>#&gt;  5     3                 1                  2</span></span>
<span><span class='c'>#&gt;  6     6                 1                  3</span></span>
<span><span class='c'>#&gt;  7    12                 1                  2</span></span>
<span><span class='c'>#&gt;  8    20                 1                  3</span></span>
<span><span class='c'>#&gt;  9     8                 1                  2</span></span>
<span><span class='c'>#&gt; 10    13                 1                  2</span></span>
<span><span class='c'>#&gt; 11    19                 1                  2</span></span>
<span><span class='c'>#&gt; 12     4                 1                  2</span></span>
<span><span class='c'>#&gt; 13    10                 1                  3</span></span>
<span><span class='c'>#&gt; 14    18                 1                  2</span></span>
<span><span class='c'>#&gt; 15    15                 2                  3</span></span>
<span><span class='c'>#&gt; 16     5                 1                  2</span></span>
<span><span class='c'>#&gt; 17     9                 2                  3</span></span>
<span><span class='c'>#&gt; 18    14                 1                  2</span></span>
<span><span class='c'>#&gt; 19    17                 1                  3</span></span>
<span><span class='c'>#&gt; 20     2                 1                  2</span></span></pre>

The data are in a wide format right now, so we need to pivot them into a
longer shape. I am going to use duckdb's own functions (written in all
caps) to accomplish this task:

<pre class='chroma'>
<span><span class='nv'>tbl_pairs_to_keep</span> <span class='o'>&lt;-</span> <span class='nv'>tbl_selected_pairs</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate.html'>mutate</a></span><span class='o'>(</span></span>
<span>    <span class='c'># unpivoting by nesting and unnesting values</span></span>
<span>    listener_num <span class='o'>=</span> <span class='nf'>LIST_VALUE</span><span class='o'>(</span><span class='nv'>listener_num_left</span>, <span class='nv'>listener_num_right</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'>UNNEST</span><span class='o'>(</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/group_by.html'>ungroup</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='o'>!</span><span class='o'>!</span><span class='o'>!</span> <span class='nv'>group_vars</span>, <span class='nv'>listener_num</span><span class='o'>)</span></span>
<span><span class='nv'>tbl_pairs_to_keep</span></span>
<span><span class='c'>#&gt; # A query:  ?? x 2</span></span>
<span><span class='c'>#&gt; # Database: DuckDB 1.5.5 [Tristan@Windows 10 x64:R 4.6.0/:memory:]</span></span>
<span><span class='c'>#&gt;       id listener_num</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;        &lt;int&gt;</span></span>
<span><span class='c'>#&gt;  1     8            1</span></span>
<span><span class='c'>#&gt;  2     8            2</span></span>
<span><span class='c'>#&gt;  3    13            1</span></span>
<span><span class='c'>#&gt;  4    13            2</span></span>
<span><span class='c'>#&gt;  5    19            1</span></span>
<span><span class='c'>#&gt;  6    19            2</span></span>
<span><span class='c'>#&gt;  7    10            1</span></span>
<span><span class='c'>#&gt;  8    10            3</span></span>
<span><span class='c'>#&gt;  9    18            1</span></span>
<span><span class='c'>#&gt; 10    18            2</span></span>
<span><span class='c'>#&gt; # ℹ more rows</span></span></pre>

Finally, we can filter down to the desired listener ids.

<pre class='chroma'>
<span><span class='nv'>tbl_keep</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/tbl.html'>tbl</a></span><span class='o'>(</span><span class='nv'>db</span>, <span class='s'>"data"</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate-joins.html'>inner_join</a></span><span class='o'>(</span><span class='nv'>tbl_pairs_to_keep</span>, by <span class='o'>=</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/join_by.html'>join_by</a></span><span class='o'>(</span><span class='nv'>id</span>, <span class='nv'>listener_num</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='nv'>tbl_keep</span></span>
<span><span class='c'>#&gt; # A query:  ?? x 3</span></span>
<span><span class='c'>#&gt; # Database: DuckDB 1.5.5 [Tristan@Windows 10 x64:R 4.6.0/:memory:]</span></span>
<span><span class='c'>#&gt;       id listener_num     y</span></span>
<span><span class='c'>#&gt;    &lt;int&gt;        &lt;int&gt; &lt;dbl&gt;</span></span>
<span><span class='c'>#&gt;  1     1            1 0.226</span></span>
<span><span class='c'>#&gt;  2     1            2 0.214</span></span>
<span><span class='c'>#&gt;  3     2            1 0.911</span></span>
<span><span class='c'>#&gt;  4     2            2 0.888</span></span>
<span><span class='c'>#&gt;  5     3            1 0.147</span></span>
<span><span class='c'>#&gt;  6     3            2 0.217</span></span>
<span><span class='c'>#&gt;  7     4            1 0.232</span></span>
<span><span class='c'>#&gt;  8     4            2 0.317</span></span>
<span><span class='c'>#&gt;  9     5            1 0.295</span></span>
<span><span class='c'>#&gt; 10     5            2 0.267</span></span>
<span><span class='c'>#&gt; # ℹ more rows</span></span></pre>

Each `tbl_` here is a SQL query until we finally `collect()` the data
into R, so we can marvel at the SQL we generated:

<pre class='chroma'>
<span><span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/explain.html'>show_query</a></span><span class='o'>(</span><span class='nv'>tbl_keep</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; &lt;SQL&gt;</span></span>
<span><span class='c'>#&gt; SELECT "data".*</span></span>
<span><span class='c'>#&gt; FROM "data"</span></span>
<span><span class='c'>#&gt; INNER JOIN (</span></span>
<span><span class='c'>#&gt;   SELECT</span></span>
<span><span class='c'>#&gt;     id,</span></span>
<span><span class='c'>#&gt;     UNNEST(LIST_VALUE(listener_num_left, listener_num_right)) AS listener_num</span></span>
<span><span class='c'>#&gt;   FROM (</span></span>
<span><span class='c'>#&gt;     SELECT</span></span>
<span><span class='c'>#&gt;       id,</span></span>
<span><span class='c'>#&gt;       listener_num_left,</span></span>
<span><span class='c'>#&gt;       y_left,</span></span>
<span><span class='c'>#&gt;       listener_num_right,</span></span>
<span><span class='c'>#&gt;       y_right,</span></span>
<span><span class='c'>#&gt;       diffs,</span></span>
<span><span class='c'>#&gt;       MIN(listener_num_left) OVER (PARTITION BY id) AS col02</span></span>
<span><span class='c'>#&gt;     FROM (</span></span>
<span><span class='c'>#&gt;       SELECT *, MIN(listener_num_right) OVER (PARTITION BY id) AS col01</span></span>
<span><span class='c'>#&gt;       FROM (</span></span>
<span><span class='c'>#&gt;         SELECT *, ABS(y_left - y_right) AS diffs</span></span>
<span><span class='c'>#&gt;         FROM (</span></span>
<span><span class='c'>#&gt;           SELECT</span></span>
<span><span class='c'>#&gt;             data_LHS.id AS id,</span></span>
<span><span class='c'>#&gt;             data_LHS.listener_num AS listener_num_left,</span></span>
<span><span class='c'>#&gt;             data_LHS.y AS y_left,</span></span>
<span><span class='c'>#&gt;             data_RHS.listener_num AS listener_num_right,</span></span>
<span><span class='c'>#&gt;             data_RHS.y AS y_right</span></span>
<span><span class='c'>#&gt;           FROM "data" AS data_LHS</span></span>
<span><span class='c'>#&gt;           LEFT JOIN "data" AS data_RHS</span></span>
<span><span class='c'>#&gt;             ON (</span></span>
<span><span class='c'>#&gt;               data_LHS.id = data_RHS.id AND</span></span>
<span><span class='c'>#&gt;               data_LHS.listener_num &lt; data_RHS.listener_num</span></span>
<span><span class='c'>#&gt;             )</span></span>
<span><span class='c'>#&gt;         ) AS q01</span></span>
<span><span class='c'>#&gt;       ) AS q01</span></span>
<span><span class='c'>#&gt;       WHERE (diffs &lt;= 0.1)</span></span>
<span><span class='c'>#&gt;     ) AS q01</span></span>
<span><span class='c'>#&gt;     WHERE (listener_num_right = col01)</span></span>
<span><span class='c'>#&gt;   ) AS q01</span></span>
<span><span class='c'>#&gt;   WHERE (listener_num_left = col02)</span></span>
<span><span class='c'>#&gt; ) AS RHS</span></span>
<span><span class='c'>#&gt;   ON ("data".id = RHS.id AND "data".listener_num = RHS.listener_num)</span></span></pre>

Finally, we can check that the two versions agree:

<pre class='chroma'>
<span><span class='nv'>a</span> <span class='o'>&lt;-</span> <span class='nv'>data_with_pairs</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>in_pair</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='m'>1</span>, <span class='m'>2</span>, <span class='m'>3</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/arrange.html'>arrange</a></span><span class='o'>(</span><span class='nv'>id</span>, <span class='nv'>listener_num</span><span class='o'>)</span></span>
<span><span class='nv'>b</span> <span class='o'>&lt;-</span> <span class='nv'>tbl_keep</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/compute.html'>collect</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://dplyr.tidyverse.org/reference/arrange.html'>arrange</a></span><span class='o'>(</span><span class='nv'>id</span>, <span class='nv'>listener_num</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/all.html'>all</a></span><span class='o'>(</span><span class='nv'>a</span> <span class='o'>==</span> <span class='nv'>b</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE</span></span></pre>


