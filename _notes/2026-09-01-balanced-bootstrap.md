---
title: "The balanced cluster bootstrap and implementing it with rsample"
date: 2026-09-01
tags: [r, rsample, bootstrapping]
---



We want to resample repeated measures data at the cluster level, not at
the individual observation level. In the past, I have just resampled the
cluster IDs with replacement and then joined the original data to the
resampled IDs. In fact, I once wrote a helper function
[`wisclabmisc::join_to_split()`](https://wisclab.github.io/wisclabmisc/reference/join_to_split.html)
for combining resampled IDs to their parent dataset in an
[rsample](https://rsample.tidymodels.org/index.html)-based workflow.

<!--

``` r
library(dplyr)
set.seed(20260903)
d <- lme4::toenail
data_ids <- unique(d[, "patientID", drop = FALSE])

data_ids |> 
  rsample::bootstraps(times = 10, apparent = TRUE) |>
  rename(splits_id = splits) |> 
  # Attach data to resampled ids
  mutate(
    data_splits = splits_id |> purrr::map(
      wisclabmisc::join_to_split,
      d,
      by = "patientID",
      validate = TRUE
    )
  )
#> # A tibble: 11 × 3
#>    splits_id         id          data_splits       
#>    <list>            <chr>       <list>            
#>  1 <split [294/107]> Bootstrap01 <split [1900/702]>
#>  2 <split [294/112]> Bootstrap02 <split [1924/722]>
#>  3 <split [294/109]> Bootstrap03 <split [1900/717]>
#>  4 <split [294/107]> Bootstrap04 <split [1922/696]>
#>  5 <split [294/116]> Bootstrap05 <split [1900/762]>
#>  6 <split [294/105]> Bootstrap06 <split [1947/669]>
#>  7 <split [294/104]> Bootstrap07 <split [1893/684]>
#>  8 <split [294/107]> Bootstrap08 <split [1923/694]>
#>  9 <split [294/99]>  Bootstrap09 <split [1906/641]>
#> 10 <split [294/113]> Bootstrap10 <split [1927/726]>
#> 11 <split [294/294]> Apparent    <split [1908/294]>
```
-->
<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://dplyr.tidyverse.org'>dplyr</a></span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/Random.html'>set.seed</a></span><span class='o'>(</span><span class='m'>20260903</span><span class='o'>)</span></span>
<span><span class='nv'>d</span> <span class='o'>&lt;-</span> <span class='nf'>lme4</span><span class='nf'>::</span><span class='nv'><a href='https://rdrr.io/pkg/lme4/man/toenail.html'>toenail</a></span></span>
<span><span class='nv'>data_ids</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/unique.html'>unique</a></span><span class='o'>(</span><span class='nv'>d</span><span class='o'>[</span>, <span class='s'>"patientID"</span>, drop <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>]</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>data_ids</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>rsample</span><span class='nf'>::</span><span class='nf'><a href='https://rsample.tidymodels.org/reference/bootstraps.html'>bootstraps</a></span><span class='o'>(</span>times <span class='o'>=</span> <span class='m'>10</span>, apparent <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/rename.html'>rename</a></span><span class='o'>(</span>splits_id <span class='o'>=</span> <span class='nv'>splits</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='c'># Attach data to resampled ids</span></span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate.html'>mutate</a></span><span class='o'>(</span></span>
<span>    data_splits <span class='o'>=</span> <span class='nv'>splits_id</span> <span class='o'>|&gt;</span> <span class='nf'>purrr</span><span class='nf'>::</span><span class='nf'><a href='https://purrr.tidyverse.org/reference/map.html'>map</a></span><span class='o'>(</span></span>
<span>      <span class='nf'>wisclabmisc</span><span class='nf'>::</span><span class='nv'><a href='https://wisclab.github.io/wisclabmisc/reference/join_to_split.html'>join_to_split</a></span>,</span>
<span>      <span class='nv'>d</span>,</span>
<span>      by <span class='o'>=</span> <span class='s'>"patientID"</span>,</span>
<span>      validate <span class='o'>=</span> <span class='kc'>TRUE</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 11 × 3</span></span>
<span><span class='c'>#&gt;    splits_id         id          data_splits       </span></span>
<span><span class='c'>#&gt;    &lt;list&gt;            &lt;chr&gt;       &lt;list&gt;            </span></span>
<span><span class='c'>#&gt;  1 &lt;split [294/107]&gt; Bootstrap01 &lt;split [1900/702]&gt;</span></span>
<span><span class='c'>#&gt;  2 &lt;split [294/112]&gt; Bootstrap02 &lt;split [1924/722]&gt;</span></span>
<span><span class='c'>#&gt;  3 &lt;split [294/109]&gt; Bootstrap03 &lt;split [1900/717]&gt;</span></span>
<span><span class='c'>#&gt;  4 &lt;split [294/107]&gt; Bootstrap04 &lt;split [1922/696]&gt;</span></span>
<span><span class='c'>#&gt;  5 &lt;split [294/116]&gt; Bootstrap05 &lt;split [1900/762]&gt;</span></span>
<span><span class='c'>#&gt;  6 &lt;split [294/105]&gt; Bootstrap06 &lt;split [1947/669]&gt;</span></span>
<span><span class='c'>#&gt;  7 &lt;split [294/104]&gt; Bootstrap07 &lt;split [1893/684]&gt;</span></span>
<span><span class='c'>#&gt;  8 &lt;split [294/107]&gt; Bootstrap08 &lt;split [1923/694]&gt;</span></span>
<span><span class='c'>#&gt;  9 &lt;split [294/99]&gt;  Bootstrap09 &lt;split [1906/641]&gt;</span></span>
<span><span class='c'>#&gt; 10 &lt;split [294/113]&gt; Bootstrap10 &lt;split [1927/726]&gt;</span></span>
<span><span class='c'>#&gt; 11 &lt;split [294/294]&gt; Apparent    &lt;split [1908/294]&gt;</span></span></pre>

But I was intrigued by this neat "balanced bootstrap" approach described
by [Deen and de Rooij
(2020)](https://doi.org/10.3758/s13428-019-01252-y):

> The balanced bootstrap can be used to ensure that every individual
> appears exactly *B* times in the bootstrap samples, in contrast to
> randomly drawing bootstrap samples from the parent sample. Davison and
> Hinkley (1997) show that the balanced bootstrap results in an
> efficiency gain.
> 
> For unbalanced longitudinal data, where some subjects have more
> measurements than others, the balanced bootstrap ensures that the
> average size of the bootstrap samples equals the (subject) sample size
> *N*. In the balanced bootstrap, rather than simply drawing at random,
> a matrix is made with *B* copies of the numbers *1* to *N*. This
> matrix is vectorized, randomly shuffled, and turned back into a matrix
> of size *N* × *B* (Gleason, 1988). Each of the columns of this latter
> matrix gives the indices of a single bootstrap sample.


The procedure for populating bootstrap replicates (what I call "straps")
with cluster IDs is straightforward, even in base R. Enumerate, repeat,
shuffle, split, and recombine:

<!--

``` r
d <- lme4::toenail
data_ids <- unique(d[, "patientID", drop = FALSE])
n_ids <- nrow(data_ids)
b <- 10

assignment <- seq_len(n_ids) |>
  # Repeat and shuffle cluster indices
  rep(b) |> 
  sample() |> 
  # Divide the indices into each strap
  split(rep(seq_len(b), each = n_ids)) |> 
  unname() |>
  lapply(function(x) { 
    x <- data_ids[x, , drop = FALSE] 
    x[["cluster_id"]] <- seq_len(n_ids)
    merge(x, d)
  })

assignment |> head(2) |> str()
#> List of 2
#>  $ :'data.frame':	1896 obs. of  6 variables:
#>   ..$ patientID : Factor w/ 294 levels "1","2","3","4",..: 8 8 8 8 8 8 8 78 78 78 ...
#>   ..$ cluster_id: int [1:1896] 263 263 263 263 263 263 263 288 288 288 ...
#>   ..$ outcome   : Factor w/ 2 levels "none or mild",..: 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ treatment : Factor w/ 2 levels "itraconazole",..: 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ time      : num [1:1896] 1 9 0 6 2 ...
#>   ..$ visit     : int [1:1896] 2 6 1 5 3 7 4 5 4 6 ...
#>  $ :'data.frame':	1928 obs. of  6 variables:
#>   ..$ patientID : Factor w/ 294 levels "1","2","3","4",..: 8 8 8 8 8 8 8 78 78 78 ...
#>   ..$ cluster_id: int [1:1928] 92 92 92 92 92 92 92 62 62 62 ...
#>   ..$ outcome   : Factor w/ 2 levels "none or mild",..: 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ treatment : Factor w/ 2 levels "itraconazole",..: 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ time      : num [1:1928] 6 1 2 12 3 ...
#>   ..$ visit     : int [1:1928] 5 2 3 7 4 6 1 6 2 1 ...
```
-->
<pre class='chroma'>
<span><span class='nv'>d</span> <span class='o'>&lt;-</span> <span class='nf'>lme4</span><span class='nf'>::</span><span class='nv'><a href='https://rdrr.io/pkg/lme4/man/toenail.html'>toenail</a></span></span>
<span><span class='nv'>data_ids</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/unique.html'>unique</a></span><span class='o'>(</span><span class='nv'>d</span><span class='o'>[</span>, <span class='s'>"patientID"</span>, drop <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>]</span><span class='o'>)</span></span>
<span><span class='nv'>n_ids</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>nrow</a></span><span class='o'>(</span><span class='nv'>data_ids</span><span class='o'>)</span></span>
<span><span class='nv'>b</span> <span class='o'>&lt;-</span> <span class='m'>10</span></span>
<span></span>
<span><span class='nv'>assignment</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nv'>n_ids</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='c'># Repeat and shuffle cluster indices</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/rep.html'>rep</a></span><span class='o'>(</span><span class='nv'>b</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/sample.html'>sample</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='c'># Divide the indices into each strap</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/split.html'>split</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/rep.html'>rep</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nv'>b</span><span class='o'>)</span>, each <span class='o'>=</span> <span class='nv'>n_ids</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/unname.html'>unname</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='kr'>function</span><span class='o'>(</span><span class='nv'>x</span><span class='o'>)</span> <span class='o'>{</span> </span>
<span>    <span class='nv'>x</span> <span class='o'>&lt;-</span> <span class='nv'>data_ids</span><span class='o'>[</span><span class='nv'>x</span>, , drop <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>]</span> </span>
<span>    <span class='nv'>x</span><span class='o'>[[</span><span class='s'>"cluster_id"</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nv'>n_ids</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/merge.html'>merge</a></span><span class='o'>(</span><span class='nv'>x</span>, <span class='nv'>d</span><span class='o'>)</span></span>
<span>  <span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>assignment</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/utils/head.html'>head</a></span><span class='o'>(</span><span class='m'>2</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/utils/str.html'>str</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; List of 2</span></span>
<span><span class='c'>#&gt;  $ :'data.frame':    1896 obs. of  6 variables:</span></span>
<span><span class='c'>#&gt;   ..$ patientID : Factor w/ 294 levels "1","2","3","4",..: 8 8 8 8 8 8 8 78 78 78 ...</span></span>
<span><span class='c'>#&gt;   ..$ cluster_id: int [1:1896] 263 263 263 263 263 263 263 288 288 288 ...</span></span>
<span><span class='c'>#&gt;   ..$ outcome   : Factor w/ 2 levels "none or mild",..: 1 1 1 1 1 1 1 1 1 1 ...</span></span>
<span><span class='c'>#&gt;   ..$ treatment : Factor w/ 2 levels "itraconazole",..: 1 1 1 1 1 1 1 1 1 1 ...</span></span>
<span><span class='c'>#&gt;   ..$ time      : num [1:1896] 1 9 0 6 2 ...</span></span>
<span><span class='c'>#&gt;   ..$ visit     : int [1:1896] 2 6 1 5 3 7 4 5 4 6 ...</span></span>
<span><span class='c'>#&gt;  $ :'data.frame':    1928 obs. of  6 variables:</span></span>
<span><span class='c'>#&gt;   ..$ patientID : Factor w/ 294 levels "1","2","3","4",..: 8 8 8 8 8 8 8 78 78 78 ...</span></span>
<span><span class='c'>#&gt;   ..$ cluster_id: int [1:1928] 92 92 92 92 92 92 92 62 62 62 ...</span></span>
<span><span class='c'>#&gt;   ..$ outcome   : Factor w/ 2 levels "none or mild",..: 1 1 1 1 1 1 1 1 1 1 ...</span></span>
<span><span class='c'>#&gt;   ..$ treatment : Factor w/ 2 levels "itraconazole",..: 1 1 1 1 1 1 1 1 1 1 ...</span></span>
<span><span class='c'>#&gt;   ..$ time      : num [1:1928] 6 1 2 12 3 ...</span></span>
<span><span class='c'>#&gt;   ..$ visit     : int [1:1928] 5 2 3 7 4 6 1 6 2 1 ...</span></span></pre>

I've included a `cluster_id` column in the resulting straps, because
here's an important question: If Patient 10 shows up twice in a strap,
do we have Patient 10's rows repeated twice? Or do we just so happen to
have two separate patients have the same observations? Let's save that
for another day.

### An rsample version of the balanced cluster bootstrap

I'll port the above procedure to rsample. Part of the sales pitch for 
rsample is that its dataframe for resample assignments is lightweight, 
memory-wise:

<!--

``` r
library(rsample)
bootstraps(d, times = 10) |> lobstr::obj_size()
#> 153.63 kB

# There is NOT a 10x increase in object size
bootstraps(d, times = 100) |> lobstr::obj_size()
#> 912.42 kB
```
-->
<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://rsample.tidymodels.org'>rsample</a></span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rsample.tidymodels.org/reference/bootstraps.html'>bootstraps</a></span><span class='o'>(</span><span class='nv'>d</span>, times <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>lobstr</span><span class='nf'>::</span><span class='nf'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; 153.63 kB</span></span>
<span></span>
<span><span class='c'># There is NOT a 10x increase in object size</span></span>
<span><span class='nf'><a href='https://rsample.tidymodels.org/reference/bootstraps.html'>bootstraps</a></span><span class='o'>(</span><span class='nv'>d</span>, times <span class='o'>=</span> <span class='m'>100</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>lobstr</span><span class='nf'>::</span><span class='nf'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; 912.42 kB</span></span></pre>

Each row here is a few vectors of row indices along with a shallow copy of the
original data. The actual memory cost we would expect for resampling isn't 
incurred until the resampled data is "materialized":

<!--

``` r
data_example <- bootstraps(d, times = 100)
data_example |> lobstr::obj_size()
#> 912.42 kB

# build and store the actual resampled copies of the data
data_example$data <- data_example$splits |> lapply(analysis)
data_example |> lobstr::obj_size()
#> 5.58 MB
```
-->
<pre class='chroma'>
<span><span class='nv'>data_example</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rsample.tidymodels.org/reference/bootstraps.html'>bootstraps</a></span><span class='o'>(</span><span class='nv'>d</span>, times <span class='o'>=</span> <span class='m'>100</span><span class='o'>)</span></span>
<span><span class='nv'>data_example</span> <span class='o'>|&gt;</span> <span class='nf'>lobstr</span><span class='nf'>::</span><span class='nf'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; 912.42 kB</span></span>
<span></span>
<span><span class='c'># build and store the actual resampled copies of the data</span></span>
<span><span class='nv'>data_example</span><span class='o'>$</span><span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nv'>data_example</span><span class='o'>$</span><span class='nv'>splits</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='nv'>analysis</span><span class='o'>)</span></span>
<span><span class='nv'>data_example</span> <span class='o'>|&gt;</span> <span class='nf'>lobstr</span><span class='nf'>::</span><span class='nf'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; 5.58 MB</span></span></pre>

An rsample "split" object is a dataframe plus two vectors of row indices
indicating which data are kept (*analysis*) and which are withheld
(*assessment*):

<!--

``` r
split <- make_splits(
  list(analysis = 1:10, assessment = 11:20), 
  data = d[1:20, ]
)
split
#> <Analysis/Assess/Total>
#> <10/10/20>

# data plus two vectors
str(split)
#> List of 3
#>  $ data  :'data.frame':	20 obs. of  5 variables:
#>   ..$ patientID: Factor w/ 294 levels "1","2","3","4",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   ..$ outcome  : Factor w/ 2 levels "none or mild",..: 2 2 2 1 1 1 1 1 1 2 ...
#>   ..$ treatment: Factor w/ 2 levels "itraconazole",..: 2 2 2 2 2 2 2 1 1 1 ...
#>   ..$ time     : num [1:20] 0 0.857 3.536 4.536 7.536 ...
#>   ..$ visit    : int [1:20] 1 2 3 4 5 6 7 1 2 3 ...
#>  $ in_id : int [1:10] 1 2 3 4 5 6 7 8 9 10
#>  $ out_id: int [1:10] 11 12 13 14 15 16 17 18 19 20
#>  - attr(*, "class")= chr "rsplit"
```
-->
<pre class='chroma'>
<span><span class='nv'>split</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rsample.tidymodels.org/reference/make_splits.html'>make_splits</a></span><span class='o'>(</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span>analysis <span class='o'>=</span> <span class='m'>1</span><span class='o'>:</span><span class='m'>10</span>, assessment <span class='o'>=</span> <span class='m'>11</span><span class='o'>:</span><span class='m'>20</span><span class='o'>)</span>, </span>
<span>  data <span class='o'>=</span> <span class='nv'>d</span><span class='o'>[</span><span class='m'>1</span><span class='o'>:</span><span class='m'>20</span>, <span class='o'>]</span></span>
<span><span class='o'>)</span></span>
<span><span class='nv'>split</span></span>
<span><span class='c'>#&gt; &lt;Analysis/Assess/Total&gt;</span></span>
<span><span class='c'>#&gt; &lt;10/10/20&gt;</span></span>
<span></span>
<span><span class='c'># data plus two vectors</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/utils/str.html'>str</a></span><span class='o'>(</span><span class='nv'>split</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; List of 3</span></span>
<span><span class='c'>#&gt;  $ data  :'data.frame':  20 obs. of  5 variables:</span></span>
<span><span class='c'>#&gt;   ..$ patientID: Factor w/ 294 levels "1","2","3","4",..: 1 1 1 1 1 1 1 2 2 2 ...</span></span>
<span><span class='c'>#&gt;   ..$ outcome  : Factor w/ 2 levels "none or mild",..: 2 2 2 1 1 1 1 1 1 2 ...</span></span>
<span><span class='c'>#&gt;   ..$ treatment: Factor w/ 2 levels "itraconazole",..: 2 2 2 2 2 2 2 1 1 1 ...</span></span>
<span><span class='c'>#&gt;   ..$ time     : num [1:20] 0 0.857 3.536 4.536 7.536 ...</span></span>
<span><span class='c'>#&gt;   ..$ visit    : int [1:20] 1 2 3 4 5 6 7 1 2 3 ...</span></span>
<span><span class='c'>#&gt;  $ in_id : int [1:10] 1 2 3 4 5 6 7 8 9 10</span></span>
<span><span class='c'>#&gt;  $ out_id: int [1:10] 11 12 13 14 15 16 17 18 19 20</span></span>
<span><span class='c'>#&gt;  - attr(*, "class")= chr "rsplit"</span></span></pre>

The `analysis()` and `assessment()` methods are what actually generate 
the resulting replicates:

<!--

``` r
analysis(split)
#>    patientID            outcome    treatment       time visit
#> 1          1 moderate or severe  terbinafine  0.0000000     1
#> 2          1 moderate or severe  terbinafine  0.8571429     2
#> 3          1 moderate or severe  terbinafine  3.5357140     3
#> 4          1       none or mild  terbinafine  4.5357140     4
#> 5          1       none or mild  terbinafine  7.5357140     5
#> 6          1       none or mild  terbinafine 10.0357100     6
#> 7          1       none or mild  terbinafine 13.0714300     7
#> 8          2       none or mild itraconazole  0.0000000     1
#> 9          2       none or mild itraconazole  0.9642857     2
#> 10         2 moderate or severe itraconazole  2.0000000     3
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://rsample.tidymodels.org/reference/as.data.frame.rsplit.html'>analysis</a></span><span class='o'>(</span><span class='nv'>split</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;    patientID            outcome    treatment       time visit</span></span>
<span><span class='c'>#&gt; 1          1 moderate or severe  terbinafine  0.0000000     1</span></span>
<span><span class='c'>#&gt; 2          1 moderate or severe  terbinafine  0.8571429     2</span></span>
<span><span class='c'>#&gt; 3          1 moderate or severe  terbinafine  3.5357140     3</span></span>
<span><span class='c'>#&gt; 4          1       none or mild  terbinafine  4.5357140     4</span></span>
<span><span class='c'>#&gt; 5          1       none or mild  terbinafine  7.5357140     5</span></span>
<span><span class='c'>#&gt; 6          1       none or mild  terbinafine 10.0357100     6</span></span>
<span><span class='c'>#&gt; 7          1       none or mild  terbinafine 13.0714300     7</span></span>
<span><span class='c'>#&gt; 8          2       none or mild itraconazole  0.0000000     1</span></span>
<span><span class='c'>#&gt; 9          2       none or mild itraconazole  0.9642857     2</span></span>
<span><span class='c'>#&gt; 10         2 moderate or severe itraconazole  2.0000000     3</span></span></pre>

My idea for implementing a balanced cluster bootstrap would be to do the
balanced bootstrapping manually as above but also stash the full parent
data inside of each split object. Here is a version of the function with
a couple of bells and whistles:

<!--

``` r
balanced_cluster_bootstraps <- function(data, cluster_vars, times, apparent) {
  # Helper for naming the straps
  zero_pad <- function(xs, prefix = "", width = 0) {
    # use widest element if bigger than `width`
    width <- max(c(nchar(xs), width))
    sprintf(paste0(prefix, "%0", width, "d"), xs)    
  }

  cols <- tidyselect::eval_select(rlang::enquo(cluster_vars), data) 
  data_ids <- unique(data[, cols, drop = FALSE])
  n_ids <- nrow(data_ids)
  ids <- seq_len(n_ids)
  
  assignments <- ids |>
    rep(times) |> 
    sample() |> 
    split(rep(seq_len(times), each = n_ids)) |> 
    unname()
  
  labels <- zero_pad(seq_len(times), prefix = "Bootstrap")
  
  if (apparent) {
    assignments <- c(assignments, list(ids))
    labels <- c(labels, "Apparent")
  }  
  
  splits <- assignments |> 
    lapply(function(xs) {
      l <- rsample::make_splits(
        list(analysis = xs, assessment = setdiff(ids, xs)), 
        data = data_ids,
        class = "cluster_id_sample"
      )
      l$data_parent <- data
      l
    })
  
  rsample::new_rset(
    splits = splits,
    id = labels,
    subclass = c("balanced_cluster_bootstrap", "rset")
  ) 
}

balanced_cluster_bootstraps(d, patientID, 10, apparent = TRUE)
#> # A tibble: 11 × 2
#>    splits            id         
#>    <list>            <chr>      
#>  1 <split [294/108]> Bootstrap01
#>  2 <split [294/99]>  Bootstrap02
#>  3 <split [294/105]> Bootstrap03
#>  4 <split [294/105]> Bootstrap04
#>  5 <split [294/107]> Bootstrap05
#>  6 <split [294/112]> Bootstrap06
#>  7 <split [294/112]> Bootstrap07
#>  8 <split [294/102]> Bootstrap08
#>  9 <split [294/106]> Bootstrap09
#> 10 <split [294/111]> Bootstrap10
#> 11 <split [294/0]>   Apparent
```
-->
<pre class='chroma'>
<span><span class='nv'>balanced_cluster_bootstraps</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>cluster_vars</span>, <span class='nv'>times</span>, <span class='nv'>apparent</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='c'># Helper for naming the straps</span></span>
<span>  <span class='nv'>zero_pad</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>xs</span>, <span class='nv'>prefix</span> <span class='o'>=</span> <span class='s'>""</span>, <span class='nv'>width</span> <span class='o'>=</span> <span class='m'>0</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='c'># use widest element if bigger than `width`</span></span>
<span>    <span class='nv'>width</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/Extremes.html'>max</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/nchar.html'>nchar</a></span><span class='o'>(</span><span class='nv'>xs</span><span class='o'>)</span>, <span class='nv'>width</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/paste.html'>paste0</a></span><span class='o'>(</span><span class='nv'>prefix</span>, <span class='s'>"%0"</span>, <span class='nv'>width</span>, <span class='s'>"d"</span><span class='o'>)</span>, <span class='nv'>xs</span><span class='o'>)</span>    </span>
<span>  <span class='o'>}</span></span>
<span></span>
<span>  <span class='nv'>cols</span> <span class='o'>&lt;-</span> <span class='nf'>tidyselect</span><span class='nf'>::</span><span class='nf'><a href='https://tidyselect.r-lib.org/reference/eval_select.html'>eval_select</a></span><span class='o'>(</span><span class='nf'>rlang</span><span class='nf'>::</span><span class='nf'><a href='https://rlang.r-lib.org/reference/enquo.html'>enquo</a></span><span class='o'>(</span><span class='nv'>cluster_vars</span><span class='o'>)</span>, <span class='nv'>data</span><span class='o'>)</span> </span>
<span>  <span class='nv'>data_ids</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/unique.html'>unique</a></span><span class='o'>(</span><span class='nv'>data</span><span class='o'>[</span>, <span class='nv'>cols</span>, drop <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>]</span><span class='o'>)</span></span>
<span>  <span class='nv'>n_ids</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>nrow</a></span><span class='o'>(</span><span class='nv'>data_ids</span><span class='o'>)</span></span>
<span>  <span class='nv'>ids</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nv'>n_ids</span><span class='o'>)</span></span>
<span>  </span>
<span>  <span class='nv'>assignments</span> <span class='o'>&lt;-</span> <span class='nv'>ids</span> <span class='o'>|&gt;</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/rep.html'>rep</a></span><span class='o'>(</span><span class='nv'>times</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/sample.html'>sample</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/split.html'>split</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/rep.html'>rep</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nv'>times</span><span class='o'>)</span>, each <span class='o'>=</span> <span class='nv'>n_ids</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/unname.html'>unname</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>  </span>
<span>  <span class='nv'>labels</span> <span class='o'>&lt;-</span> <span class='nf'>zero_pad</span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nv'>times</span><span class='o'>)</span>, prefix <span class='o'>=</span> <span class='s'>"Bootstrap"</span><span class='o'>)</span></span>
<span>  </span>
<span>  <span class='kr'>if</span> <span class='o'>(</span><span class='nv'>apparent</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>assignments</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='nv'>assignments</span>, <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span><span class='nv'>ids</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>labels</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='nv'>labels</span>, <span class='s'>"Apparent"</span><span class='o'>)</span></span>
<span>  <span class='o'>}</span>  </span>
<span>  </span>
<span>  <span class='nv'>splits</span> <span class='o'>&lt;-</span> <span class='nv'>assignments</span> <span class='o'>|&gt;</span> </span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='kr'>function</span><span class='o'>(</span><span class='nv'>xs</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>      <span class='nv'>l</span> <span class='o'>&lt;-</span> <span class='nf'>rsample</span><span class='nf'>::</span><span class='nf'><a href='https://rsample.tidymodels.org/reference/make_splits.html'>make_splits</a></span><span class='o'>(</span></span>
<span>        <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span>analysis <span class='o'>=</span> <span class='nv'>xs</span>, assessment <span class='o'>=</span> <span class='nf'><a href='https://generics.r-lib.org/reference/setops.html'>setdiff</a></span><span class='o'>(</span><span class='nv'>ids</span>, <span class='nv'>xs</span><span class='o'>)</span><span class='o'>)</span>, </span>
<span>        data <span class='o'>=</span> <span class='nv'>data_ids</span>,</span>
<span>        class <span class='o'>=</span> <span class='s'>"cluster_id_sample"</span></span>
<span>      <span class='o'>)</span></span>
<span>      <span class='nv'>l</span><span class='o'>$</span><span class='nv'>data_parent</span> <span class='o'>&lt;-</span> <span class='nv'>data</span></span>
<span>      <span class='nv'>l</span></span>
<span>    <span class='o'>}</span><span class='o'>)</span></span>
<span>  </span>
<span>  <span class='nf'>rsample</span><span class='nf'>::</span><span class='nf'><a href='https://rsample.tidymodels.org/reference/new_rset.html'>new_rset</a></span><span class='o'>(</span></span>
<span>    splits <span class='o'>=</span> <span class='nv'>splits</span>,</span>
<span>    id <span class='o'>=</span> <span class='nv'>labels</span>,</span>
<span>    subclass <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>"balanced_cluster_bootstrap"</span>, <span class='s'>"rset"</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span> </span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nf'>balanced_cluster_bootstraps</span><span class='o'>(</span><span class='nv'>d</span>, <span class='nv'>patientID</span>, <span class='m'>10</span>, apparent <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 11 × 2</span></span>
<span><span class='c'>#&gt;    splits            id         </span></span>
<span><span class='c'>#&gt;    &lt;list&gt;            &lt;chr&gt;      </span></span>
<span><span class='c'>#&gt;  1 &lt;split [294/108]&gt; Bootstrap01</span></span>
<span><span class='c'>#&gt;  2 &lt;split [294/99]&gt;  Bootstrap02</span></span>
<span><span class='c'>#&gt;  3 &lt;split [294/105]&gt; Bootstrap03</span></span>
<span><span class='c'>#&gt;  4 &lt;split [294/105]&gt; Bootstrap04</span></span>
<span><span class='c'>#&gt;  5 &lt;split [294/107]&gt; Bootstrap05</span></span>
<span><span class='c'>#&gt;  6 &lt;split [294/112]&gt; Bootstrap06</span></span>
<span><span class='c'>#&gt;  7 &lt;split [294/112]&gt; Bootstrap07</span></span>
<span><span class='c'>#&gt;  8 &lt;split [294/102]&gt; Bootstrap08</span></span>
<span><span class='c'>#&gt;  9 &lt;split [294/106]&gt; Bootstrap09</span></span>
<span><span class='c'>#&gt; 10 &lt;split [294/111]&gt; Bootstrap10</span></span>
<span><span class='c'>#&gt; 11 &lt;split [294/0]&gt;   Apparent</span></span></pre>

Time for the clever part, I think. We override the `analysis()` and `assessment()`
methods and have them perform the table join for us:

<!--

``` r
analysis.cluster_id_sample <- function(
    x, 
    data_parent = NULL, 
    name = "cluster_id", 
    ...
) {
  data <- as.data.frame(x, data = "analysis")
  join_names <- names(data)
  data[[name]] <- seq_len(nrow(data))
  if (is.null(data_parent)) {
    data_parent <- x$data_parent
  }
  merge(data, data_parent, by = join_names)
}

assessment.cluster_id_sample <- function(
    x, 
    data_parent = NULL, 
    name = "cluster_id", 
    ...
) {
  data <- as.data.frame(x, data = "assessment")
  join_names <- names(data)
  data[[name]] <- seq_len(nrow(data))
  if (is.null(data_parent)) {
    data_parent <- x$data_parent
  }
  merge(data, data_parent, by = join_names)
}

data_a <- balanced_cluster_bootstraps(d, patientID, 10, TRUE)
data_a[["data_analysis"]] <- lapply(data_a$splits, analysis)
data_a[["data_assessment"]] <- lapply(data_a$splits, assessment)
data_a
#> # A tibble: 11 × 4
#>    splits            id          data_analysis    data_assessment
#>    <list>            <chr>       <list>           <list>         
#>  1 <split [294/100]> Bootstrap01 <df [1,918 × 6]> <df [647 × 6]> 
#>  2 <split [294/92]>  Bootstrap02 <df [1,951 × 6]> <df [576 × 6]> 
#>  3 <split [294/103]> Bootstrap03 <df [1,923 × 6]> <df [673 × 6]> 
#>  4 <split [294/105]> Bootstrap04 <df [1,925 × 6]> <df [669 × 6]> 
#>  5 <split [294/104]> Bootstrap05 <df [1,919 × 6]> <df [663 × 6]> 
#>  6 <split [294/101]> Bootstrap06 <df [1,866 × 6]> <df [676 × 6]> 
#>  7 <split [294/109]> Bootstrap07 <df [1,884 × 6]> <df [716 × 6]> 
#>  8 <split [294/104]> Bootstrap08 <df [1,884 × 6]> <df [677 × 6]> 
#>  9 <split [294/105]> Bootstrap09 <df [1,919 × 6]> <df [666 × 6]> 
#> 10 <split [294/103]> Bootstrap10 <df [1,891 × 6]> <df [670 × 6]> 
#> 11 <split [294/0]>   Apparent    <df [1,908 × 6]> <df [0 × 6]>
```
-->
<pre class='chroma'>
<span><span class='nv'>analysis.cluster_id_sample</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span></span>
<span>    <span class='nv'>x</span>, </span>
<span>    <span class='nv'>data_parent</span> <span class='o'>=</span> <span class='kc'>NULL</span>, </span>
<span>    <span class='nv'>name</span> <span class='o'>=</span> <span class='s'>"cluster_id"</span>, </span>
<span>    <span class='nv'>...</span></span>
<span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/as.data.frame.html'>as.data.frame</a></span><span class='o'>(</span><span class='nv'>x</span>, data <span class='o'>=</span> <span class='s'>"analysis"</span><span class='o'>)</span></span>
<span>  <span class='nv'>join_names</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span></span>
<span>  <span class='nv'>data</span><span class='o'>[[</span><span class='nv'>name</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>nrow</a></span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  <span class='kr'>if</span> <span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/NULL.html'>is.null</a></span><span class='o'>(</span><span class='nv'>data_parent</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>data_parent</span> <span class='o'>&lt;-</span> <span class='nv'>x</span><span class='o'>$</span><span class='nv'>data_parent</span></span>
<span>  <span class='o'>}</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/merge.html'>merge</a></span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>data_parent</span>, by <span class='o'>=</span> <span class='nv'>join_names</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nv'>assessment.cluster_id_sample</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span></span>
<span>    <span class='nv'>x</span>, </span>
<span>    <span class='nv'>data_parent</span> <span class='o'>=</span> <span class='kc'>NULL</span>, </span>
<span>    <span class='nv'>name</span> <span class='o'>=</span> <span class='s'>"cluster_id"</span>, </span>
<span>    <span class='nv'>...</span></span>
<span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/as.data.frame.html'>as.data.frame</a></span><span class='o'>(</span><span class='nv'>x</span>, data <span class='o'>=</span> <span class='s'>"assessment"</span><span class='o'>)</span></span>
<span>  <span class='nv'>join_names</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span></span>
<span>  <span class='nv'>data</span><span class='o'>[[</span><span class='nv'>name</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/seq.html'>seq_len</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>nrow</a></span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  <span class='kr'>if</span> <span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/NULL.html'>is.null</a></span><span class='o'>(</span><span class='nv'>data_parent</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>data_parent</span> <span class='o'>&lt;-</span> <span class='nv'>x</span><span class='o'>$</span><span class='nv'>data_parent</span></span>
<span>  <span class='o'>}</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/merge.html'>merge</a></span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>data_parent</span>, by <span class='o'>=</span> <span class='nv'>join_names</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nv'>data_a</span> <span class='o'>&lt;-</span> <span class='nf'>balanced_cluster_bootstraps</span><span class='o'>(</span><span class='nv'>d</span>, <span class='nv'>patientID</span>, <span class='m'>10</span>, <span class='kc'>TRUE</span><span class='o'>)</span></span>
<span><span class='nv'>data_a</span><span class='o'>[[</span><span class='s'>"data_analysis"</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='nv'>data_a</span><span class='o'>$</span><span class='nv'>splits</span>, <span class='nv'>analysis</span><span class='o'>)</span></span>
<span><span class='nv'>data_a</span><span class='o'>[[</span><span class='s'>"data_assessment"</span><span class='o'>]</span><span class='o'>]</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='nv'>data_a</span><span class='o'>$</span><span class='nv'>splits</span>, <span class='nv'>assessment</span><span class='o'>)</span></span>
<span><span class='nv'>data_a</span></span>
<span><span class='c'>#&gt; # A tibble: 11 × 4</span></span>
<span><span class='c'>#&gt;    splits            id          data_analysis    data_assessment</span></span>
<span><span class='c'>#&gt;    &lt;list&gt;            &lt;chr&gt;       &lt;list&gt;           &lt;list&gt;         </span></span>
<span><span class='c'>#&gt;  1 &lt;split [294/100]&gt; Bootstrap01 &lt;df [1,918 × 6]&gt; &lt;df [647 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  2 &lt;split [294/92]&gt;  Bootstrap02 &lt;df [1,951 × 6]&gt; &lt;df [576 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  3 &lt;split [294/103]&gt; Bootstrap03 &lt;df [1,923 × 6]&gt; &lt;df [673 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  4 &lt;split [294/105]&gt; Bootstrap04 &lt;df [1,925 × 6]&gt; &lt;df [669 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  5 &lt;split [294/104]&gt; Bootstrap05 &lt;df [1,919 × 6]&gt; &lt;df [663 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  6 &lt;split [294/101]&gt; Bootstrap06 &lt;df [1,866 × 6]&gt; &lt;df [676 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  7 &lt;split [294/109]&gt; Bootstrap07 &lt;df [1,884 × 6]&gt; &lt;df [716 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  8 &lt;split [294/104]&gt; Bootstrap08 &lt;df [1,884 × 6]&gt; &lt;df [677 × 6]&gt; </span></span>
<span><span class='c'>#&gt;  9 &lt;split [294/105]&gt; Bootstrap09 &lt;df [1,919 × 6]&gt; &lt;df [666 × 6]&gt; </span></span>
<span><span class='c'>#&gt; 10 &lt;split [294/103]&gt; Bootstrap10 &lt;df [1,891 × 6]&gt; &lt;df [670 × 6]&gt; </span></span>
<span><span class='c'>#&gt; 11 &lt;split [294/0]&gt;   Apparent    &lt;df [1,908 × 6]&gt; &lt;df [0 × 6]&gt;</span></span></pre>

Finally, let's do a quick check for cluster balance:

<!--

``` r
data_a_unnested <- data_a |> 
  dplyr::select(-data_assessment) |> 
  dplyr::filter(id != "Apparent") |> 
  tidyr::unnest(cols = data_analysis)

# Number of strap:cluster_ids combinations =? 
# number of unique clusters times number of straps
data_a_unnested |> 
  dplyr::distinct(id, patientID, cluster_id) |> 
  nrow()
#> [1] 2940

length(unique(d$patientID)) * 10
#> [1] 2940

# Each patientID is replicated 10 times
counts_patient_ids <- data_a_unnested |> 
  dplyr::distinct(id, patientID, cluster_id) |> 
  dplyr::count(patientID) 
all(counts_patient_ids$n == 10)
#> [1] TRUE

# Each strap has the same number of clusters as original sample
count_replicate_ids <- data_a_unnested |> 
  dplyr::distinct(id, cluster_id) |> 
  dplyr::count(id)
all(count_replicate_ids$n == length(unique(d$patientID)))
#> [1] TRUE
```
-->
<pre class='chroma'>
<span><span class='nv'>data_a_unnested</span> <span class='o'>&lt;-</span> <span class='nv'>data_a</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='o'>-</span><span class='nv'>data_assessment</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>id</span> <span class='o'>!=</span> <span class='s'>"Apparent"</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>tidyr</span><span class='nf'>::</span><span class='nf'><a href='https://tidyr.tidyverse.org/reference/unnest.html'>unnest</a></span><span class='o'>(</span>cols <span class='o'>=</span> <span class='nv'>data_analysis</span><span class='o'>)</span></span>
<span></span>
<span><span class='c'># Number of strap:cluster_ids combinations =? </span></span>
<span><span class='c'># number of unique clusters times number of straps</span></span>
<span><span class='nv'>data_a_unnested</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/distinct.html'>distinct</a></span><span class='o'>(</span><span class='nv'>id</span>, <span class='nv'>patientID</span>, <span class='nv'>cluster_id</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/nrow.html'>nrow</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 2940</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/length.html'>length</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/unique.html'>unique</a></span><span class='o'>(</span><span class='nv'>d</span><span class='o'>$</span><span class='nv'>patientID</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>*</span> <span class='m'>10</span></span>
<span><span class='c'>#&gt; [1] 2940</span></span>
<span></span>
<span><span class='c'># Each patientID is replicated 10 times</span></span>
<span><span class='nv'>counts_patient_ids</span> <span class='o'>&lt;-</span> <span class='nv'>data_a_unnested</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/distinct.html'>distinct</a></span><span class='o'>(</span><span class='nv'>id</span>, <span class='nv'>patientID</span>, <span class='nv'>cluster_id</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/count.html'>count</a></span><span class='o'>(</span><span class='nv'>patientID</span><span class='o'>)</span> </span>
<span><span class='nf'><a href='https://rdrr.io/r/base/all.html'>all</a></span><span class='o'>(</span><span class='nv'>counts_patient_ids</span><span class='o'>$</span><span class='nv'>n</span> <span class='o'>==</span> <span class='m'>10</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE</span></span>
<span></span>
<span><span class='c'># Each strap has the same number of clusters as original sample</span></span>
<span><span class='nv'>count_replicate_ids</span> <span class='o'>&lt;-</span> <span class='nv'>data_a_unnested</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/distinct.html'>distinct</a></span><span class='o'>(</span><span class='nv'>id</span>, <span class='nv'>cluster_id</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/count.html'>count</a></span><span class='o'>(</span><span class='nv'>id</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/all.html'>all</a></span><span class='o'>(</span><span class='nv'>count_replicate_ids</span><span class='o'>$</span><span class='nv'>n</span> <span class='o'>==</span> <span class='nf'><a href='https://rdrr.io/r/base/length.html'>length</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/unique.html'>unique</a></span><span class='o'>(</span><span class='nv'>d</span><span class='o'>$</span><span class='nv'>patientID</span><span class='o'>)</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE</span></span></pre>
