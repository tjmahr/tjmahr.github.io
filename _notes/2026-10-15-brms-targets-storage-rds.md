---
title: "Storing brms models in a targets workflow"
date: 2026-10-15
tags: [r, brms, readr]
---



This note is a quick test for how to set up a function that fits a brms model
in a targets workflow. Specifically, should my function for fitting a brms model 
return a model or a path to saved model? That is,

<!--

``` r
f_ret_model <- function(...) {
  brm(..., file = "model", file_refit = "on_change")
}

f_ret_file <- function(...) {
  m <- brm(..., file = "model", file_refit = "on_change")
  m$file
}
```
-->
<pre class='chroma'>
<span><span class='nv'>f_ret_model</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>...</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span><span class='nv'>...</span>, file <span class='o'>=</span> <span class='s'>"model"</span>, file_refit <span class='o'>=</span> <span class='s'>"on_change"</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nv'>f_ret_file</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>...</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span><span class='nv'>...</span>, file <span class='o'>=</span> <span class='s'>"model"</span>, file_refit <span class='o'>=</span> <span class='s'>"on_change"</span><span class='o'>)</span></span>
<span>  <span class='nv'>m</span><span class='o'>$</span><span class='nv'>file</span></span>
<span><span class='o'>}</span></span></pre>

We definitely want to leverage the `file` argument because it caches 
the model so that it will only refit if the model's formula, data, 
priors, or generated Stan code change. But should I use inside of a 
[targets](https://books.ropensci.org/targets/) workflow?

## Setting up a temporary targets project

First, because this notebook and website is rendered using a targets
pipeline, I am going to set up a temporary directory to serve as the
working directory for this post.

<!--

``` r
library(targets)
library(tarchetypes)
library(brms)

dir <- tempfile() 
dir.create(dir)
setwd(dir)
knitr::opts_knit$set(root.dir = dir)

# I don't want to accidentally mangle my website's targets files,
# so I'm adding an extra underscore
targets::tar_config_set(script = "__targets.R")
targets::tar_config_set(store = "__targets")
```
-->
<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://docs.ropensci.org/targets/'>targets</a></span><span class='o'>)</span></span>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://docs.ropensci.org/tarchetypes/'>tarchetypes</a></span><span class='o'>)</span></span>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://github.com/paul-buerkner/brms'>brms</a></span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>dir</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span><span class='o'>)</span> </span>
<span><span class='nf'><a href='https://rdrr.io/r/base/files2.html'>dir.create</a></span><span class='o'>(</span><span class='nv'>dir</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/getwd.html'>setwd</a></span><span class='o'>(</span><span class='nv'>dir</span><span class='o'>)</span></span>
<span><span class='nf'>knitr</span><span class='nf'>::</span><span class='nv'><a href='https://rdrr.io/pkg/knitr/man/opts_knit.html'>opts_knit</a></span><span class='o'>$</span><span class='nf'>set</span><span class='o'>(</span>root.dir <span class='o'>=</span> <span class='nv'>dir</span><span class='o'>)</span></span>
<span></span>
<span><span class='c'># I don't want to accidentally mangle my website's targets files,</span></span>
<span><span class='c'># so I'm adding an extra underscore</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_config_set.html'>tar_config_set</a></span><span class='o'>(</span>script <span class='o'>=</span> <span class='s'>"__targets.R"</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_config_set.html'>tar_config_set</a></span><span class='o'>(</span>store <span class='o'>=</span> <span class='s'>"__targets"</span><span class='o'>)</span></span></pre>

Next, I am going to create a `_targets.R` file that will fit the same
model three times, but the models will try different things:

  - `model_1`: returns the model object
  - `model_2`: returns the path to the model's .rds file
  - `model_3`: returns the model's .rds file and targets reads in the
    .rds file

<!--

``` r
tar_script({
  library(targets)
  library(brms)
  dir.create("models", showWarnings = FALSE)
  dir.create("stan", showWarnings = FALSE)
  # Avoids recompilations
  options(cmdstanr_write_stan_file_dir = normalizePath("stan"))
  
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    fit1 <- brm(
      count ~ zBase * Trt + (1|patient),
      data = data, 
      family = poisson(),
      prior = p + prior(cauchy(0, 2), class = sd),
      file = file,
      file_refit = "on_change",
      backend = "cmdstanr",
      silent = 2, 
      chains = 1
    )
    fit1
  }
  
  f1 <- function(data, .sd = 10) {
    m <- fit(data, "models/test1", .sd = .sd)
    m
  }
  f2 <- function(data, .sd = 10) {
    m <- fit(data, "models/test2", .sd = .sd)
    m$file
  }
  f3 <- function(data, .sd = 10) {
    m <- fit(data, "models/test3", .sd = .sd)
    m$file
  }

  tarchetypes::tar_assign({
    data <- tar_target(epilepsy)
    model_1 <- tar_target(f1(data))
    model_2 <- tar_target(f2(data), format = "file")
    model_3 <- tarchetypes::tar_file_read(
      command = f3(data),
      read = readRDS(file = !! .x)
    )
  })
}, ask = FALSE)
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_script.html'>tar_script</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://docs.ropensci.org/targets/'>targets</a></span><span class='o'>)</span></span>
<span>  <span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://github.com/paul-buerkner/brms'>brms</a></span><span class='o'>)</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/files2.html'>dir.create</a></span><span class='o'>(</span><span class='s'>"models"</span>, showWarnings <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>)</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/files2.html'>dir.create</a></span><span class='o'>(</span><span class='s'>"stan"</span>, showWarnings <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>)</span></span>
<span>  <span class='c'># Avoids recompilations</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/options.html'>options</a></span><span class='o'>(</span>cmdstanr_write_stan_file_dir <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/normalizePath.html'>normalizePath</a></span><span class='o'>(</span><span class='s'>"stan"</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  </span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nv'>fit1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>      <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>      data <span class='o'>=</span> <span class='nv'>data</span>, </span>
<span>      family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>      prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>      file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>      file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>      backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>      silent <span class='o'>=</span> <span class='m'>2</span>, </span>
<span>      chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>fit1</span></span>
<span>  <span class='o'>}</span></span>
<span>  </span>
<span>  <span class='nv'>f1</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'>fit</span><span class='o'>(</span><span class='nv'>data</span>, <span class='s'>"models/test1"</span>, .sd <span class='o'>=</span> <span class='nv'>.sd</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span></span>
<span>  <span class='o'>}</span></span>
<span>  <span class='nv'>f2</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'>fit</span><span class='o'>(</span><span class='nv'>data</span>, <span class='s'>"models/test2"</span>, .sd <span class='o'>=</span> <span class='nv'>.sd</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span><span class='o'>$</span><span class='nv'>file</span></span>
<span>  <span class='o'>}</span></span>
<span>  <span class='nv'>f3</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'>fit</span><span class='o'>(</span><span class='nv'>data</span>, <span class='s'>"models/test3"</span>, .sd <span class='o'>=</span> <span class='nv'>.sd</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span><span class='o'>$</span><span class='nv'>file</span></span>
<span>  <span class='o'>}</span></span>
<span></span>
<span>  <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_assign.html'>tar_assign</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>    <span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nv'>epilepsy</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f1</span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f2</span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span>, format <span class='o'>=</span> <span class='s'>"file"</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_3</span> <span class='o'>&lt;-</span> <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_file_read.html'>tar_file_read</a></span><span class='o'>(</span></span>
<span>      command <span class='o'>=</span> <span class='nf'>f3</span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span>,</span>
<span>      read <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span>file <span class='o'>=</span> <span class='o'>!</span><span class='o'>!</span> <span class='nv'>.x</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>}</span><span class='o'>)</span></span>
<span><span class='o'>}</span>, ask <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>)</span></span></pre>

Now, we fit the models:

<!--

``` r
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + data dispatched
#> ✔ data completed [0ms, 2.68 kB]
#> + model_3_file dispatched
#> ✔ model_3_file completed [54.6s, 561.05 kB]
#> + model_1 dispatched
#> ✔ model_1 completed [9.4s, 1.09 MB]
#> + model_2 dispatched
#> ✔ model_2 completed [10.8s, 552.96 kB]
#> + model_3 dispatched
#> ✔ model_3 completed [0ms, 561.07 kB]
#> ✔ ended pipeline [1m 15.1s, 5 completed, 0 skipped]
#> Warning message:
#> package 'Rcpp' was built under R version 4.6.1 
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + data dispatched</span></span>
<span><span class='c'>#&gt; ✔ data completed [0ms, 2.68 kB]</span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [54.6s, 561.05 kB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [9.4s, 1.09 MB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [10.8s, 552.96 kB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [0ms, 561.07 kB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [1m 15.1s, 5 completed, 0 skipped]</span></span>
<span><span class='c'>#&gt; Warning message:</span></span>
<span><span class='c'>#&gt; package 'Rcpp' was built under R version 4.6.1 </span></span></pre>

Confirm that they don't refit when nothing changes:

<!--

``` r
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> ✔ skipped pipeline [93ms, 5 skipped]
#> Warning message:
#> package 'Rcpp' was built under R version 4.6.1 
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; ✔ skipped pipeline [93ms, 5 skipped]</span></span>
<span><span class='c'>#&gt; Warning message:</span></span>
<span><span class='c'>#&gt; package 'Rcpp' was built under R version 4.6.1 </span></span></pre>

When we try to load in the targets, we see that `model_2` is *just* a
path, and it's a relative path at that. We have to take an extra step of
reading the file content. `model_1` and `model_3` provide the actual
brms objects:

<!--

``` r
targets::tar_load(model_1)
class(model_1)
#> [1] "brmsfit"

targets::tar_load(model_2)
model_2
#> [1] "models/test2.rds"
class(readRDS(model_2))
#> [1] "brmsfit"

targets::tar_load(model_3)
class(model_3)
#> [1] "brmsfit"
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/class.html'>class</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "brmsfit"</span></span>
<span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_2</span><span class='o'>)</span></span>
<span><span class='nv'>model_2</span></span>
<span><span class='c'>#&gt; [1] "models/test2.rds"</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/class.html'>class</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span><span class='nv'>model_2</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "brmsfit"</span></span>
<span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_3</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/class.html'>class</a></span><span class='o'>(</span><span class='nv'>model_3</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "brmsfit"</span></span></pre>

`model_3` is actually two targets. `tar_file_read(name, command, read)`
creates a `[name]_file` for the result of the `command` and creates
`[name]` target for the result of `read`:

<!--

``` r
targets::tar_visnetwork()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> Warning message:
#> package 'Rcpp' was built under R version 4.6.1 
```

<div class="figure" style="text-align: center">
<img src="/figs/notes/2026-10-15-brms-targets-storage-rds/unnamed-chunk-8-1.png" alt="dependency graph for this targets project" width="80%" />
<p class="caption">center</p>
</div>
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_visnetwork.html'>tar_visnetwork</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Warning message:</span></span>
<span><span class='c'>#&gt; package 'Rcpp' was built under R version 4.6.1 </span></span></pre>

<div class="figure" style="text-align: center">
<img src="/figs/notes/2026-10-15-brms-targets-storage-rds/unnamed-chunk-8-1.png" alt="dependency graph for this targets project" width="80%" />
<p class="caption">center</p>
</div>

Because targets never does read in the contents of `model_2` as R data,
it has no storage footprint:

<!--

``` r
targets::tar_path_store() |> 
  fs::dir_ls() |> 
  fs::dir_ls(regexp = "objects") |>
  fs::file_info() |> 
  dplyr::select(path, size)
#> # A tibble: 3 × 2
#>   path                             size
#>   <fs::path>                <fs::bytes>
#> 1 __targets/objects/data          2.62K
#> 2 __targets/objects/model_1       1.04M
#> 3 __targets/objects/model_3     547.92K
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_store.html'>tar_path_store</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span>regexp <span class='o'>=</span> <span class='s'>"objects"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/file_info.html'>file_info</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>path</span>, <span class='nv'>size</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 3 × 2</span></span>
<span><span class='c'>#&gt;   path                             size</span></span>
<span><span class='c'>#&gt;   &lt;fs::path&gt;                &lt;fs::bytes&gt;</span></span>
<span><span class='c'>#&gt; 1 __targets/objects/data          2.62K</span></span>
<span><span class='c'>#&gt; 2 __targets/objects/model_1       1.04M</span></span>
<span><span class='c'>#&gt; 3 __targets/objects/model_3     547.92K</span></span></pre>

So, the `model_2` approach means that there is no duplicated storage of
models between `_targets/objects/` and `models/`.


### At this point, I went down a terrible rabbit-hole

*Pop quiz*: In the filesize printout, why is `model_1` twice as big as
`model_3`?

Here is a hint. `lobstr::obj_size()` measures the size of an R object, 
including environments referenced by the object:

<!--

``` r
targets::tar_load(model_1)
targets::tar_load(model_2)
sizes_1 <- model_1 |> lapply(lobstr::obj_size) |> unlist()
sizes_3 <- model_3 |> lapply(lobstr::obj_size) |> unlist()
sizes_3 - sizes_1
#>   formula      data     prior     data2  stanvars     model save_pars algorithm 
#>  -1119784  -1132736         0         0         0         0         0         0 
#>   backend   threads    opencl stan_args       fit     basis  criteria      file 
#>         0         0         0         0     86544         0         0         0 
#>   version    family   autocor     ranef cov_ranef stan_funs data.name 
#>         0         0         0  -1115512         0         0         0
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_2</span><span class='o'>)</span></span>
<span><span class='nv'>sizes_1</span> <span class='o'>&lt;-</span> <span class='nv'>model_1</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='nf'>lobstr</span><span class='nf'>::</span><span class='nv'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/unlist.html'>unlist</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nv'>sizes_3</span> <span class='o'>&lt;-</span> <span class='nv'>model_3</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='nf'>lobstr</span><span class='nf'>::</span><span class='nv'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/unlist.html'>unlist</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nv'>sizes_3</span> <span class='o'>-</span> <span class='nv'>sizes_1</span></span>
<span><span class='c'>#&gt;   formula      data     prior     data2  stanvars     model save_pars algorithm </span></span>
<span><span class='c'>#&gt;  -1119784  -1132736         0         0         0         0         0         0 </span></span>
<span><span class='c'>#&gt;   backend   threads    opencl stan_args       fit     basis  criteria      file </span></span>
<span><span class='c'>#&gt;         0         0         0         0     86544         0         0         0 </span></span>
<span><span class='c'>#&gt;   version    family   autocor     ranef cov_ranef stan_funs data.name </span></span>
<span><span class='c'>#&gt;         0         0         0  -1115512         0         0         0</span></span></pre>

That's where within the model structure the sizes differ. What extra
thing is in those parts of the model?

Here is another hint:

<!--

``` r
m <- function() {
  fit <- lm(mpg ~ 1, mtcars)
  fit
}
lobstr::obj_size(m())
#> 11.29 kB

m <- function() {
  fit <- lm(mpg ~ 1, mtcars)
  BIG_DATA <- rnorm(1000000)
  fit
}
lobstr::obj_size(m())
#> 8.01 MB
```
-->
<pre class='chroma'>
<span><span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/lm.html'>lm</a></span><span class='o'>(</span><span class='nv'>mpg</span> <span class='o'>~</span> <span class='m'>1</span>, <span class='nv'>mtcars</span><span class='o'>)</span></span>
<span>  <span class='nv'>fit</span></span>
<span><span class='o'>}</span></span>
<span><span class='nf'>lobstr</span><span class='nf'>::</span><span class='nf'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>(</span><span class='nf'>m</span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; 11.29 kB</span></span>
<span></span>
<span><span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/lm.html'>lm</a></span><span class='o'>(</span><span class='nv'>mpg</span> <span class='o'>~</span> <span class='m'>1</span>, <span class='nv'>mtcars</span><span class='o'>)</span></span>
<span>  <span class='nv'>BIG_DATA</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='o'>(</span><span class='m'>1000000</span><span class='o'>)</span></span>
<span>  <span class='nv'>fit</span></span>
<span><span class='o'>}</span></span>
<span><span class='nf'>lobstr</span><span class='nf'>::</span><span class='nf'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>(</span><span class='nf'>m</span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; 8.01 MB</span></span></pre>

That's how the model's context surrounding context can influence the size of 
the model. What's going on?

Here is yet another hint:

<!--

``` r
model_1$formula$formula |> environment() |> ls()
#> [1] "data" "file" "fit1" "p"
model_3$formula$formula |> environment() |> ls()
#> [1] "data" "file" "p"
```
-->
<pre class='chroma'>
<span><span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "fit1" "p"</span></span>
<span><span class='nv'>model_3</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "p"</span></span></pre>

That's the precise difference in the context. But how did that self-reference 
happen?

Here is a final hint:

<!--

``` r
tar_path_target("model_1") |> file.size()
#> [1] 1087307
model_1$file |> file.size()
#> [1] 552379
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_target.html'>tar_path_target</a></span><span class='o'>(</span><span class='s'>"model_1"</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 1087307</span></span>
<span><span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>file</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 552379</span></span></pre>

The formulas in the model are R formula objects which means that they
contain a reference to their surrounding environment. That is a
*reference*, as opposed to a copy of the environment. If we ask the
model for the environment, we see the environment as it is *right now*,
including any changes after the fitting of the model. That's why
`BIG_DATA` in the second hint can make the model's `lobstr::obj_size()`
increase. 

In `f1()`, this sequence happens:

1.  The model is fit and saved to the `models/` file, and return by
    `brm()`.
2.  The model object is assigned to `fit1`. `fit1` is now part of the
    environment, so that if we asked the model's formula for the
    environment, we would see `fit1`.
3.  targets saves the model object to the `_targets/objects` including
    the updated environment.

This behavior means that we should not assign the model inside `f1()` if
possible. Now I'm starting to worry about much memory I've wasted by mostly 
following pattern 1 (storing models in both `models` and `_targets/objects` and 
not being careful about assignments).

One might ask if the formula's environment references the fitted model,
then doesn't that "inner" model's formula also have its own model, ad
infinitum? Yes, but it's just a loop. The environment captured by the
out model's formula and the inner model's formula are the same.

<!--

``` r
targets::tar_load(model_1)
outer_e <- model_1$formula$formula |> 
  environment() 
inner_model <- outer_e$fit1

inner_e <- inner_model$formula$formula |> 
  environment() 
outer_e
#> <environment: 0x0000020349d919e0>
inner_e
#> <environment: 0x0000020349d919e0>
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nv'>outer_e</span> <span class='o'>&lt;-</span> <span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> </span>
<span><span class='nv'>inner_model</span> <span class='o'>&lt;-</span> <span class='nv'>outer_e</span><span class='o'>$</span><span class='nv'>fit1</span></span>
<span></span>
<span><span class='nv'>inner_e</span> <span class='o'>&lt;-</span> <span class='nv'>inner_model</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> </span>
<span><span class='nv'>outer_e</span></span>
<span><span class='c'>#&gt; &lt;environment: 0x0000020349d919e0&gt;</span></span>
<span><span class='nv'>inner_e</span></span>
<span><span class='c'>#&gt; &lt;environment: 0x0000020349d919e0&gt;</span></span></pre>

So, what is being duplicated? Let's surgically remove the referenced
model from the formula environment and compare file sizes:

<!--

``` r
model_1_copy <- unserialize(serialize(model_1, NULL))

# Remove "fit1" in the formula's environment 
e <- model_1_copy$formula$formula |> 
  environment()
rm("fit1", envir = e) 

saveRDS(model_1, "models/model-1-resave.rds")
saveRDS(model_1_copy, "models/model-1-clean.rds")

file.size("models/model-1-resave.rds")
#> [1] 1087315
file.size("models/model-1-clean.rds")
#> [1] 552392
```
-->
<pre class='chroma'>
<span><span class='nv'>model_1_copy</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/serialize.html'>unserialize</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/serialize.html'>serialize</a></span><span class='o'>(</span><span class='nv'>model_1</span>, <span class='kc'>NULL</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='c'># Remove "fit1" in the formula's environment </span></span>
<span><span class='nv'>e</span> <span class='o'>&lt;-</span> <span class='nv'>model_1_copy</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/rm.html'>rm</a></span><span class='o'>(</span><span class='s'>"fit1"</span>, envir <span class='o'>=</span> <span class='nv'>e</span><span class='o'>)</span> </span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>saveRDS</a></span><span class='o'>(</span><span class='nv'>model_1</span>, <span class='s'>"models/model-1-resave.rds"</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>saveRDS</a></span><span class='o'>(</span><span class='nv'>model_1_copy</span>, <span class='s'>"models/model-1-clean.rds"</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='s'>"models/model-1-resave.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 1087315</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='s'>"models/model-1-clean.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 552392</span></span></pre>

Maybe it's the posterior draws in the inner model? Let's only remove those and 
see what happens.

<!--

``` r
model_1_copy2 <- unserialize(serialize(model_1, NULL))

# Ablate the `$fit` part of the inner model
e <- model_1_copy2$formula$formula |> 
  environment()
with(e, fit1$fit <- NULL)
is.null(e$fit1$fit)
#> [1] TRUE
```
-->
<pre class='chroma'>
<span><span class='nv'>model_1_copy2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/serialize.html'>unserialize</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/serialize.html'>serialize</a></span><span class='o'>(</span><span class='nv'>model_1</span>, <span class='kc'>NULL</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='c'># Ablate the `$fit` part of the inner model</span></span>
<span><span class='nv'>e</span> <span class='o'>&lt;-</span> <span class='nv'>model_1_copy2</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/with.html'>with</a></span><span class='o'>(</span><span class='nv'>e</span>, <span class='nv'>fit1</span><span class='o'>$</span><span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kc'>NULL</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/NULL.html'>is.null</a></span><span class='o'>(</span><span class='nv'>e</span><span class='o'>$</span><span class='nv'>fit1</span><span class='o'>$</span><span class='nv'>fit</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE</span></span></pre>

The outer model has its draws.

<!--

``` r
model_1_copy2
#> Loading required namespace: rstan
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: count ~ zBase * Trt + (1 | patient) 
#>    Data: data (Number of observations: 236) 
#>   Draws: 1 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 1000
#> 
#> Multilevel Hyperparameters:
#> ~patient (Number of levels: 59) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.59      0.07     0.46     0.74 1.00      178      379
#> 
#> Regression Coefficients:
#>            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept      1.78      0.11     1.56     2.01 1.00      192      296
#> zBase          0.70      0.11     0.46     0.91 1.00      209      394
#> Trt1          -0.28      0.16    -0.62     0.01 1.00      186      467
#> zBase:Trt1     0.03      0.16    -0.29     0.31 1.00      201      345
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```
-->
<pre class='chroma'>
<span><span class='nv'>model_1_copy2</span></span>
<span><span class='c'>#&gt; Loading required namespace: rstan</span></span>
<span><span class='c'>#&gt;  Family: poisson </span></span>
<span><span class='c'>#&gt;   Links: mu = log </span></span>
<span><span class='c'>#&gt; Formula: count ~ zBase * Trt + (1 | patient) </span></span>
<span><span class='c'>#&gt;    Data: data (Number of observations: 236) </span></span>
<span><span class='c'>#&gt;   Draws: 1 chains, each with iter = 2000; warmup = 1000; thin = 1;</span></span>
<span><span class='c'>#&gt;          total post-warmup draws = 1000</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Multilevel Hyperparameters:</span></span>
<span><span class='c'>#&gt; ~patient (Number of levels: 59) </span></span>
<span><span class='c'>#&gt;               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS</span></span>
<span><span class='c'>#&gt; sd(Intercept)     0.59      0.07     0.46     0.74 1.00      178      379</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Regression Coefficients:</span></span>
<span><span class='c'>#&gt;            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS</span></span>
<span><span class='c'>#&gt; Intercept      1.78      0.11     1.56     2.01 1.00      192      296</span></span>
<span><span class='c'>#&gt; zBase          0.70      0.11     0.46     0.91 1.00      209      394</span></span>
<span><span class='c'>#&gt; Trt1          -0.28      0.16    -0.62     0.01 1.00      186      467</span></span>
<span><span class='c'>#&gt; zBase:Trt1     0.03      0.16    -0.29     0.31 1.00      201      345</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Draws were sampled using sample(hmc). For each parameter, Bulk_ESS</span></span>
<span><span class='c'>#&gt; and Tail_ESS are effective sample size measures, and Rhat is the potential</span></span>
<span><span class='c'>#&gt; scale reduction factor on split chains (at convergence, Rhat = 1).</span></span></pre>

But the inner one gives a summary I had never seen before:

<!--

``` r
e$fit1
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: count ~ zBase * Trt + (1 | patient) 
#>    Data: data (Number of observations: 236) 
#> 
#> The model does not contain posterior draws.
```
-->
<pre class='chroma'>
<span><span class='nv'>e</span><span class='o'>$</span><span class='nv'>fit1</span></span>
<span><span class='c'>#&gt;  Family: poisson </span></span>
<span><span class='c'>#&gt;   Links: mu = log </span></span>
<span><span class='c'>#&gt; Formula: count ~ zBase * Trt + (1 | patient) </span></span>
<span><span class='c'>#&gt;    Data: data (Number of observations: 236) </span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The model does not contain posterior draws.</span></span></pre>

Comparing file sizes, it does seem like the duplicated posterior draws
are responsible for the inflated size.

<!--

``` r
saveRDS(model_1_copy2, "models/model-1-clean2.rds")

file.size("models/model-1-resave.rds")
#> [1] 1087315
file.size("models/model-1-clean.rds")
#> [1] 552392
file.size("models/model-1-clean2.rds")
#> [1] 555383
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>saveRDS</a></span><span class='o'>(</span><span class='nv'>model_1_copy2</span>, <span class='s'>"models/model-1-clean2.rds"</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='s'>"models/model-1-resave.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 1087315</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='s'>"models/model-1-clean.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 552392</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='s'>"models/model-1-clean2.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] 555383</span></span></pre>


### Some nonstandard evaluation to make it easy to tweak the targets scripts

This note is now off the rails. We're in the rabbithole. I want to 
mess with this environment stuff some more. 

In the next few tests, I'll be tweaking parts of the `tar_script()`
code, so I am going to define some code chunks and assemble the final
code block by injecting expressions. This move cuts down on copy-pasting
and makes sure the same code is reused across demos.

<!--

``` r
s_setup <- quote({
  library(targets)
  library(brms)
  dir.create("models", showWarnings = FALSE)
  dir.create("stan", showWarnings = FALSE)
  options(cmdstanr_write_stan_file_dir = normalizePath("stan"))
})

s_function <- quote({
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    fit1 <- brm(
      count ~ zBase * Trt + (1|patient),
      data = data, 
      family = poisson(),
      prior = p + prior(cauchy(0, 2), class = sd),
      file = file,
      file_refit = "on_change",
      backend = "cmdstanr",
      silent = 2,
      chains = 1
    )
    fit1
  }
})

s_subfunctions <- quote({
  f1 <- function(data, .sd = 10) {
    m <- fit(data, "models/test1", .sd = .sd)
    m
  }
  f2 <- function(data, .sd = 10) {
    m <- fit(data, "models/test2", .sd = .sd)
    m$file
  }
  f3 <- function(data, .sd = 10) {
    m <- fit(data, "models/test3", .sd = .sd)
    m$file
  }
})

s_targets <- quote({
  tarchetypes::tar_assign({
    data <- tar_target(epilepsy)
    model_1 <- tar_target(f1(data))
    model_2 <- tar_target(f2(data), format = "file")
    model_3 <- tarchetypes::tar_file_read(
      command = f3(data),
      read = readRDS(file = !! .x)
    )
  })
})

write_tar_script <- function(...) {
  code <- rlang::expr({!!! list(...)})
  rlang::eval_tidy(rlang::expr(tar_script(!! code, ask = FALSE)))
}

write_tar_script(s_setup, s_function, s_subfunctions, s_targets)
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> ✔ skipped pipeline [93ms, 5 skipped]
#> Warning message:
#> package 'Rcpp' was built under R version 4.6.1 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_setup</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://docs.ropensci.org/targets/'>targets</a></span><span class='o'>)</span></span>
<span>  <span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://github.com/paul-buerkner/brms'>brms</a></span><span class='o'>)</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/files2.html'>dir.create</a></span><span class='o'>(</span><span class='s'>"models"</span>, showWarnings <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>)</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/files2.html'>dir.create</a></span><span class='o'>(</span><span class='s'>"stan"</span>, showWarnings <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>)</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/options.html'>options</a></span><span class='o'>(</span>cmdstanr_write_stan_file_dir <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/normalizePath.html'>normalizePath</a></span><span class='o'>(</span><span class='s'>"stan"</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>s_function</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nv'>fit1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>      <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>      data <span class='o'>=</span> <span class='nv'>data</span>, </span>
<span>      family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>      prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>      file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>      file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>      backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>      silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>      chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>fit1</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>s_subfunctions</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='nv'>f1</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'>fit</span><span class='o'>(</span><span class='nv'>data</span>, <span class='s'>"models/test1"</span>, .sd <span class='o'>=</span> <span class='nv'>.sd</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span></span>
<span>  <span class='o'>}</span></span>
<span>  <span class='nv'>f2</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'>fit</span><span class='o'>(</span><span class='nv'>data</span>, <span class='s'>"models/test2"</span>, .sd <span class='o'>=</span> <span class='nv'>.sd</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span><span class='o'>$</span><span class='nv'>file</span></span>
<span>  <span class='o'>}</span></span>
<span>  <span class='nv'>f3</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'>fit</span><span class='o'>(</span><span class='nv'>data</span>, <span class='s'>"models/test3"</span>, .sd <span class='o'>=</span> <span class='nv'>.sd</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span><span class='o'>$</span><span class='nv'>file</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>s_targets</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_assign.html'>tar_assign</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>    <span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nv'>epilepsy</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f1</span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f2</span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span>, format <span class='o'>=</span> <span class='s'>"file"</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_3</span> <span class='o'>&lt;-</span> <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_file_read.html'>tar_file_read</a></span><span class='o'>(</span></span>
<span>      command <span class='o'>=</span> <span class='nf'>f3</span><span class='o'>(</span><span class='nv'>data</span><span class='o'>)</span>,</span>
<span>      read <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span>file <span class='o'>=</span> <span class='o'>!</span><span class='o'>!</span> <span class='nv'>.x</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>}</span><span class='o'>)</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>write_tar_script</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>...</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>code</span> <span class='o'>&lt;-</span> <span class='nf'>rlang</span><span class='nf'>::</span><span class='nf'><a href='https://rlang.r-lib.org/reference/expr.html'>expr</a></span><span class='o'>(</span><span class='o'>{</span><span class='o'>!</span><span class='o'>!</span><span class='o'>!</span> <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span><span class='nv'>...</span><span class='o'>)</span><span class='o'>}</span><span class='o'>)</span></span>
<span>  <span class='nf'>rlang</span><span class='nf'>::</span><span class='nf'><a href='https://rlang.r-lib.org/reference/eval_tidy.html'>eval_tidy</a></span><span class='o'>(</span><span class='nf'>rlang</span><span class='nf'>::</span><span class='nf'><a href='https://rlang.r-lib.org/reference/expr.html'>expr</a></span><span class='o'>(</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_script.html'>tar_script</a></span><span class='o'>(</span><span class='o'>!</span><span class='o'>!</span> <span class='nv'>code</span>, ask <span class='o'>=</span> <span class='kc'>FALSE</span><span class='o'>)</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; ✔ skipped pipeline [93ms, 5 skipped]</span></span>
<span><span class='c'>#&gt; Warning message:</span></span>
<span><span class='c'>#&gt; package 'Rcpp' was built under R version 4.6.1 </span></span></pre>

Let's make a superficial change to the pipeline by 1) cleaning up the
`fit1 <-` assignment and 2) including the `.sd` value in the function
arguments in the targets commands. These change affects the targets'
commands so they need to be remade but it does not change the brms
models so they do not need to be refitted. The saved files should suffice:

<!--

``` r
s_function2 <- quote({
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    brm(
      count ~ zBase * Trt + (1|patient),
      data = data, 
      family = poisson(),
      prior = p + prior(cauchy(0, 2), class = sd),
      file = file,
      file_refit = "on_change",
      backend = "cmdstanr",
      silent = 2,
      chains = 1
    )
  }
})

s_targets2 <- quote({
  tarchetypes::tar_assign({
    data <- tar_target(epilepsy)
    model_1 <- tar_target(f1(data, .sd = 10))
    model_2 <- tar_target(f2(data, .sd = 10), format = "file")
    model_3 <- tarchetypes::tar_file_read(
      command = f3(data, .sd = 10),
      read = readRDS(file = !! .x)
    )
  })
})

write_tar_script(s_setup, s_function2, s_subfunctions, s_targets2)
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> ✔ model_3_file completed [359ms, 561.05 kB]
#> + model_1 dispatched
#> ✔ model_1 completed [187ms, 552.39 kB]
#> + model_2 dispatched
#> ✔ model_2 completed [187ms, 552.96 kB]
#> ✔ ended pipeline [6.1s, 3 completed, 2 skipped]
#> Warning message:
#> package 'Rcpp' was built under R version 4.6.1 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_function2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>      <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>      data <span class='o'>=</span> <span class='nv'>data</span>, </span>
<span>      family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>      prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>      file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>      file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>      backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>      silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>      chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>s_targets2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_assign.html'>tar_assign</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>    <span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nv'>epilepsy</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f1</span><span class='o'>(</span><span class='nv'>data</span>, .sd <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f2</span><span class='o'>(</span><span class='nv'>data</span>, .sd <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span>, format <span class='o'>=</span> <span class='s'>"file"</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_3</span> <span class='o'>&lt;-</span> <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_file_read.html'>tar_file_read</a></span><span class='o'>(</span></span>
<span>      command <span class='o'>=</span> <span class='nf'>f3</span><span class='o'>(</span><span class='nv'>data</span>, .sd <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span>,</span>
<span>      read <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span>file <span class='o'>=</span> <span class='o'>!</span><span class='o'>!</span> <span class='nv'>.x</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>}</span><span class='o'>)</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function2</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets2</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [359ms, 561.05 kB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [187ms, 552.39 kB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [187ms, 552.96 kB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [6.1s, 3 completed, 2 skipped]</span></span>
<span><span class='c'>#&gt; Warning message:</span></span>
<span><span class='c'>#&gt; package 'Rcpp' was built under R version 4.6.1 </span></span></pre>

Okay, as expected, `model_3_file` had to be remade, but not `model_3`
because the underlying file did not change.

And removing the assignment from the model fitting function reduced the
file-size of the `_targets` version of the model:

<!--

``` r
targets::tar_path_store() |> 
  fs::dir_ls() |> 
  fs::dir_ls(regexp = "objects") |>
  fs::file_info() |> 
  dplyr::select(path, size)
#> # A tibble: 3 × 2
#>   path                             size
#>   <fs::path>                <fs::bytes>
#> 1 __targets/objects/data          2.62K
#> 2 __targets/objects/model_1     539.45K
#> 3 __targets/objects/model_3     547.92K
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_store.html'>tar_path_store</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span>regexp <span class='o'>=</span> <span class='s'>"objects"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/file_info.html'>file_info</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>path</span>, <span class='nv'>size</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 3 × 2</span></span>
<span><span class='c'>#&gt;   path                             size</span></span>
<span><span class='c'>#&gt;   &lt;fs::path&gt;                &lt;fs::bytes&gt;</span></span>
<span><span class='c'>#&gt; 1 __targets/objects/data          2.62K</span></span>
<span><span class='c'>#&gt; 2 __targets/objects/model_1     539.45K</span></span>
<span><span class='c'>#&gt; 3 __targets/objects/model_3     547.92K</span></span></pre>

Let's actually change the prior, forcing a refit, and see what happens:

<!--

``` r
s_targets3 <- quote({
  tarchetypes::tar_assign({
    data <- tar_target(epilepsy)
    model_1 <- tar_target(f1(data, .sd = 5))
    model_2 <- tar_target(f2(data, .sd = 5), format = "file")
    model_3 <- tarchetypes::tar_file_read(
      command = f3(data, .sd = 5),
      read = readRDS(file = !! .x)
    )
  })
})

write_tar_script(s_setup, s_function2, s_subfunctions, s_targets3)
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> ✔ model_3_file completed [31.9s, 561.26 kB]
#> + model_1 dispatched
#> ✔ model_1 completed [10.4s, 552.85 kB]
#> + model_2 dispatched
#> ✔ model_2 completed [10.3s, 552.53 kB]
#> + model_3 dispatched
#> ✔ model_3 completed [15ms, 561.28 kB]
#> ✔ ended pipeline [58.1s, 4 completed, 1 skipped]
#> Warning message:
#> package 'Rcpp' was built under R version 4.6.1 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_targets3</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_assign.html'>tar_assign</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>    <span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nv'>epilepsy</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f1</span><span class='o'>(</span><span class='nv'>data</span>, .sd <span class='o'>=</span> <span class='m'>5</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_target.html'>tar_target</a></span><span class='o'>(</span><span class='nf'>f2</span><span class='o'>(</span><span class='nv'>data</span>, .sd <span class='o'>=</span> <span class='m'>5</span><span class='o'>)</span>, format <span class='o'>=</span> <span class='s'>"file"</span><span class='o'>)</span></span>
<span>    <span class='nv'>model_3</span> <span class='o'>&lt;-</span> <span class='nf'>tarchetypes</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/tarchetypes/reference/tar_file_read.html'>tar_file_read</a></span><span class='o'>(</span></span>
<span>      command <span class='o'>=</span> <span class='nf'>f3</span><span class='o'>(</span><span class='nv'>data</span>, .sd <span class='o'>=</span> <span class='m'>5</span><span class='o'>)</span>,</span>
<span>      read <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span>file <span class='o'>=</span> <span class='o'>!</span><span class='o'>!</span> <span class='nv'>.x</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>  <span class='o'>}</span><span class='o'>)</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function2</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets3</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [31.9s, 561.26 kB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [10.4s, 552.85 kB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [10.3s, 552.53 kB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [15ms, 561.28 kB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [58.1s, 4 completed, 1 skipped]</span></span>
<span><span class='c'>#&gt; Warning message:</span></span>
<span><span class='c'>#&gt; package 'Rcpp' was built under R version 4.6.1 </span></span></pre>

As expected, all three models had to refit. Yes, I feel like inventing
an intransitive *refit*.

<!--

``` r
targets::tar_load(model_1)
targets::tar_load(model_2)
targets::tar_load(model_3)

# Confirm change in prior
prior_summary(model_1)[1, 1]
#> [1] "normal(0, 5)"
prior_summary(readRDS(model_2))[1, 1]
#> [1] "normal(0, 5)"
prior_summary(model_3)[1, 1]
#> [1] "normal(0, 5)"
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_2</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_3</span><span class='o'>)</span></span>
<span></span>
<span><span class='c'># Confirm change in prior</span></span>
<span><span class='nf'><a href='https://mc-stan.org/rstantools/reference/prior_summary.html'>prior_summary</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span><span class='o'>[</span><span class='m'>1</span>, <span class='m'>1</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; [1] "normal(0, 5)"</span></span>
<span><span class='nf'><a href='https://mc-stan.org/rstantools/reference/prior_summary.html'>prior_summary</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span><span class='nv'>model_2</span><span class='o'>)</span><span class='o'>)</span><span class='o'>[</span><span class='m'>1</span>, <span class='m'>1</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; [1] "normal(0, 5)"</span></span>
<span><span class='nf'><a href='https://mc-stan.org/rstantools/reference/prior_summary.html'>prior_summary</a></span><span class='o'>(</span><span class='nv'>model_3</span><span class='o'>)</span><span class='o'>[</span><span class='m'>1</span>, <span class='m'>1</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; [1] "normal(0, 5)"</span></span></pre>

### Adding LOO scores

brms will store derived model data such as LOO scores inside of the
`brmsfit` (model) object, and if the model has an associated file, that
file will be updated with the derived model data. Thus, we might run into a 
case where an assignment creates something in the model's environment that 
inflates the model size. 

**Adding LOO scores to an already fitted model**. We add an assignment to the 
function and use `add_criterion()` on the model:

<!--

``` r
s_function3 <- quote({
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    m <- brm(
      count ~ zBase * Trt + (1|patient),
      data = data, 
      family = poisson(),
      prior = p + prior(cauchy(0, 2), class = sd),
      file = file,
      file_refit = "on_change",
      backend = "cmdstanr",
      silent = 2,
      chains = 1
    )
    add_criterion(m, "loo")
  }
})

write_tar_script(s_setup, s_function3, s_subfunctions, s_targets3)
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> Automatically saving the model object in 'models/test3.rds'
#> ✔ model_3_file completed [1.7s, 572.84 kB]
#> + model_1 dispatched
#> Automatically saving the model object in 'models/test1.rds'
#> ✔ model_1 completed [953ms, 564.26 kB]
#> + model_2 dispatched
#> Automatically saving the model object in 'models/test2.rds'
#> ✔ model_2 completed [922ms, 563.95 kB]
#> + model_3 dispatched
#> ✔ model_3 completed [0ms, 572.84 kB]
#> ✔ ended pipeline [9s, 4 completed, 1 skipped]
#> Warning messages:
#> 1: package 'Rcpp' was built under R version 4.6.1 
#> 2: Found 14 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 3: The ESS has been capped to avoid unstable estimates. 
#> 4: Found 10 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 5: Found 7 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_function3</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>      <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>      data <span class='o'>=</span> <span class='nv'>data</span>, </span>
<span>      family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>      prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>      file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>      file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>      backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>      silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>      chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://paulbuerkner.com/brms/reference/add_criterion.html'>add_criterion</a></span><span class='o'>(</span><span class='nv'>m</span>, <span class='s'>"loo"</span><span class='o'>)</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function3</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets3</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test3.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [1.7s, 572.84 kB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test1.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [953ms, 564.26 kB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test2.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [922ms, 563.95 kB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [0ms, 572.84 kB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [9s, 4 completed, 1 skipped]</span></span>
<span><span class='c'>#&gt; Warning messages:</span></span>
<span><span class='c'>#&gt; 1: package 'Rcpp' was built under R version 4.6.1 </span></span>
<span><span class='c'>#&gt; 2: Found 14 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 3: The ESS has been capped to avoid unstable estimates. </span></span>
<span><span class='c'>#&gt; 4: Found 10 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 5: Found 7 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. </span></span></pre>

We can see the loo scores inside of the model object:

<!--

``` r
targets::tar_load(model_1)
model_1$criteria$loo
#> 
#> Computed from 1000 posterior draws and 236 log-likelihood terms.
#> 
#>          Estimate   SE
#> elpd_loo   -668.5 35.6
#> p_loo        90.7 12.8
#> looic      1337.1 71.1
#> ------
#> MCSE of elpd_loo is NA.
#> MCSE and ESS estimates assume MCMC draws (r_eff in [0.3, 3.0]).
#> 
#> Pareto k diagnostic values:
#>                           Count Pct.    Min. ESS
#> (-Inf, 0.67]   (good)     226   95.8%   60      
#>    (0.67, 1]   (bad)        9    3.8%   <NA>    
#>     (1, Inf)   (very bad)   1    0.4%   <NA>    
#> See help('pareto-k-diagnostic') for details.
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>criteria</span><span class='o'>$</span><span class='nv'>loo</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Computed from 1000 posterior draws and 236 log-likelihood terms.</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;          Estimate   SE</span></span>
<span><span class='c'>#&gt; elpd_loo   -668.5 35.6</span></span>
<span><span class='c'>#&gt; p_loo        90.7 12.8</span></span>
<span><span class='c'>#&gt; looic      1337.1 71.1</span></span>
<span><span class='c'>#&gt; ------</span></span>
<span><span class='c'>#&gt; MCSE of elpd_loo is NA.</span></span>
<span><span class='c'>#&gt; MCSE and ESS estimates assume MCMC draws (r_eff in [0.3, 3.0]).</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Pareto k diagnostic values:</span></span>
<span><span class='c'>#&gt;                           Count Pct.    Min. ESS</span></span>
<span><span class='c'>#&gt; (-Inf, 0.67]   (good)     226   95.8%   60      </span></span>
<span><span class='c'>#&gt;    (0.67, 1]   (bad)        9    3.8%   &lt;NA&gt;    </span></span>
<span><span class='c'>#&gt;     (1, Inf)   (very bad)   1    0.4%   &lt;NA&gt;    </span></span>
<span><span class='c'>#&gt; See help('pareto-k-diagnostic') for details.</span></span></pre>

The file sizes look okay:

<!--

``` r
targets::tar_path_store() |> 
  fs::dir_ls() |> 
  fs::dir_ls(regexp = "objects") |>
  fs::file_info() |> 
  dplyr::select(path, size)
#> # A tibble: 3 × 2
#>   path                             size
#>   <fs::path>                <fs::bytes>
#> 1 __targets/objects/data          2.62K
#> 2 __targets/objects/model_1     551.04K
#> 3 __targets/objects/model_3     559.41K
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_store.html'>tar_path_store</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span>regexp <span class='o'>=</span> <span class='s'>"objects"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/file_info.html'>file_info</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>path</span>, <span class='nv'>size</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 3 × 2</span></span>
<span><span class='c'>#&gt;   path                             size</span></span>
<span><span class='c'>#&gt;   &lt;fs::path&gt;                &lt;fs::bytes&gt;</span></span>
<span><span class='c'>#&gt; 1 __targets/objects/data          2.62K</span></span>
<span><span class='c'>#&gt; 2 __targets/objects/model_1     551.04K</span></span>
<span><span class='c'>#&gt; 3 __targets/objects/model_3     559.41K</span></span></pre>

So do the environments:

<!--

``` r
targets::tar_load(model_1)
targets::tar_load(model_3)
model_1$formula$formula |> environment() |> ls()
#> [1] "data" "file" "p"
model_3$formula$formula |> environment() |> ls()
#> [1] "data" "file" "p"
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_3</span><span class='o'>)</span></span>
<span><span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "p"</span></span>
<span><span class='nv'>model_3</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "p"</span></span></pre>

**Adding LOO scores to a forced refit**. But let's force the models to
refit and have their LOO scores computed.

<!--

``` r
# roll back to default priors
write_tar_script(s_setup, s_function3, s_subfunctions, s_targets2)
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> Automatically saving the model object in 'models/test3.rds'
#> ✔ model_3_file completed [10.5s, 1.10 MB]
#> + model_1 dispatched
#> Automatically saving the model object in 'models/test1.rds'
#> ✔ model_1 completed [10s, 1.10 MB]
#> + model_2 dispatched
#> Automatically saving the model object in 'models/test2.rds'
#> ✔ model_2 completed [11.2s, 1.10 MB]
#> + model_3 dispatched
#> ✔ model_3 completed [15ms, 1.10 MB]
#> ✔ ended pipeline [37.2s, 4 completed, 1 skipped]
#> Warning messages:
#> 1: package 'Rcpp' was built under R version 4.6.1 
#> 2: Found 15 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 3: Found 14 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 4: Found 15 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 5: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. 
```
-->
<pre class='chroma'>
<span><span class='c'># roll back to default priors</span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function3</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets2</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test3.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [10.5s, 1.10 MB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test1.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [10s, 1.10 MB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test2.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [11.2s, 1.10 MB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [15ms, 1.10 MB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [37.2s, 4 completed, 1 skipped]</span></span>
<span><span class='c'>#&gt; Warning messages:</span></span>
<span><span class='c'>#&gt; 1: package 'Rcpp' was built under R version 4.6.1 </span></span>
<span><span class='c'>#&gt; 2: Found 15 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 3: Found 14 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 4: Found 15 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 5: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. </span></span></pre>

We can see the changed prior and the loo scores inside of the model object:

<!--

``` r
targets::tar_load(model_1)
prior_summary(model_1)[1, 1]
#> [1] "normal(0, 10)"
class(model_1$criteria$loo)
#> [1] "psis_loo"                "importance_sampling_loo"
#> [3] "loo"
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://mc-stan.org/rstantools/reference/prior_summary.html'>prior_summary</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span><span class='o'>[</span><span class='m'>1</span>, <span class='m'>1</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; [1] "normal(0, 10)"</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/class.html'>class</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>criteria</span><span class='o'>$</span><span class='nv'>loo</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "psis_loo"                "importance_sampling_loo"</span></span>
<span><span class='c'>#&gt; [3] "loo"</span></span></pre>

The file sizes look suspicious:

<!--

``` r
targets::tar_path_store() |> 
  fs::dir_ls() |> 
  fs::dir_ls(regexp = "objects") |>
  fs::file_info() |> 
  dplyr::select(path, size)
#> # A tibble: 3 × 2
#>   path                             size
#>   <fs::path>                <fs::bytes>
#> 1 __targets/objects/data          2.62K
#> 2 __targets/objects/model_1       1.05M
#> 3 __targets/objects/model_3       1.05M
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_store.html'>tar_path_store</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span>regexp <span class='o'>=</span> <span class='s'>"objects"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/file_info.html'>file_info</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>path</span>, <span class='nv'>size</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 3 × 2</span></span>
<span><span class='c'>#&gt;   path                             size</span></span>
<span><span class='c'>#&gt;   &lt;fs::path&gt;                &lt;fs::bytes&gt;</span></span>
<span><span class='c'>#&gt; 1 __targets/objects/data          2.62K</span></span>
<span><span class='c'>#&gt; 2 __targets/objects/model_1       1.05M</span></span>
<span><span class='c'>#&gt; 3 __targets/objects/model_3       1.05M</span></span></pre>

Here we can see the larger environments affecting all of the models:

<!--

``` r
targets::tar_load(model_1)
model_2 <- readRDS(targets::tar_read(model_2))
targets::tar_load(model_3)

model_1$formula$formula |> environment() |> ls()
#> [1] "data" "file" "m"    "p"
model_2$formula$formula |> environment() |> ls()
#> [1] "data" "file" "m"    "p"
model_3$formula$formula |> environment() |> ls()
#> [1] "data" "file" "m"    "p"
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nv'>model_2</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_read.html'>tar_read</a></span><span class='o'>(</span><span class='nv'>model_2</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_3</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "m"    "p"</span></span>
<span><span class='nv'>model_2</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "m"    "p"</span></span>
<span><span class='nv'>model_3</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "m"    "p"</span></span></pre>

This is utterly strange, but here is what is going on. Think about two  
environments:

  - the environment where model-fitting happens (referenced in
    `model$formula$formula`)
  - the environment where `add_criterion()` happens.

When the model has already been fitted and cached and the LOO criterion
needs to be computed, these two environments are different. They
essentially happen in different R sessions.

When the model needs to refit and the LOO criterion needs to be
computed, these two environments are the same. When the model with the
added LOO scores is saved, the reference to the model-fitting
environment is still active so the extra (self) copy sneaks in.

**Force refit and use a pipe to skip an intermediate assignment**. Let's
rewrite the model-fitting call and LOO-computation call into piped
function form. We'll change the priors again to force a refit.

<!--

``` r
s_function4 <- quote({
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    data |> 
      brm(
        count ~ zBase * Trt + (1|patient),
        data = _, 
        family = poisson(),
        prior = p + prior(cauchy(0, 2), class = sd),
        file = file,
        file_refit = "on_change",
        backend = "cmdstanr",
        silent = 2,
        chains = 1
      ) |> 
      add_criterion("loo")
  }
})

write_tar_script(s_setup, s_function4, s_subfunctions, s_targets3)
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> Automatically saving the model object in 'models/test3.rds'
#> ✔ model_3_file completed [10.3s, 564.27 kB]
#> + model_1 dispatched
#> Automatically saving the model object in 'models/test1.rds'
#> ✔ model_1 completed [10.6s, 564.39 kB]
#> + model_2 dispatched
#> Automatically saving the model object in 'models/test2.rds'
#> ✔ model_2 completed [10.1s, 564.07 kB]
#> + model_3 dispatched
#> ✔ model_3 completed [0ms, 564.29 kB]
#> ✔ ended pipeline [36.4s, 4 completed, 1 skipped]
#> Warning messages:
#> 1: package 'Rcpp' was built under R version 4.6.1 
#> 2: Found 14 observations with a pareto_k > 0.67 in model 'brm(count ~ zBase * Trt + (1 | patient), data = data, family = poisson(),     prior = p + prior(cauchy(0, 2), class = sd), file = file,     file_refit = "on_change", backend = "cmdstanr", silent = 2,     chains = 1)'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 3: The ESS has been capped to avoid unstable estimates. 
#> 4: Found 10 observations with a pareto_k > 0.67 in model 'brm(count ~ zBase * Trt + (1 | patient), data = data, family = poisson(),     prior = p + prior(cauchy(0, 2), class = sd), file = file,     file_refit = "on_change", backend = "cmdstanr", silent = 2,     chains = 1)'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 5: Found 7 observations with a pareto_k > 0.67 in model 'brm(count ~ zBase * Trt + (1 | patient), data = data, family = poisson(),     prior = p + prior(cauchy(0, 2), class = sd), file = file,     file_refit = "on_change", backend = "cmdstanr", silent = 2,     chains = 1)'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_function4</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nv'>data</span> <span class='o'>|&gt;</span> </span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>        <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>        data <span class='o'>=</span> <span class='nv'>_</span>, </span>
<span>        family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>        prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>        file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>        file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>        backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>        silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>        chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>      <span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/add_criterion.html'>add_criterion</a></span><span class='o'>(</span><span class='s'>"loo"</span><span class='o'>)</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function4</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets3</span><span class='o'>)</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test3.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [10.3s, 564.27 kB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test1.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [10.6s, 564.39 kB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test2.rds'</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [10.1s, 564.07 kB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [0ms, 564.29 kB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [36.4s, 4 completed, 1 skipped]</span></span>
<span><span class='c'>#&gt; Warning messages:</span></span>
<span><span class='c'>#&gt; 1: package 'Rcpp' was built under R version 4.6.1 </span></span>
<span><span class='c'>#&gt; 2: Found 14 observations with a pareto_k &gt; 0.67 in model 'brm(count ~ zBase * Trt + (1 | patient), data = data, family = poisson(),     prior = p + prior(cauchy(0, 2), class = sd), file = file,     file_refit = "on_change", backend = "cmdstanr", silent = 2,     chains = 1)'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 3: The ESS has been capped to avoid unstable estimates. </span></span>
<span><span class='c'>#&gt; 4: Found 10 observations with a pareto_k &gt; 0.67 in model 'brm(count ~ zBase * Trt + (1 | patient), data = data, family = poisson(),     prior = p + prior(cauchy(0, 2), class = sd), file = file,     file_refit = "on_change", backend = "cmdstanr", silent = 2,     chains = 1)'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 5: Found 7 observations with a pareto_k &gt; 0.67 in model 'brm(count ~ zBase * Trt + (1 | patient), data = data, family = poisson(),     prior = p + prior(cauchy(0, 2), class = sd), file = file,     file_refit = "on_change", backend = "cmdstanr", silent = 2,     chains = 1)'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. </span></span></pre>

The file sizes look good again:

<!--

``` r
targets::tar_path_store() |> 
  fs::dir_ls() |> 
  fs::dir_ls(regexp = "objects") |>
  fs::file_info() |> 
  dplyr::select(path, size)
#> # A tibble: 3 × 2
#>   path                             size
#>   <fs::path>                <fs::bytes>
#> 1 __targets/objects/data          2.62K
#> 2 __targets/objects/model_1     551.16K
#> 3 __targets/objects/model_3     551.06K
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_store.html'>tar_path_store</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span>regexp <span class='o'>=</span> <span class='s'>"objects"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/file_info.html'>file_info</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>path</span>, <span class='nv'>size</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 3 × 2</span></span>
<span><span class='c'>#&gt;   path                             size</span></span>
<span><span class='c'>#&gt;   &lt;fs::path&gt;                &lt;fs::bytes&gt;</span></span>
<span><span class='c'>#&gt; 1 __targets/objects/data          2.62K</span></span>
<span><span class='c'>#&gt; 2 __targets/objects/model_1     551.16K</span></span>
<span><span class='c'>#&gt; 3 __targets/objects/model_3     551.06K</span></span></pre>

Add another point on the scoreboard for the pipe-programming style.


**Explicitly isolate the model fitting environment**. I have a hunch
that if I call `brm()` twice the second `brm()` will load the saved
model, and when it unserializes (loads) the formula's saved environment,
it will create a new environment object that can't be contaminated by
new data in the model-fitting environment.

To clarify things, I am also going to print out environment addresses
too. I've lost track of things, so I am going to force a refit by
changing the code and deleting the models.

<!--

``` r
s_function5 <- quote({
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    message("Model fitting: ", environment() |> format())
    m <- brm(
        count ~ zBase * Trt + (1|patient),
        data,
        family = poisson(),
        prior = p + prior(cauchy(0, 2), class = sd),
        file = file,
        file_refit = "on_change",
        backend = "cmdstanr",
        silent = 2,
        chains = 1
      )
    message(
      "Formula after fitting: ", 
      m$formula$formula |> environment() |> format()
    )
    m <- brm(
        count ~ zBase * Trt + (1|patient),
        data,
        family = poisson(),
        prior = p + prior(cauchy(0, 2), class = sd),
        file = file,
        file_refit = "on_change",
        backend = "cmdstanr",
        silent = 2,
        chains = 1
      )
    message(
      "Formula after reading cached model: ", 
      m$formula$formula |> environment() |> format()
    )
    m <- add_criterion(m, "loo")
    message(
      "Formula after add_criterion(): ", 
      m$formula$formula |> environment() |> format()
    )
    m
  }
})
write_tar_script(s_setup, s_function5, s_subfunctions, s_targets3)

file.remove("models/test1.rds", "models/test2.rds", "models/test3.rds")
#> [1] TRUE TRUE TRUE
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> Model fitting: <environment: 0x0000021abf48ba00>
#> Formula after fitting: <environment: 0x0000021abf48ba00>
#> Formula after reading cached model: <environment: 0x0000021b2855fa58>
#> Automatically saving the model object in 'models/test3.rds'
#> Formula after add_criterion(): <environment: 0x0000021b2855fa58>
#> ✔ model_3_file completed [10.6s, 564.16 kB]
#> + model_1 dispatched
#> Model fitting: <environment: 0x0000021ac215d110>
#> Formula after fitting: <environment: 0x0000021ac215d110>
#> Formula after reading cached model: <environment: 0x0000021b24a09bf8>
#> Automatically saving the model object in 'models/test1.rds'
#> Formula after add_criterion(): <environment: 0x0000021b24a09bf8>
#> ✔ model_1 completed [10.8s, 564.27 kB]
#> + model_2 dispatched
#> Model fitting: <environment: 0x0000021b23c9f900>
#> Formula after fitting: <environment: 0x0000021b23c9f900>
#> Formula after reading cached model: <environment: 0x0000021ac1838730>
#> Automatically saving the model object in 'models/test2.rds'
#> Formula after add_criterion(): <environment: 0x0000021ac1838730>
#> ✔ model_2 completed [10.1s, 563.95 kB]
#> + model_3 dispatched
#> ✔ model_3 completed [0ms, 564.16 kB]
#> ✔ ended pipeline [37s, 4 completed, 1 skipped]
#> Warning messages:
#> 1: package 'Rcpp' was built under R version 4.6.1 
#> 2: Found 14 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 3: The ESS has been capped to avoid unstable estimates. 
#> 4: Found 10 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 5: Found 7 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_function5</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span><span class='s'>"Model fitting: "</span>, <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>        <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>        <span class='nv'>data</span>,</span>
<span>        family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>        prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>        file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>        file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>        backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>        silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>        chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>      <span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after fitting: "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>        <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>        <span class='nv'>data</span>,</span>
<span>        family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>        prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>        file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>        file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>        backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>        silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>        chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>      <span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after reading cached model: "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/add_criterion.html'>add_criterion</a></span><span class='o'>(</span><span class='nv'>m</span>, <span class='s'>"loo"</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after add_criterion(): "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function5</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets3</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/files.html'>file.remove</a></span><span class='o'>(</span><span class='s'>"models/test1.rds"</span>, <span class='s'>"models/test2.rds"</span>, <span class='s'>"models/test3.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE TRUE TRUE</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x0000021abf48ba00&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x0000021abf48ba00&gt;</span></span>
<span><span class='c'>#&gt; Formula after reading cached model: &lt;environment: 0x0000021b2855fa58&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test3.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x0000021b2855fa58&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [10.6s, 564.16 kB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x0000021ac215d110&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x0000021ac215d110&gt;</span></span>
<span><span class='c'>#&gt; Formula after reading cached model: &lt;environment: 0x0000021b24a09bf8&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test1.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x0000021b24a09bf8&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [10.8s, 564.27 kB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x0000021b23c9f900&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x0000021b23c9f900&gt;</span></span>
<span><span class='c'>#&gt; Formula after reading cached model: &lt;environment: 0x0000021ac1838730&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test2.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x0000021ac1838730&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [10.1s, 563.95 kB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [0ms, 564.16 kB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [37s, 4 completed, 1 skipped]</span></span>
<span><span class='c'>#&gt; Warning messages:</span></span>
<span><span class='c'>#&gt; 1: package 'Rcpp' was built under R version 4.6.1 </span></span>
<span><span class='c'>#&gt; 2: Found 14 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 3: The ESS has been capped to avoid unstable estimates. </span></span>
<span><span class='c'>#&gt; 4: Found 10 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 5: Found 7 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. </span></span></pre>

The file sizes look good again:

<!--

``` r
targets::tar_path_store() |> 
  fs::dir_ls() |> 
  fs::dir_ls(regexp = "objects") |>
  fs::file_info() |> 
  dplyr::select(path, size)
#> # A tibble: 3 × 2
#>   path                             size
#>   <fs::path>                <fs::bytes>
#> 1 __targets/objects/data          2.62K
#> 2 __targets/objects/model_1     551.05K
#> 3 __targets/objects/model_3     550.94K
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_store.html'>tar_path_store</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span>regexp <span class='o'>=</span> <span class='s'>"objects"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/file_info.html'>file_info</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>path</span>, <span class='nv'>size</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 3 × 2</span></span>
<span><span class='c'>#&gt;   path                             size</span></span>
<span><span class='c'>#&gt;   &lt;fs::path&gt;                &lt;fs::bytes&gt;</span></span>
<span><span class='c'>#&gt; 1 __targets/objects/data          2.62K</span></span>
<span><span class='c'>#&gt; 2 __targets/objects/model_1     551.05K</span></span>
<span><span class='c'>#&gt; 3 __targets/objects/model_3     550.94K</span></span></pre>

I can't believe this do-it-twice trick worked.

<!--

``` r
targets::tar_load(model_1)
model_1$formula$formula |> environment() |> ls()
#> [1] "data" "file" "p"
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_load.html'>tar_load</a></span><span class='o'>(</span><span class='nv'>model_1</span><span class='o'>)</span></span>
<span><span class='nv'>model_1</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/ls.html'>ls</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "data" "file" "p"</span></span></pre>

For completeness, let's look at the environments from a do-it-once version:

<!--

``` r
s_function6 <- quote({
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    message("Model fitting: ", environment() |> format())
    m <- brm(
        count ~ zBase * Trt + (1|patient),
        data,
        family = poisson(),
        prior = p + prior(cauchy(0, 2), class = sd),
        file = file,
        file_refit = "on_change",
        backend = "cmdstanr",
        silent = 2,
        chains = 1
      )
    message(
      "Formula after fitting: ", 
      m$formula$formula |> environment() |> format()
    )
    m <- add_criterion(m, "loo")
    message(
      "Formula after add_criterion(): ", 
      m$formula$formula |> environment() |> format()
    )
    m
  }
})
write_tar_script(s_setup, s_function6, s_subfunctions, s_targets3)

file.remove("models/test1.rds", "models/test2.rds", "models/test3.rds")
#> [1] TRUE TRUE TRUE
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> Model fitting: <environment: 0x000002b52c49e4a8>
#> Formula after fitting: <environment: 0x000002b52c49e4a8>
#> Automatically saving the model object in 'models/test3.rds'
#> Formula after add_criterion(): <environment: 0x000002b52c49e4a8>
#> ✔ model_3_file completed [10.3s, 1.10 MB]
#> + model_1 dispatched
#> Model fitting: <environment: 0x000002b53076a900>
#> Formula after fitting: <environment: 0x000002b53076a900>
#> Automatically saving the model object in 'models/test1.rds'
#> Formula after add_criterion(): <environment: 0x000002b53076a900>
#> ✔ model_1 completed [10.5s, 1.11 MB]
#> + model_2 dispatched
#> Model fitting: <environment: 0x000002b5b1603dd0>
#> Formula after fitting: <environment: 0x000002b5b1603dd0>
#> Automatically saving the model object in 'models/test2.rds'
#> Formula after add_criterion(): <environment: 0x000002b5b1603dd0>
#> ✔ model_2 completed [9.9s, 1.10 MB]
#> + model_3 dispatched
#> ✔ model_3 completed [15ms, 1.10 MB]
#> ✔ ended pipeline [36.2s, 4 completed, 1 skipped]
#> Warning messages:
#> 1: package 'Rcpp' was built under R version 4.6.1 
#> 2: Found 14 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 3: The ESS has been capped to avoid unstable estimates. 
#> 4: Found 10 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 5: Found 7 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_function6</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span><span class='s'>"Model fitting: "</span>, <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>        <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>        <span class='nv'>data</span>,</span>
<span>        family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>        prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>        file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>        file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>        backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>        silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>        chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>      <span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after fitting: "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/add_criterion.html'>add_criterion</a></span><span class='o'>(</span><span class='nv'>m</span>, <span class='s'>"loo"</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after add_criterion(): "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function6</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets3</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/files.html'>file.remove</a></span><span class='o'>(</span><span class='s'>"models/test1.rds"</span>, <span class='s'>"models/test2.rds"</span>, <span class='s'>"models/test3.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE TRUE TRUE</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x000002b52c49e4a8&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x000002b52c49e4a8&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test3.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x000002b52c49e4a8&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [10.3s, 1.10 MB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x000002b53076a900&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x000002b53076a900&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test1.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x000002b53076a900&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [10.5s, 1.11 MB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x000002b5b1603dd0&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x000002b5b1603dd0&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test2.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x000002b5b1603dd0&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [9.9s, 1.10 MB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [15ms, 1.10 MB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [36.2s, 4 completed, 1 skipped]</span></span>
<span><span class='c'>#&gt; Warning messages:</span></span>
<span><span class='c'>#&gt; 1: package 'Rcpp' was built under R version 4.6.1 </span></span>
<span><span class='c'>#&gt; 2: Found 14 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 3: The ESS has been capped to avoid unstable estimates. </span></span>
<span><span class='c'>#&gt; 4: Found 10 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 5: Found 7 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. </span></span></pre>

The environments are all the same! 

That is why adding the criterion's embloats the models. The do-it-twice version
creates a new environment when it loads the model's saved formula environment.

**A clean version of the do-it-twice trick**. It looks pretty odd to fit the 
same model twice. We can just read in the saved model with 
`m <- readRDS(m$file)` instead for the same effect:

<!--

``` r
s_function7 <- quote({
  # Fit a model and return the model
  fit <- function(data, file, .sd = 10) {
    p <- sprintf("normal(0, %s)", .sd) |>
      prior_string(class = "b")
    message("Model fitting: ", environment() |> format())
    m <- brm(
        count ~ zBase * Trt + (1|patient),
        data,
        family = poisson(),
        prior = p + prior(cauchy(0, 2), class = sd),
        file = file,
        file_refit = "on_change",
        backend = "cmdstanr",
        silent = 2,
        chains = 1
      )
    message(
      "Formula after fitting: ", 
      m$formula$formula |> environment() |> format()
    )
    m <- readRDS(m$file)
    message(
      "Formula after reading cached model: ", 
      m$formula$formula |> environment() |> format()
    )
    m <- add_criterion(m, "loo", )
    message(
      "Formula after add_criterion(): ", 
      m$formula$formula |> environment() |> format()
    )
    m
  }
})
write_tar_script(s_setup, s_function7, s_subfunctions, s_targets3)

file.remove("models/test1.rds", "models/test2.rds", "models/test3.rds")
#> [1] TRUE TRUE TRUE
targets::tar_make()
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> 
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> 
#> + model_3_file dispatched
#> Model fitting: <environment: 0x000001934059b7e8>
#> Formula after fitting: <environment: 0x000001934059b7e8>
#> Formula after reading cached model: <environment: 0x00000193a5d5d960>
#> Automatically saving the model object in 'models/test3.rds'
#> Formula after add_criterion(): <environment: 0x00000193a5d5d960>
#> ✔ model_3_file completed [10.2s, 564.16 kB]
#> + model_1 dispatched
#> Model fitting: <environment: 0x0000019342f04458>
#> Formula after fitting: <environment: 0x0000019342f04458>
#> Formula after reading cached model: <environment: 0x00000193a6c311c8>
#> Automatically saving the model object in 'models/test1.rds'
#> Formula after add_criterion(): <environment: 0x00000193a6c311c8>
#> ✔ model_1 completed [10.5s, 564.26 kB]
#> + model_2 dispatched
#> Model fitting: <environment: 0x00000193a5637a50>
#> Formula after fitting: <environment: 0x00000193a5637a50>
#> Formula after reading cached model: <environment: 0x00000193ae914e80>
#> Automatically saving the model object in 'models/test2.rds'
#> Formula after add_criterion(): <environment: 0x00000193ae914e80>
#> ✔ model_2 completed [10.2s, 563.95 kB]
#> + model_3 dispatched
#> ✔ model_3 completed [0ms, 564.16 kB]
#> ✔ ended pipeline [36.4s, 4 completed, 1 skipped]
#> Warning messages:
#> 1: package 'Rcpp' was built under R version 4.6.1 
#> 2: Found 14 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 3: The ESS has been capped to avoid unstable estimates. 
#> 4: Found 10 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 5: Found 7 observations with a pareto_k > 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. 
#> 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. 
```
-->
<pre class='chroma'>
<span><span class='nv'>s_function7</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='o'>{</span></span>
<span>  <span class='c'># Fit a model and return the model</span></span>
<span>  <span class='nv'>fit</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>data</span>, <span class='nv'>file</span>, <span class='nv'>.sd</span> <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='nv'>p</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/sprintf.html'>sprintf</a></span><span class='o'>(</span><span class='s'>"normal(0, %s)"</span>, <span class='nv'>.sd</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>      <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior_string</a></span><span class='o'>(</span>class <span class='o'>=</span> <span class='s'>"b"</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span><span class='s'>"Model fitting: "</span>, <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brm.html'>brm</a></span><span class='o'>(</span></span>
<span>        <span class='nv'>count</span> <span class='o'>~</span> <span class='nv'>zBase</span> <span class='o'>*</span> <span class='nv'>Trt</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span><span class='o'>|</span><span class='nv'>patient</span><span class='o'>)</span>,</span>
<span>        <span class='nv'>data</span>,</span>
<span>        family <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/stats/family.html'>poisson</a></span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>        prior <span class='o'>=</span> <span class='nv'>p</span> <span class='o'>+</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>prior</a></span><span class='o'>(</span><span class='nf'>cauchy</span><span class='o'>(</span><span class='m'>0</span>, <span class='m'>2</span><span class='o'>)</span>, class <span class='o'>=</span> <span class='nv'>sd</span><span class='o'>)</span>,</span>
<span>        file <span class='o'>=</span> <span class='nv'>file</span>,</span>
<span>        file_refit <span class='o'>=</span> <span class='s'>"on_change"</span>,</span>
<span>        backend <span class='o'>=</span> <span class='s'>"cmdstanr"</span>,</span>
<span>        silent <span class='o'>=</span> <span class='m'>2</span>,</span>
<span>        chains <span class='o'>=</span> <span class='m'>1</span></span>
<span>      <span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after fitting: "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span><span class='nv'>m</span><span class='o'>$</span><span class='nv'>file</span><span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after reading cached model: "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/add_criterion.html'>add_criterion</a></span><span class='o'>(</span><span class='nv'>m</span>, <span class='s'>"loo"</span>, <span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/message.html'>message</a></span><span class='o'>(</span></span>
<span>      <span class='s'>"Formula after add_criterion(): "</span>, </span>
<span>      <span class='nv'>m</span><span class='o'>$</span><span class='nv'>formula</span><span class='o'>$</span><span class='nv'>formula</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/format.html'>format</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>m</span></span>
<span>  <span class='o'>}</span></span>
<span><span class='o'>}</span><span class='o'>)</span></span>
<span><span class='nf'>write_tar_script</span><span class='o'>(</span><span class='nv'>s_setup</span>, <span class='nv'>s_function7</span>, <span class='nv'>s_subfunctions</span>, <span class='nv'>s_targets3</span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/files.html'>file.remove</a></span><span class='o'>(</span><span class='s'>"models/test1.rds"</span>, <span class='s'>"models/test2.rds"</span>, <span class='s'>"models/test3.rds"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE TRUE TRUE</span></span>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_make.html'>tar_make</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Loading required package: Rcpp</span></span>
<span><span class='c'>#&gt; Loading 'brms' package (version 2.23.0). Useful instructions</span></span>
<span><span class='c'>#&gt; can be found by typing help('brms'). A more detailed introduction</span></span>
<span><span class='c'>#&gt; to the package is available through vignette('brms_overview').</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'brms'</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; The following object is masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     ar</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; + model_3_file dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x000001934059b7e8&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x000001934059b7e8&gt;</span></span>
<span><span class='c'>#&gt; Formula after reading cached model: &lt;environment: 0x00000193a5d5d960&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test3.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x00000193a5d5d960&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_3_file completed [10.2s, 564.16 kB]</span></span>
<span><span class='c'>#&gt; + model_1 dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x0000019342f04458&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x0000019342f04458&gt;</span></span>
<span><span class='c'>#&gt; Formula after reading cached model: &lt;environment: 0x00000193a6c311c8&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test1.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x00000193a6c311c8&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_1 completed [10.5s, 564.26 kB]</span></span>
<span><span class='c'>#&gt; + model_2 dispatched</span></span>
<span><span class='c'>#&gt; Model fitting: &lt;environment: 0x00000193a5637a50&gt;</span></span>
<span><span class='c'>#&gt; Formula after fitting: &lt;environment: 0x00000193a5637a50&gt;</span></span>
<span><span class='c'>#&gt; Formula after reading cached model: &lt;environment: 0x00000193ae914e80&gt;</span></span>
<span><span class='c'>#&gt; Automatically saving the model object in 'models/test2.rds'</span></span>
<span><span class='c'>#&gt; Formula after add_criterion(): &lt;environment: 0x00000193ae914e80&gt;</span></span>
<span><span class='c'>#&gt; ✔ model_2 completed [10.2s, 563.95 kB]</span></span>
<span><span class='c'>#&gt; + model_3 dispatched</span></span>
<span><span class='c'>#&gt; ✔ model_3 completed [0ms, 564.16 kB]</span></span>
<span><span class='c'>#&gt; ✔ ended pipeline [36.4s, 4 completed, 1 skipped]</span></span>
<span><span class='c'>#&gt; Warning messages:</span></span>
<span><span class='c'>#&gt; 1: package 'Rcpp' was built under R version 4.6.1 </span></span>
<span><span class='c'>#&gt; 2: Found 14 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 3: The ESS has been capped to avoid unstable estimates. </span></span>
<span><span class='c'>#&gt; 4: Found 10 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 5: Found 7 observations with a pareto_k &gt; 0.67 in model 'm'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy. </span></span>
<span><span class='c'>#&gt; 6: 3 targets produced warnings. Run targets::tar_meta(fields = warnings, complete_only = TRUE) for the messages. </span></span></pre>

Quick check of the file sizes:

<!--

``` r
targets::tar_path_store() |> 
  fs::dir_ls() |> 
  fs::dir_ls(regexp = "objects") |>
  fs::file_info() |> 
  dplyr::select(path, size)
#> # A tibble: 3 × 2
#>   path                             size
#>   <fs::path>                <fs::bytes>
#> 1 __targets/objects/data          2.62K
#> 2 __targets/objects/model_1     551.04K
#> 3 __targets/objects/model_3     550.94K
```
-->
<pre class='chroma'>
<span><span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_path_store.html'>tar_path_store</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/dir_ls.html'>dir_ls</a></span><span class='o'>(</span>regexp <span class='o'>=</span> <span class='s'>"objects"</span><span class='o'>)</span> <span class='o'>|&gt;</span></span>
<span>  <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/file_info.html'>file_info</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>dplyr</span><span class='nf'>::</span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>path</span>, <span class='nv'>size</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 3 × 2</span></span>
<span><span class='c'>#&gt;   path                             size</span></span>
<span><span class='c'>#&gt;   &lt;fs::path&gt;                &lt;fs::bytes&gt;</span></span>
<span><span class='c'>#&gt; 1 __targets/objects/data          2.62K</span></span>
<span><span class='c'>#&gt; 2 __targets/objects/model_1     551.04K</span></span>
<span><span class='c'>#&gt; 3 __targets/objects/model_3     550.94K</span></span></pre>


### Observations

The problem of environments inflating model sizes is a known one. The
[butcher R package](https://butcher.tidymodels.org/), for example, will
hack off parts of model to lower their size. I did not expect it to rear 
its self-referential head into these experiments. I just wanted to 
experiment to see if its possible to work with just one file per model in 
a targets workflow. (It is, using the model-2 style).

I've been coding in the model-1 style for years. brms caches it so that
the model is only refit when the formula, priors or data changes, and
targets has its own copy of the model in it data store. I'm now inclined
to use the model-3 style because it hands targets the `.rds` file
created by brms and avoids at least one unexpected environment/size
issue. But it was truly a baffling result that the newly-fitted-model
plus assignment plus LOO-score case led to an inflated file size, and
the model-3 style could not avoid that issue. Immediately loading the saved
model file seemed to help this problem.

I recently completed a project that had some impressively large model objects. 
I am going to paste in the R results from inspecting one of them:

<!--

``` r
# [... omitted code to locate/store the model path ...]

big_model <- path_big_model |> readRDS()
path_big_model |> file.size() |> scales::label_bytes()()
#> [1] "433 MB"

big_model |> 
  lapply(lobstr::obj_size) |> 
  unlist() |> 
  scales::label_bytes()()
#>   formula      data     prior     data2  stanvars     model 
#>  "230 MB"  "231 MB"    "6 kB"    "48 B"   "280 B"    "8 kB" 
#> save_pars algorithm   backend   threads    opencl stan_args 
#>   "944 B"   "120 B"   "120 B"   "904 B"   "512 B"    "1 kB" 
#>       fit     basis  criteria      file   version    family 
#>  "229 MB"  "460 kB"   "89 kB"   "168 B"    "3 kB"   "18 kB" 
#>   autocor     ranef cov_ranef stan_funs data.name 
#>     "0 B"  "230 MB"     "0 B"     "0 B"   "112 B" 
```
-->
<pre class='chroma'>
<span><span class='c'># [... omitted code to locate/store the model path ...]</span></span>
<span></span>
<span><span class='nv'>big_model</span> <span class='o'>&lt;-</span> <span class='nv'>path_big_model</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/readRDS.html'>readRDS</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='nv'>path_big_model</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/file.info.html'>file.size</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/label_bytes.html'>label_bytes</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "433 MB"</span></span>
<span></span>
<span><span class='nv'>big_model</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>lapply</a></span><span class='o'>(</span><span class='nf'>lobstr</span><span class='nf'>::</span><span class='nv'><a href='https://lobstr.r-lib.org/reference/obj_size.html'>obj_size</a></span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/unlist.html'>unlist</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>scales</span><span class='nf'>::</span><span class='nf'><a href='https://scales.r-lib.org/reference/label_bytes.html'>label_bytes</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;   formula      data     prior     data2  stanvars     model </span></span>
<span><span class='c'>#&gt;  "230 MB"  "231 MB"    "6 kB"    "48 B"   "280 B"    "8 kB" </span></span>
<span><span class='c'>#&gt; save_pars algorithm   backend   threads    opencl stan_args </span></span>
<span><span class='c'>#&gt;   "944 B"   "120 B"   "120 B"   "904 B"   "512 B"    "1 kB" </span></span>
<span><span class='c'>#&gt;       fit     basis  criteria      file   version    family </span></span>
<span><span class='c'>#&gt;  "229 MB"  "460 kB"   "89 kB"   "168 B"    "3 kB"   "18 kB" </span></span>
<span><span class='c'>#&gt;   autocor     ranef cov_ranef stan_funs data.name </span></span>
<span><span class='c'>#&gt;     "0 B"  "230 MB"     "0 B"     "0 B"   "112 B" </span></span></pre>

It really does look like the `formula`, `data`, and `ranef` entries, which all 
contain formulas, are carrying around an extra 230 MB of stuff. 

