---
title: "Using closures to make 16 related functions"
date: 2021-11-01
tags: [r, programming, closures]
---

The [R Open Sci
newsletter](https://ropensci.org/blog/2021/11/30/ropensci-news-digest-november-2021/#function-specific-and-package-specific-environments)
linked to [a mailing list
thread](https://www.mail-archive.com/r-package-devel@r-project.org/msg07413.html)
about how to stash data inside of an R package.

Duncan Murdoch suggests using a function factory to create closures that
can access data in the parent environment.

> Your `local()` call can create several functions and return them in a
> list; then just those functions have access to the local variables.
> For example,
> 
> ```r
> createFns <- function() {
> 
>    .fooInfo <- NULL
> 
>    fn1 <- function (...) { ... }
>    fn2 <- function (...) { ... }
> 
>    list(fn1 = fn1, fn2 = fn2)
> }
> 
> fns <- createFns()
> fn1 <- fns$fn1
> fn2 <- fns$fn2
> ```
> 
> Now `fn1` and `fn2` are functions that can see `.fooInfo`, and nobody
> else can (without going through contortions).

(I had to fix the code to create a real function factory.) 

Then Martin Mächler replies to say yup, this is what we do in base R.

> Note that the above approach has been how `nls()` has been
> implemented for R ... a very long time ago {before R 1.0.0}
> 
> e.g. from  `example(nls)`:
> 
<!--
> 
> ``` r
> DNase1 <- subset(DNase, Run == 1)
> fm1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
> str(fm1$m)
> #> List of 16
> #>  $ resid     :function ()  
> #>  $ fitted    :function ()  
> #>  $ formula   :function ()  
> #>  $ deviance  :function ()  
> #>  $ lhs       :function ()  
> #>  $ gradient  :function ()  
> #>  $ conv      :function ()  
> #>  $ incr      :function ()  
> #>  $ setVarying:function (vary = rep_len(TRUE, np))  
> #>  $ setPars   :function (newPars)  
> #>  $ getPars   :function ()  
> #>  $ getAllPars:function ()  
> #>  $ getEnv    :function ()  
> #>  $ trace     :function ()  
> #>  $ Rmat      :function ()  
> #>  $ predict   :function (newdata = list(), qr = FALSE)  
> #>  - attr(*, "class")= chr "nlsModel"
> 
> # so 16 functions, all sharing the *same* environment very
> # efficiently and nicely
> 
> # this is *the* environment for the fitted model:
> fmE <- environment(fm1$m[[1]])
> ls.str(fmE)
> #> convCrit : function ()  
> #> dev :  num 0.00479
> #> env : <environment: 0x0000019a2a51e4c8> 
> #> form : Class 'formula'  language density ~ SSlogis(log(conc), Asym, xmid, scal)
> #> getPars : function ()  
> #> getPars.noVarying : function ()  
> #> getPars.varying : function ()  
> #> getRHS : function ()  
> #> getRHS.noVarying : function ()  
> #> getRHS.varying : function ()  
> #> gradCall :  language attr(ans, "gradient")[c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE| __truncated__ ...
> #> ind : List of 3
> #>  $ Asym: int 1
> #>  $ xmid: int 2
> #>  $ scal: int 3
> #> internalPars :  Named num [1:3] 2.35 1.48 1.04
> #> lhs :  num [1:16] 0.017 0.018 0.121 0.124 0.206 0.215 0.377 0.374 0.614 0.609 ...
> #> nDcentral :  logi FALSE
> #> npar :  int 3
> #> QR : List of 4
> #>  $ qr   : num [1:16, 1:3] -1.5221 0.0086 0.0314 0.0314 0.0584 ...
> #>  $ rank : int 3
> #>  $ qraux: num [1:3] 1.01 1.03 1.28
> #>  $ pivot: int [1:3] 1 2 3
> #> resid :  num [1:16] -0.01368 -0.01268 0.00895 0.01195 -0.00258 ...
> #> rhs :  num [1:16] 0.0307 0.0307 0.1121 0.1121 0.2086 ...
> #> scaleOffset :  num 0
> #> setPars : function (newPars)  
> #> setPars.noVarying : function (newPars)  
> #> setPars.varying : function (newPars)  
> #> upper :  NULL
> #> useParams :  logi [1:3] TRUE TRUE TRUE
> #> wts :  num [1:16] 1 1 1 1 1 1 1 1 1 1 ...
> ```
-->
> <pre class='chroma'>
> <span><span class='nv'>DNase1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/subset.html'>subset</a></span><span class='o'>(</span><span class='nv'>DNase</span>, <span class='nv'>Run</span> <span class='o'>==</span> <span class='m'>1</span><span class='o'>)</span></span>
> <span><span class='nv'>fm1</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/nls.html'>nls</a></span><span class='o'>(</span><span class='nv'>density</span> <span class='o'>~</span> <span class='nf'><a href='https://rdrr.io/r/stats/SSlogis.html'>SSlogis</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/Log.html'>log</a></span><span class='o'>(</span><span class='nv'>conc</span><span class='o'>)</span>, <span class='nv'>Asym</span>, <span class='nv'>xmid</span>, <span class='nv'>scal</span><span class='o'>)</span>, <span class='nv'>DNase1</span><span class='o'>)</span></span>
> <span><span class='nf'><a href='https://rdrr.io/r/utils/str.html'>str</a></span><span class='o'>(</span><span class='nv'>fm1</span><span class='o'>$</span><span class='nv'>m</span><span class='o'>)</span></span>
> <span><span class='c'>#&gt; List of 16</span></span>
> <span><span class='c'>#&gt;  $ resid     :function ()  </span></span>
> <span><span class='c'>#&gt;  $ fitted    :function ()  </span></span>
> <span><span class='c'>#&gt;  $ formula   :function ()  </span></span>
> <span><span class='c'>#&gt;  $ deviance  :function ()  </span></span>
> <span><span class='c'>#&gt;  $ lhs       :function ()  </span></span>
> <span><span class='c'>#&gt;  $ gradient  :function ()  </span></span>
> <span><span class='c'>#&gt;  $ conv      :function ()  </span></span>
> <span><span class='c'>#&gt;  $ incr      :function ()  </span></span>
> <span><span class='c'>#&gt;  $ setVarying:function (vary = rep_len(TRUE, np))  </span></span>
> <span><span class='c'>#&gt;  $ setPars   :function (newPars)  </span></span>
> <span><span class='c'>#&gt;  $ getPars   :function ()  </span></span>
> <span><span class='c'>#&gt;  $ getAllPars:function ()  </span></span>
> <span><span class='c'>#&gt;  $ getEnv    :function ()  </span></span>
> <span><span class='c'>#&gt;  $ trace     :function ()  </span></span>
> <span><span class='c'>#&gt;  $ Rmat      :function ()  </span></span>
> <span><span class='c'>#&gt;  $ predict   :function (newdata = list(), qr = FALSE)  </span></span>
> <span><span class='c'>#&gt;  - attr(*, "class")= chr "nlsModel"</span></span>
> <span></span>
> <span><span class='c'># so 16 functions, all sharing the *same* environment very</span></span>
> <span><span class='c'># efficiently and nicely</span></span>
> <span></span>
> <span><span class='c'># this is *the* environment for the fitted model:</span></span>
> <span><span class='nv'>fmE</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>environment</a></span><span class='o'>(</span><span class='nv'>fm1</span><span class='o'>$</span><span class='nv'>m</span><span class='o'>[[</span><span class='m'>1</span><span class='o'>]</span><span class='o'>]</span><span class='o'>)</span></span>
> <span><span class='nf'><a href='https://rdrr.io/r/utils/ls_str.html'>ls.str</a></span><span class='o'>(</span><span class='nv'>fmE</span><span class='o'>)</span></span>
> <span><span class='c'>#&gt; convCrit : function ()  </span></span>
> <span><span class='c'>#&gt; dev :  num 0.00479</span></span>
> <span><span class='c'>#&gt; env : &lt;environment: 0x0000019a2a51e4c8&gt; </span></span>
> <span><span class='c'>#&gt; form : Class 'formula'  language density ~ SSlogis(log(conc), Asym, xmid, scal)</span></span>
> <span><span class='c'>#&gt; getPars : function ()  </span></span>
> <span><span class='c'>#&gt; getPars.noVarying : function ()  </span></span>
> <span><span class='c'>#&gt; getPars.varying : function ()  </span></span>
> <span><span class='c'>#&gt; getRHS : function ()  </span></span>
> <span><span class='c'>#&gt; getRHS.noVarying : function ()  </span></span>
> <span><span class='c'>#&gt; getRHS.varying : function ()  </span></span>
> <span><span class='c'>#&gt; gradCall :  language attr(ans, "gradient")[c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE| __truncated__ ...</span></span>
> <span><span class='c'>#&gt; ind : List of 3</span></span>
> <span><span class='c'>#&gt;  $ Asym: int 1</span></span>
> <span><span class='c'>#&gt;  $ xmid: int 2</span></span>
> <span><span class='c'>#&gt;  $ scal: int 3</span></span>
> <span><span class='c'>#&gt; internalPars :  Named num [1:3] 2.35 1.48 1.04</span></span>
> <span><span class='c'>#&gt; lhs :  num [1:16] 0.017 0.018 0.121 0.124 0.206 0.215 0.377 0.374 0.614 0.609 ...</span></span>
> <span><span class='c'>#&gt; nDcentral :  logi FALSE</span></span>
> <span><span class='c'>#&gt; npar :  int 3</span></span>
> <span><span class='c'>#&gt; QR : List of 4</span></span>
> <span><span class='c'>#&gt;  $ qr   : num [1:16, 1:3] -1.5221 0.0086 0.0314 0.0314 0.0584 ...</span></span>
> <span><span class='c'>#&gt;  $ rank : int 3</span></span>
> <span><span class='c'>#&gt;  $ qraux: num [1:3] 1.01 1.03 1.28</span></span>
> <span><span class='c'>#&gt;  $ pivot: int [1:3] 1 2 3</span></span>
> <span><span class='c'>#&gt; resid :  num [1:16] -0.01368 -0.01268 0.00895 0.01195 -0.00258 ...</span></span>
> <span><span class='c'>#&gt; rhs :  num [1:16] 0.0307 0.0307 0.1121 0.1121 0.2086 ...</span></span>
> <span><span class='c'>#&gt; scaleOffset :  num 0</span></span>
> <span><span class='c'>#&gt; setPars : function (newPars)  </span></span>
> <span><span class='c'>#&gt; setPars.noVarying : function (newPars)  </span></span>
> <span><span class='c'>#&gt; setPars.varying : function (newPars)  </span></span>
> <span><span class='c'>#&gt; upper :  NULL</span></span>
> <span><span class='c'>#&gt; useParams :  logi [1:3] TRUE TRUE TRUE</span></span>
> <span><span class='c'>#&gt; wts :  num [1:16] 1 1 1 1 1 1 1 1 1 1 ...</span></span></pre>
> 
> so the environment "contains" the functions themselves (but quite
> a few more things) and for an environment that means it only
> has pointers to the same function objects which are *also* in `fm1$m`.
> 
> **So, there has been a nice convincing and important example on
> how to do this - inside R for more than two decennia.**

Final emphasis was added by me. 
