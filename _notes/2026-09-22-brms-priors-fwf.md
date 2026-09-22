---
title: "Quick hack to parse brms priors print-out"
date: 2026-09-22
tags: [r, brms, readr]
---



At the time of writing, the `print()` function for a brms prior specification  
does some row-filling to make things more verbose and explicit. But these 
details are not available as a dataframe for the user to manipulate or wrangle.
[Weirdness about the prior dataframe has been reported on GitHub](https://github.com/paul-buerkner/brms/issues/1761).
This note describes a quick data-cleaning workaround. 

Let's make a small model.

<!--

``` r
library(brms)

fivenum(Orange$circumference)
#> [1]  30.0  65.5 115.0 161.5 214.0
fivenum(Orange$age)
#> [1]  118  484 1004 1372 1582

f <- bf(circumference ~ s(age, k = 4) + (1 | Tree))

p_user <- c(
  set_prior("normal(0, 100)", class = "sd"),
  set_prior("exponential(.1)", class = "sigma")
)
prior_user <- validate_prior(p_user, f, data = Orange)
prior_default <- get_prior(f, data = Orange)
```
-->
<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://github.com/paul-buerkner/brms'>brms</a></span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/stats/fivenum.html'>fivenum</a></span><span class='o'>(</span><span class='nv'>Orange</span><span class='o'>$</span><span class='nv'>circumference</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1]  30.0  65.5 115.0 161.5 214.0</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/stats/fivenum.html'>fivenum</a></span><span class='o'>(</span><span class='nv'>Orange</span><span class='o'>$</span><span class='nv'>age</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1]  118  484 1004 1372 1582</span></span>
<span></span>
<span><span class='nv'>f</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/brmsformula.html'>bf</a></span><span class='o'>(</span><span class='nv'>circumference</span> <span class='o'>~</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/s.html'>s</a></span><span class='o'>(</span><span class='nv'>age</span>, k <span class='o'>=</span> <span class='m'>4</span><span class='o'>)</span> <span class='o'>+</span> <span class='o'>(</span><span class='m'>1</span> <span class='o'>|</span> <span class='nv'>Tree</span><span class='o'>)</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>p_user</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span></span>
<span>  <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>set_prior</a></span><span class='o'>(</span><span class='s'>"normal(0, 100)"</span>, class <span class='o'>=</span> <span class='s'>"sd"</span><span class='o'>)</span>,</span>
<span>  <span class='nf'><a href='https://paulbuerkner.com/brms/reference/set_prior.html'>set_prior</a></span><span class='o'>(</span><span class='s'>"exponential(.1)"</span>, class <span class='o'>=</span> <span class='s'>"sigma"</span><span class='o'>)</span></span>
<span><span class='o'>)</span></span>
<span><span class='nv'>prior_user</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/validate_prior.html'>validate_prior</a></span><span class='o'>(</span><span class='nv'>p_user</span>, <span class='nv'>f</span>, data <span class='o'>=</span> <span class='nv'>Orange</span><span class='o'>)</span></span>
<span><span class='nv'>prior_default</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://paulbuerkner.com/brms/reference/default_prior.html'>get_prior</a></span><span class='o'>(</span><span class='nv'>f</span>, data <span class='o'>=</span> <span class='nv'>Orange</span><span class='o'>)</span></span></pre>

Note how the default prior print-out includes `(flat)` in the description:

<!--

``` r
print(prior_default)
#>                    prior     class          coef group resp dpar nlpar lb ub
#>                   (flat)         b                                          
#>                   (flat)         b        sage_1                            
#>  student_t(3, 115, 77.1) Intercept                                          
#>    student_t(3, 0, 77.1)        sd                                      0   
#>    student_t(3, 0, 77.1)        sd                Tree                  0   
#>    student_t(3, 0, 77.1)        sd     Intercept  Tree                  0   
#>    student_t(3, 0, 77.1)       sds                                      0   
#>    student_t(3, 0, 77.1)       sds s(age, k = 4)                        0   
#>    student_t(3, 0, 77.1)     sigma                                      0   
#>  tag       source
#>           default
#>      (vectorized)
#>           default
#>           default
#>      (vectorized)
#>      (vectorized)
#>           default
#>      (vectorized)
#>           default
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/base/print.html'>print</a></span><span class='o'>(</span><span class='nv'>prior_default</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;                    prior     class          coef group resp dpar nlpar lb ub</span></span>
<span><span class='c'>#&gt;                   (flat)         b                                          </span></span>
<span><span class='c'>#&gt;                   (flat)         b        sage_1                            </span></span>
<span><span class='c'>#&gt;  student_t(3, 115, 77.1) Intercept                                          </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)        sd                                      0   </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)        sd                Tree                  0   </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)        sd     Intercept  Tree                  0   </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)       sds                                      0   </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)       sds s(age, k = 4)                        0   </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)     sigma                                      0   </span></span>
<span><span class='c'>#&gt;  tag       source</span></span>
<span><span class='c'>#&gt;           default</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;           default</span></span>
<span><span class='c'>#&gt;           default</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;           default</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;           default</span></span></pre>

If we try to use this prior object as a data.frame, the prior column has 
blanks in it instead:

<!--

``` r
as.data.frame(prior_default)
#>                     prior     class          coef group resp dpar nlpar lb ub
#> 1                                 b                                          
#> 2                                 b        sage_1                            
#> 3 student_t(3, 115, 77.1) Intercept                                          
#> 4   student_t(3, 0, 77.1)        sd                                      0   
#> 5                                sd                Tree                      
#> 6                                sd     Intercept  Tree                      
#> 7   student_t(3, 0, 77.1)       sds                                      0   
#> 8                               sds s(age, k = 4)                            
#> 9   student_t(3, 0, 77.1)     sigma                                      0   
#>   tag  source
#> 1     default
#> 2     default
#> 3     default
#> 4     default
#> 5     default
#> 6     default
#> 7     default
#> 8     default
#> 9     default
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://rdrr.io/r/base/as.data.frame.html'>as.data.frame</a></span><span class='o'>(</span><span class='nv'>prior_default</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;                     prior     class          coef group resp dpar nlpar lb ub</span></span>
<span><span class='c'>#&gt; 1                                 b                                          </span></span>
<span><span class='c'>#&gt; 2                                 b        sage_1                            </span></span>
<span><span class='c'>#&gt; 3 student_t(3, 115, 77.1) Intercept                                          </span></span>
<span><span class='c'>#&gt; 4   student_t(3, 0, 77.1)        sd                                      0   </span></span>
<span><span class='c'>#&gt; 5                                sd                Tree                      </span></span>
<span><span class='c'>#&gt; 6                                sd     Intercept  Tree                      </span></span>
<span><span class='c'>#&gt; 7   student_t(3, 0, 77.1)       sds                                      0   </span></span>
<span><span class='c'>#&gt; 8                               sds s(age, k = 4)                            </span></span>
<span><span class='c'>#&gt; 9   student_t(3, 0, 77.1)     sigma                                      0   </span></span>
<span><span class='c'>#&gt;   tag  source</span></span>
<span><span class='c'>#&gt; 1     default</span></span>
<span><span class='c'>#&gt; 2     default</span></span>
<span><span class='c'>#&gt; 3     default</span></span>
<span><span class='c'>#&gt; 4     default</span></span>
<span><span class='c'>#&gt; 5     default</span></span>
<span><span class='c'>#&gt; 6     default</span></span>
<span><span class='c'>#&gt; 7     default</span></span>
<span><span class='c'>#&gt; 8     default</span></span>
<span><span class='c'>#&gt; 9     default</span></span></pre>

Here are print-outs with a `"user"` `source`d prior:

<!--

``` r
prior_user
#>                    prior     class          coef group resp dpar nlpar lb ub
#>                   (flat)         b                                          
#>                   (flat)         b        sage_1                            
#>  student_t(3, 115, 77.1) Intercept                                          
#>           normal(0, 100)        sd                                      0   
#>           normal(0, 100)        sd                Tree                  0   
#>           normal(0, 100)        sd     Intercept  Tree                  0   
#>    student_t(3, 0, 77.1)       sds                                      0   
#>    student_t(3, 0, 77.1)       sds s(age, k = 4)                        0   
#>          exponential(.1)     sigma                                      0   
#>  tag       source
#>           default
#>      (vectorized)
#>           default
#>              user
#>      (vectorized)
#>      (vectorized)
#>           default
#>      (vectorized)
#>              user

as.data.frame(prior_user)
#>                     prior     class          coef group resp dpar nlpar lb ub
#> 1                                 b                                          
#> 2                                 b        sage_1                            
#> 3 student_t(3, 115, 77.1) Intercept                                          
#> 4          normal(0, 100)        sd                                      0   
#> 5                                sd                Tree                      
#> 6                                sd     Intercept  Tree                      
#> 7   student_t(3, 0, 77.1)       sds                                      0   
#> 8                               sds s(age, k = 4)                            
#> 9         exponential(.1)     sigma                                      0   
#>   tag  source
#> 1     default
#> 2     default
#> 3     default
#> 4        user
#> 5     default
#> 6     default
#> 7     default
#> 8     default
#> 9        user
```
-->
<pre class='chroma'>
<span><span class='nv'>prior_user</span></span>
<span><span class='c'>#&gt;                    prior     class          coef group resp dpar nlpar lb ub</span></span>
<span><span class='c'>#&gt;                   (flat)         b                                          </span></span>
<span><span class='c'>#&gt;                   (flat)         b        sage_1                            </span></span>
<span><span class='c'>#&gt;  student_t(3, 115, 77.1) Intercept                                          </span></span>
<span><span class='c'>#&gt;           normal(0, 100)        sd                                      0   </span></span>
<span><span class='c'>#&gt;           normal(0, 100)        sd                Tree                  0   </span></span>
<span><span class='c'>#&gt;           normal(0, 100)        sd     Intercept  Tree                  0   </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)       sds                                      0   </span></span>
<span><span class='c'>#&gt;    student_t(3, 0, 77.1)       sds s(age, k = 4)                        0   </span></span>
<span><span class='c'>#&gt;          exponential(.1)     sigma                                      0   </span></span>
<span><span class='c'>#&gt;  tag       source</span></span>
<span><span class='c'>#&gt;           default</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;           default</span></span>
<span><span class='c'>#&gt;              user</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;           default</span></span>
<span><span class='c'>#&gt;      (vectorized)</span></span>
<span><span class='c'>#&gt;              user</span></span>
<span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/as.data.frame.html'>as.data.frame</a></span><span class='o'>(</span><span class='nv'>prior_user</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;                     prior     class          coef group resp dpar nlpar lb ub</span></span>
<span><span class='c'>#&gt; 1                                 b                                          </span></span>
<span><span class='c'>#&gt; 2                                 b        sage_1                            </span></span>
<span><span class='c'>#&gt; 3 student_t(3, 115, 77.1) Intercept                                          </span></span>
<span><span class='c'>#&gt; 4          normal(0, 100)        sd                                      0   </span></span>
<span><span class='c'>#&gt; 5                                sd                Tree                      </span></span>
<span><span class='c'>#&gt; 6                                sd     Intercept  Tree                      </span></span>
<span><span class='c'>#&gt; 7   student_t(3, 0, 77.1)       sds                                      0   </span></span>
<span><span class='c'>#&gt; 8                               sds s(age, k = 4)                            </span></span>
<span><span class='c'>#&gt; 9         exponential(.1)     sigma                                      0   </span></span>
<span><span class='c'>#&gt;   tag  source</span></span>
<span><span class='c'>#&gt; 1     default</span></span>
<span><span class='c'>#&gt; 2     default</span></span>
<span><span class='c'>#&gt; 3     default</span></span>
<span><span class='c'>#&gt; 4        user</span></span>
<span><span class='c'>#&gt; 5     default</span></span>
<span><span class='c'>#&gt; 6     default</span></span>
<span><span class='c'>#&gt; 7     default</span></span>
<span><span class='c'>#&gt; 8     default</span></span>
<span><span class='c'>#&gt; 9        user</span></span></pre>

It seems like `print()` adjusts the `prior` and `source` columns so that there
are no blank values, either by using `"(flat)"` or by inheriting a missing prior
from the generic prior used for a parameter `class`. Visually, this inheritance
looks like the `prior` is carried down from a preceding row and the `source`
column gets the value `"(vectorized)"`.

The `as.data.frame()` version seems to have the bare minimum information
about the priors, just the non-flat distributions provided to the model.

### The Easy Workaround

If we inspect the `print()` method of the priors, we see a call to
`prepare_print_prior()`:

<!--

``` r
brms:::print.brmsprior
#> function (x, show_df = NULL, ...) 
#> {
#>     if (is.null(show_df)) {
#>         show_df <- nrow(x) > 1L
#>     }
#>     show_df <- as_one_logical(show_df)
#>     y <- prepare_print_prior(x)
#>     if (show_df) {
#>         print.data.frame(y, row.names = FALSE, ...)
#>     }
#>     else {
#>         cat(collapse(.print_prior(y), "\n"))
#>     }
#>     invisible(x)
#> }
#> <bytecode: 0x000001ae5b87d468>
#> <environment: namespace:brms>
```
-->
<pre class='chroma'>
<span><span class='nf'>brms</span><span class='nf'>:::</span><span class='nv'><a href='https://paulbuerkner.com/brms/reference/print.brmsprior.html'>print.brmsprior</a></span></span>
<span><span class='c'>#&gt; function (x, show_df = NULL, ...) </span></span>
<span><span class='c'>#&gt; {</span></span>
<span><span class='c'>#&gt;     if (is.null(show_df)) {</span></span>
<span><span class='c'>#&gt;         show_df &lt;- nrow(x) &gt; 1L</span></span>
<span><span class='c'>#&gt;     }</span></span>
<span><span class='c'>#&gt;     show_df &lt;- as_one_logical(show_df)</span></span>
<span><span class='c'>#&gt;     y &lt;- prepare_print_prior(x)</span></span>
<span><span class='c'>#&gt;     if (show_df) {</span></span>
<span><span class='c'>#&gt;         print.data.frame(y, row.names = FALSE, ...)</span></span>
<span><span class='c'>#&gt;     }</span></span>
<span><span class='c'>#&gt;     else {</span></span>
<span><span class='c'>#&gt;         cat(collapse(.print_prior(y), "\n"))</span></span>
<span><span class='c'>#&gt;     }</span></span>
<span><span class='c'>#&gt;     invisible(x)</span></span>
<span><span class='c'>#&gt; }</span></span>
<span><span class='c'>#&gt; &lt;bytecode: 0x000001ae5b87d468&gt;</span></span>
<span><span class='c'>#&gt; &lt;environment: namespace:brms&gt;</span></span></pre>

This function does return a dataframe with filled rows that is suitable for 
wrangling:


<!--

``` r
library(tidyverse)

brms:::prepare_print_prior(prior_user) |> 
  as.data.frame() 
#>                     prior     class          coef group resp dpar nlpar lb ub
#> 1                  (flat)         b                                          
#> 2                  (flat)         b        sage_1                            
#> 3 student_t(3, 115, 77.1) Intercept                                          
#> 4          normal(0, 100)        sd                                      0   
#> 5          normal(0, 100)        sd                Tree                  0   
#> 6          normal(0, 100)        sd     Intercept  Tree                  0   
#> 7   student_t(3, 0, 77.1)       sds                                      0   
#> 8   student_t(3, 0, 77.1)       sds s(age, k = 4)                        0   
#> 9         exponential(.1)     sigma                                      0   
#>   tag       source
#> 1          default
#> 2     (vectorized)
#> 3          default
#> 4             user
#> 5     (vectorized)
#> 6     (vectorized)
#> 7          default
#> 8     (vectorized)
#> 9             user

# For example:
brms:::prepare_print_prior(prior_user) |> 
  as.data.frame() |> 
  filter(source != "(vectorized)")
#>                     prior     class coef group resp dpar nlpar lb ub tag
#> 1                  (flat)         b                                     
#> 2 student_t(3, 115, 77.1) Intercept                                     
#> 3          normal(0, 100)        sd                             0       
#> 4   student_t(3, 0, 77.1)       sds                             0       
#> 5         exponential(.1)     sigma                             0       
#>    source
#> 1 default
#> 2 default
#> 3    user
#> 4 default
#> 5    user
```
-->
<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://tidyverse.tidyverse.org'>tidyverse</a></span><span class='o'>)</span></span>
<span></span>
<span><span class='nf'>brms</span><span class='nf'>:::</span><span class='nf'>prepare_print_prior</span><span class='o'>(</span><span class='nv'>prior_user</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/as.data.frame.html'>as.data.frame</a></span><span class='o'>(</span><span class='o'>)</span> </span>
<span><span class='c'>#&gt;                     prior     class          coef group resp dpar nlpar lb ub</span></span>
<span><span class='c'>#&gt; 1                  (flat)         b                                          </span></span>
<span><span class='c'>#&gt; 2                  (flat)         b        sage_1                            </span></span>
<span><span class='c'>#&gt; 3 student_t(3, 115, 77.1) Intercept                                          </span></span>
<span><span class='c'>#&gt; 4          normal(0, 100)        sd                                      0   </span></span>
<span><span class='c'>#&gt; 5          normal(0, 100)        sd                Tree                  0   </span></span>
<span><span class='c'>#&gt; 6          normal(0, 100)        sd     Intercept  Tree                  0   </span></span>
<span><span class='c'>#&gt; 7   student_t(3, 0, 77.1)       sds                                      0   </span></span>
<span><span class='c'>#&gt; 8   student_t(3, 0, 77.1)       sds s(age, k = 4)                        0   </span></span>
<span><span class='c'>#&gt; 9         exponential(.1)     sigma                                      0   </span></span>
<span><span class='c'>#&gt;   tag       source</span></span>
<span><span class='c'>#&gt; 1          default</span></span>
<span><span class='c'>#&gt; 2     (vectorized)</span></span>
<span><span class='c'>#&gt; 3          default</span></span>
<span><span class='c'>#&gt; 4             user</span></span>
<span><span class='c'>#&gt; 5     (vectorized)</span></span>
<span><span class='c'>#&gt; 6     (vectorized)</span></span>
<span><span class='c'>#&gt; 7          default</span></span>
<span><span class='c'>#&gt; 8     (vectorized)</span></span>
<span><span class='c'>#&gt; 9             user</span></span>
<span></span>
<span><span class='c'># For example:</span></span>
<span><span class='nf'>brms</span><span class='nf'>:::</span><span class='nf'>prepare_print_prior</span><span class='o'>(</span><span class='nv'>prior_user</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/as.data.frame.html'>as.data.frame</a></span><span class='o'>(</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>source</span> <span class='o'>!=</span> <span class='s'>"(vectorized)"</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;                     prior     class coef group resp dpar nlpar lb ub tag</span></span>
<span><span class='c'>#&gt; 1                  (flat)         b                                     </span></span>
<span><span class='c'>#&gt; 2 student_t(3, 115, 77.1) Intercept                                     </span></span>
<span><span class='c'>#&gt; 3          normal(0, 100)        sd                             0       </span></span>
<span><span class='c'>#&gt; 4   student_t(3, 0, 77.1)       sds                             0       </span></span>
<span><span class='c'>#&gt; 5         exponential(.1)     sigma                             0       </span></span>
<span><span class='c'>#&gt;    source</span></span>
<span><span class='c'>#&gt; 1 default</span></span>
<span><span class='c'>#&gt; 2 default</span></span>
<span><span class='c'>#&gt; 3    user</span></span>
<span><span class='c'>#&gt; 4 default</span></span>
<span><span class='c'>#&gt; 5    user</span></span></pre>

### The Harder Workaround

We do not want to rely on private functions from the brms package. Private 
functions belong to the package, so they can break or change. So, let's try to
get the same sort of dataframe from just the `print()` results. The trick will be to 
treat the print-out as a fixed-width formatted data file. Instead of using a 
character to separate columns in a row, a fixed-width format gives each column a 
fixed character width. Parsing the columns is then a matter of breaking the row at 
different character positions.

If we look at the print output as data, we notice that each column is a chunk 
of leading space followed by a chunk of characters.

<!--

``` r
l <- capture.output(print(prior_user, width = 10000))
l[1]
#> [1] "                   prior     class          coef group resp dpar nlpar lb ub tag       source"

first_row <- l[1] |> 
  stringr::str_extract_all("\\s+\\w+") |> 
  unlist()

first_row |> 
  # show one per line
  print(width = 10)
#>  [1] "                   prior"
#>  [2] "     class"              
#>  [3] "          coef"          
#>  [4] " group"                  
#>  [5] " resp"                   
#>  [6] " dpar"                   
#>  [7] " nlpar"                  
#>  [8] " lb"                     
#>  [9] " ub"                     
#> [10] " tag"                    
#> [11] "       source"

col_names <- first_row |> stringr::str_trim()

widths <- nchar(first_row)
names(widths) <- col_names
widths
#>  prior  class   coef  group   resp   dpar  nlpar     lb     ub    tag source 
#>     24     10     14      6      5      5      6      3      3      4     13
```
-->
<pre class='chroma'>
<span><span class='nv'>l</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/utils/capture.output.html'>capture.output</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/print.html'>print</a></span><span class='o'>(</span><span class='nv'>prior_user</span>, width <span class='o'>=</span> <span class='m'>10000</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='nv'>l</span><span class='o'>[</span><span class='m'>1</span><span class='o'>]</span></span>
<span><span class='c'>#&gt; [1] "                   prior     class          coef group resp dpar nlpar lb ub tag       source"</span></span>
<span></span>
<span><span class='nv'>first_row</span> <span class='o'>&lt;-</span> <span class='nv'>l</span><span class='o'>[</span><span class='m'>1</span><span class='o'>]</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'>stringr</span><span class='nf'>::</span><span class='nf'><a href='https://stringr.tidyverse.org/reference/str_extract.html'>str_extract_all</a></span><span class='o'>(</span><span class='s'>"\\s+\\w+"</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/unlist.html'>unlist</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>first_row</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='c'># show one per line</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/print.html'>print</a></span><span class='o'>(</span>width <span class='o'>=</span> <span class='m'>10</span><span class='o'>)</span></span>
<span><span class='c'>#&gt;  [1] "                   prior"</span></span>
<span><span class='c'>#&gt;  [2] "     class"              </span></span>
<span><span class='c'>#&gt;  [3] "          coef"          </span></span>
<span><span class='c'>#&gt;  [4] " group"                  </span></span>
<span><span class='c'>#&gt;  [5] " resp"                   </span></span>
<span><span class='c'>#&gt;  [6] " dpar"                   </span></span>
<span><span class='c'>#&gt;  [7] " nlpar"                  </span></span>
<span><span class='c'>#&gt;  [8] " lb"                     </span></span>
<span><span class='c'>#&gt;  [9] " ub"                     </span></span>
<span><span class='c'>#&gt; [10] " tag"                    </span></span>
<span><span class='c'>#&gt; [11] "       source"</span></span>
<span></span>
<span><span class='nv'>col_names</span> <span class='o'>&lt;-</span> <span class='nv'>first_row</span> <span class='o'>|&gt;</span> <span class='nf'>stringr</span><span class='nf'>::</span><span class='nf'><a href='https://stringr.tidyverse.org/reference/str_trim.html'>str_trim</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>widths</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/nchar.html'>nchar</a></span><span class='o'>(</span><span class='nv'>first_row</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span> <span class='o'>&lt;-</span> <span class='nv'>col_names</span></span>
<span><span class='nv'>widths</span></span>
<span><span class='c'>#&gt;  prior  class   coef  group   resp   dpar  nlpar     lb     ub    tag source </span></span>
<span><span class='c'>#&gt;     24     10     14      6      5      5      6      3      3      4     13</span></span></pre>

We can tell `readr::read_fwf()` to parse the print results using these 
character counts as widths:

<!--

``` r
df <- readr::read_fwf(
  I(l), 
  readr::fwf_widths(widths), 
  skip = 1, 
  show_col_types = FALSE
)

df
#> # A tibble: 9 × 11
#>   X1                 X2    X3    X4    X5    X6    X7       X8 X9    X10   X11  
#>   <chr>              <chr> <chr> <chr> <lgl> <lgl> <lgl> <dbl> <lgl> <lgl> <chr>
#> 1 (flat)             b     <NA>  <NA>  NA    NA    NA       NA NA    NA    defa…
#> 2 (flat)             b     sage… <NA>  NA    NA    NA       NA NA    NA    (vec…
#> 3 student_t(3, 115,… Inte… <NA>  <NA>  NA    NA    NA       NA NA    NA    defa…
#> 4 normal(0, 100)     sd    <NA>  <NA>  NA    NA    NA        0 NA    NA    user 
#> 5 normal(0, 100)     sd    <NA>  Tree  NA    NA    NA        0 NA    NA    (vec…
#> 6 normal(0, 100)     sd    Inte… Tree  NA    NA    NA        0 NA    NA    (vec…
#> 7 student_t(3, 0, 7… sds   <NA>  <NA>  NA    NA    NA        0 NA    NA    defa…
#> 8 student_t(3, 0, 7… sds   s(ag… <NA>  NA    NA    NA        0 NA    NA    (vec…
#> 9 exponential(.1)    sigma <NA>  <NA>  NA    NA    NA        0 NA    NA    user
```
-->
<pre class='chroma'>
<span><span class='nv'>df</span> <span class='o'>&lt;-</span> <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/read_fwf.html'>read_fwf</a></span><span class='o'>(</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/AsIs.html'>I</a></span><span class='o'>(</span><span class='nv'>l</span><span class='o'>)</span>, </span>
<span>  <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/read_fwf.html'>fwf_widths</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span>, </span>
<span>  skip <span class='o'>=</span> <span class='m'>1</span>, </span>
<span>  show_col_types <span class='o'>=</span> <span class='kc'>FALSE</span></span>
<span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>df</span></span>
<span><span class='c'>#&gt; # A tibble: 9 × 11</span></span>
<span><span class='c'>#&gt;   X1                 X2    X3    X4    X5    X6    X7       X8 X9    X10   X11  </span></span>
<span><span class='c'>#&gt;   &lt;chr&gt;              &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;lgl&gt; &lt;lgl&gt; &lt;lgl&gt; &lt;dbl&gt; &lt;lgl&gt; &lt;lgl&gt; &lt;chr&gt;</span></span>
<span><span class='c'>#&gt; 1 (flat)             b     &lt;NA&gt;  &lt;NA&gt;  NA    NA    NA       NA NA    NA    defa…</span></span>
<span><span class='c'>#&gt; 2 (flat)             b     sage… &lt;NA&gt;  NA    NA    NA       NA NA    NA    (vec…</span></span>
<span><span class='c'>#&gt; 3 student_t(3, 115,… Inte… &lt;NA&gt;  &lt;NA&gt;  NA    NA    NA       NA NA    NA    defa…</span></span>
<span><span class='c'>#&gt; 4 normal(0, 100)     sd    &lt;NA&gt;  &lt;NA&gt;  NA    NA    NA        0 NA    NA    user </span></span>
<span><span class='c'>#&gt; 5 normal(0, 100)     sd    &lt;NA&gt;  Tree  NA    NA    NA        0 NA    NA    (vec…</span></span>
<span><span class='c'>#&gt; 6 normal(0, 100)     sd    Inte… Tree  NA    NA    NA        0 NA    NA    (vec…</span></span>
<span><span class='c'>#&gt; 7 student_t(3, 0, 7… sds   &lt;NA&gt;  &lt;NA&gt;  NA    NA    NA        0 NA    NA    defa…</span></span>
<span><span class='c'>#&gt; 8 student_t(3, 0, 7… sds   s(ag… &lt;NA&gt;  NA    NA    NA        0 NA    NA    (vec…</span></span>
<span><span class='c'>#&gt; 9 exponential(.1)    sigma &lt;NA&gt;  &lt;NA&gt;  NA    NA    NA        0 NA    NA    user</span></span></pre>

Two things to note:

1.  This approach treats the first row (column names) as data, so we
    skip that first line.

2.  readr inferred column types, so that the blank cells from above were
    converted to `NA`s. 

Let's try this instead:

<!--

``` r
df <- readr::read_fwf(
  I(l), 
  readr::fwf_widths(widths), 
  skip = 1, 
  show_col_types = FALSE, 
  col_types = readr::cols(.default = readr::col_character()), 
  na = character()
)
colnames(df) <- names(widths)
df
#> # A tibble: 9 × 11
#>   prior             class coef  group resp  dpar  nlpar lb    ub    tag   source
#>   <chr>             <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> 
#> 1 (flat)            b     ""    ""    ""    ""    ""    ""    ""    ""    defau…
#> 2 (flat)            b     "sag… ""    ""    ""    ""    ""    ""    ""    (vect…
#> 3 student_t(3, 115… Inte… ""    ""    ""    ""    ""    ""    ""    ""    defau…
#> 4 normal(0, 100)    sd    ""    ""    ""    ""    ""    "0"   ""    ""    user  
#> 5 normal(0, 100)    sd    ""    "Tre… ""    ""    ""    "0"   ""    ""    (vect…
#> 6 normal(0, 100)    sd    "Int… "Tre… ""    ""    ""    "0"   ""    ""    (vect…
#> 7 student_t(3, 0, … sds   ""    ""    ""    ""    ""    "0"   ""    ""    defau…
#> 8 student_t(3, 0, … sds   "s(a… ""    ""    ""    ""    "0"   ""    ""    (vect…
#> 9 exponential(.1)   sigma ""    ""    ""    ""    ""    "0"   ""    ""    user
```
-->
<pre class='chroma'>
<span><span class='nv'>df</span> <span class='o'>&lt;-</span> <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/read_fwf.html'>read_fwf</a></span><span class='o'>(</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/AsIs.html'>I</a></span><span class='o'>(</span><span class='nv'>l</span><span class='o'>)</span>, </span>
<span>  <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/read_fwf.html'>fwf_widths</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span>, </span>
<span>  skip <span class='o'>=</span> <span class='m'>1</span>, </span>
<span>  show_col_types <span class='o'>=</span> <span class='kc'>FALSE</span>, </span>
<span>  col_types <span class='o'>=</span> <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/cols.html'>cols</a></span><span class='o'>(</span>.default <span class='o'>=</span> <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/parse_atomic.html'>col_character</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span>, </span>
<span>  na <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/character.html'>character</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/colnames.html'>colnames</a></span><span class='o'>(</span><span class='nv'>df</span><span class='o'>)</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span></span>
<span><span class='nv'>df</span></span>
<span><span class='c'>#&gt; # A tibble: 9 × 11</span></span>
<span><span class='c'>#&gt;   prior             class coef  group resp  dpar  nlpar lb    ub    tag   source</span></span>
<span><span class='c'>#&gt;   &lt;chr&gt;             &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; &lt;chr&gt; </span></span>
<span><span class='c'>#&gt; 1 (flat)            b     ""    ""    ""    ""    ""    ""    ""    ""    defau…</span></span>
<span><span class='c'>#&gt; 2 (flat)            b     "sag… ""    ""    ""    ""    ""    ""    ""    (vect…</span></span>
<span><span class='c'>#&gt; 3 student_t(3, 115… Inte… ""    ""    ""    ""    ""    ""    ""    ""    defau…</span></span>
<span><span class='c'>#&gt; 4 normal(0, 100)    sd    ""    ""    ""    ""    ""    "0"   ""    ""    user  </span></span>
<span><span class='c'>#&gt; 5 normal(0, 100)    sd    ""    "Tre… ""    ""    ""    "0"   ""    ""    (vect…</span></span>
<span><span class='c'>#&gt; 6 normal(0, 100)    sd    "Int… "Tre… ""    ""    ""    "0"   ""    ""    (vect…</span></span>
<span><span class='c'>#&gt; 7 student_t(3, 0, … sds   ""    ""    ""    ""    ""    "0"   ""    ""    defau…</span></span>
<span><span class='c'>#&gt; 8 student_t(3, 0, … sds   "s(a… ""    ""    ""    ""    "0"   ""    ""    (vect…</span></span>
<span><span class='c'>#&gt; 9 exponential(.1)   sigma ""    ""    ""    ""    ""    "0"   ""    ""    user</span></span></pre>

Putting this together into a single function:

<!--

``` r
get_formatted_priors <- function(prior) {
  .prior_spec <- as.character(substitute(prior))
  
  l <- capture.output(print(prior, width = 10000))
  first_row <- l[1] |> 
    stringr::str_extract_all("\\s+\\w+") |> 
    unlist()
  col_names <- stringr::str_trim(first_row)
  widths <- nchar(first_row)
  names(widths) <- col_names

  df <- readr::read_fwf(
    I(l), 
    readr::fwf_widths(widths), 
    skip = 1, 
    show_col_types = FALSE, 
    col_types = readr::cols(.default = readr::col_character()), 
    na = character()
  )
  
  colnames(df) <- names(widths)
  df$.prior_spec <- .prior_spec
  df[c(".prior_spec", names(widths))]
}
```
-->
<pre class='chroma'>
<span><span class='nv'>get_formatted_priors</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>prior</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>.prior_spec</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/character.html'>as.character</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>substitute</a></span><span class='o'>(</span><span class='nv'>prior</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  </span>
<span>  <span class='nv'>l</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/utils/capture.output.html'>capture.output</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/print.html'>print</a></span><span class='o'>(</span><span class='nv'>prior</span>, width <span class='o'>=</span> <span class='m'>10000</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  <span class='nv'>first_row</span> <span class='o'>&lt;-</span> <span class='nv'>l</span><span class='o'>[</span><span class='m'>1</span><span class='o'>]</span> <span class='o'>|&gt;</span> </span>
<span>    <span class='nf'>stringr</span><span class='nf'>::</span><span class='nf'><a href='https://stringr.tidyverse.org/reference/str_extract.html'>str_extract_all</a></span><span class='o'>(</span><span class='s'>"\\s+\\w+"</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/unlist.html'>unlist</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>  <span class='nv'>col_names</span> <span class='o'>&lt;-</span> <span class='nf'>stringr</span><span class='nf'>::</span><span class='nf'><a href='https://stringr.tidyverse.org/reference/str_trim.html'>str_trim</a></span><span class='o'>(</span><span class='nv'>first_row</span><span class='o'>)</span></span>
<span>  <span class='nv'>widths</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/nchar.html'>nchar</a></span><span class='o'>(</span><span class='nv'>first_row</span><span class='o'>)</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span> <span class='o'>&lt;-</span> <span class='nv'>col_names</span></span>
<span></span>
<span>  <span class='nv'>df</span> <span class='o'>&lt;-</span> <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/read_fwf.html'>read_fwf</a></span><span class='o'>(</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/r/base/AsIs.html'>I</a></span><span class='o'>(</span><span class='nv'>l</span><span class='o'>)</span>, </span>
<span>    <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/read_fwf.html'>fwf_widths</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span>, </span>
<span>    skip <span class='o'>=</span> <span class='m'>1</span>, </span>
<span>    show_col_types <span class='o'>=</span> <span class='kc'>FALSE</span>, </span>
<span>    col_types <span class='o'>=</span> <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/cols.html'>cols</a></span><span class='o'>(</span>.default <span class='o'>=</span> <span class='nf'>readr</span><span class='nf'>::</span><span class='nf'><a href='https://readr.tidyverse.org/reference/parse_atomic.html'>col_character</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span>, </span>
<span>    na <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/character.html'>character</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span></span>
<span>  </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/colnames.html'>colnames</a></span><span class='o'>(</span><span class='nv'>df</span><span class='o'>)</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span></span>
<span>  <span class='nv'>df</span><span class='o'>$</span><span class='nv'>.prior_spec</span> <span class='o'>&lt;-</span> <span class='nv'>.prior_spec</span></span>
<span>  <span class='nv'>df</span><span class='o'>[</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='o'>(</span><span class='s'>".prior_spec"</span>, <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span><span class='o'>(</span><span class='nv'>widths</span><span class='o'>)</span><span class='o'>)</span><span class='o'>]</span></span>
<span><span class='o'>}</span></span></pre>

Now we can, e.g., bundle up different sets of priors:

<!--

``` r
bind_rows(
  prior_default |> get_formatted_priors(),
  prior_user |> get_formatted_priors()
) |> 
  filter(source != "(vectorized)") |> 
  select(where(function(x) any(x != "")))
#> # A tibble: 10 × 5
#>    .prior_spec   prior                   class     lb    source 
#>    <chr>         <chr>                   <chr>     <chr> <chr>  
#>  1 prior_default (flat)                  b         ""    default
#>  2 prior_default student_t(3, 115, 77.1) Intercept ""    default
#>  3 prior_default student_t(3, 0, 77.1)   sd        "0"   default
#>  4 prior_default student_t(3, 0, 77.1)   sds       "0"   default
#>  5 prior_default student_t(3, 0, 77.1)   sigma     "0"   default
#>  6 prior_user    (flat)                  b         ""    default
#>  7 prior_user    student_t(3, 115, 77.1) Intercept ""    default
#>  8 prior_user    normal(0, 100)          sd        "0"   user   
#>  9 prior_user    student_t(3, 0, 77.1)   sds       "0"   default
#> 10 prior_user    exponential(.1)         sigma     "0"   user
```
-->
<pre class='chroma'>
<span><span class='nf'><a href='https://dplyr.tidyverse.org/reference/bind_rows.html'>bind_rows</a></span><span class='o'>(</span></span>
<span>  <span class='nv'>prior_default</span> <span class='o'>|&gt;</span> <span class='nf'>get_formatted_priors</span><span class='o'>(</span><span class='o'>)</span>,</span>
<span>  <span class='nv'>prior_user</span> <span class='o'>|&gt;</span> <span class='nf'>get_formatted_priors</span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/filter.html'>filter</a></span><span class='o'>(</span><span class='nv'>source</span> <span class='o'>!=</span> <span class='s'>"(vectorized)"</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nf'><a href='https://tidyselect.r-lib.org/reference/where.html'>where</a></span><span class='o'>(</span><span class='kr'>function</span><span class='o'>(</span><span class='nv'>x</span><span class='o'>)</span> <span class='nf'><a href='https://rdrr.io/r/base/any.html'>any</a></span><span class='o'>(</span><span class='nv'>x</span> <span class='o'>!=</span> <span class='s'>""</span><span class='o'>)</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; # A tibble: 10 × 5</span></span>
<span><span class='c'>#&gt;    .prior_spec   prior                   class     lb    source </span></span>
<span><span class='c'>#&gt;    &lt;chr&gt;         &lt;chr&gt;                   &lt;chr&gt;     &lt;chr&gt; &lt;chr&gt;  </span></span>
<span><span class='c'>#&gt;  1 prior_default (flat)                  b         ""    default</span></span>
<span><span class='c'>#&gt;  2 prior_default student_t(3, 115, 77.1) Intercept ""    default</span></span>
<span><span class='c'>#&gt;  3 prior_default student_t(3, 0, 77.1)   sd        "0"   default</span></span>
<span><span class='c'>#&gt;  4 prior_default student_t(3, 0, 77.1)   sds       "0"   default</span></span>
<span><span class='c'>#&gt;  5 prior_default student_t(3, 0, 77.1)   sigma     "0"   default</span></span>
<span><span class='c'>#&gt;  6 prior_user    (flat)                  b         ""    default</span></span>
<span><span class='c'>#&gt;  7 prior_user    student_t(3, 115, 77.1) Intercept ""    default</span></span>
<span><span class='c'>#&gt;  8 prior_user    normal(0, 100)          sd        "0"   user   </span></span>
<span><span class='c'>#&gt;  9 prior_user    student_t(3, 0, 77.1)   sds       "0"   default</span></span>
<span><span class='c'>#&gt; 10 prior_user    exponential(.1)         sigma     "0"   user</span></span></pre>

This approach, it should be noted, is brittle. If brms ever changes how it 
prints out priors---for example, using a `tibble()` or abbreviating long 
column entries---then the above workaround breaks.
