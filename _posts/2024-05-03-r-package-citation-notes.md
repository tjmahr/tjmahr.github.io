---
title: "Notes on Citing R and R Packages"
excerpt: Who, what, where, when and which
tags:
  - r
  - brms
  - stan
share: true
header:
  overlay_image: "assets/images/2024-05-parked-cars.jpg"
  image_description: "Parked car also receive citations"
  overlay_filter: rgba(10, 10, 10, 0.5)
  caption: "Photo credit: [**Hamza ERBAY**](https://unsplash.com/photos/black-and-white-concrete-building-eyKWqgAuZlU)"
---





Our group has started using a new knowledge base system, so I have been
writing up and revisiting some of my documentation. Here I am going to
share a guide I wrote about citing R packages in academic writing.

## Which software to cite

Let's make a distinction here between *reporting* (or summarizing) an
analysis and *reproducing* (or carrying out) an analysis.

Our main manuscript document is for *reporting*. We want to report which
tools and which versions of those tools we used to get our statistical
results. We don't need to include every computational detail. We will
save that level of detail for a supplemental document that shows the
exact modeling code and [`sessioninfo::session_info()`](https://r-lib.github.io/sessioninfo/reference/session_info.html) for
*reproducing* our results. Moreover, journals will sometimes limit the
number of references in a manuscript and a full R analysis might draw
on 15 packages, so we in general cannot cite everything that helped us
get our results. So, we can think more generally about **citation
priorities**.

For an analysis carried out in R, these items have the **highest
priority** for citations:

  - R (the programming language / analysis environment).
  - Third party packages that carried out the analyses.
      - For example, nlme, lme4, ordinal, rms, brms.
  - If a package calls on another language or analysis tool, cite that
    tool as well.
      - For example, brms and rstanarm fit models using the Stan
        programming language, so we need to cite and version Stan as
        well.
  - Packages that performed additional computation on analysis results.
      - For example, emmeans to get marginal means from a fitted model.
  - Packages that visualized analysis results automatically. For
    example, [see](https://easystats.github.io/see/) or
    [interactions](https://interactions.jacob-long.com/).

The following items would have the **lowest priority** for citations:

  - RStudio: It's just an interface to the language. (Ideally, an
    analysis could be run without touching RStudio.)
  - The built-in stats package.
  - knitr/quarto/rmarkdown: These performed R computations for us and
    stored the results in a document.
  - Siloed off parts of a main package. 
    - For example, the gamlss package
      fits GAMLSS models but the distributions for model families are
      stored in the package gamlss.dist. gamlss needs gamlss.dist to work,
      but gamlss is the main important thing to cite.
  - Data storage formats.

If space and the publication venue permit, we can also cite and version
the key R packages that manipulated or visualized the data such as
tidyverse, ggplot2, broom, tidybayes/ggdist, etc. Be generous. We do
want to credit the tools we used to get our results after all!



## Where to get citation information

Creators of scientific software will often tell users how to cite their
software. Scientific software tools often have an associated article
that announces the software and describes how to use it, and authors will
ask users to cite that publication so they can obtain academic credit
for their software work.

For R and R packages, the [`citation()`](https://rdrr.io/r/utils/citation.html) function will tell
users how to cite their software. lme4 is one of those packages that
directs users to a publication.


```r
citation("lme4")
#> To cite lme4 in publications use:
#> 
#>   Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015).
#>   Fitting Linear Mixed-Effects Models Using lme4. Journal of
#>   Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Fitting Linear Mixed-Effects Models Using {lme4}},
#>     author = {Douglas Bates and Martin M{\"a}chler and Ben Bolker and Steve Walker},
#>     journal = {Journal of Statistical Software},
#>     year = {2015},
#>     volume = {67},
#>     number = {1},
#>     pages = {1--48},
#>     doi = {10.18637/jss.v067.i01},
#>   }
```

Notice in the BibTeX entry at the bottom how `{lme4}` is put in braces.
These braces tell LaTeX not to change the capitalization of that word
when printing the title. Some journals or formats have different
preferences for how to capitalize titles, but as a general rule of
thumb, software titles need to be printed verbatim, or as they would be
used by the user. (That is, `library(Lme4)` will not load the lme4
package). When creating bibliography entries, take care to preserve the
capitalization so that the software name is accurate. Take care also to
differentiate between statistical methods and software names: "We fit
GAMLSS models with the gamlss package".

For CRAN packages, the output of `citation()` is also provided online in
HTML. The CRAN package description page (e.g.,
[lme4](https://cran.r-project.org/web/packages/lme4/)) includes a
*Citation* entry which generates a formatted version of the citation
information (e.g., [lme4 citation
info](https://cran.r-project.org/web/packages/lme4/citation.html)).

When the software doesn't have a publication, R will generate a citation
for you. The ordinal package is one such example.


```r
citation("ordinal")
#> To cite 'ordinal' in publications use:
#> 
#>   Christensen R (2023). _ordinal-Regression Models for Ordinal Data_. R
#>   package version 2023.12-4,
#>   <https://CRAN.R-project.org/package=ordinal>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ordinal---Regression Models for Ordinal Data},
#>     author = {Rune H. B. Christensen},
#>     year = {2023},
#>     note = {R package version 2023.12-4},
#>     url = {https://CRAN.R-project.org/package=ordinal},
#>   }
```
The underscores `_` in the title indicate that the title would be
italicized when the citation is [viewed on
CRAN](https://cran.r-project.org/web/packages/ordinal/citation.html). 


## How to cite and version R and R packages

As a rule of thumb, any citation of a resource should answer these
questions:

  - Who (authors)
  - What (title and sometimes format)
  - When (year)
  - Where (journal, URL, book, DOI)

Then for software, we can add the following:

  - Which (version)

The `citation()` will answer these questions for you. 

There are a couple of other functions to know when it comes to package versions.
[`utils::packageVersion()`](https://rdrr.io/r/utils/packageDescription.html) provides the package version as a string:


```r
utils::packageVersion("lme4")
#> [1] '1.1.35.3'
utils::packageVersion("ordinal")
#> [1] '2023.12.4'
```

For the current R version, a bunch of built-in functions can tell you
everything you need to know. I can never remember which of these
functions I want (it's [`getRversion()`](https://rdrr.io/r/base/numeric_version.html)), so I will sometimes use
`utils::packageVersion("base")` to get a simple version number.


```r
R.version.string
#> [1] "R version 4.4.0 (2024-04-24 ucrt)"
R.version
#>                _                                
#> platform       x86_64-w64-mingw32               
#> arch           x86_64                           
#> os             mingw32                          
#> crt            ucrt                             
#> system         x86_64, mingw32                  
#> status                                          
#> major          4                                
#> minor          4.0                              
#> year           2024                             
#> month          04                               
#> day            24                               
#> svn rev        86474                            
#> language       R                                
#> version.string R version 4.4.0 (2024-04-24 ucrt)
#> nickname       Puppy Cup
getRversion()
#> [1] '4.4.0'

utils::packageVersion("base")
#> [1] '4.4.0'
```

For Stan, depending on the backend used, the software version is available via:


```r
# rstanarm and default brms
rstan::stan_version()
#> [1] "2.32.2"

# non-default for brms
cmdstanr::cmdstan_version()
#> [1] "2.34.1"
```


## Examples

A simple example of R, a modeling R package and a helper R package:

> Analyses were carried out the R programming language (vers. 4.2.0, R
> Core Team, 2021). Mixed models were estimated using the lme4 package
> (vers. 1.1.28, Bates et al., 2015). We estimated marginal means and
> contrasts using the emmeans package (vers. 1.7.2, Lenth, 2021).

Below is the actual RMarkdown content, so that version numbers and
citations are inlined automatically. (We're omitting details on creating
.bib files or using pandoc's `@` citations.)

````md
```{r}
v_lme4 <- packageVersion("lme4")
v_r <- packageVersion("base")
v_emmeans <- packageVersion("emmeans")
```

Analyses were carried out the R programming language [vers. `r v_r`,
@rstats]. Mixed models were estimated using the lme4 package
[vers. `r v_lme4`, @lme4]. We estimated marginal means and contrasts
using the emmeans package [vers. `r v_emmeans`, @emmeans].
````

This aspect of the code is invisible, but I use [nonbreaking
spaces](https://en.wikipedia.org/wiki/Non-breaking_space) (HTML
`@nbsp;`) after `vers.` so that the `vers.` and the version number stay
on the same line.

Here is a more involved example involving an additional language and an R
package that interfaces to that language:

> We estimated the models using Stan (vers. 2.27.0, Carpenter et al., 2017) via
> the brms package (vers. 2.16.1, Bürkner, 2017) and tidybayes package
> (vers. 3.0.4, Kay, 2021) in R (vers. 4.3.0, R Core Team, 2021).


Behind the scenes, I had written the following RMarkdown:

````md
```{r}
model <- targets::tar_read(model_random_slope)
v_stan <- model$version$cmdstan
v_brms <- model$version$brms
v_tidybayes <- packageVersion("tidybayes")
v_r <- getRversion()
```

We estimated the models using Stan [vers. `r v_stan`, @stan] via the
brms package [vers. `r v_brms`, @brms-jss] and tidybayes package
[vers. `r v_tidybayes`, @R-tidybayes] in R [vers. `r v_r`, @r-base].
````

Notice that I am reading in a cached model object (`targets::tar_read()`) and
reading the software versions from that object. This arrangement avoids problems
where models are fitted with one version of a package but
`utils::packageVersion()` returns a different, more recent package version. brms
stored these versions automatically for me. In general, when I cache a model
like this, I store the package version in the model object.

## A note on automatic citation helpers

A tool like the [grateful package](https://pakillo.github.io/grateful/)
will generate a list of references and citations for us. Suppose we had
the following where we fit a model and look at a summary of it.


```r
library(tidyverse)
library(mgcv)
#> Loading required package: nlme
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse
#> This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.

data <- MASS::mcycle
model <- gam(accel ~ s(times, bs = "cr"), data = data)
broom::tidy(model)
#> # A tibble: 1 × 5
#>   term       edf ref.df statistic p.value
#>   <chr>    <dbl>  <dbl>     <dbl>   <dbl>
#> 1 s(times)  8.39   8.87      53.8       0
```

grateful detects the following packages in use:


```r
grateful::scan_packages(pkgs = "Session")
#>         pkg version
#> 1      base   4.4.0
#> 2     knitr    1.46
#> 3      mgcv   1.9.1
#> 4      nlme 3.1.164
#> 5 tidyverse   2.0.0
```

Note that broom is excluded despite being used and that nlme is included 
despite not being loaded directly. That's because broom is loaded by 
tidyverse and gets absorbed by the tidyverse citation and because mgcv 
loads nlme (as its start-up message says). (And knitr is loaded when I render 
my blog posts but not when I work within this R session interactively.)

Okay, so we should just list the packages manually and disable tidyverse
from absorbing broom. grateful can create a bibliography for us:


```r
grateful::get_pkgs_info(
  pkgs = c("mgcv", "broom"), 
  out.dir = getwd(), 
  cite.tidyverse = FALSE
)
#>     pkg version                                         citekeys
#> 1 broom   1.0.5                                            broom
#> 2  mgcv   1.9.1 mgcv2011, mgcv2016, mgcv2004, mgcv2017, mgcv2003

a <- grateful::cite_packages(
  pkgs = c("mgcv", "broom"), 
  out.dir = getwd(), 
  cite.tidyverse = FALSE, 
  output = "paragraph"
)
a |> stringr::str_wrap(width = 72) |> writeLines()
#> We used the following R packages: broom v. 1.0.5 [@broom], mgcv v. 1.9.1
#> [@mgcv2003; @mgcv2004; @mgcv2011; @mgcv2016; @mgcv2017].
```

Now, we have the issue where `citation(mgcv)` has [multiple publications
associated](https://cran.r-project.org/web/packages/mgcv/citation.html)
with it, not all of which are relevant for our usage.

The point of this example is that a tool like grateful---and more
generally tools that produce code for us---can be useful to compile 
information and get the ball rolling for us. But, we still have
to edit and refine the outputs to work correctly.







***

*Last knitted on 2024-05-07. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2024-05-03-r-package-citation-notes.Rmd).*[^si] 

[^si]: 
    
    ```r
    .session_info
    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting         value
    #>  version         R version 4.4.0 (2024-04-24 ucrt)
    #>  os              Windows 10 x64 (build 19045)
    #>  system          x86_64, mingw32
    #>  ui              RTerm
    #>  language        (EN)
    #>  collate         English_United States.utf8
    #>  ctype           English_United States.utf8
    #>  tz              America/Chicago
    #>  date            2024-05-07
    #>  pandoc          NA
    #>  stan (rstan)    2.32.2
    #>  stan (cmdstanr) 2.34.1
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package        * version  date (UTC) lib source
    #>    abind            1.4-5    2016-07-21 [1] CRAN (R 4.4.0)
    #>    backports        1.4.1    2021-12-13 [1] CRAN (R 4.4.0)
    #>    broom            1.0.5    2023-06-09 [1] CRAN (R 4.4.0)
    #>    cachem           1.0.8    2023-05-01 [1] CRAN (R 4.4.0)
    #>    checkmate        2.3.1    2023-12-04 [1] CRAN (R 4.4.0)
    #>    cli              3.6.2    2023-12-11 [1] CRAN (R 4.4.0)
    #>    cmdstanr         0.7.1    2024-05-03 [1] local
    #>    codetools        0.2-20   2024-03-31 [2] CRAN (R 4.4.0)
    #>    colorspace       2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
    #>    curl             5.2.1    2024-03-01 [1] CRAN (R 4.4.0)
    #>    distributional   0.4.0    2024-02-07 [1] CRAN (R 4.4.0)
    #>    downlit          0.4.3    2023-06-29 [1] CRAN (R 4.4.0)
    #>    dplyr          * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
    #>    evaluate         0.23     2023-11-01 [1] CRAN (R 4.4.0)
    #>    fansi            1.0.6    2023-12-08 [1] CRAN (R 4.4.0)
    #>    fastmap          1.1.1    2023-02-24 [1] CRAN (R 4.4.0)
    #>    forcats        * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
    #>    generics         0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
    #>    ggplot2        * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
    #>    git2r            0.33.0   2023-11-26 [1] CRAN (R 4.4.0)
    #>    glue             1.7.0    2024-01-09 [1] CRAN (R 4.4.0)
    #>    grateful         0.2.4    2023-10-22 [1] CRAN (R 4.4.0)
    #>    gridExtra        2.3      2017-09-09 [1] CRAN (R 4.4.0)
    #>    gtable           0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
    #>    here             1.0.1    2020-12-13 [1] CRAN (R 4.4.0)
    #>    hms              1.1.3    2023-03-21 [1] CRAN (R 4.4.0)
    #>    inline           0.3.19   2021-05-31 [1] CRAN (R 4.4.0)
    #>    jsonlite         1.8.8    2023-12-04 [1] CRAN (R 4.4.0)
    #>    knitr          * 1.46     2024-04-06 [1] CRAN (R 4.4.0)
    #>    lattice          0.22-6   2024-03-20 [2] CRAN (R 4.4.0)
    #>    lifecycle        1.0.4    2023-11-07 [1] CRAN (R 4.4.0)
    #>    loo              2.7.0    2024-02-24 [1] CRAN (R 4.4.0)
    #>    lubridate      * 1.9.3    2023-09-27 [1] CRAN (R 4.4.0)
    #>    magrittr         2.0.3    2022-03-30 [1] CRAN (R 4.4.0)
    #>    MASS             7.3-60.2 2024-04-24 [2] local
    #>    Matrix           1.7-0    2024-03-22 [2] CRAN (R 4.4.0)
    #>    matrixStats      1.3.0    2024-04-11 [1] CRAN (R 4.4.0)
    #>    memoise          2.0.1    2021-11-26 [1] CRAN (R 4.4.0)
    #>    mgcv           * 1.9-1    2023-12-21 [2] CRAN (R 4.4.0)
    #>    munsell          0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
    #>    nlme           * 3.1-164  2023-11-27 [2] CRAN (R 4.4.0)
    #>    pillar           1.9.0    2023-03-22 [1] CRAN (R 4.4.0)
    #>    pkgbuild         1.4.4    2024-03-17 [1] CRAN (R 4.4.0)
    #>    pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.4.0)
    #>    posterior        1.5.0    2023-10-31 [1] CRAN (R 4.4.0)
    #>    processx         3.8.4    2024-03-16 [1] CRAN (R 4.4.0)
    #>    ps               1.7.6    2024-01-18 [1] CRAN (R 4.4.0)
    #>    purrr          * 1.0.2    2023-08-10 [1] CRAN (R 4.4.0)
    #>    QuickJSR         1.1.3    2024-01-31 [1] CRAN (R 4.4.0)
    #>    R6               2.5.1    2021-08-19 [1] CRAN (R 4.4.0)
    #>    ragg             1.3.0    2024-03-13 [1] CRAN (R 4.4.0)
    #>    Rcpp             1.0.12   2024-01-09 [1] CRAN (R 4.4.0)
    #>  D RcppParallel     5.1.7    2023-02-27 [1] CRAN (R 4.4.0)
    #>    readr          * 2.1.5    2024-01-10 [1] CRAN (R 4.4.0)
    #>    rlang            1.1.3    2024-01-10 [1] CRAN (R 4.4.0)
    #>    rprojroot        2.0.4    2023-11-05 [1] CRAN (R 4.4.0)
    #>    rstan            2.32.6   2024-03-05 [1] CRAN (R 4.4.0)
    #>    rstudioapi       0.16.0   2024-03-24 [1] CRAN (R 4.4.0)
    #>    scales           1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
    #>    sessioninfo      1.2.2    2021-12-06 [1] CRAN (R 4.4.0)
    #>    StanHeaders      2.32.7   2024-04-25 [1] CRAN (R 4.4.0)
    #>    stringi          1.8.3    2023-12-11 [1] CRAN (R 4.4.0)
    #>    stringr        * 1.5.1    2023-11-14 [1] CRAN (R 4.4.0)
    #>    systemfonts      1.0.6    2024-03-07 [1] CRAN (R 4.4.0)
    #>    tensorA          0.36.2.1 2023-12-13 [1] CRAN (R 4.4.0)
    #>    textshaping      0.3.7    2023-10-09 [1] CRAN (R 4.4.0)
    #>    tibble         * 3.2.1    2023-03-20 [1] CRAN (R 4.4.0)
    #>    tidyr          * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
    #>    tidyselect       1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
    #>    tidyverse      * 2.0.0    2023-02-22 [1] CRAN (R 4.4.0)
    #>    timechange       0.3.0    2024-01-18 [1] CRAN (R 4.4.0)
    #>    tzdb             0.4.0    2023-05-12 [1] CRAN (R 4.4.0)
    #>    utf8             1.2.4    2023-10-22 [1] CRAN (R 4.4.0)
    #>    V8               4.4.2    2024-02-15 [1] CRAN (R 4.4.0)
    #>    vctrs            0.6.5    2023-12-01 [1] CRAN (R 4.4.0)
    #>    withr            3.0.0    2024-01-16 [1] CRAN (R 4.4.0)
    #>    xfun             0.43     2024-03-25 [1] CRAN (R 4.4.0)
    #>    yaml             2.3.8    2023-12-11 [1] CRAN (R 4.4.0)
    #> 
    #>  [1] C:/Users/mahr/AppData/Local/R/win-library/4.4
    #>  [2] C:/Program Files/R/R-4.4.0/library
    #> 
    #>  D ── DLL MD5 mismatch, broken installation.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
    ```
