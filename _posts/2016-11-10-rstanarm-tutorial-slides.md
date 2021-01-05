---
title: "Slides from my RStanARM tutorial"
excerpt: "Trial by 🔥"
tags:
  - rstanarm
  - bayesian
---

Back in September, I gave a tutorial on
[RStanARM](https://cran.rstudio.com/web/packages/rstanarm/) to the [Madison R
user's group](https://www.meetup.com/MadR-Madison-R-Programming-UseRs-Group/).
As I did for [my magrittr tutorial](https://github.com/tjmahr/MadR_Pipelines), I
broke the content down into slide decks. They were:

  - [How I got into Bayesian statistics](http://rpubs.com/tjmahr/rep-crisis)
  - [Some intuition-building about Bayes
    theorem](http://rpubs.com/tjmahr/bayes-theorem)
  - [Tour of RStanARM](http://rpubs.com/tjmahr/rstanarm-tour)
  - [Where to learn more about Bayesian
    statistics](http://rpubs.com/tjmahr/bayes-learn-more)

The source code and supporting materials are [on
Github](https://github.com/tjmahr/MadR_RStanARM).


Observations (training data) 
-------------------------------------------------------------------------------

The intuition-building section was the most challenging and rewarding, because I
had to brush up on Bayesian statistics well enough to informally, hand-wavily 
teach about it to a crowd of R users. Like, I have good sense of how to fit 
these models and interpret them in practice, but there's a gulf between 
understanding something and teaching about it. It was a bit of trial by fire
🔥.

One thing I did was work through a toy Bayesian updating demo. What's the mean of
some IQ scores, assuming a standard deviation of 15 and a flat prior over a 
reasonable range values? Cue some plots of how the distribution of probabilities
update as new data is observed.

<img src="/figs/2016-11-10-rstanarm-tutorial-slides/iq-00-data-1.png" title="A frame of my Bayesian updating animation" alt="A frame of my Bayesian updating animation" width="50%" /><img src="/figs/2016-11-10-rstanarm-tutorial-slides/iq-01-data-1.png" title="A frame of my Bayesian updating animation" alt="A frame of my Bayesian updating animation" width="50%" /><img src="/figs/2016-11-10-rstanarm-tutorial-slides/iq-02-data-1.png" title="A frame of my Bayesian updating animation" alt="A frame of my Bayesian updating animation" width="50%" /><img src="/figs/2016-11-10-rstanarm-tutorial-slides/iq-03-data-1.png" title="A frame of my Bayesian updating animation" alt="A frame of my Bayesian updating animation" width="50%" />

_See how the beliefs are updated? See how we retain uncertainty around that most
likely value?_ And so on.

Naturally, [I animated the thing](/figs/2016-11-10-rstanarm-tutorial-slides/simple-updating.gif)---I'll take any excuse to use [gganimate](https://github.com/dgrtwo/gganimate). 

Someone asked a good question about what advantages these models have over 
classical ones. I find the models more intuitive[^1], because posterior
probabilities are post-data probabilities. I also find them more flexible. For
example, I can use a _t_-distribution for my error terms---thick tails! If I
write the thing in Stan, I can incorporate measurement error into the model. If
I put my head down and work really hard, I could even fit one of [those gorgeous
Gaussian process 
models](https://matthewdharris.com/2016/05/16/gaussian-process-hyperparameter-estimation/).
We can fit vanilla regression models or get really, really fancy, but it all
kind of emerges nicely from the general framework of writing out priors and a
likelihood definition.



***

*Last knitted on 2021-02-02. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2016-11-10-rstanarm-tutorial-slides.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info ---------------------------------------------------------------
    #>  setting  value                       
    #>  version  R version 4.0.3 (2020-10-10)
    #>  os       Windows 10 x64              
    #>  system   x86_64, mingw32             
    #>  ui       RTerm                       
    #>  language (EN)                        
    #>  collate  English_United States.1252  
    #>  ctype    English_United States.1252  
    #>  tz       America/Chicago             
    #>  date     2021-02-02                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package     * version    date       lib source                     
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.2)             
    #>  cli           2.2.0      2020-11-20 [1] CRAN (R 4.0.3)             
    #>  crayon        1.3.4      2017-09-16 [1] CRAN (R 4.0.2)             
    #>  emo           0.0.0.9000 2020-07-06 [1] Github (hadley/emo@3f03b11)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.2)             
    #>  fansi         0.4.2      2021-01-15 [1] CRAN (R 4.0.3)             
    #>  generics      0.1.0      2020-10-31 [1] CRAN (R 4.0.3)             
    #>  git2r         0.28.0     2021-01-10 [1] CRAN (R 4.0.3)             
    #>  glue          1.4.2      2020-08-27 [1] CRAN (R 4.0.2)             
    #>  here          1.0.1      2020-12-13 [1] CRAN (R 4.0.3)             
    #>  highr         0.8        2019-03-20 [1] CRAN (R 4.0.2)             
    #>  knitr       * 1.31       2021-01-27 [1] CRAN (R 4.0.3)             
    #>  lubridate     1.7.9.2    2020-11-13 [1] CRAN (R 4.0.3)             
    #>  magrittr      2.0.1      2020-11-17 [1] CRAN (R 4.0.3)             
    #>  purrr         0.3.4      2020-04-17 [1] CRAN (R 4.0.2)             
    #>  Rcpp          1.0.6      2021-01-15 [1] CRAN (R 4.0.3)             
    #>  rlang         0.4.10     2020-12-30 [1] CRAN (R 4.0.3)             
    #>  rprojroot     2.0.2      2020-11-15 [1] CRAN (R 4.0.3)             
    #>  rstudioapi    0.13       2020-11-12 [1] CRAN (R 4.0.3)             
    #>  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.2)             
    #>  stringi       1.5.3      2020-09-09 [1] CRAN (R 4.0.2)             
    #>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.2)             
    #>  withr         2.4.1      2021-01-26 [1] CRAN (R 4.0.3)             
    #>  xfun          0.20       2021-01-06 [1] CRAN (R 4.0.3)             
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.3/library
    ```

[^1]: But I was taught the classical models first... I sometimes think that these models are only more intuitive because this is my second bite at the apple. This learning came more easily because the first time I learned regression, I was a total novice and had to learn everything. I had learn to about _t_-test, reductions in variance, collinearity, and what interactions do. Here, I can build off of that prior learning. Maybe if I learn everything again---as what? everything as a neural network?---it will be even more intuitive. 
