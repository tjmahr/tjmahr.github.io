---
title: "Confusion matrix statistics on late talker diagnoses"
excerpt: Posterior predictive values and the like.
tags:
  - caret
  - r
---

How many late talkers are just _late bloomers_? More precisely, how many
children identified as late talkers at 18 months catch up to the normal range
by one year later? This is an important question. From a clinical perspective,
we want to support children with language delays, but it is also inefficient to
spend resources fixing a self-correcting problem.

[Fernald and Marchman (2012)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3266972/) 
touch on this question. Children falling below the 20<sup>th</sup> percentile in
vocabulary score at 18 months were labeled "late talkers". These children, along
with a control group of timely-talkers, participated in an eyetracking study at
18 months and had their vocabulary measured every 3 months until 30 months of
age.

In their sample, 22 of 36 late talkers were late bloomers, catching up to the
normal vocabulary range at 30 months, and 42 of 46 timely talkers remained in
the normal range of vocab development. The authors later report that eyetracking
reaction times at 18 months predicted rates of vocabulary growth in both groups.
In particular, the late-bloomers were significantly faster than the children who
did not catch up.

The authors repeatedly report confusion matrix statistics on different subsets
of the data. Which make sense: The question of late bloomers is also a question
about the _positive predictive value_ of a late-talker diagnosis. In the
majority of cases, a "late talker" label at 18 months did not predict continued
delay one year later. Therefore, the diagnosis has poor positive predictive
value (14/36 = 39%).

## Confusion Matrix Measures in R

I would like to report similar classification quantities in my own analyses, so
I figured out how to reproduce their results in R. And it's as simple as calling
the `confusionMatrix` function in the caret package. 

First, let's re-create their data. We'll make a long dataframe with one row
per child reported in the study. We will have fields for each child's initial
`Group` (late talking or within-normal-limits at 18 months), their `Predicted`
group (assuming late-talking children remain delayed), and the observed
`Outcome`.


```r
library(dplyr)

# LT: late talking
# WNL: within normal limits
groups <- c("WNL at 18m", "LT at 18m")
outcomes <- c("WNL at 30m", "Delayed at 30m")

# Counts from paper
lt_still_delayed <- 14
lt_bloomed <- 22

wnl_still_wnl <- 42
wnl_delayed <- 4

# Reproduce their data-set (one row per reported child)
wnl_data <- tibble(
  Group = groups[1],
  Predicted = outcomes[1],
  Outcome = rep(outcomes, times = c(wnl_still_wnl, wnl_delayed))
)

lt_data <- tibble(
  Group = "LT at 18m",
  Outcome = rep(outcomes, times = c(lt_bloomed, lt_still_delayed)),
  Predicted = outcomes[2]
)


all_kids <- bind_rows(wnl_data, lt_data) %>%
  mutate(ChildID = seq_along(Outcome)) %>% 
  select(ChildID, Group, Predicted, Outcome) %>% 
  mutate(
    Predicted = factor(Predicted, outcomes),
    Outcome = factor(Outcome, outcomes)
  )
```

What we have looks like a real data-set now.




```r
all_kids %>% 
  sample_n(8, replace = FALSE) %>% 
  arrange(Group, Predicted, Outcome)
#> # A tibble: 8 x 4
#>   ChildID Group      Predicted      Outcome   
#>     <int> <chr>      <fct>          <fct>     
#> 1      47 LT at 18m  Delayed at 30m WNL at 30m
#> 2      52 LT at 18m  Delayed at 30m WNL at 30m
#> 3      60 LT at 18m  Delayed at 30m WNL at 30m
#> 4       1 WNL at 18m WNL at 30m     WNL at 30m
#> 5      16 WNL at 18m WNL at 30m     WNL at 30m
#> 6      19 WNL at 18m WNL at 30m     WNL at 30m
#> 7      34 WNL at 18m WNL at 30m     WNL at 30m
#> 8      27 WNL at 18m WNL at 30m     WNL at 30m
```

Next, we just call `confusionMatrix` on the predicted values and the reference
values.


```r
conf_mat <- caret::confusionMatrix(all_kids$Predicted, all_kids$Outcome)
conf_mat
#> Confusion Matrix and Statistics
#> 
#>                 Reference
#> Prediction       WNL at 30m Delayed at 30m
#>   WNL at 30m             42              4
#>   Delayed at 30m         22             14
#>                                           
#>                Accuracy : 0.6829          
#>                  95% CI : (0.5708, 0.7813)
#>     No Information Rate : 0.7805          
#>     P-Value [Acc > NIR] : 0.9855735       
#>                                           
#>                   Kappa : 0.3193          
#>                                           
#>  Mcnemar's Test P-Value : 0.0008561       
#>                                           
#>             Sensitivity : 0.6562          
#>             Specificity : 0.7778          
#>          Pos Pred Value : 0.9130          
#>          Neg Pred Value : 0.3889          
#>              Prevalence : 0.7805          
#>          Detection Rate : 0.5122          
#>    Detection Prevalence : 0.5610          
#>       Balanced Accuracy : 0.7170          
#>                                           
#>        'Positive' Class : WNL at 30m      
#> 
```



Here, we can confirm the positive predictive value (true positives / positive
calls)[^PPV] is 14/36 = 0.913. The negative predictive value is noteworthy;
most children not diagnosed as late talkers did not show a delay one year later
(NPV = 42/46 = 0.3889).



***

*Last knitted on 2021-02-02. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2015-10-06-confusion-matrix-late-talkers.Rmd).*[^si] 

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
    #>  package      * version    date       lib source        
    #>  assertthat     0.2.1      2019-03-21 [1] CRAN (R 4.0.2)
    #>  caret          6.0-86     2020-03-20 [1] CRAN (R 4.0.2)
    #>  class          7.3-18     2021-01-24 [1] CRAN (R 4.0.3)
    #>  cli            2.2.0      2020-11-20 [1] CRAN (R 4.0.3)
    #>  codetools      0.2-18     2020-11-04 [1] CRAN (R 4.0.2)
    #>  colorspace     2.0-0      2020-11-11 [1] CRAN (R 4.0.3)
    #>  crayon         1.3.4      2017-09-16 [1] CRAN (R 4.0.2)
    #>  data.table     1.13.6     2020-12-30 [1] CRAN (R 4.0.3)
    #>  DBI            1.1.1      2021-01-15 [1] CRAN (R 4.0.3)
    #>  dplyr        * 1.0.3      2021-01-15 [1] CRAN (R 4.0.3)
    #>  e1071          1.7-4      2020-10-14 [1] CRAN (R 4.0.3)
    #>  ellipsis       0.3.1      2020-05-15 [1] CRAN (R 4.0.2)
    #>  evaluate       0.14       2019-05-28 [1] CRAN (R 4.0.2)
    #>  fansi          0.4.2      2021-01-15 [1] CRAN (R 4.0.3)
    #>  foreach        1.5.1      2020-10-15 [1] CRAN (R 4.0.3)
    #>  generics       0.1.0      2020-10-31 [1] CRAN (R 4.0.3)
    #>  ggplot2        3.3.3      2020-12-30 [1] CRAN (R 4.0.3)
    #>  git2r          0.28.0     2021-01-10 [1] CRAN (R 4.0.3)
    #>  glue           1.4.2      2020-08-27 [1] CRAN (R 4.0.2)
    #>  gower          0.2.2      2020-06-23 [1] CRAN (R 4.0.2)
    #>  gtable         0.3.0      2019-03-25 [1] CRAN (R 4.0.2)
    #>  here           1.0.1      2020-12-13 [1] CRAN (R 4.0.3)
    #>  ipred          0.9-9      2019-04-28 [1] CRAN (R 4.0.2)
    #>  iterators      1.0.13     2020-10-15 [1] CRAN (R 4.0.3)
    #>  knitr        * 1.31       2021-01-27 [1] CRAN (R 4.0.3)
    #>  lattice        0.20-41    2020-04-02 [1] CRAN (R 4.0.2)
    #>  lava           1.6.8.1    2020-11-04 [1] CRAN (R 4.0.2)
    #>  lifecycle      0.2.0      2020-03-06 [1] CRAN (R 4.0.2)
    #>  lubridate      1.7.9.2    2020-11-13 [1] CRAN (R 4.0.3)
    #>  magrittr       2.0.1      2020-11-17 [1] CRAN (R 4.0.3)
    #>  MASS           7.3-53     2020-09-09 [1] CRAN (R 4.0.2)
    #>  Matrix         1.2-18     2019-11-27 [1] CRAN (R 4.0.3)
    #>  ModelMetrics   1.2.2.2    2020-03-17 [1] CRAN (R 4.0.2)
    #>  munsell        0.5.0      2018-06-12 [1] CRAN (R 4.0.2)
    #>  nlme           3.1-151    2020-12-10 [1] CRAN (R 4.0.3)
    #>  nnet           7.3-15     2021-01-24 [1] CRAN (R 4.0.3)
    #>  pillar         1.4.7      2020-11-20 [1] CRAN (R 4.0.3)
    #>  pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.0.2)
    #>  plyr           1.8.6      2020-03-03 [1] CRAN (R 4.0.2)
    #>  pROC           1.17.0.1   2021-01-13 [1] CRAN (R 4.0.3)
    #>  prodlim        2019.11.13 2019-11-17 [1] CRAN (R 4.0.2)
    #>  purrr          0.3.4      2020-04-17 [1] CRAN (R 4.0.2)
    #>  R6             2.5.0      2020-10-28 [1] CRAN (R 4.0.2)
    #>  Rcpp           1.0.6      2021-01-15 [1] CRAN (R 4.0.3)
    #>  recipes        0.1.15     2020-11-11 [1] CRAN (R 4.0.2)
    #>  reshape2       1.4.4      2020-04-09 [1] CRAN (R 4.0.2)
    #>  rlang          0.4.10     2020-12-30 [1] CRAN (R 4.0.3)
    #>  rpart          4.1-15     2019-04-12 [1] CRAN (R 4.0.2)
    #>  rprojroot      2.0.2      2020-11-15 [1] CRAN (R 4.0.3)
    #>  scales         1.1.1      2020-05-11 [1] CRAN (R 4.0.2)
    #>  sessioninfo    1.1.1      2018-11-05 [1] CRAN (R 4.0.2)
    #>  stringi        1.5.3      2020-09-09 [1] CRAN (R 4.0.2)
    #>  stringr        1.4.0      2019-02-10 [1] CRAN (R 4.0.2)
    #>  survival       3.2-7      2020-09-28 [1] CRAN (R 4.0.2)
    #>  tibble         3.0.5      2021-01-15 [1] CRAN (R 4.0.3)
    #>  tidyselect     1.1.0      2020-05-11 [1] CRAN (R 4.0.2)
    #>  timeDate       3043.102   2018-02-21 [1] CRAN (R 4.0.0)
    #>  utf8           1.1.4      2018-05-24 [1] CRAN (R 4.0.2)
    #>  vctrs          0.3.6      2020-12-17 [1] CRAN (R 4.0.3)
    #>  withr          2.4.1      2021-01-26 [1] CRAN (R 4.0.3)
    #>  xfun           0.20       2021-01-06 [1] CRAN (R 4.0.3)
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.3/library
    ```

[^PPV]: Technically, caret uses the [sensitivity, specificity and prevalence](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values) form of the PPV calculation.
