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

*Last knitted on 2021-11-16. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2015-10-06-confusion-matrix-late-talkers.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info  --------------------------------------------------------------
    #>  hash: raised back of hand: light skin tone, backhand index pointing up: medium-light skin tone, dna
    #> 
    #>  setting  value
    #>  version  R version 4.1.2 (2021-11-01)
    #>  os       Windows 10 x64 (build 22000)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language (EN)
    #>  collate  English_United States.1252
    #>  ctype    English_United States.1252
    #>  tz       America/Chicago
    #>  date     2021-11-16
    #>  pandoc   NA
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package      * version    date (UTC) lib source
    #>  assertthat     0.2.1      2019-03-21 [1] CRAN (R 4.1.0)
    #>  caret          6.0-90     2021-10-09 [1] CRAN (R 4.1.1)
    #>  class          7.3-19     2021-05-03 [2] CRAN (R 4.1.2)
    #>  cli            3.1.0      2021-10-27 [1] CRAN (R 4.1.1)
    #>  codetools      0.2-18     2020-11-04 [2] CRAN (R 4.1.2)
    #>  colorspace     2.0-2      2021-06-24 [1] CRAN (R 4.1.0)
    #>  crayon         1.4.2      2021-10-29 [1] CRAN (R 4.1.1)
    #>  data.table     1.14.2     2021-09-27 [1] CRAN (R 4.1.1)
    #>  DBI            1.1.1      2021-01-15 [1] CRAN (R 4.1.0)
    #>  digest         0.6.28     2021-09-23 [1] CRAN (R 4.1.1)
    #>  dplyr        * 1.0.7      2021-06-18 [1] CRAN (R 4.1.0)
    #>  e1071          1.7-9      2021-09-16 [1] CRAN (R 4.1.1)
    #>  ellipsis       0.3.2      2021-04-29 [1] CRAN (R 4.1.0)
    #>  evaluate       0.14       2019-05-28 [1] CRAN (R 4.1.0)
    #>  fansi          0.5.0      2021-05-25 [1] CRAN (R 4.1.0)
    #>  foreach        1.5.1      2020-10-15 [1] CRAN (R 4.1.1)
    #>  future         1.23.0     2021-10-31 [1] CRAN (R 4.1.1)
    #>  future.apply   1.8.1      2021-08-10 [1] CRAN (R 4.1.1)
    #>  generics       0.1.1      2021-10-25 [1] CRAN (R 4.1.1)
    #>  ggplot2        3.3.5      2021-06-25 [1] CRAN (R 4.1.0)
    #>  git2r          0.28.0     2021-01-10 [1] CRAN (R 4.1.1)
    #>  globals        0.14.0     2020-11-22 [1] CRAN (R 4.1.0)
    #>  glue           1.4.2      2020-08-27 [1] CRAN (R 4.1.1)
    #>  gower          0.2.2      2020-06-23 [1] CRAN (R 4.1.1)
    #>  gtable         0.3.0      2019-03-25 [1] CRAN (R 4.1.0)
    #>  here           1.0.1      2020-12-13 [1] CRAN (R 4.1.0)
    #>  ipred          0.9-12     2021-09-15 [1] CRAN (R 4.1.1)
    #>  iterators      1.0.13     2020-10-15 [1] CRAN (R 4.1.1)
    #>  knitr        * 1.36       2021-09-29 [1] CRAN (R 4.1.1)
    #>  lattice        0.20-45    2021-09-22 [2] CRAN (R 4.1.2)
    #>  lava           1.6.10     2021-09-02 [1] CRAN (R 4.1.1)
    #>  lifecycle      1.0.1      2021-09-24 [1] CRAN (R 4.1.1)
    #>  listenv        0.8.0      2019-12-05 [1] CRAN (R 4.1.0)
    #>  lubridate      1.8.0      2021-10-07 [1] CRAN (R 4.1.1)
    #>  magrittr       2.0.1      2020-11-17 [1] CRAN (R 4.1.0)
    #>  MASS           7.3-54     2021-05-03 [2] CRAN (R 4.1.2)
    #>  Matrix         1.3-4      2021-06-01 [2] CRAN (R 4.1.2)
    #>  ModelMetrics   1.2.2.2    2020-03-17 [1] CRAN (R 4.1.1)
    #>  munsell        0.5.0      2018-06-12 [1] CRAN (R 4.1.0)
    #>  nlme           3.1-153    2021-09-07 [2] CRAN (R 4.1.2)
    #>  nnet           7.3-16     2021-05-03 [2] CRAN (R 4.1.2)
    #>  parallelly     1.28.1     2021-09-09 [1] CRAN (R 4.1.1)
    #>  pillar         1.6.4      2021-10-18 [1] CRAN (R 4.1.1)
    #>  pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.1.0)
    #>  plyr           1.8.6      2020-03-03 [1] CRAN (R 4.1.0)
    #>  pROC           1.18.0     2021-09-03 [1] CRAN (R 4.1.1)
    #>  prodlim        2019.11.13 2019-11-17 [1] CRAN (R 4.1.1)
    #>  proxy          0.4-26     2021-06-07 [1] CRAN (R 4.1.1)
    #>  purrr          0.3.4      2020-04-17 [1] CRAN (R 4.1.0)
    #>  R6             2.5.1      2021-08-19 [1] CRAN (R 4.1.1)
    #>  ragg           1.2.0      2021-10-30 [1] CRAN (R 4.1.1)
    #>  Rcpp           1.0.7      2021-07-07 [1] CRAN (R 4.1.0)
    #>  recipes        0.1.17     2021-09-27 [1] CRAN (R 4.1.1)
    #>  reshape2       1.4.4      2020-04-09 [1] CRAN (R 4.1.0)
    #>  rlang          0.4.12     2021-10-18 [1] CRAN (R 4.1.1)
    #>  rpart          4.1-15     2019-04-12 [2] CRAN (R 4.1.2)
    #>  rprojroot      2.0.2      2020-11-15 [1] CRAN (R 4.1.0)
    #>  rstudioapi     0.13       2020-11-12 [1] CRAN (R 4.1.0)
    #>  scales         1.1.1      2020-05-11 [1] CRAN (R 4.1.0)
    #>  sessioninfo    1.2.1      2021-11-02 [1] CRAN (R 4.1.2)
    #>  stringi        1.7.5      2021-10-04 [1] CRAN (R 4.1.1)
    #>  stringr        1.4.0      2019-02-10 [1] CRAN (R 4.1.0)
    #>  survival       3.2-13     2021-08-24 [2] CRAN (R 4.1.2)
    #>  systemfonts    1.0.3      2021-10-13 [1] CRAN (R 4.1.1)
    #>  textshaping    0.3.6      2021-10-13 [1] CRAN (R 4.1.1)
    #>  tibble         3.1.5      2021-09-30 [1] CRAN (R 4.1.1)
    #>  tidyselect     1.1.1      2021-04-30 [1] CRAN (R 4.1.0)
    #>  timeDate       3043.102   2018-02-21 [1] CRAN (R 4.1.1)
    #>  utf8           1.2.2      2021-07-24 [1] CRAN (R 4.1.0)
    #>  vctrs          0.3.8      2021-04-29 [1] CRAN (R 4.1.0)
    #>  withr          2.4.2      2021-04-18 [1] CRAN (R 4.1.0)
    #>  xfun           0.27       2021-10-18 [1] CRAN (R 4.1.1)
    #> 
    #>  [1] C:/Users/trist/Documents/R/win-library/4.1
    #>  [2] C:/Program Files/R/R-4.1.2/library
    #> 
    #> ------------------------------------------------------------------------------
    ```

[^PPV]: Technically, caret uses the [sensitivity, specificity and prevalence](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values) form of the PPV calculation.
