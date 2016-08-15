---
title: "Confusion matrix statistics on late talker diagnoses"
excerpt:
share: false
tags:
  - caret
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
library("dplyr")

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
wnl_data <- data_frame(
  Group = groups[1],
  Predicted = outcomes[1],
  Outcome = rep(outcomes, times = c(wnl_still_wnl, wnl_delayed))
)

lt_data <- data_frame(
  Group = "LT at 18m",
  Outcome = rep(outcomes, times = c(lt_bloomed, lt_still_delayed)),
  Predicted = outcomes[2]
)

all_kids <- bind_rows(wnl_data, lt_data) %>%
  mutate(ChildID = seq_along(Outcome)) %>% 
  select(ChildID, Group, Predicted, Outcome)
```

What we have looks like a real data-set now.




```r
sample_n(all_kids, 8, replace = FALSE) %>% 
  arrange(Group, Predicted, Outcome)
#> # A tibble: 8 x 4
#>   ChildID      Group      Predicted        Outcome
#>     <int>      <chr>          <chr>          <chr>
#> 1      77  LT at 18m Delayed at 30m Delayed at 30m
#> 2      73  LT at 18m Delayed at 30m Delayed at 30m
#> 3      79  LT at 18m Delayed at 30m Delayed at 30m
#> 4      49  LT at 18m Delayed at 30m     WNL at 30m
#> 5      65  LT at 18m Delayed at 30m     WNL at 30m
#> 6      43 WNL at 18m     WNL at 30m Delayed at 30m
#> 7      33 WNL at 18m     WNL at 30m     WNL at 30m
#> 8      13 WNL at 18m     WNL at 30m     WNL at 30m
```

Next, we just call `confusionMatrix` on the predicted values and the reference
values.


```r
conf_mat <- caret::confusionMatrix(all_kids$Predicted, all_kids$Outcome)
conf_mat
#> Confusion Matrix and Statistics
#> 
#>                 Reference
#> Prediction       Delayed at 30m WNL at 30m
#>   Delayed at 30m             14         22
#>   WNL at 30m                  4         42
#>                                           
#>                Accuracy : 0.6829          
#>                  95% CI : (0.5708, 0.7813)
#>     No Information Rate : 0.7805          
#>     P-Value [Acc > NIR] : 0.9855735       
#>                                           
#>                   Kappa : 0.3193          
#>  Mcnemar's Test P-Value : 0.0008561       
#>                                           
#>             Sensitivity : 0.7778          
#>             Specificity : 0.6562          
#>          Pos Pred Value : 0.3889          
#>          Neg Pred Value : 0.9130          
#>              Prevalence : 0.2195          
#>          Detection Rate : 0.1707          
#>    Detection Prevalence : 0.4390          
#>       Balanced Accuracy : 0.7170          
#>                                           
#>        'Positive' Class : Delayed at 30m  
#> 
```



Here, we can confirm the positive predictive value (true positives / positive
calls)[^PPV] is 14/36 = 0.3889. The negative predictive value is noteworthy;
most children not diagnosed as late talkers did not show a delay one year later
(NPV = 42/46 = 0.913).

[^PPV]: Technically, caret uses the [sensitivity, specificity and prevalance](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values) form of the PPV calculation.
