---
title: "sample_n_of(): a useful helper function"
excerpt: Randomly sampling subsets of data
tags:
  - r
  - nonstandard evaluation
  - dplyr
  - babynames
share: true
header:
  overlay_image: "assets/images/marisa-morton-1280.jpg"
  image_description: "A wall of donuts"
  overlay_filter: rgba(10, 10, 10, 0.1)
  caption: "Photo credit: [**Marisa Morton**](https://unsplash.com/photos/Rtr7JeG4too/info)"
---



Here's the problem: I have some data with nested time series. Lots of
them. It's like there's many, many little datasets inside my data. There
are too many groups to plot all of the time series at once, so I just
want to preview a handful of them.

For a working example, suppose we want to visualize the top 50 American
female baby names over time. I start by adding up the total number of
births for each name, finding the overall top 50 most populous names,
and then keeping just the time series from those top names.


```r
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

babynames <- babynames::babynames %>% 
  filter(sex == "F")

top50 <- babynames %>% 
  group_by(name) %>% 
  summarise(total = sum(n)) %>% 
  top_n(50, total) 

# keep just rows in babynames that match a row in top50
top_names <- babynames %>%
  semi_join(top50, by = "name")
```

Hmm, so what does this look like?


```r
ggplot(top_names) + 
  aes(x = year, y = n) + 
  geom_line() + 
  facet_wrap("name")
```

<img src="/figs/2018-05-24-sample-n-groups/overplotted-1.png" title="An illegible plot because too many facets are plotted" alt="An illegible plot because too many facets are plotted" width="80%" style="display: block; margin: auto;" />

Aaack, I can't read anything! Can't I just see a few of them?

This is a problem I face frequently, so frequently that I wrote a helper
function to handle this problem: `sample_n_of()`. This is not a very
clever name, but it works. Below I call the function from my personal R
package and plot just the data from four names.


```r
# For reproducible blogging
set.seed(20180524)

top_names %>% 
  tjmisc::sample_n_of(4, name) %>% 
  ggplot() + 
    aes(x = year, y = n) + 
    geom_line() + 
    facet_wrap("name")
```

<img src="/figs/2018-05-24-sample-n-groups/sample-plot-1.png" title="A plot with four faceted timeseries" alt="A plot with four faceted timeseries" width="80%" style="display: block; margin: auto;" />

In this post, I walk through how this function works. It's not very
complicated: It relies on some light tidy evaluation plus one obscure
dplyr function.

## Working through the function

As usual, let's start by sketching out the function we want to write:


```r
sample_n_of <- function(data, size, ...) {
  # quote the dots
  dots <- quos(...)
  
  # ...now make things happen...
}
```

where `size` are the number of groups to sample and `...` are the
columns names that define the groups. We use `quos(...)` to capture and
quote those column names. ([As I wrote
before](/set-na-where-nonstandard-evaluation-use-case/), quotation is
how we bottle up R code so we can deploy it for later.)

For interactive testing, suppose our dataset are the time series from
the top 50 names and we want data from a sample of 5 names. In this
case, the values for the arguments would be:


```r
data <- top_names
size <- 5
dots <- quos(name)
```

A natural way to think about this problem is that we want to sample
subgroups of the dataframe. First, we create a grouped version of the
dataframe using `group_by()`. The function `group_by()` also takes a
`...` argument where the dots are typically names of columns in the
dataframe. We want to take the names inside of our `dots`, unquote them
and plug them in to where the `...` goes in `group_by()`. This is what
the tidy evaluation world calls
[*splicing*](https://dplyr.tidyverse.org/articles/programming.html#unquote-splicing).

Think of splicing as doing this:


```r
# Demo function that counts the number of arguments in the dots
count_args <- function(...) length(quos(...))
example_dots <- quos(var1, var2, var2)

# Splicing turns the first form into the second one
count_args(!!! example_dots)
#> [1] 3
count_args(var1, var2, var2)
#> [1] 3
```

So, we create a grouped dataframe by splicing our dots into the
`group_by()` function.


```r
grouped <- data %>% 
  group_by(!!! dots)
```

There is a helper function buried in dplyr called `group_indices()`
which returns the grouping index for each row in a grouped dataframe.


```r
grouped %>% 
  tibble::add_column(group_index = group_indices(grouped)) 
#> # A tibble: 6,507 x 6
#> # Groups:   name [50]
#>     year sex   name          n    prop group_index
#>    <dbl> <chr> <chr>     <int>   <dbl>       <int>
#>  1  1880 F     Mary       7065 0.0724           33
#>  2  1880 F     Anna       2604 0.0267            5
#>  3  1880 F     Emma       2003 0.0205           19
#>  4  1880 F     Elizabeth  1939 0.0199           17
#>  5  1880 F     Margaret   1578 0.0162           32
#>  6  1880 F     Alice      1414 0.0145            1
#>  7  1880 F     Sarah      1288 0.0132           45
#>  8  1880 F     Laura      1012 0.0104           29
#>  9  1880 F     Catherine   688 0.00705          11
#> 10  1880 F     Helen       636 0.00652          22
#> # ... with 6,497 more rows
```

We can randomly sample five of the group indices and keep the rows for
just those groups.


```r
unique_groups <- unique(group_indices(grouped))
sampled_groups <- sample(unique_groups, size)
sampled_groups
#> [1] 34 25 30 19 32

subset_of_the_data <- data %>% 
  filter(group_indices(grouped) %in% sampled_groups)
subset_of_the_data
#> # A tibble: 684 x 5
#>     year sex   name         n      prop
#>    <dbl> <chr> <chr>    <int>     <dbl>
#>  1  1880 F     Emma      2003 0.0205   
#>  2  1880 F     Margaret  1578 0.0162   
#>  3  1880 F     Melissa     33 0.000338 
#>  4  1880 F     Linda       27 0.000277 
#>  5  1881 F     Emma      2034 0.0206   
#>  6  1881 F     Margaret  1658 0.0168   
#>  7  1881 F     Melissa     40 0.000405 
#>  8  1881 F     Linda       38 0.000384 
#>  9  1881 F     Karen        6 0.0000607
#> 10  1882 F     Emma      2303 0.0199   
#> # ... with 674 more rows

# Confirm that only five names are in the dataset
subset_of_the_data %>% 
  distinct(name)
#> # A tibble: 5 x 1
#>   name    
#>   <chr>   
#> 1 Emma    
#> 2 Margaret
#> 3 Melissa 
#> 4 Linda   
#> 5 Karen
```

Putting these steps together, we get:


```r
sample_n_of <- function(data, size, ...) {
  dots <- quos(...)
  
  group_ids <- data %>% 
    group_by(!!! dots) %>% 
    group_indices()
  
  sampled_groups <- sample(unique(group_ids), size)
  
  data %>% 
    filter(group_ids %in% sampled_groups)
}
```

We can test that the function works as we might expect. Sampling 10
names returns the data for 10 names.


```r
ten_names <- top_names %>% 
  sample_n_of(10, name) %>% 
  print()
#> # A tibble: 1,271 x 5
#>     year sex   name         n     prop
#>    <dbl> <chr> <chr>    <int>    <dbl>
#>  1  1880 F     Margaret  1578 0.0162  
#>  2  1880 F     Alice     1414 0.0145  
#>  3  1880 F     Rebecca    236 0.00242 
#>  4  1880 F     Ruth       234 0.00240 
#>  5  1880 F     Betty      117 0.00120 
#>  6  1880 F     Samantha    21 0.000215
#>  7  1881 F     Margaret  1658 0.0168  
#>  8  1881 F     Alice     1308 0.0132  
#>  9  1881 F     Ruth       275 0.00278 
#> 10  1881 F     Rebecca    226 0.00229 
#> # ... with 1,261 more rows

ten_names %>% 
  distinct(name)
#> # A tibble: 10 x 1
#>    name    
#>    <chr>   
#>  1 Margaret
#>  2 Alice   
#>  3 Rebecca 
#>  4 Ruth    
#>  5 Betty   
#>  6 Samantha
#>  7 Patricia
#>  8 Lisa    
#>  9 Brenda  
#> 10 Nicole
```

We can sample based on multiple columns too. Ten combinations of names
and years should return just ten rows.


```r
top_names %>% 
  sample_n_of(10, name, year) 
#> # A tibble: 10 x 5
#>     year sex   name          n      prop
#>    <dbl> <chr> <chr>     <int>     <dbl>
#>  1  1882 F     Evelyn      125 0.00108  
#>  2  1887 F     Sarah      1436 0.00924  
#>  3  1890 F     Elizabeth  3112 0.0154   
#>  4  1899 F     Amanda      326 0.00132  
#>  5  1911 F     Shirley     362 0.000819 
#>  6  1939 F     Margaret  14952 0.0132   
#>  7  1946 F     Samantha     33 0.0000205
#>  8  1956 F     Mary      61750 0.0300   
#>  9  1961 F     Lisa      42702 0.0206   
#> 10  2000 F     Helen       890 0.000446
```

## Next steps

There are a few tweaks we could make to this function. For example, in
my package's version, I warn the user when the number of groups is too
large.


```r
too_many <- top_names %>% 
  tjmisc::sample_n_of(100, name)
#> Warning: Sample size (100) is larger than number of groups (50). Using size =
#> 50.
```

My version also randomly samples *n* of the rows when there are no
grouping variables provided.


```r
top_names %>% 
  tjmisc::sample_n_of(2)
#> # A tibble: 2 x 5
#>    year sex   name        n     prop
#>   <dbl> <chr> <chr>   <int>    <dbl>
#> 1  1882 F     Donna      19 0.000164
#> 2  1930 F     Shirley 14776 0.0127
```

One open question is how to handle data that's already grouped. The
function we wrote above fails. 


```r
top_names %>% 
  group_by(name) %>% 
  sample_n_of(2, year)
#> Error: Problem with `filter()` input `..1`.
#> x Input `..1` must be of size 138 or 1, not size 6507.
#> i Input `..1` is `group_ids %in% sampled_groups`.
#> i The error occurred in group 1: name = "Alice".
```

Is this a problem?

Here I think failure is okay because what do we think should happen?
It's not obvious. It should randomly choose 2 of the years for each
name. Should it be the same two years? Then this should be fine.


```r
top_names %>% 
  sample_n_of(2, year)
#> # A tibble: 90 x 5
#>     year sex   name          n    prop
#>    <dbl> <chr> <chr>     <int>   <dbl>
#>  1  1895 F     Mary      13446 0.0544 
#>  2  1895 F     Anna       5950 0.0241 
#>  3  1895 F     Helen      4023 0.0163 
#>  4  1895 F     Margaret   3931 0.0159 
#>  5  1895 F     Elizabeth  3603 0.0146 
#>  6  1895 F     Ruth       3551 0.0144 
#>  7  1895 F     Emma       2952 0.0119 
#>  8  1895 F     Alice      2457 0.00994
#>  9  1895 F     Frances    1834 0.00742
#> 10  1895 F     Sarah      1777 0.00719
#> # ... with 80 more rows
```

Or, should those two years be randomly selected for each name? Then, we
should let `do()` handle that. `do()` takes some code that returns a
dataframe, applies it to each group, and returns the combined result.


```r
top_names %>% 
  group_by(name) %>% 
  do(sample_n_of(., 2, year))
#> # A tibble: 100 x 5
#> # Groups:   name [50]
#>     year sex   name       n     prop
#>    <dbl> <chr> <chr>  <int>    <dbl>
#>  1  1889 F     Alice   2145 0.0113  
#>  2  1983 F     Alice    699 0.000391
#>  3  1902 F     Amanda   301 0.00107 
#>  4  1997 F     Amanda 12242 0.00641 
#>  5  1913 F     Amy      387 0.000591
#>  6  2013 F     Amy     2233 0.00116 
#>  7  1933 F     Angela   577 0.000552
#>  8  1936 F     Angela   595 0.000552
#>  9  1881 F     Anna    2698 0.0273  
#> 10  1965 F     Anna    3921 0.00215 
#> # ... with 90 more rows
```

I think raising an error and forcing the user to clarify their code is a
better than choosing one of these options and not doing what the user
expects.



***

*Last knitted on 2021-02-22. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2018-05-24-sample-n-groups.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info ---------------------------------------------------------------
    #>  setting  value                       
    #>  version  R version 4.0.4 (2021-02-15)
    #>  os       Windows 10 x64              
    #>  system   x86_64, mingw32             
    #>  ui       RTerm                       
    #>  language (EN)                        
    #>  collate  English_United States.1252  
    #>  ctype    English_United States.1252  
    #>  tz       America/Chicago             
    #>  date     2021-02-22                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package     * version    date       lib source        
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.2)
    #>  babynames     1.0.0      2019-01-12 [1] CRAN (R 4.0.2)
    #>  cli           2.3.0      2021-01-31 [1] CRAN (R 4.0.3)
    #>  colorspace    2.0-0      2020-11-11 [1] CRAN (R 4.0.3)
    #>  crayon        1.4.1      2021-02-08 [1] CRAN (R 4.0.3)
    #>  DBI           1.1.1      2021-01-15 [1] CRAN (R 4.0.3)
    #>  digest        0.6.27     2020-10-24 [1] CRAN (R 4.0.3)
    #>  dplyr       * 1.0.4      2021-02-02 [1] CRAN (R 4.0.3)
    #>  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.2)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.2)
    #>  fansi         0.4.2      2021-01-15 [1] CRAN (R 4.0.3)
    #>  farver        2.0.3      2020-01-16 [1] CRAN (R 4.0.2)
    #>  generics      0.1.0      2020-10-31 [1] CRAN (R 4.0.3)
    #>  ggplot2     * 3.3.3      2020-12-30 [1] CRAN (R 4.0.3)
    #>  git2r         0.28.0     2021-01-10 [1] CRAN (R 4.0.3)
    #>  glue          1.4.2      2020-08-27 [1] CRAN (R 4.0.2)
    #>  gtable        0.3.0      2019-03-25 [1] CRAN (R 4.0.2)
    #>  here          1.0.1      2020-12-13 [1] CRAN (R 4.0.3)
    #>  highr         0.8        2019-03-20 [1] CRAN (R 4.0.2)
    #>  knitr       * 1.31       2021-01-27 [1] CRAN (R 4.0.3)
    #>  labeling      0.4.2      2020-10-20 [1] CRAN (R 4.0.2)
    #>  lifecycle     1.0.0      2021-02-15 [1] CRAN (R 4.0.3)
    #>  magrittr      2.0.1      2020-11-17 [1] CRAN (R 4.0.3)
    #>  munsell       0.5.0      2018-06-12 [1] CRAN (R 4.0.2)
    #>  pillar        1.4.7      2020-11-20 [1] CRAN (R 4.0.3)
    #>  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.0.2)
    #>  purrr         0.3.4      2020-04-17 [1] CRAN (R 4.0.2)
    #>  R6            2.5.0      2020-10-28 [1] CRAN (R 4.0.2)
    #>  ragg          1.1.0      2021-02-15 [1] CRAN (R 4.0.4)
    #>  rlang         0.4.10     2020-12-30 [1] CRAN (R 4.0.3)
    #>  rprojroot     2.0.2      2020-11-15 [1] CRAN (R 4.0.3)
    #>  scales        1.1.1      2020-05-11 [1] CRAN (R 4.0.2)
    #>  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.2)
    #>  stringi       1.5.3      2020-09-09 [1] CRAN (R 4.0.2)
    #>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.2)
    #>  systemfonts   1.0.1      2021-02-09 [1] CRAN (R 4.0.3)
    #>  textshaping   0.3.0      2021-02-10 [1] CRAN (R 4.0.3)
    #>  tibble        3.0.6      2021-01-29 [1] CRAN (R 4.0.3)
    #>  tidyselect    1.1.0      2020-05-11 [1] CRAN (R 4.0.2)
    #>  tjmisc        0.0.0.9000 2021-02-19 [1] local         
    #>  utf8          1.1.4      2018-05-24 [1] CRAN (R 4.0.2)
    #>  vctrs         0.3.6      2020-12-17 [1] CRAN (R 4.0.3)
    #>  withr         2.4.1      2021-01-26 [1] CRAN (R 4.0.3)
    #>  xfun          0.21       2021-02-10 [1] CRAN (R 4.0.3)
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.4/library
    ```

