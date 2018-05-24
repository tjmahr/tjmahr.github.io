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



Here's the problem: I have some data with nested time series. Lots of them. It's
like there's many, many little datasets inside my data. There are too many
groups to plot all of the time series at once, so I just want to preview a
handful of them.

For a working example, suppose we want to visualize the top 50 American female
baby names over time. I start by adding up the total number of births for each
name, finding the overall top 50 most populous names, and then keeping just the
time series from those top names.


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

<img src="/figs//2018-05-24-sample-n-groups/overplotted-1.png" title="An illegible plot because too many facets are plotted" alt="An illegible plot because too many facets are plotted" width="80%" style="display: block; margin: auto;" />

Aaack, I can't read anything! Can't I just see a few of them?

This is a problem I face frequently, so frequently that I wrote a helper
function to handle this problem: `sample_n_of()`. This is not a very clever 
name, but it works. Below I call the function from my personal R package
and plot just the data from four names.


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

<img src="/figs//2018-05-24-sample-n-groups/sample-plot-1.png" title="A plot with four faceted timeseries" alt="A plot with four faceted timeseries" width="80%" style="display: block; margin: auto;" />

In this post, I walk through how this function works. It's not very
complicated: It relies on some light tidy evaluation plus one obscure dplyr
function.

## Working through the function

As usual, let's start by sketching out the function we want to write: 


```r
sample_n_of <- function(data, size, ...) {
  # quote the dots
  dots <- quos(...)
  
  # ...now make things happen...
}
```

where `size` are the number of groups to sample and `...` are the columns names
that define the groups. We use `quos(...)` to capture and quote those column
names. ([As I wrote before](/set-na-where-nonstandard-evaluation-use-case/),
quotation is how we bottle up R code so we can deploy it for later.)

For interactive testing, suppose our dataset are the time series from the top 50
names and we want data from a sample of 5 names. In this case, the values for
the arguments would be:


```r
data <- top_names
size <- 5
dots <- quos(name)
```

A natural way to think about this problem is that we want to sample subgroups of
the dataframe. First, we create a grouped version of the dataframe using
`group_by()`. The function `group_by()` also takes a `...` argument where the
dots are typically names of columns in the dataframe. We want to take the
names inside of our `dots`, unquote them and plug them in to where the `...`
goes in `group_by()`. This is what the tidy evaluation world calls 
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

So, we create a grouped dataframe by splicing our dots into the `group_by()`
function.


```r
grouped <- data %>% 
  group_by(!!! dots)
```

There is a helper function buried in dplyr called `group_indices()` which
returns the grouping index for each row in a grouped dataframe.


```r
grouped %>% 
  tibble::add_column(group_index = group_indices(grouped)) 
#> # A tibble: 6,407 x 6
#> # Groups:   name [50]
#>     year sex   name          n    prop group_index
#>    <dbl> <chr> <chr>     <int>   <dbl>       <int>
#>  1  1880 F     Mary       7065 0.0724           33
#>  2  1880 F     Anna       2604 0.0267            4
#>  3  1880 F     Emma       2003 0.0205           19
#>  4  1880 F     Elizabeth  1939 0.0199           17
#>  5  1880 F     Margaret   1578 0.0162           32
#>  6  1880 F     Sarah      1288 0.0132           45
#>  7  1880 F     Laura      1012 0.0104           29
#>  8  1880 F     Catherine   688 0.00705          11
#>  9  1880 F     Helen       636 0.00652          21
#> 10  1880 F     Frances     605 0.00620          20
#> # ... with 6,397 more rows
```

We can randomly sample five of the group indices and keep the rows for just
those groups.


```r
unique_groups <- unique(group_indices(grouped))
sampled_groups <- sample(unique_groups, size)
sampled_groups
#> [1]  4 25 43 20 21

subset_of_the_data <- data %>% 
  filter(group_indices(grouped) %in% sampled_groups)
subset_of_the_data
#> # A tibble: 674 x 5
#>     year sex   name         n      prop
#>    <dbl> <chr> <chr>    <int>     <dbl>
#>  1  1880 F     Anna      2604 0.0267   
#>  2  1880 F     Helen      636 0.00652  
#>  3  1880 F     Frances    605 0.00620  
#>  4  1880 F     Samantha    21 0.000215 
#>  5  1881 F     Anna      2698 0.0273   
#>  6  1881 F     Helen      612 0.00619  
#>  7  1881 F     Frances    586 0.00593  
#>  8  1881 F     Samantha    12 0.000121 
#>  9  1881 F     Karen        6 0.0000607
#> 10  1882 F     Anna      3143 0.0272   
#> # ... with 664 more rows

# Confirm that only five names are in the dataset
subset_of_the_data %>% 
  distinct(name)
#> # A tibble: 5 x 1
#>   name    
#>   <chr>   
#> 1 Anna    
#> 2 Helen   
#> 3 Frances 
#> 4 Samantha
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

We can test that the function works as we might expect. Sampling 10 names
returns the data for 10 names.


```r
ten_names <- top_names %>% 
  sample_n_of(10, name) %>% 
  print()
#> # A tibble: 1,326 x 5
#>     year sex   name         n      prop
#>    <dbl> <chr> <chr>    <int>     <dbl>
#>  1  1880 F     Sarah     1288 0.0132   
#>  2  1880 F     Frances    605 0.00620  
#>  3  1880 F     Rachel     166 0.00170  
#>  4  1880 F     Samantha    21 0.000215 
#>  5  1880 F     Deborah     12 0.000123 
#>  6  1880 F     Shirley      8 0.0000820
#>  7  1880 F     Carol        7 0.0000717
#>  8  1880 F     Jessica      7 0.0000717
#>  9  1881 F     Sarah     1226 0.0124   
#> 10  1881 F     Frances    586 0.00593  
#> # ... with 1,316 more rows

ten_names %>% 
  distinct(name)
#> # A tibble: 10 x 1
#>    name    
#>    <chr>   
#>  1 Sarah   
#>  2 Frances 
#>  3 Rachel  
#>  4 Samantha
#>  5 Deborah 
#>  6 Shirley 
#>  7 Carol   
#>  8 Jessica 
#>  9 Patricia
#> 10 Sharon
```

We can sample based on multiple columns too. Ten combinations of names and years
should return just ten rows.


```r
top_names %>% 
  sample_n_of(10, name, year) 
#> # A tibble: 10 x 5
#>     year sex   name          n      prop
#>    <dbl> <chr> <chr>     <int>     <dbl>
#>  1  1907 F     Jessica      17 0.0000504
#>  2  1932 F     Catherine  5446 0.00492  
#>  3  1951 F     Nicole       94 0.0000509
#>  4  1953 F     Janet     17761 0.00921  
#>  5  1970 F     Sharon     9174 0.00501  
#>  6  1983 F     Melissa   23473 0.0131   
#>  7  1989 F     Brenda     2270 0.00114  
#>  8  1989 F     Pamela     1334 0.000670 
#>  9  1994 F     Samantha  22817 0.0117   
#> 10  2014 F     Kimberly   2891 0.00148
```

## Next steps

There are a few tweaks we could make to this function. For example, in my
package's version, I warn the user when the number of groups is too large.


```r
too_many <- top_names %>% 
  tjmisc::sample_n_of(100, name)
#> Warning: Sample size (100) is larger than number of groups (50). Using size
#> = 50.
```

My version also randomly samples *n* of the rows when there are no grouping
variables provided.


```r
top_names %>% 
  tjmisc::sample_n_of(2)
#> # A tibble: 2 x 5
#>    year sex   name          n     prop
#>   <dbl> <chr> <chr>     <int>    <dbl>
#> 1  1934 F     Stephanie   128 0.000118
#> 2  2007 F     Mary       3674 0.00174
```

One open question is how to handle data that's already grouped. The function we
wrote above fails. 


```r
top_names %>% 
  group_by(name) %>% 
  sample_n_of(2, year)
#> Error in filter_impl(.data, quo): Result must have length 136, not 6407
```

Is this a problem? 

Here I think failure is okay because what do we think should happen? It's not
obvious. It should randomly choose 2 of the years for each name.
Should it be the same two years? Then this should be fine.


```r
top_names %>% 
  sample_n_of(2, year)
#> # A tibble: 100 x 5
#>     year sex   name         n    prop
#>    <dbl> <chr> <chr>    <int>   <dbl>
#>  1  1970 F     Jennifer 46160 0.0252 
#>  2  1970 F     Lisa     38965 0.0213 
#>  3  1970 F     Kimberly 34141 0.0186 
#>  4  1970 F     Michelle 34053 0.0186 
#>  5  1970 F     Amy      25212 0.0138 
#>  6  1970 F     Angela   24926 0.0136 
#>  7  1970 F     Melissa  23742 0.0130 
#>  8  1970 F     Mary     19204 0.0105 
#>  9  1970 F     Karen    16701 0.00912
#> 10  1970 F     Laura    16497 0.00901
#> # ... with 90 more rows
```

Or, should those two years be randomly selected for each name? Then, we should
let `do()` handle that. `do()` takes some code that returns a dataframe, applies
it to each group, and returns the combined result.


```r
top_names %>% 
  group_by(name) %>% 
  do(sample_n_of(., 2, year))
#> # A tibble: 100 x 5
#> # Groups:   name [50]
#>     year sex   name       n      prop
#>    <dbl> <chr> <chr>  <int>     <dbl>
#>  1  1913 F     Amanda   346 0.000528 
#>  2  1953 F     Amanda   428 0.000222 
#>  3  1899 F     Amy      281 0.00114  
#>  4  1964 F     Amy     9579 0.00489  
#>  5  1916 F     Angela   715 0.000659 
#>  6  2005 F     Angela  2893 0.00143  
#>  7  1999 F     Anna    9092 0.00467  
#>  8  2011 F     Anna    5649 0.00292  
#>  9  1952 F     Ashley    24 0.0000126
#> 10  2006 F     Ashley 12340 0.00591  
#> # ... with 90 more rows
```

I think raising an error and forcing the user to clarify their code is a better
than choosing one of these options and not doing what the user expects.
