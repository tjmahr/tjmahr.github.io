---
title: Recode values with character subsetting
excerpt: Wait, what's wrong with seven ifelse statements?
tags:
  - r
  - stack exchange
---




Do you ever have to recode many values at once? It's a frequent chore when 
preparing data. For example, suppose we had to replace state abbreviations 
with the full names:


```r
abbs <- c("AL", "AK", "AZ", "AZ", "WI", "WS")
```

You could write several `ifelse()` statements.


```r
ifelse(abbs == "AL", "Alabama", 
       ifelse(abbs == "AK", "Alaska", 
              ifelse(abbs == "AZ", "Arizona", 
```

Actually, never mind! That gets out of hand very quickly.

`case_when()` is nice, especially when the replacement rules are more complex
than 1-to-1 matching.


```r
dplyr::case_when(
  # Syntax: logical test ~ value to use when test is TRUE
  abbs == "AL" ~ "Alabama",
  abbs == "AK" ~ "Alaska",
  abbs == "AZ" ~ "Arizona",
  abbs == "WI" ~ "Wisconsin",
  # a fallback/default value
  TRUE ~ "No match"
)
#> [1] "Alabama"   "Alaska"    "Arizona"   "Arizona"   "Wisconsin" "No match"
```

We could also use one of my very favorite R tricks: 
[**Character subsetting**](http://adv-r.had.co.nz/Subsetting.html#applications). 
We create a named vector where the names are the data we have and the values are
the data we want. I use the mnemonic `old_value = new_value`. In this case, we
make a lookup table like so:


```r
lookup <- c(
  # Syntax: name = value
  "AL" = "Alabama",
  "AK" = "Alaska",
  "AZ" = "Arizona",
  "WI" = "Wisconsin"
)
```

For example, subsetting with the string `"AL"` will retrieve the value with the
name `"AL"`.


```r
lookup["AL"]
#>        AL 
#> "Alabama"
```

With a vector of names, we can look up the values all at once.


```r
lookup[abbs]
#>          AL          AK          AZ          AZ          WI        <NA> 
#>   "Alabama"    "Alaska"   "Arizona"   "Arizona" "Wisconsin"          NA
```

If the names and the replacement values are stored in vectors, we can construct 
the lookup table programmatically using `setNames()`. In our case, the `datasets` 
package provides vectors with state names and state abbreviations.


```r
full_lookup <- setNames(datasets::state.name, datasets::state.abb)
head(full_lookup)
#>           AL           AK           AZ           AR           CA           CO 
#>    "Alabama"     "Alaska"    "Arizona"   "Arkansas" "California"   "Colorado"

full_lookup[abbs]
#>          AL          AK          AZ          AZ          WI        <NA> 
#>   "Alabama"    "Alaska"   "Arizona"   "Arizona" "Wisconsin"          NA
```

One complication is that the character subsetting yields `NA` when the 
lookup table doesn't have a matching name. That's what's happening above with 
the illegal abbreviation `"WS"`. We can fix this by replacing the `NA` 
values with some default value.


```r
matches <- full_lookup[abbs]
matches[is.na(matches)] <- "No match"
matches
#>          AL          AK          AZ          AZ          WI        <NA> 
#>   "Alabama"    "Alaska"   "Arizona"   "Arizona" "Wisconsin"  "No match"
```

Finally, to clean away any traces of the matching process, we can `unname()` the
results.


```r
unname(matches)
#> [1] "Alabama"   "Alaska"    "Arizona"   "Arizona"   "Wisconsin" "No match"
```

### Many-to-one lookup tables

By the way, the lookup tables can be many-to-one. That is, different names can
retrieve the same value. For example, we can handle this example that has
synonymous names and differences in capitalization with many-to-one matching.


```r
lookup <- c(
  "python" = "Python", 
  "r" = "R", 
  "node" = "Javascript", 
  "js" = "Javascript", 
  "javascript" = "Javascript"
)

languages <- c("JS", "js", "Node", "R", "Python", "r", "JAvascript")

# Use tolower() to normalize the language names so 
# e.g., "R" and "r" can both match R
lookup[tolower(languages)]
#>           js           js         node            r       python            r 
#> "Javascript" "Javascript" "Javascript"          "R"     "Python"          "R" 
#>   javascript 
#> "Javascript"
```


## Character by character string replacement

I'm motivated to write about character subsetting today because I used it in a
[Stack Overflow answer](https://stackoverflow.com/a/48170630/1084259). 
Here is my paraphrasing of the problem.

> Let's say I have a long character string, and I'd like to use
> `stringr::str_replace_all` to replace certain letters with others.
> According to the documentation, `str_replace_all` can take a named
> vector and replaces the name with the value. That works fine for 1
> replacement, but for multiple, it seems to do the replacements
> iteratively, so that one replacement can replace another one.
> 
>     library(tidyverse)
>     text_string = "developer"
>     
>     # This works fine
>     text_string %>% 
>       str_replace_all(c(e ="X")) 
>     #> [1] "dXvXlopXr"
>     
>     # But this is not what I want
>     text_string %>% 
>       str_replace_all(c(e ="p", p = "e"))
>     #> [1] "develoeer"
>     
>     # Desired result would be "dpvploepr"

The iterative behavior here is that 
`str_replace_all("developer", c(e ="p", p = "e"))` first replaces `e` with `p` 
(yielding `"dpvploppr"`) and then it applies the second rule on the output of 
the first rule, replacing `p` with `e` (yielding `"develoeer"`). 

When I read this question, the replacement rules looked a lot like the lookup 
tables that I use in character subsetting so I presented a function that 
handles this problem by using character subsetting. 

Let's work through the question's example. First, let's break the string into
characters.


```r
input <- "developer"
rules <- c(e = "p", p = "e")

chars <- unlist(strsplit(input, ""))
chars
#> [1] "d" "e" "v" "e" "l" "o" "p" "e" "r"
```

To avoid the issue of `NAs`, we create default rules so that every character in
the input is replaced by itself.


```r
unique_chars <- unique(chars)
complete_rules <- setNames(unique_chars, unique_chars)
complete_rules
#>   d   e   v   l   o   p   r 
#> "d" "e" "v" "l" "o" "p" "r"
```

Now, we overwrite the default rules with the specific ones we are interested in.


```r
# Find rules with the names as the real rules. 
# Replace them with the real rules.
complete_rules[names(rules)] <- rules
complete_rules
#>   d   e   v   l   o   p   r 
#> "d" "p" "v" "l" "o" "e" "r"
```

Then lookup with character subsetting will effectively apply all the replacement
rules. We glue the characters back together again to finish the transformation


```r
replaced <- unname(complete_rules[chars])
paste0(replaced, collapse = "")
#> [1] "dpvploepr"
```

Here is everything combined into a single function, with some additional steps
needed to handle multiple strings at once.


```r
str_replace_chars <- function(string, rules) {
  # Expand rules to replace characters with themselves 
  # if those characters do not have a replacement rule
  chars <- unique(unlist(strsplit(string, "")))
  complete_rules <- setNames(chars, chars)
  complete_rules[names(rules)] <- rules

  # Split each string into characters, replace and unsplit
  for (string_i in seq_along(string)) {
    chars_i <- unlist(strsplit(string[string_i], ""))
    string[string_i] <- paste0(complete_rules[chars_i], collapse = "")
  }
  string
}

rules <- c(a = "X", p = "e", e = "p")
strings <- c("application", "developer")

str_replace_chars(strings, rules)
#> [1] "XeelicXtion" "dpvploepr"
```



***

*Last knitted on 2021-02-08. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2018-01-09-recode-values-with-character-subsetting.Rmd).*[^si] 

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
    #>  date     2021-02-08                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package     * version date       lib source        
    #>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
    #>  cli           2.2.0   2020-11-20 [1] CRAN (R 4.0.3)
    #>  crayon        1.4.0   2021-01-30 [1] CRAN (R 4.0.3)
    #>  DBI           1.1.1   2021-01-15 [1] CRAN (R 4.0.3)
    #>  dplyr         1.0.3   2021-01-15 [1] CRAN (R 4.0.3)
    #>  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)
    #>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.2)
    #>  fansi         0.4.2   2021-01-15 [1] CRAN (R 4.0.3)
    #>  generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.3)
    #>  git2r         0.28.0  2021-01-10 [1] CRAN (R 4.0.3)
    #>  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    #>  here          1.0.1   2020-12-13 [1] CRAN (R 4.0.3)
    #>  knitr       * 1.31    2021-01-27 [1] CRAN (R 4.0.3)
    #>  lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.2)
    #>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
    #>  pillar        1.4.7   2020-11-20 [1] CRAN (R 4.0.3)
    #>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
    #>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
    #>  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.2)
    #>  rlang         0.4.10  2020-12-30 [1] CRAN (R 4.0.3)
    #>  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.3)
    #>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
    #>  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)
    #>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    #>  tibble        3.0.6   2021-01-29 [1] CRAN (R 4.0.3)
    #>  tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.2)
    #>  vctrs         0.3.6   2020-12-17 [1] CRAN (R 4.0.3)
    #>  withr         2.4.1   2021-01-26 [1] CRAN (R 4.0.3)
    #>  xfun          0.20    2021-01-06 [1] CRAN (R 4.0.3)
    #> 
    #> [1] C:/Users/Tristan/Documents/R/win-library/4.0
    #> [2] C:/Program Files/R/R-4.0.3/library
    ```
