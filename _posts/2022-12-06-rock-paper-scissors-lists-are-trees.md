---
title: How to score Rock Paper Scissors
excerpt: Lists are trees
tags:
  - r
  - advent of code
header:
  overlay_image: "assets/images/2022-12-trees.jpg"
  image_description: "A trio of bottlebrush trees"
  overlay_filter: rgba(10, 50, 50, 0.4)
  caption: "Photo credit: [**Debby Hudson**](https://unsplash.com/photos/tEnxDUbWds0)"
---




Ho ho ho, it is the most wonderful time of the year: Advent of code!

AOC is a yearly collection of programming puzzles throughout the
first 25 days of December. I like it... so much so that I wrote [an R
package](https://github.com/tjmahr/aoc) for completing my puzzles using
the structure of an R package. The puzzles start out easy and get
progressively more elaborate or devious in their requirements. But I am
going to talk about an easy puzzle in this post, and specifically, one
little trick I used in my solution.

[Day 2 of 2022](https://adventofcode.com/2022/day/2) requires us to score games of Rock Paper Scissors. The
moves are encoded using letters, where our opponent's moves are coded as
`A`, `B`, `C` and ours are coded as `X`, `Y`, `Z`. So, an input
describing three moves will look like the following:


```r
example_input <- c(
  "A Y",
  "B X",
  "C Z"
)
```

Where the letters mean the following:


```r
move_codes <- c(
  "A" = "rock",
  "B" = "paper",
  "C" = "scissors",
  "X" = "rock",
  "Y" = "paper",
  "Z" = "scissors"
)
```

This encoding seems like a weird bit of indirection thrown on, and *it is*,
because the puzzle changes the meanings of the letters in Part 2. Still,
it is straightforward to parse the input into a list of roshambo moves.


```r
input <- example_input |> 
  strsplit(" ") |> 
  # Use character subsetting to convert letters to moves
  lapply(function(x) unname(move_codes[x])) 

# Our character's move is the second element in each vector
str(input)
#> List of 3
#>  $ : chr [1:2] "rock" "paper"
#>  $ : chr [1:2] "paper" "rock"
#>  $ : chr [1:2] "scissors" "scissors"
```

Now, for the point of this post, **how do we score each game?**

The naive approach is to start typing away furiously



<img src="/figs/2022-12-06-rock-paper-scissors-lists-are-trees//unnamed-chunk-5.svg" alt="center" width="100%" style="display: block; margin: auto;" />

before eventually noping the hell out of there.

What we have is a decision tree: we need to follow a branch for player
one and another branch for player two. And here's the main point of this
post: **nested lists are trees**. (Yes, I love lists---see [this
post](/lists-knitr-secret-weapon/) where I use them in my knitr
reporting.) The top (outer) level of the list will be all of the player
one options, and then the bottom (inner) level will be all the player
two options. The nodes of the tree (bottom level values) are the
outcomes of the games.


```r
run_game <- function(pair) {
  # nested lists are trees
  rules <- list(
    rock = list(
      rock = "draw",
      scissors = "lose",
      paper = "win"
    ),
    scissors = list(
      scissors = "draw",
      rock = "win",
      paper = "lose"
    ),
    paper = list(
      paper = "draw",
      scissors = "win",
      rock = "lose"
    )
  )

  # Because `rules[[pair[1]]][[pair[2]]]` is unsightly:
  rules |>
    getElement(pair[1]) |>
    getElement(pair[2])
}
```

At this point, we could take a second to ponder how the structure of
several nested if-elses---the actual shape of the code, indenting in and
out in and in again---resembles the structure and the shape of the
nested list, and ponder further about how the regular, orderly shape of
code could be the whispers of hidden data, saying "`list()` me, `list()`
me". Or, we could run the code and see it in action.


```r
input |> 
  lapply(run_game)
#> [[1]]
#> [1] "win"
#> 
#> [[2]]
#> [1] "lose"
#> 
#> [[3]]
#> [1] "draw"

# Or to repeat the input
input |> 
  stats::setNames(input) |> 
  lapply(run_game)
#> $`c("rock", "paper")`
#> [1] "win"
#> 
#> $`c("paper", "rock")`
#> [1] "lose"
#> 
#> $`c("scissors", "scissors")`
#> [1] "draw"
```

***

Earlier in the post, I used [character
subsetting](https://adv-r.hadley.nz/subsetting.html#lookup-tables) to
convert letters into moves. This process turned a matching/replacement
problem into a data lookup problem. The Rock Paper Scissors are the same
trick again: converting a decision tree into a data lookup problem.






***

*Last knitted on 2022-12-06. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2022-12-06-rock-paper-scissors-lists-are-trees.Rmd).*[^si] 

[^si]: 
    
    ```r
    .session_info
    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.2.2 (2022-10-31 ucrt)
    #>  os       Windows 10 x64 (build 22621)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language (EN)
    #>  collate  English_United States.utf8
    #>  ctype    English_United States.utf8
    #>  tz       America/Chicago
    #>  date     2022-12-06
    #>  pandoc   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package     * version date (UTC) lib source
    #>  asciicast     2.3.0   2022-12-05 [1] CRAN (R 4.2.2)
    #>  cli           3.4.1   2022-09-23 [1] CRAN (R 4.2.1)
    #>  curl          4.3.3   2022-10-06 [1] CRAN (R 4.2.1)
    #>  evaluate      0.18    2022-11-07 [1] CRAN (R 4.2.2)
    #>  fansi         1.0.3   2022-03-24 [1] CRAN (R 4.2.0)
    #>  git2r         0.30.1  2022-03-16 [1] CRAN (R 4.2.0)
    #>  glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
    #>  here          1.0.1   2020-12-13 [1] CRAN (R 4.2.0)
    #>  highr         0.9     2021-04-16 [1] CRAN (R 4.2.0)
    #>  jsonlite      1.8.3   2022-10-21 [1] CRAN (R 4.2.1)
    #>  knitr       * 1.40    2022-08-24 [1] CRAN (R 4.2.1)
    #>  lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.2.1)
    #>  magick        2.7.3   2021-08-18 [1] CRAN (R 4.2.0)
    #>  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
    #>  pillar        1.8.1   2022-08-19 [1] CRAN (R 4.2.1)
    #>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
    #>  processx      3.8.0   2022-10-26 [1] CRAN (R 4.2.1)
    #>  ps            1.7.2   2022-10-26 [1] CRAN (R 4.2.1)
    #>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
    #>  ragg          1.2.4   2022-10-24 [1] CRAN (R 4.2.1)
    #>  Rcpp          1.0.9   2022-07-08 [1] CRAN (R 4.2.1)
    #>  rlang         1.0.6   2022-09-24 [1] CRAN (R 4.2.1)
    #>  rprojroot     2.0.3   2022-04-02 [1] CRAN (R 4.2.0)
    #>  rstudioapi    0.14    2022-08-22 [1] CRAN (R 4.2.1)
    #>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
    #>  stringi       1.7.8   2022-07-11 [1] CRAN (R 4.2.1)
    #>  stringr       1.4.1   2022-08-20 [1] CRAN (R 4.2.1)
    #>  systemfonts   1.0.4   2022-02-11 [1] CRAN (R 4.2.0)
    #>  textshaping   0.3.6   2021-10-13 [1] CRAN (R 4.2.0)
    #>  tibble        3.1.8   2022-07-22 [1] CRAN (R 4.2.1)
    #>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.2.0)
    #>  V8            4.2.2   2022-11-03 [1] CRAN (R 4.2.2)
    #>  vctrs         0.5.0   2022-10-22 [1] CRAN (R 4.2.1)
    #>  withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
    #>  xfun          0.34    2022-10-18 [1] CRAN (R 4.2.1)
    #> 
    #>  [1] C:/Users/trist/AppData/Local/R/win-library/4.2
    #>  [2] C:/Program Files/R/R-4.2.2/library
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
    ```
