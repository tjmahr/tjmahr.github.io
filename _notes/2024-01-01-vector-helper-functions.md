---
title: "Helper functions for vector indexing and manipulation"
date: 2024-01-01
tags: [r, utilities]
---

Here are some pipe-friendly functions I made during an Advent of Code run. When I do AOC, I limit myself to base R, stringr, and rlang.



``` r
vec_element <- function(xs, i) {
  xs[[i]]
}

vec_index <- function(xs, i) {
  xs[i]
}

vec_index_into <- function(i, xs) {
  xs[i]
}

vec_replace_na <- function(xs, replacement) {
  xs[is.na(xs)] <- replacement
  xs
}

vec_remove_na <- function(xs) {
  xs[!is.na(xs)]
}

vec_set_names <- function(xs, ns) {
  names(xs) <- ns
  xs
}
```

There were two other functions alongside these helpers that weren't
meant to solve any specific piping problem. I came to adore them when
I was doing some Advent of Code puzzles for fun in base R.


``` r
vec_which_value <- function(xs, value, negate = FALSE) {
  (xs %in% value) |>
    xor(negate) |>
    which()
}

vec_which_name <- function(xs, name, negate = FALSE) {
  vec_which_value(names(xs), name, negate)
}

# Remove the vowels, in a pipeline
letters |> 
  vec_which_value(c("a", "e", "i", "o", "u"), negate = TRUE) |> 
  vec_index_into(letters)
#>  [1] "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x"
#> [20] "y" "z"
```

