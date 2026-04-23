---
title: "'A random variable is not random and not a variable'"
date: 2025-01-01
tags: [r, rng, programming]
---


["A random variable is not random and not a
variable"](https://youtu.be/KQHfOZHNZ3k). Yep, it's a random number
generator or a function. Lol, just for fun, let's make a random variable
into an actual random variable (a code variable that emits a random
value every time it is evaluated):


``` r
makeActiveBinding("die_value", function(x) sample(1:6, 1), .GlobalEnv)
die_value
#> [1] 4
c(die_value, die_value, die_value, die_value)
#> [1] 5 1 3 6


mean(replicate(1000, die_value))
#> [1] 3.406
mean(1:6)
#> [1] 3.5
```

