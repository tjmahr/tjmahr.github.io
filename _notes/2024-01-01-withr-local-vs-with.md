---
title: "local_ and with_ symmetry in withr"
date: 2024-01-01
tags: [r, withr, scoping]
---


I was reading the [withr 3.0.0 release
notes](https://www.tidyverse.org/blog/2024/01/withr-3-0-0/) and noticed
a pleasing symmetry the `with_` and `local_` functions.

The `local_` functions set a temporary value within some local scope,
like a `local()` or a `function()`.


``` r
local({
  withr::local_language("fr")
  plot[[1]]
})
#> Error in `plot[[1]]`:
#> ! objet de type 'closure' non indiçable
plot[[1]]
#> Error in `plot[[1]]`:
#> ! object of type 'closure' is not subsettable
```

The `with_` functions set a temporary value around some block of code.


``` r
withr::with_language("fr", {
  plot[[1]]
})
#> Error in `plot[[1]]`:
#> ! objet de type 'closure' non indiçable
plot[[1]]
#> Error in `plot[[1]]`:
#> ! object of type 'closure' is not subsettable
```

So where are we working from?

- `local_`: within the code (scope)
- `with_`: around the code

Oh, and the notes for withr 3.0.0, indicates that I should keep an eye
on how `source()` interacts with withr.
