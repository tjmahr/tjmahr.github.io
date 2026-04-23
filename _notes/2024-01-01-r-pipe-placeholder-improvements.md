---
title: "The pipe placeholder works better now"
date: 2024-01-01
tags: [r, language, piping]
---

[Andrew Heiss pointed out to
me](https://staging.bsky.app/profile/andrew.heiss.phd/post/3ki4b46li6l2y)
that the placeholder since 4.3.0 is a lot better. 


``` r
quote(
  model |> _$coef |> _[1]
) 
#> model$coef[1]
```

Previously, I had been circumventing these functions with things like
`getElement()` or making my own pipe-friendly wrapper functions.
