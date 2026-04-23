---
title: "A leave-one-out apply helper"
date: 2025-08-01
tags: [r, functional programming, helpers]
---

I had to compare how each listener differed from the mean of the other
listeners, so I cooked up:

```
leave_one_out_apply <- function(xs, f, ...) {
  lapply(seq_along(xs), function(i) f(xs[-i], ...) )
}
```
