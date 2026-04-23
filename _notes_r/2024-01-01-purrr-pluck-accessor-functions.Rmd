---
title: "purrr::pluck() accepts accessor functions"
date: 2024-01-01
tags: [r, purrr]
---

`purrr::pluck()` is better than I had realized. `?purrr::pluck()`:

> [`pluck()`] also accepts arbitrary accessor functions, i.e.
> functions that take an object and return some internal piece. [...]
> Compare: `accessor(x[[1]])$foo` to `pluck(x, 1, accessor, "foo")`.

