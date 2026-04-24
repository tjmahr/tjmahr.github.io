---
title: "Base graphics in knitr and global.par"
date: 2021-11-01
tags: [r, knitr, graphics]
---

I learned while porting over some old notes
that `par()` options are not preserved between knitr chunks unless
`opts_knit$get("global.par")` is `TRUE`. It's `FALSE` by default. No
funny business using `old <- par(...)`
