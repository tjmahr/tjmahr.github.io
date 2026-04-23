---
title: "Bits and pieces"
date: 2025-01-01
tags: [r, packages, miscellany]
---

This entry collects some quick links and tips.

🔗 a [good
distribution](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14387)
for count data (DOI: 10.1111/2041-210X.14387)

📌 use `parallelly::availableCores()` instead of
`parallel::detectCores()` because of [problems with the
latter](https://www.jottr.org/2022/12/05/avoid-detectcores/)

🔗 [flint R package](https://github.com/etiennebacher/flint) for fast
code linting and fixing lints. I like how the author tested the package
by linting and repairing huge R packages (e.g., ggplot2 or targets) and
getting the patches accepted.

**Default for an NA value**. A trick in the linked post on
`parallel::detectCores()` to replace a possible `NA` with a safe
default: `max(default, possible_na, na.rm = TRUE)`.

🔗 <https://0.30000000000000004.com/> - I can never remember how to search for this URL.

📌 Two-tailed *p*-value: `2 * pt(-abs(t), df)`, or to stay on the log-scale
`log(2) + pt(-abs(t), df, log.p = TRUE)`.




