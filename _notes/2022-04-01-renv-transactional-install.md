---
title: "renv transactional installs and my .Rprofile setup"
date: 2022-04-01
tags: [r, renv, reproducibility, configuration]
---

I am now adding the following to my .Rprofile file. By default,
`renv::restore()` will undo the entire restore attempt if any of the
package installs fails. (It wants the the whole *transaction* to
succeed.) The undoes the default so that successful package installs are
kept.


``` r
options(renv.config.install.transactional = FALSE)
```

My full project-level .Rprofile tends to be something like (**last
updated April 2026**):


``` r
options(
  repos = c(
    tjmahr = "https://tjmahr.r-universe.dev",
    Stan = "https://stan-dev.r-universe.dev",
    getOption("repos")
  )
)
source("renv/activate.R")
options(
  renv.config.install.transactional = FALSE,
  renv.config.pak.enabled = TRUE
)
options(WrapRmd.width = 72)
options(styler.addins_style_transformer = "grkstyle::grk_style_transformer()")
options(cli.palette = "vscode")
```
