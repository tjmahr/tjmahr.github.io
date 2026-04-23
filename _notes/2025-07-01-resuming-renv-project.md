---
title: "Resuming a renv project without restoring the environment"
date: 2025-07-01
tags: [r, renv, reproducibility]
---

Steps for resuming a renv project when you don't care about restoring
the previous package environment.

  - Make life easier with `utils::install.packages(c("usethis", "gitcreds"))`
    to avoid problems with GitHub credentials if you use GitHub packages.

  - Install renv anew and then do `renv::record("renv@1.1.4")` or
    whatever to record that version.

  - Install the packages used by the project. We can use `renv::update()` or 
    install as needed as we run code in the project's files.
    
  - Take a `renv::snapshot()`.
  
Note that the way to record an updated version of R is with `renv::snapshot()`.
