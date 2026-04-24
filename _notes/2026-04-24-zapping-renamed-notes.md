---
title: "Zapping orphaned/outdated rendered notes"
date: 2026-01-01
tags: [meta, targets]
---

I write notes in RMarkdown and then use knitr to convert them to
Jekyll-friendly markdown. The steps are orchestrated by the targets
package so that the pages are only rendered on demand. But what if I
rename a `.Rmd` that already had its `.md` buddy rendered? Then I would
have an orphaned notes that should be removed.

Here is a quick script to remove those orphaned files. I should probably put it
in .Rprofile for this site.


``` r
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.5.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

if (interactive()) targets::tar_prune()

data_current_target_paths <- targets::tar_meta(starts_with("note_")) |> 
  select(name, format, path) |> 
  mutate(
    path = unlist(path) |> fs::path_norm()
  )
  
data_current_target_paths
#> # A tibble: 106 × 3
#>    name                                                format path              
#>    <chr>                                               <chr>  <fs::path>        
#>  1 note_rmd_2025_01_01_sweep_demo                      file   …01-sweep-demo.Rmd
#>  2 note_rmd_2025_01_01_gams_for_assessing_nonlinearity file   …-nonlinearity.Rmd
#>  3 note_rmd_2025_01_01_log_likelihood_functions        file   …ood-functions.Rmd
#>  4 note_md_2025_01_01_gams_for_assessing_nonlinearity  file   …g-nonlinearity.md
#>  5 note_rmd_2025_01_01_random_variable_active_binding  file   …ctive-binding.Rmd
#>  6 note_md_2025_01_01_sweep_demo                       file   …-01-sweep-demo.md
#>  7 note_md_2025_01_01_log_likelihood_functions         file   …hood-functions.md
#>  8 note_md_2025_01_01_random_variable_active_binding   file   …active-binding.md
#>  9 note_rmd_2024_01_01_mfa_conda_solver_warning        file   …olver-warning.Rmd
#> 10 note_rmd_2024_01_01_nullsafe_map                    file   …-nullsafe-map.Rmd
#> # ℹ 96 more rows

data_current_paths <- list.files("_notes/") |> file.path("_notes", ..2 = _)

# Excess paths
data_current_paths |> 
  setdiff(data_current_target_paths$path) |> 
  file.remove()
#> logical(0)
```

