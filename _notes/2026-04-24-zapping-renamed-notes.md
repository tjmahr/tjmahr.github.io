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

<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://dplyr.tidyverse.org'>dplyr</a></span><span class='o'>)</span></span>
<span><span class='c'>#&gt; Warning: package 'dplyr' was built under R version 4.5.2</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; Attaching package: 'dplyr'</span></span>
<span><span class='c'>#&gt; The following objects are masked from 'package:stats':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     filter, lag</span></span>
<span><span class='c'>#&gt; The following objects are masked from 'package:base':</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt;     intersect, setdiff, setequal, union</span></span>
<span></span>
<span><span class='kr'>if</span> <span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/interactive.html'>interactive</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span> <span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_prune.html'>tar_prune</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span></span>
<span><span class='nv'>data_current_target_paths</span> <span class='o'>&lt;-</span> <span class='nf'>targets</span><span class='nf'>::</span><span class='nf'><a href='https://docs.ropensci.org/targets/reference/tar_meta.html'>tar_meta</a></span><span class='o'>(</span><span class='nf'><a href='https://tidyselect.r-lib.org/reference/starts_with.html'>starts_with</a></span><span class='o'>(</span><span class='s'>"note_"</span><span class='o'>)</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/select.html'>select</a></span><span class='o'>(</span><span class='nv'>name</span>, <span class='nv'>format</span>, <span class='nv'>path</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://dplyr.tidyverse.org/reference/mutate.html'>mutate</a></span><span class='o'>(</span></span>
<span>    path <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/unlist.html'>unlist</a></span><span class='o'>(</span><span class='nv'>path</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'>fs</span><span class='nf'>::</span><span class='nf'><a href='https://fs.r-lib.org/reference/path_math.html'>path_norm</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span></span>
<span>  </span>
<span><span class='nv'>data_current_target_paths</span></span>
<span><span class='c'>#&gt; # A tibble: 107 × 3</span></span>
<span><span class='c'>#&gt;    name                                                format path              </span></span>
<span><span class='c'>#&gt;    &lt;chr&gt;                                               &lt;chr&gt;  &lt;fs::path&gt;        </span></span>
<span><span class='c'>#&gt;  1 note_rmd_2025_01_01_sweep_demo                      file   …01-sweep-demo.Rmd</span></span>
<span><span class='c'>#&gt;  2 note_rmd_2025_01_01_gams_for_assessing_nonlinearity file   …-nonlinearity.Rmd</span></span>
<span><span class='c'>#&gt;  3 note_rmd_2025_01_01_log_likelihood_functions        file   …ood-functions.Rmd</span></span>
<span><span class='c'>#&gt;  4 note_md_2025_01_01_gams_for_assessing_nonlinearity  file   …g-nonlinearity.md</span></span>
<span><span class='c'>#&gt;  5 note_rmd_2025_01_01_random_variable_active_binding  file   …ctive-binding.Rmd</span></span>
<span><span class='c'>#&gt;  6 note_md_2025_01_01_sweep_demo                       file   …-01-sweep-demo.md</span></span>
<span><span class='c'>#&gt;  7 note_md_2025_01_01_log_likelihood_functions         file   …hood-functions.md</span></span>
<span><span class='c'>#&gt;  8 note_md_2025_01_01_random_variable_active_binding   file   …active-binding.md</span></span>
<span><span class='c'>#&gt;  9 note_rmd_2024_01_01_mfa_conda_solver_warning        file   …olver-warning.Rmd</span></span>
<span><span class='c'>#&gt; 10 note_rmd_2024_01_01_nullsafe_map                    file   …-nullsafe-map.Rmd</span></span>
<span><span class='c'>#&gt; # ℹ 97 more rows</span></span>
<span></span>
<span><span class='nv'>data_current_paths</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/list.files.html'>list.files</a></span><span class='o'>(</span><span class='s'>"_notes/"</span><span class='o'>)</span> <span class='o'>|&gt;</span> <span class='nf'><a href='https://rdrr.io/r/base/file.path.html'>file.path</a></span><span class='o'>(</span><span class='s'>"_notes"</span>, ..2 <span class='o'>=</span> <span class='nv'>_</span><span class='o'>)</span></span>
<span></span>
<span><span class='c'># Excess paths</span></span>
<span><span class='nv'>data_current_paths</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://generics.r-lib.org/reference/setops.html'>setdiff</a></span><span class='o'>(</span><span class='nv'>data_current_target_paths</span><span class='o'>$</span><span class='nv'>path</span><span class='o'>)</span> <span class='o'>|&gt;</span> </span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/files.html'>file.remove</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] TRUE</span></span></pre>

