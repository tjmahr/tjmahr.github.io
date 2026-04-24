---
title: "R 4.2.0 alpha notes on Windows"
date: 2022-04-01
tags: [r, windows, rtools, tooling]
---

These are some notes about workarounds etc. for the devel/alpha version
of R 4.2.0 that I have been using on Windows for the past few weeks.

  - to fix the text shadow in the syntax highlighting in the docs in
    RStudio, one workaround is to replace the prism.css file in the
    folder `C:\Program Files\R\R-4.2.0alpha\doc\html` with a modified
    css file. I just download the Tomorrow Night theme from
    <https://prismjs.com/> and use that one because it doesn't have a
    shadow by default.

  - for a clean break, I removed Rtools40 and remove any use of it in
    the PATH. Rtools42/R 4.2.0 do not depend on the path.
    
  - Rtools42 adds Rtools42 Bash to the start menu. I used `pacman -Syuu`
    to update the Msys2 stuff in Rtools. If I run this again, it will update
    other packages.
    
  - Here is the documentation page for Rtools42
    <https://cran.r-project.org/bin/windows/base/howto-R-4.2.html>

(2026 note: idk why I was on an alpha. Maybe the native pipe or cmdstanr
support?)
