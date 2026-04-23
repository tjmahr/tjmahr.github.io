---
title: "Getting downlit to work on notebook HTML"
date: 2022-06-06
tags: [r, downlit, html, tooling]
---

notestar, which I use to create project notebooks, produces an HTML file. I
can then post-process this HTML file using
`downlit::downlit_html_path()` so that R function calls are
automatically linked to documentation pages. This feature is great, but
out of the box, it ignores the syntax highlighting of the output
document. The solution, it turns out, is buried in
`?rmarkdown::html_document`. I
make sure that the following css file is included in the build process,
so that links are underlined and the style defers to the default syntax
highlighting coloring.


``` css
code a:any-link {
 color: inherit; /* use colour from syntax highlighting */
 text-decoration: underline;
 text-decoration-color: #ccc;
}
```
