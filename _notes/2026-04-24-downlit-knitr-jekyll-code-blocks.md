---
title: "Using downlit on knitr code chunks for Jekyll"
date: 2026-04-24
tags: [r, meta, knitr, downlit, jekyll, syntax-highlighting]
---

I wanted to use `downlit` to add package/function links to code blocks in
my Jekyll site. downlit is an amazing feature that I have never seen used in 
other languages, and it's the main thing drawing me towards quarto instead of 
vintage RMarkdown.

Currently, this site is built in 2 steps: 

```
.Rmd files ---knitr---> 
    .md files ---jekyll---> 
       .html files
```

The simple approach *should* be 3 steps, by using `downlit::downlit_md_path()`:

```
.Rmd files ---knitr---> 
    .md files ---downlit---> 
       autolinked .md files ---jekyll---> 
       .html files
```

But this will not work cleanly because `downlit` inspects the structure
of the .md file using Pandoc, and Pandoc strips away YAML front
matter, which is bad because the YAML frontmatter is Jekyll metadata. I
know about this problem because I wrote the GitHub issue for it five
years ago 🤓 <https://github.com/r-lib/downlit/issues/123>.

But you know who else sees the code blocks? knitr. I added a knitr
hook to run the chunk text through downlit. Below is my main knitting
function. It's very verbose and hard-codes my default knitr settings
because it's meant to run in a separate (clean) R session via callr. I
added the `use_downlit_chunk_hook()` function to register the chunk
hook, and this hook runs the knitr chunk through downlit.

<pre class='chroma'>
<span>  <span class='nv'>knit_it</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>path_in</span>, <span class='nv'>path_out</span>, <span class='nv'>path_figs</span>, <span class='nv'>path_cache</span>, <span class='nv'>base_url</span>, <span class='nv'>use_downlit</span> <span class='o'>=</span> <span class='nv'>use_downlit</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>    <span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://yihui.org/knitr/'>knitr</a></span><span class='o'>)</span></span>
<span></span>
<span>    <span class='nv'>use_downlit_chunk_hook</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>      <span class='nv'>old_chunk_hook</span> <span class='o'>&lt;-</span> <span class='nf'>knitr</span><span class='nf'>::</span><span class='nv'><a href='https://rdrr.io/pkg/knitr/man/knit_hooks.html'>knit_hooks</a></span><span class='o'>$</span><span class='nf'>get</span><span class='o'>(</span><span class='s'>"chunk"</span><span class='o'>)</span></span>
<span>      <span class='nf'>knitr</span><span class='nf'>::</span><span class='nv'><a href='https://rdrr.io/pkg/knitr/man/knit_hooks.html'>knit_hooks</a></span><span class='o'>$</span><span class='nf'>set</span><span class='o'>(</span>chunk <span class='o'>=</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>x</span>, <span class='nv'>options</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>        <span class='nv'>md</span> <span class='o'>&lt;-</span> <span class='nf'>old_chunk_hook</span><span class='o'>(</span><span class='nv'>x</span>, <span class='nv'>options</span><span class='o'>)</span></span>
<span>        <span class='nv'>tmp_in</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span>fileext <span class='o'>=</span> <span class='s'>".md"</span><span class='o'>)</span></span>
<span>        <span class='nv'>tmp_out</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/tempfile.html'>tempfile</a></span><span class='o'>(</span>fileext <span class='o'>=</span> <span class='s'>".md"</span><span class='o'>)</span></span>
<span>        <span class='nf'><a href='https://rdrr.io/r/base/writeLines.html'>writeLines</a></span><span class='o'>(</span><span class='nv'>md</span>, <span class='nv'>tmp_in</span>, useBytes <span class='o'>=</span> <span class='kc'>TRUE</span><span class='o'>)</span></span>
<span></span>
<span>        <span class='nf'>downlit</span><span class='nf'>::</span><span class='nf'><a href='https://downlit.r-lib.org/reference/downlit_md_path.html'>downlit_md_path</a></span><span class='o'>(</span></span>
<span>          in_path <span class='o'>=</span> <span class='nv'>tmp_in</span>,</span>
<span>          out_path <span class='o'>=</span> <span class='nv'>tmp_out</span>,</span>
<span>          format <span class='o'>=</span> <span class='s'>"gfm"</span></span>
<span>        <span class='o'>)</span></span>
<span>        <span class='nf'><a href='https://rdrr.io/r/base/paste.html'>paste</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/readLines.html'>readLines</a></span><span class='o'>(</span><span class='nv'>tmp_out</span>, warn <span class='o'>=</span> <span class='kc'>FALSE</span>, encoding <span class='o'>=</span> <span class='s'>"UTF-8"</span><span class='o'>)</span>, collapse <span class='o'>=</span> <span class='s'>"\n"</span><span class='o'>)</span></span>
<span>      <span class='o'>}</span><span class='o'>)</span></span>
<span>    <span class='o'>}</span></span>
<span></span>
<span>    <span class='nv'>opts_knit</span><span class='o'>$</span><span class='nf'>set</span><span class='o'>(</span></span>
<span>      base.url <span class='o'>=</span> <span class='nv'>base_url</span>,</span>
<span>      root.dir <span class='o'>=</span> <span class='nf'>here</span><span class='nf'>::</span><span class='nf'><a href='https://here.r-lib.org/reference/here.html'>here</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nv'>opts_chunk</span><span class='o'>$</span><span class='nf'>set</span><span class='o'>(</span></span>
<span>      fig.asp <span class='o'>=</span> <span class='m'>0.618</span>,</span>
<span>      fig.width <span class='o'>=</span> <span class='m'>6</span>,</span>
<span>      dpi <span class='o'>=</span> <span class='m'>300</span>,</span>
<span>      fig.align <span class='o'>=</span> <span class='s'>"center"</span>,</span>
<span>      out.width <span class='o'>=</span> <span class='s'>"80%"</span>,</span>
<span>      fig.path <span class='o'>=</span> <span class='nv'>path_figs</span>,</span>
<span>      cache.path <span class='o'>=</span> <span class='nv'>path_cache</span>,</span>
<span>      fig.cap <span class='o'>=</span> <span class='s'>"center"</span>,</span>
<span>      comment <span class='o'>=</span> <span class='s'>"#&gt;"</span>,</span>
<span>      collapse <span class='o'>=</span> <span class='kc'>TRUE</span>,</span>
<span>      dev <span class='o'>=</span> <span class='s'>"ragg_png"</span></span>
<span>    <span class='o'>)</span></span>
<span>    <span class='nf'><a href='https://rdrr.io/pkg/knitr/man/output_hooks.html'>render_markdown</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>    <span class='kr'>if</span> <span class='o'>(</span><span class='nv'>use_downlit</span><span class='o'>)</span> <span class='nf'>use_downlit_chunk_hook</span><span class='o'>(</span><span class='o'>)</span></span>
<span></span>
<span>    <span class='nf'><a href='https://rdrr.io/pkg/knitr/man/knit.html'>knit</a></span><span class='o'>(</span><span class='nv'>path_in</span>, <span class='nv'>path_out</span>, envir <span class='o'>=</span> <span class='nf'><a href='https://rdrr.io/r/base/environment.html'>new.env</a></span><span class='o'>(</span><span class='o'>)</span>, encoding <span class='o'>=</span> <span class='s'>"UTF-8"</span><span class='o'>)</span></span>
<span>  <span class='o'>}</span></span></pre>

Two limitations I have to admit here:

  - Pandoc writes out markdown file, so it needs to be a flavor of
    markdown it understands. If I use Jekyll-markdown features, they
    might get mangled.
  - Because knitr is doing the hooking, downlit only sees the code
    blocks. Inline links for something like `dplyr::select()` are not
    available.


After getting the hook working, I had to make autolinked code blocks
look nice. downlit outputs HTML code blocks like:

```html
<pre class="chroma"><code>...</code></pre>
```

My site get its syntax highlighting from some Ruby libraries, so I need
to put together some CSS rules for syntax highlighting that matched the
current color set. I (w/ ChatGPT) added the following lines to my .scss file:

```scss
/* ==========================================================================
   downlit / chroma code blocks
   ========================================================================== */

pre.chroma {
  position: relative;
  margin-bottom: 1em;
  padding: 1em;
  overflow-x: auto;
  background: $base00;
  color: $base05;
  font-family: $monospace;
  font-size: $type-size-7;
  line-height: 1.5;
  border-radius: $border-radius;

  [dir=rtl] & {
    direction: ltr;
    text-align: start;
  }

  code {
    padding: 0;
    background: transparent;
    color: inherit;
    font-family: inherit;
    font-size: inherit;
  }
}

/* downlit::classes_chroma() */
.chroma {
  .c {
    /* COMMENT */
    color: $base04;
  }

  .kc {
    /* constant */
    color: $base0e;
  }

  .m {
    /* NUM_CONST */
    color: $base09;
  }

  .s {
    /* STR_CONST */
    color: $base0b;
  }

  .kr {
    /* special */
    color: $base0e;
  }

  .o {
    /* parens, infix */
    color: $base05;
  }

  .nv {
    /* SLOT, SYMBOL, SYMBOL_FORMALS */
    color: $base05;
  }

  .nf {
    /* NS_GET, NS_GET_INT, SYMBOL_FUNCTION_CALL, SYMBOL_PACKAGE */
    color: $base0c;
  }
}

pre.chroma a {
  color: inherit; /* use colour from syntax highlighting */
  text-decoration: underline;
  text-decoration-color: #ccc;
}
```

Here is a comparison of the syntax highlighting.

<pre class='chroma'>
<span><span class='c'># downlit</span></span>
<span><span class='nv'>data</span> <span class='o'>&lt;-</span> <span class='nv'>mtcars</span><span class='o'>$</span><span class='nv'>mpg</span><span class='o'>[</span><span class='m'>1</span><span class='o'>]</span></span>
<span><span class='nv'>x</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='o'>(</span><span class='m'>100</span><span class='o'>)</span></span>
<span><span class='nv'>name</span> <span class='o'>&lt;-</span> <span class='s'>"test"</span></span></pre>

```r
# rouge
data <- mtcars$mpg[1]
x <- rnorm(100)
name <- "test"
```


