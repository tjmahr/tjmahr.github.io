---
title: readtextgrid now uses C++ (and ChatGPT helped)
excerpt: Text parsing and thoughts about LLM-assisted R development
tags: 
  - r
  - c++
  - cpp11
  - llms
toc: true
header:
  overlay_image: "assets/images/2025-11-library-tidyverse-spect.png"
  image_description: "Spectrogram of me saying 'library tidyverse library brms"
  overlay_filter: rgba(100, 100, 100, 0)
---





In this post, I announce the release of version of 0.2.0 of the
[readtextgrid][rtg-cran] R package, describe the problem that
the package solves, and share some thoughts on LLM-assisted programming.

## Textgrids are a way to annotate audio data

[Praat](https://www.fon.hum.uva.nl/praat/) is a program for speech and 
acoustic analysis that has been around for over 30 years. It includes a 
scripting language for manipulating and analyzing data and for creating
annotation workflows. Users can annotate intervals or points of time 
in a sound file using a **textgrid** object. Here is a screenshot of a
textgrid in Praat:

{% include figure image_path="/assets/images/2025-11-library-tidyverse.png" alt="Screenshot of a Praat editor window showing the amplitude wave form, spectrogram, and textgrid annotations. The audio file is of me saying *library tidyverse library brms*." caption="Screenshot of a Praat editor window." %}{: style="max-width: 100%; display: block; margin: 2em auto;"}


There are three rows in the image, all three of them sharing the same
*x* axis (time).

1.  Amplitude waveform, showing intensity over time
2.  Spectrogram, showing how the intensity (*color*) at frequencies (*y*) changes over
    time. Red dots mark estimated formants (resonances) in the speech signal. 
3.  Textgrid of text annotations for the recording

A user can edit the textgrid by adding or adjusting boundaries and
adding annotations, and Praat will save this data to a `.TextGrid` file. 

Other programs can produce `.TextGrid` files: the textgrid pictured here
is the result of forced alignment, specifically by the [Montreal Forced
Aligner][mfa]. I told the program I said "library tidy verse library
b r m s", and it looked up the pronunciations of those words and used an
acoustic model to estimate the time intervals of each word and each
speech sound. The aligner produced a `.TextGrid` file for this alignment. 

[mfa]: https://montreal-forced-aligner.readthedocs.io/en/latest/ "Montreal Forced Aligner homepage"

These textgrids are the bread and butter of some of the research that we
do. For example, [our article][wf2] on speaking/articulation rate in children
involved over 30,000 single-sentence `.wav` files and `.TextGrid` files. We
used the alignments to determine the duration of time spent speaking, the
number of vowels in each utterance and hence the speaking rate in
syllables per second.

[wf2]: https://pubs.asha.org/doi/10.1044/2021_JSLHR-21-00206 "Speech Development Between 30 and 119 Months in Typical Children II: Articulation Rate Growth Curves"

Reading these `.TextGrid` files into R was cumbersome, so I wrote and
released [readtextgrid][rtg-cran], an R package built around one 
simple function:

[rtg-cran]: https://cran.r-project.org/package=readtextgrid "readtextgrid on CRAN"


``` r
library(tidyverse)
library(readtextgrid)

path_tg <- "_R/data/mfa-out/library-tidyverse-library-brms.TextGrid" 
data_tg <- read_textgrid(path_tg)

data_tg
#> # A tibble: 43 × 10
#>    file       tier_num tier_name tier_type tier_xmin tier_xmax  xmin  xmax text 
#>    <chr>         <int> <chr>     <chr>         <dbl>     <dbl> <dbl> <dbl> <chr>
#>  1 library-t…        1 words     Interval…         0      3.60  0     0.08 ""   
#>  2 library-t…        1 words     Interval…         0      3.60  0.08  0.74 "lib…
#>  3 library-t…        1 words     Interval…         0      3.60  0.74  1.12 "tid…
#>  4 library-t…        1 words     Interval…         0      3.60  1.12  1.58 "ver…
#>  5 library-t…        1 words     Interval…         0      3.60  1.58  1.74 ""   
#>  6 library-t…        1 words     Interval…         0      3.60  1.74  2.46 "lib…
#>  7 library-t…        1 words     Interval…         0      3.60  2.46  2.72 "b"  
#>  8 library-t…        1 words     Interval…         0      3.60  2.72  2.9  "r"  
#>  9 library-t…        1 words     Interval…         0      3.60  2.9   3.04 "m"  
#> 10 library-t…        1 words     Interval…         0      3.60  3.04  3.46 "s"  
#> # ℹ 33 more rows
#> # ℹ 1 more variable: annotation_num <int>
```

The function returns a tidy tibble with one row per annotation. The filename is
stored as a column too so that we can `lapply()` over a directory of files.
Annotations are numbered so that we can `group_by(text, annotation_num)` and 
have repeated words handled separately. 

With this textgrid in R, I can measure speaking rate, for example:


``` r
data_tg |> 
  filter(tier_name == "phones", text != "") |> 
  summarise(
    speaking_time = sum(xmax - xmin),
    # vowels have numbers to indicate degree of stress
    num_vowels = sum(str_detect(text, "\\d"))
  ) |> 
  mutate(
    syllables_per_sec = num_vowels / speaking_time 
  )
#> # A tibble: 1 × 3
#>   speaking_time num_vowels syllables_per_sec
#>           <dbl>      <int>             <dbl>
#> 1          3.22         13              4.04
```

Or annotate a spectrogram:


``` r
library(tidyverse)
library(ggplot2)
path_spectrogram <- "_R/data/mfa/library-tidyverse-library-brms.csv"
data_spectrogram <- readr::read_csv(path_spectrogram)
#> Rows: 249366 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (6): y, x, power, time, frequency, db
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

data_spectrogram |> 
  mutate(
    # reserve more of the color variation for intensities above 15 dB
    db = ifelse(db < 15, 15, db)
  ) |> 
  ggplot() + 
  aes(x = time, y = frequency) +
  geom_raster(aes(fill = db)) +
  geom_text(
    aes(label = text, x = (xmin + xmax) / 2),
    data = data_tg |> filter(tier_name == "words"),
    y = 6500,
    vjust = 0
  )  +
  geom_text(
    aes(label = text, x = (xmin + xmax) / 2),
    data = data_tg |> filter(tier_name == "phones"),
    y = 6100,
    vjust = 0,
    size = 2
  )  +
  ylim(c(NA, 6600)) +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "black") +
  guides(fill = "none") +
  labs(x = "time [s]", y = "frequency [Hz]")
```

<div class="figure" style="text-align: center">
<img src="/figs/2025-11-14-readtextgrid-cpp-llms/unnamed-chunk-4-1.png" alt="Spectrogram of me saying 'library tidyverse library brms'" width="80%" />
<p class="caption">Spectrogram of me saying 'library tidyverse library brms'</p>
</div>



![Package hex logo](/assets/images/2021-03-read-textgrid-logo.png){: .align-right style="max-width: 30%;"}

I released the first version of the package in 2020. This package,
notably for me, contains the first hex badge I ever made.


## My original `.TextGrid` parser and its problem

Here is what the contents of the `.TextGrid` file look like. It's not the whole
file but enough to give a sense of the structure:


``` r
path_tg |> 
  readLines() |> 
  head(26) |> 
  c("[... TRUNCATED ... ]") |> 
  writeLines()
#> File type = "ooTextFile"
#> Object class = "TextGrid"
#> 
#> xmin = 0 
#> xmax = 3.596009 
#> tiers? <exists> 
#> size = 2 
#> item []: 
#>     item [1]:
#>         class = "IntervalTier" 
#>         name = "words" 
#>         xmin = 0 
#>         xmax = 3.596009 
#>         intervals: size = 11 
#>         intervals [1]:
#>             xmin = 0.0 
#>             xmax = 0.08 
#>             text = "" 
#>         intervals [2]:
#>             xmin = 0.08 
#>             xmax = 0.74 
#>             text = "library" 
#>         intervals [3]:
#>             xmin = 0.74 
#>             xmax = 1.12 
#>             text = "tidy" 
#> [... TRUNCATED ... ]
```

The first 7 lines provide some metadata about the time range of the
audio and the number of tiers (`size = 2`). The file then writes out each
tier (`item [n]` lines) by first giving the `class`, `name`, time
duration and number of marks or intervals. Each mark or interval is
enumerated with time values `xmin`, `xmax` and `text` values.

Because nearly everything here follows a `key = value` syntax and
because sections are split from each other very neatly with `item [n]:`
or `interval [n]:` lines, I was able to write **a simple parser using
regular expressions**: Split the file into `item [n]` sections, split
those into `interval [n]` sections, and extract key-value pairs.

This easy approach came with limitations. First, the [TextGrid
specification][tg-spec] was much more flexible. For example, Praat
also provides much less verbose "short" format textgrids which are like
a stream of time and text annotations:

[tg-spec]: https://www.fon.hum.uva.nl/praat/manual/TextGrid_file_formats.html "TextGrid file formats"


``` r
path_tg_short <- "_R/data/mfa-out/library-tidyverse-library-brms-short.TextGrid"
path_tg_short |> 
  readLines() |> 
  head(26) |> 
  c("[... TRUNCATED ... ]") |> 
  writeLines()
#> File type = "ooTextFile"
#> Object class = "TextGrid"
#> 
#> 0
#> 3.596009
#> <exists>
#> 2
#> "IntervalTier"
#> "words"
#> 0
#> 3.596009
#> 11
#> 0
#> 0.08
#> ""
#> 0.08
#> 0.74
#> "library"
#> 0.74
#> 1.12
#> "tidy"
#> 1.12
#> 1.58
#> "verse"
#> 1.58
#> 1.74
#> [... TRUNCATED ... ]
```

Everything is in the same order, but the annotations are gone. It turns
out that all of the helpful labels from before were actually *comments*
that get ignored. Everything that isn't a number or a string in
double-quotes (or a `<flag>`) is a comment.

There are also other quirks (`"` escapement, `!` comments, deviations
between the Praat description of the format and the behavior of
`praat.exe`). I have them documented as a kind of [unofficial
specification][unofficial-spec] in an article on the package website.

[unofficial-spec]: https://www.tjmahr.com/readtextgrid/articles/textgrid-specification.html "Textgrid Specification article"

But my original regular-expression based parser could only handle the
verbose long-format textgrids. I knew this. I put this in [a GitHub issue
in 2020][rtg-issue-4]. And this compatibility oversight was never a
problem for me until I tried a new phonetics tool that defaulted to
saving the textgrids in the short format. Now, readtextgrid could not
in fact "read textgrid".

[rtg-issue-4]: https://github.com/tjmahr/readtextgrid/issues/4 "ugh support textgrid grammar #4"


## The new R-based tokenizer

[Josef Fruehwald][jf-1], a linguist with [lots of
acoustics/phonetics software][jf-2], submitted a pull request to implement a
proper parser that I eventually rewrote to handle various edge cases and
undocumented behavior in the `.TextGrid` specification. I made an
[adversarial `.TextGrid` file][hard-file] 😈 that could still be opened
by `praat.exe` but was meant to be difficult to parse. This was a fun
development loop: Make the file harder, update the parser to handle the
new feature, repeat.

[hard-file]: https://github.com/tjmahr/readtextgrid/blob/ed971e48ab3ea33e3efe0ba59f45ae3e41d07a32/tests/testthat/test-data/hard-to-parse.TextGrid "hard-to-parse.TextGrid on GitHub"

[jf-1]: https://jofrhwld.github.io/ "Josef Fruehwald's homepage"

[jf-2]: https://jofrhwld.github.io/software/ "Josef Fruehwald's software page"

Because the essential data in the file are just string tokens and
number tokens, I needed to make a [tokenizer][wiki-lexer]: a piece
of software that reads in characters, groups them into tokens, and
figures out what kind of data the token represents. The initial R-based
version of the tokenizer did the following:

  - Read the file character by character
  - Gather the characters for the current token and keep them when they
    form a valid string or number
  - Shift between three states (`in_string`, `in_strong_comment` for `!
    comments`, `in_escaped_quote`)

[wiki-lexer]: https://en.wikipedia.org/wiki/Lexical_analysis "Wikipedia page on Lexical Analysis"

These three states determine how we interpret spaces, newlines, and `"`
characters. For example, a newline ends a `! comment` but a newline can
appear in a string so it doesn't end a string. Moreover, in a comment,
`"` is ignored, but in a string, it might be the end of the string or an
escaped quote (doubled double-quotes are used for `"` characters: the
string `"""a"""` has the text `"a"`).

But at a high level, the code was simple:


``` r
for (i in seq_along(all_char)) {

  # { ... examine current character ... }
  
  # { ... handle comment state ... }
  
  # { ... collect token if we see whitespace and are not in a string  ... }
  
  # { ... handle string and escaped quote state ... }

}
```

The new character-by-character parser worked 🎉. It had conquered
the adversarial example file, but there was still one more problem. It was
slower than the original regular-expression parser!


``` r
tg_lines <- readLines(path_tg)

bench::mark(
  legacy = readtextgrid:::legacy_read_textgrid_lines(tg_lines),
  new_r = readtextgrid:::r_read_textgrid_lines(tg_lines)
)
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 legacy       75.1ms   76.1ms      13.1    6.81MB     19.7
#> 2 new_r        70.7ms   72.3ms      13.7  590.88KB     10.3
```

At this point, I asked ChatGPT for tips on speeding up the tokenizer.



## Some thoughts about LLMs

<blockquote class="bluesky-embed" data-bluesky-uri="at://did:plc:4t2ziwnnescprzorvmrfduey/app.bsky.feed.post/3ltdjbaktss2s" data-bluesky-cid="bafyreihipumdkez3ifgsyt3eqbgelzuzljvllfwj4lbnfju55vvoxqc5gu" data-bluesky-embed-color-mode="system"><p lang="en">the thing about (the current) chatgpt is that it writes like a fucking idiot with excellent grammar</p>&mdash; sarah jeong (<a href="https://bsky.app/profile/did:plc:4t2ziwnnescprzorvmrfduey?ref_src=embed">@sarahjeong.bsky.social</a>) <a href="https://bsky.app/profile/did:plc:4t2ziwnnescprzorvmrfduey/post/3ltdjbaktss2s?ref_src=embed">July 6, 2025 at 7:20 PM</a></blockquote>

Now, let's talk about large language models (LLMs). There's a lot I
could say about them.[^fn-etc] As a language scientist, I'll start here: They
know *syntax*. They know which words go together and can generate very
plausible sequences of words. They do not know *semantics* however. They
don't have any firsthand knowledge or experience about what those
sequences express. They can't introspect about that knowledge or
experience to see whether things "make sense".[^fn-reasoning] They
[don't care][bullshit] about the truth or falsity of statements.
They just make plausible sequences of words.


[^fn-etc]: Things I won't talk about: Plagiarism, safety, energy use, hype,
    undercooked AI features making things slower and dumber, stupid people 
    emboldened by how trivial AI makes everything seem---*we won't need 
    programmers or doctors or historians or whatever* is what someone with no
    expertise in programming, medicine, history, etc. would say---dumdums 
    tearing down [fences](https://theknowledge.io/chestertons-fence-explained/),
    [creativity versus productivity](https://www.notebookcheck.net/Hideo-Kojima-says-AI-is-a-friend-not-a-threat-to-creativity-in-game-development.1141848.0.html), aesthetic homogenization or 
    how I keep seeing the same comic style in YouTube thumbnails, nobody asked 
    for slop, oh they did ask for slop, etc.

[bullshit]: https://www.andrewheiss.com/ai/#text "Andrew Heiss's AI position statement"

[^fn-reasoning]: There is something introspective about 
    [reasoning models][llm-reasoning] which will break a prompt into steps and 
    work through them. But still, I'm thinking about what the ground truth is in 
    this reasoning. The statistical regularities of word patterns?

[llm-reasoning]: https://magazine.sebastianraschka.com/p/understanding-reasoning-llms "Understanding Reasoning LLMs"

Now, it turns out that if you learn how to make sequences of words from an
Internet-sized corpus of text, then a lot of the plausible sequences you make
will turn out to be true. If you read 10,000 cookbooks, you could probably
provide a very classic recipe for scrambled eggs. But because you don't know
about sarcasm or can't draw on your own experience of trying to not ingest
non-food chemicals, you might suggest putting [glue on a pizza][bbc-glue].

[bbc-glue]: https://www.bbc.com/news/articles/cd11gzejgz4o "Glue pizza and eat rocks: Google AI search errors go viral"

So, as we use an LLM, we need to ask ourselves how much we care about
the truth or care about knowing or understanding things. That may sound
like a glib or weird statement: Shouldn't we always care about the
truth? Well, sometimes we don't. We just want some syntax; we want
[boilerplate or templates][usethis] to fill out.[^fn-tradition]
For example, I can ask an LLM to "write some unit tests for a function
`round_to(xs, unit)` that rounds a vector of values to an arbitrary
unit" and receive:

[usethis]: https://usethis.r-lib.org/ "usethis R package"


``` r
test_that("round_to() rounds to nearest multiple of unit", {
  expect_equal(round_to(5, 2), 6)
  expect_equal(round_to(4.9, 2), 4)
  expect_equal(round_to(5.1, 2), 6)
  expect_equal(round_to(c(1, 2, 3, 4), 2), c(2, 2, 4, 4))
})
```

These tests are not useful *until* I plug in the correct values for the expected 
output.

[^fn-tradition]: I think there is a great "tradition"---not sure of the right 
    word here---in learning programming and other tools where we start from a
    starter template or maybe small sample project and we experimentally
    tweak the code and iterate until it turns into the thing we want. It's
    like [scaffolding](https://en.wikipedia.org/wiki/Lev_Vygotsky#Scaffolding)
    but at a less metaphorical level: Code that sets a foundation for 
    self-directed learning.

In other cases, we don't quite care about truth or comprehension because we can
get external corroboration.[^fn-wood] When I ask ChatGPT for an obfuscated R script to
make Pac-Man in ggplot2, I can run the code to see if it works without trying to
decipher its syntax:

[^fn-wood]: I asked ChatGPT for help making a shopping list for a small woodworking
    project, and it offered a cutting plan for the lumber. *Sure, why not?* It
    messed up the math with a plan that involved cutting off 74 inches of
    wood from a 6-foot piece of lumber. My external corroboration in this case 
    was a scrap of wood.



``` r
library(ggplot2)
ggplot()+
geom_polygon(aes(x,y),
data=within(data.frame(t(sapply(seq(a<-pi/9,2*pi-a,l<-4e2),
function(t)c(cos(t),sin(t))))),
{rbind(.,0,0,cos(a),sin(a))->df;x=df[,1];y=df[,2]}),
fill="#FF0",col=1)+
annotate("point",x=.35,y=.5,size=3)+
annotate("point",x=c(1.4,2,2.6),y=0,size=3)+
coord_equal(xlim=c(-1.2,3),ylim=c(-1.2,1.2))+
theme_void()
#> Error in eval(substitute(expr), e): object '.' not found
```

(Strangely, this is the case where a dot 
[kills](https://www.youtube.com/watch?v=NxSj2T2vx7M) Pac-Man.)


### Vibes are semantic vapor

When we abandon caring about truth or understanding things and just rely
on external corroboration, we are in the realm of [vibe
coding](https://en.wikipedia.org/wiki/Vibe_coding). I like this term
because of its insouciant honesty: *Truth? Comprehension? We're just
going off the vibes.* It would be a great help if we used the word more
liberally. A YouTube video called "A vibe history of NES videogames"? No
thanks.[^fn-abadox]

[^fn-abadox]: I am still immensely annoyed about a YouTube video that tried to 
    tell me Abadox was a "controversial" NES game. Get out of here. Nobody 
    talked about that game. Show me a newspaper clipping or something.

If we lean into vibes, we need to get better at external corroboration
and know our programming languages even better. R is a flexible
programming language and it does some things that "help" the user that
can lead to silent bugs. Famously, function arguments and `$` will match
partial names.


``` r
# Look at the "Call:" in the output
lm(f = hp ~ cyl, d = mtcars)
#> 
#> Call:
#> lm(formula = hp ~ cyl, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)          cyl  
#>      -51.05        31.96

# There is no `m` column
all(mtcars$m == mtcars$mpg)
#> [1] TRUE
```

A student I work with was trying to compute sensitivity and specificity on 
weighted data. The LLM suggested the following:


``` r
# Make some weighted data using frequencies
data <- pROC::aSAH |> 
  count(outcome, age, name = "weight")

# What the LLM did:
pROC::roc(data, "outcome", "age", weights = data$weight)
#> Setting levels: control = Good, case = Poor
#> Setting direction: controls < cases
#> 
#> Call:
#> roc.data.frame(data = data, response = "outcome", predictor = "age",     weights = data$weight)
#> 
#> Data: age in 44 controls (outcome Good) < 30 cases (outcome Poor).
#> Area under the curve: 0.5947
```

This code runs without any problems. It's wrong, but it runs. The problem 
is that [`pROC::roc(...)`][proc-roc] supports variadic arguments (`...`):

[proc-roc]: https://rdrr.io/pkg/pROC/man/roc.html "Documentation for pROC::roc()"


``` r
# Note the dots
pROC:::roc |> formals() |> str()
#> Dotted pair list of 1
#>  $ ...: symbol
pROC:::roc.data.frame |> formals() |> str()
#> Dotted pair list of 5
#>  $ data     : symbol 
#>  $ response : symbol 
#>  $ predictor: symbol 
#>  $ ret      : language c("roc", "coords", "all_coords")
#>  $ ...      : symbol
```

Those `...` are for forwarding arguments to other functions that `roc()` 
might call internally. Unfortunately,
functions by default don't check the contents of the `...` to see if they
have unsupported arguments. Thus, bad arguments are ignored silently:


``` r
# method and weights are not real arguments
pROC::roc(data, "outcome", "age", method = fake, weights = fake)
#> Setting levels: control = Good, case = Poor
#> Setting direction: controls < cases
#> 
#> Call:
#> roc.data.frame(data = data, response = "outcome", predictor = "age",     method = fake, weights = fake)
#> 
#> Data: age in 44 controls (outcome Good) < 30 cases (outcome Poor).
#> Area under the curve: 0.5947
```

The LLM hallucinated a `weights` argument, which is a plausible
argument,[^fn_weights] and the `...` syntax behavior swallowed it up like
Pac-Man. It always comes back to Pac-Man. I ended up writing [a
function](https://www.tjmahr.com/wisclabmisc/reference/compute_sens_spec_from_ecdf.html)
that could compute sens and spec on weighted data.

<blockquote class="bluesky-embed" data-bluesky-uri="at://did:plc:5wm25vgenhgut3iqfjf4ozj5/app.bsky.feed.post/3m5kfxmdkx22a" data-bluesky-cid="bafyreidioowqsfnvvsvesncnb5s6ukkyhxqadinceqnax6ogjt2cj3635i" data-bluesky-embed-color-mode="system"><p lang="en">Unfortunately the space of LLM code errors and the space of human errors are not the same, making hard-won code review instincts misfire</p>&mdash; Eugene Vinitsky 🍒 (<a href="https://bsky.app/profile/did:plc:5wm25vgenhgut3iqfjf4ozj5?ref_src=embed">@eugenevinitsky.bsky.social</a>) <a href="https://bsky.app/profile/did:plc:5wm25vgenhgut3iqfjf4ozj5/post/3m5kfxmdkx22a?ref_src=embed">November 13, 2025 at 6:21 PM</a></blockquote>

As users, we can guard against the first two silent problems with
`options(warnPartialMatchArgs, warnPartialMatchDollar)`, and as
developers, we can prevent the second problem with
[`rlang::check_dots_used()`][rlang-cdu] and friends. But like I said
at the outset, external corroboration requires us to know *even more*
about the language in order to vibe safely.

[rlang-cdu]: https://rlang.r-lib.org/reference/check_dots_used.html "Documentation for check_dots_used()"

### Syntax and semantics, again

In this mini-position statement on LLM assistance, the two principles I
am trying to develop are:

- LLMs know text distributions very well. Use them to generate starter syntax. 
- LLMs don't understand anything. It's all bullshit and vibes. 

If we think of LLMs as syntax generators, we can imagine
some pretty good use cases:

- Write unit tests for a function that does...
- Set up Roxygen docs for this function
- Create a function to simulate data for a model of `rt ~ group + (1 | id)`
- Write a Stan program to fit this model. ([Mind your priors.](https://chatgpt.com/share/691763d2-87a4-8005-9342-bee0d0222348))
- *Spoiler alert*: Convert this R loop into C++ code

Still, we need to be mindful of the semantic limitations and skeptical
of the output. We should audit the results and make sure we comprehend
them, or admit upfront that this code is running on vibes. In either case, 
we also need to be vigilant about bugs that could happen silently or bugs
that a machine might make but a human wouldn't (hallucinations).

One thing I worry about with LLM reliance is skill atrophy. If I keep
using this bot as a crutch, then some of my skills will get weaker. Sam Mehr
has a take I quite like that puts this concern upfront. LLMs are
fine for code we don't feel bothered to learn:

<blockquote class="bluesky-embed" data-bluesky-uri="at://did:plc:v6qwaqo24zfrq5fj7ceibxqk/app.bsky.feed.post/3lp42tel3lk2e" data-bluesky-cid="bafyreigsxov4rjblnirmd7samors5k6ygw63lfrenx35g5iwczrs6eawxi" data-bluesky-embed-color-mode="system"><p lang="en">re AI, a PhD student mentioned sheepishly that they used chatgpt for advice on coding up an unusual element in javascript. Almost apologized
<br/><br/>
I&#x27;m like no no no you&#x27;re a psych PhD, not CS, this is exactly what LLMs are for! Doing a so-so job at things you just need done &amp; don&#x27;t care about learning!</p>&mdash; samuel mehr (<a href="https://bsky.app/profile/did:plc:v6qwaqo24zfrq5fj7ceibxqk?ref_src=embed">@mehr.nz</a>) <a href="https://bsky.app/profile/did:plc:v6qwaqo24zfrq5fj7ceibxqk/post/3lp42tel3lk2e?ref_src=embed">May 13, 2025 at 10:32 PM</a></blockquote>

I quite like programming and want to learn. I like to read the release
notes, dig into the documentation and
[experiment](https://bsky.app/profile/tjmahr.com/post/3m5hjc3ct6s26)
with new modeling features. At the same time, sometimes I just want a
bash script to unzip all `.zip` files in a directory. Time was, we would
find something from Stack Overflow to adapt for that problem. Now, we
ask ChatGPT for the code, look it over quick, test it and move on. That
seems fine. A metacognitive awareness about what is worth 
learning and what problems are worth solving in a slower methodical way 
is very useful for an LLM user. 

Finally, to be clear---I can't believe I need to make this
disclaimer---we should always care about truth and accuracy when we
write prose and publish it and put our name on it. Vibes are not
scientific or scholarly. When I see emails or code documentation with
immaculate formatting and perfect language, my bullshit sensor goes off
and I worry that I need to read extra carefully because a smooth-talking
robot is trying to pull a fast one on me. I don't use LLMs for writing
except for proofreading or requests for nitpicking. I have an
instruction in ChatGPT that says not to revise anything I write unless
it sneaks Magic: The Gathering card names into the output. (Alas, it
generally ignores that *diabolic edict* of mine.)



## AI assistance in readtextgrid

Because the old parser was outperforming the newer, more robust parser, I
asked ChatGPT for ways to make my textgrid parsing faster. For example,
one version of the loop collected characters in a vector and then
`paste0()`-ed them together. ChatGPT suggested that because we are
iterating over character indices we instead use 
`substring()` to extract tokens from the text. That worked, and it ran
faster, until it failed a unit test on a character wearing a diacritic. 
After a few rounds of trying to improve the loop, I asked quite
bluntly: "How can we move the tokenize loop into Rcpp or cpp11 with the
viewest [*sic*] headaches possible".

And it provided some very legible cpp11 code. I had never used C++ with
R before. To get started, I had to call on
[`usethis::use_cpp11()`](https://usethis.r-lib.org/reference/use_cpp11.html)
to make the necessary boilerplate---you just need syntax sometimes---and
I had to troubleshoot the first couple versions of the function because
of errors. The [cpp11
documentation](https://cpp11.r-lib.org/articles/cpp11.html) is small in
a good way. It has examples of converting R code into C++ equivalents,
which is precisely the activity that I was up to.

What I liked about the ChatGPT output is how clear the translation was.
In the R version, part of the character processing loop is to peek ahead
to the next character to see whether `"` is an escaped quote `""` or the
end of a string:


``` r
# ... in the character processing loop

    # Start or close string mode if we see "
    if (c_starts_string) {
      # Check for "" escapes
      peek_c <- all_char[i + 1]
      if (peek_c == "\"" & in_string) {
        in_escaped_quote <- TRUE
      } else {
        in_string <- !in_string
      }
    }

# ...
```

And here is the C++ version of the peek ahead code:


``` cpp
// ... helper functions ...

  // Is this a UTF-8 continuation byte? (10xxxxxx)
  auto is_cont = [](unsigned char b)->bool {
    // Are the first two bits 10?
    return (b & 0xC0) == 0x80;
  };

// ... in the character processing loop ...

    if (b == 0x22) { // '"'
      // peek ahead to see if we have a double "" escapement
      size_t j = i + 1;
      // We need the next character, not just the next byte, so we skip
      // continuation characters.
      while (j < nbytes && is_cont(static_cast<unsigned char>(src[j]))) ++j;
      // Use `0x00` dummy character if we are at the end of the string
      unsigned char nextb = (j < nbytes) ? static_cast<unsigned char>(src[j]) : 0x00;

      if (in_string && nextb == 0x22) {
        esc_next = true;    // consume next '"' once
      } else {
        in_string = !in_string;
      }
    }

// ...
```

There is a logical correspondence between the lines that I wrote myself
in R and the lines that the LLM provided for C++. The C++ version works
at the level of bytes instead of characters, and that matters:


``` r
"é" |> nchar(type = "chars")
#> [1] 1
"é" |> nchar(type = "bytes")
#> [1] 2
```

But the C++ code makes sense to me. It looks *plausible*, right? Still,
plausible isn't enough. I asked the LLM a lot of follow-up questions:
what does `auto` do, what is `size_t` doing, and so on. And I annotated
the C++ code with comments for my own understanding.

During my auditing, I went down a particular rabbithole to make sure I
understood how Unicode bytes get packed into UTF-8 sequences. I learned
how the character `é` for example has the codepoint (character number)
`U+00E9` in Unicode, so it falls in the range of codepoints that need to
be split into two bytes. The [scheme for two-byte
encoding][unicode-table] is 

[unicode-table]: https://www.unicode.org/versions/Unicode17.0.0/core-spec/chapter-3/#G27288 "Table 3-6. UTF-8 Bit Distribution"

```
character number               ->               character encoding
codepoint -> 00000yyy yyxxxxxx -> 110yyyyy 10xxxxxx -> UTF-8 bytes
00E9      -> 00000000 11101001 -> 11000011 10101001 -> c3 a9
```

Which we can check by hand:


``` r
bitchar_to_raw <- function(xs) {
  xs |> 
    strsplit("") |> 
    lapply(function(x) as.integer(x) |> rev() |> packBits()) |> 
    unlist()
}

bitchar_to_raw(c("11000011", "10101001"))
#> [1] c3 a9
charToRaw("é")
#> [1] c3 a9
```

In the UTF-8 scheme, bytes that start with `10` are only the second,
third and fourth bytes in a character's encoding---that is, only the
*continuation* bytes. Now, at this point, we can comprehend *why* the C++
is checking for continuation characters and why the check for
continuation characters involves checking the first two bits.

Another rabbithole involved how to parse numbers. At first, the LLM
suggested I use one of R's own C functions to handle it. That idea 
seems really powerful to me---wait, now I can tap into what R's own 
routines?!---but R's parser was a bit stricter than what I needed to
match `praat.exe`.

This new C++ based tokenizer yielded a **huge performance gain**:


``` r
bench::mark(
  legacy = readtextgrid:::legacy_read_textgrid_lines(tg_lines),
  new_r = readtextgrid:::r_read_textgrid_lines(tg_lines),
  new_cpp = readtextgrid::read_textgrid_lines(tg_lines)
)
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 legacy      65.22ms  68.77ms      13.8    6.49MB     4.59
#> 2 new_r       72.67ms   88.5ms      11.5  363.33KB     5.74
#> 3 new_cpp      3.12ms   3.64ms     272.    96.77KB     4.11
```

That's an improvement of 10--15x! Now, I find myself wondering: What else
could use a cpp11 speed boost?

One downside of adopting cpp11 is that the package needs to compile
code. As a result, I can't just tell people to try the developer version
of the package with
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).
CRAN compiles packages so end users don't face this issue when
installing the official released version of packages.

One workaround I adopted was relying on [R
Universe](https://ropensci.org/r-universe/) which will provide compiled
versions of packages hosted on GitHub. Then we change the installation
instructions to:


``` r
install.packages(
  "readtextgrid", 
  repos = c("https://tjmahr.r-universe.dev", "https://cloud.r-project.org")
)
```

You might have seen this pattern elsewhere.
[cmdstanr](https://mc-stan.org/cmdstanr/) skips CRAN entirely and only
uses R Universe.


## Parting thoughts

An LLM helped me translate pokey R code into fast C++ code. The code is
*live now* on [CRAN][rtg-cran], released in readtextgrid 0.2.0. I'm
maybe kind of a C++ developer now? (Nah.)

This kind of code translation strikes me as an easy win for R developers: 
"I have my version that works right now, but I think it can
go faster. Help me convert this to C++." I took care to make sure I
understood the output. The syntax came easy, but the semantics
(comprehension and validation) took more time.

If I ask myself, *could I have done this translation to C++ without an
LLM?* The answer is no, not in a reasonable timeframe, certainly not as
fast as the two days it took me in this case. That's a pretty undeniable
boost. 













[^fn_weights]: Let's count functions with `weights` arguments in some base R
    packages:

    
    ``` r
    get_funcs_with_weights <- function(pkg) {
      ns <- asNamespace(pkg)
      ls(ns) |> 
        lapply(get, envir = ns) |> 
        setNames(ls(ns)) |> 
        Filter(f = is.function) |> 
        lapply(formals) |> 
        Filter(f = function(x) "weights" %in% names(x)) |> 
        names()
    }
    
    get_funcs_with_weights("stats")
    #>  [1] "density.default" "glm"             "glm.fit"         "lm"             
    #>  [5] "loess"           "nls"             "ppr.default"     "ppr.formula"    
    #>  [9] "predict.lm"      "predLoess"       "simpleLoess"
    get_funcs_with_weights("mgcv")
    #>  [1] "bam"             "bfgs"            "deriv.check"     "deriv.check5"   
    #>  [5] "efsud"           "efsudr"          "find.null.dev"   "gam"            
    #>  [9] "gam.fit3"        "gam.fit4"        "gam.fit5"        "gamm"           
    #> [13] "gammPQL"         "initial.spg"     "jagam"           "mgcv.find.theta"
    #> [17] "mgcv.get.scale"  "newton"          "scasm"           "score.transect" 
    #> [21] "simplyFit"
    get_funcs_with_weights("MASS")
    #> [1] "glm.nb"      "glmmPQL"     "polr"        "rlm.default" "rlm.formula"
    #> [6] "theta.md"    "theta.ml"    "theta.mm"
    get_funcs_with_weights("nlme")
    #>  [1] "gls"               "gnls"              "lme"              
    #>  [4] "lme.formula"       "lme.groupedData"   "lme.lmList"       
    #>  [7] "nlme"              "nlme.formula"      "nlme.nlsList"     
    #> [10] "plot.simulate.lme"
    ```











