---
title: "Phonotactic probability and parsing KU calculator output in R"
date: 2022-02-01
tags: [r, linguistics, phonotactics, parsing]
---

I asked the PhonCorps Slack what the state of the art was for computing
phonotactic probability. I said I was using the [IPhod] database with
[SUBTLEXus] frequencies. Some responses:

  - It's not too hard to compute probabilities by hand if you have a
    pronunciation dictionary.
  - [KU Phonotactic Probability Calculator][KU].
      - One person advised that the KU uses the Webster Pocket
        Dictionary which makes it a subset of the corpus used by IPhod.
  - For neighborhood density, [CLEARPOND] and the [English Lexicon
    Project] were mentioned.

## R code to parse the KU phonotactic probability calculator output


``` r
# Parse the output of https://calculator.ku.edu/phonotactic/about
# in base R

# TJ Mahr

test_lines <- c(
  "b@d - English", "0.0512 0.0794 0.0380", "0.0059 0.0024", "1.1686 1.0083",
  "bcl - English", "0.0512 0.0165 0.0737", "0.0008 0.0035", "1.1414 1.0043",
  "bi - English", "0.0512 0.0318", "0.0022", "1.0830 1.0022", 
  "bIg - English", "0.0512 0.0962 0.0179", "0.0041 0.0035", "1.1653 1.0076", 
  "but - English", "0.0512 0.0221 0.0660", "0.0012 0.0027", "1.1393 1.0039", 
  "bW - English", "0.0512 0.0097", "0.0006", "1.0609 1.0006", 
  "bO - English", "0.0512 0.0034", "0.0003", "1.0546 1.0003", 
  "b^di - English", 
  "0.0512 0.0392 0.0380 0.0432",
  "0.0034 0.0010 0.0037", 
  "1.1716 1.0081", 
  "b^s - English", "0.0512 0.0392 0.0788", "0.0034 0.0039", "1.1692 1.0073", 
  "Cu - English", "0.0089 0.0221", "0.0002", "1.0310 1.0002"
  )

split_ku_chunks <- function(lines) {
  starts <- grep(" - ", lines)
  ends <- c(starts[-1] - 1, length(lines))
  Map(seq, starts, ends) |>
    lapply(function(i) lines[i])
}

parse_ku_chunk <- function(lines) {
  # How the output is documented in the paper.

  # Vitevitch, M.S. & Luce, P.A. (2004) A web-based interface to calculate
  # phonotactic probability for words and nonwords in English. Behavior Research
  # Methods, Instruments, and Computers, 36, 481-487.

  # "The output of the Phonotactic Probability Calculator
  # (which appears in the field on the right side of your
  # browser) typically consists of four lines of information
  # for each item entered in the left field. The first line con-
  # tains the phonemic transcription you originally entered.
  # The second line contains the position-specific probabil-
  # ity for each segment in the item entered by the user. If the
  # (non)word you entered contains more than seven pho-
  # nemes, the position-specific probabilities for each pho-
  # neme in the item will wrap around to the next line.
  #
  # "The third line (assuming the entry is seven phonemes
  # or less) contains the position-specific biphone probabil-
  # ities for each of the biphones in the word."
  pronunciation <- lines[1] |>
    strsplit(" - ") |>
    unlist() |>
    utils::head(1)

  phones <- pronunciation |>
    strsplit("")  |>
    unlist()

  biphones <- paste0(phones[-length(phones)], phones[-1])

  values <- lines[-1] |>
    paste0(collapse = " ") |>
    strsplit(" ") |>
    unlist() |>
    as.numeric()

  i_position_probs <- seq(1, length(phones))
  i_biphone_probs <- seq(1, length(biphones)) + length(phones)

  positional <- data.frame(
    pronunciation = pronunciation,
    class = "positional_prob",
    token = phones,
    value = values[i_position_probs]
  )

  biphone <- data.frame(
    pronunciation = pronunciation,
    class = "biphone_prob",
    token = biphones,
    value = values[i_biphone_probs]
  )

  rbind(positional, biphone)
}

do_call_rbind <- function(xs, ...) do.call(rbind, xs, ...)

test_lines |>
  split_ku_chunks() |>
  lapply(parse_ku_chunk) |>
  do_call_rbind()
#>    pronunciation           class token  value
#> 1            b@d positional_prob     b 0.0512
#> 2            b@d positional_prob     @ 0.0794
#> 3            b@d positional_prob     d 0.0380
#> 4            b@d    biphone_prob    b@ 0.0059
#> 5            b@d    biphone_prob    @d 0.0024
#> 6            bcl positional_prob     b 0.0512
#> 7            bcl positional_prob     c 0.0165
#> 8            bcl positional_prob     l 0.0737
#> 9            bcl    biphone_prob    bc 0.0008
#> 10           bcl    biphone_prob    cl 0.0035
#> 11            bi positional_prob     b 0.0512
#> 12            bi positional_prob     i 0.0318
#> 13            bi    biphone_prob    bi 0.0022
#> 14           bIg positional_prob     b 0.0512
#> 15           bIg positional_prob     I 0.0962
#> 16           bIg positional_prob     g 0.0179
#> 17           bIg    biphone_prob    bI 0.0041
#> 18           bIg    biphone_prob    Ig 0.0035
#> 19           but positional_prob     b 0.0512
#> 20           but positional_prob     u 0.0221
#> 21           but positional_prob     t 0.0660
#> 22           but    biphone_prob    bu 0.0012
#> 23           but    biphone_prob    ut 0.0027
#> 24            bW positional_prob     b 0.0512
#> 25            bW positional_prob     W 0.0097
#> 26            bW    biphone_prob    bW 0.0006
#> 27            bO positional_prob     b 0.0512
#> 28            bO positional_prob     O 0.0034
#> 29            bO    biphone_prob    bO 0.0003
#> 30          b^di positional_prob     b 0.0512
#> 31          b^di positional_prob     ^ 0.0392
#> 32          b^di positional_prob     d 0.0380
#> 33          b^di positional_prob     i 0.0432
#> 34          b^di    biphone_prob    b^ 0.0034
#> 35          b^di    biphone_prob    ^d 0.0010
#> 36          b^di    biphone_prob    di 0.0037
#> 37           b^s positional_prob     b 0.0512
#> 38           b^s positional_prob     ^ 0.0392
#> 39           b^s positional_prob     s 0.0788
#> 40           b^s    biphone_prob    b^ 0.0034
#> 41           b^s    biphone_prob    ^s 0.0039
#> 42            Cu positional_prob     C 0.0089
#> 43            Cu positional_prob     u 0.0221
#> 44            Cu    biphone_prob    Cu 0.0002
```


[IPhod]: https://www.iphod.com/ "IPhoD database"

[SUBTLEXus]: https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus 
  "SUBTLEXus database"

[KU]: https://calculator.ku.edu/phonotactic/about 
  "Phonotactic Probability Calculator"

[English Lexicon Project]: https://elexicon.wustl.edu/ 
  "English Lexicon Project"

[CLEARPOND]: https://clearpond.northwestern.edu/ 
  "CLEARPOND is the Cross-Linguistic Easy-Access Resource for Phonological and Orthographic Neighborhood Densities."
