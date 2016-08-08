# adapted from https://github.com/juliasilge/juliasilge.github.io/blob/master/_scripts/knitpages.R

# -------

# compiles all .Rmd files in _R directory into .md files in Pages directory,
# if the input file is older than the output file.
# this script is by David Robinson, from here:
# https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_scripts/knitpages.R

# run ./knitpages.R to update all knitr files that need to be updated.

library("knitr")
library("dplyr", warn.conflicts = FALSE)

knit_post <- function(input, outfile, dir_figs, dir_cache, base_url = "/") {
  # this function is a modified version of an example here:
  # http://jfisher-usgs.github.com/r/2012/07/03/knitr-jekyll/

  path_fig <- file.path(dir_figs, just_basename(input), "/")
  path_cache <- file.path(dir_cache, just_basename(input))

  opts_knit$set(base.url = base_url)
  opts_chunk$set(
    fig.path = path_fig,
    cache.path = path_cache,
    fig.cap = "center",
    comment = "#>",
    collapse = TRUE)
  render_jekyll()
  knit(input, outfile, envir = parent.frame())
}

knit_folder <- function(dir_in, dir_out, dir_figs, dir_cache) {
  knittable <- list.files(dir_in, pattern = "*.Rmd", full.names = TRUE)

  if (length(knittable) == 0) return(NULL)

  posts <- data_frame(Rmds = knittable, mds = rmd_to_md(Rmds, dir_out))

  to_make <- posts %>% filter(!file.exists(mds))
  to_redo <- posts %>% filter(Rmds %is_newer_than% mds)
  to_do <- bind_rows(to_make, to_redo)

  purrr::map2(to_do$Rmds, to_do$mds, knit_post, dir_figs, dir_cache)
}

`%is_newer_than%` <- function(file_path, reference_file_path) {
  file.info(file_path)$mtime > file.info(reference_file_path)$mtime
}

rmd_to_md <- function(file_path, dir_md = ".") {
  paste0(file.path(dir_md, just_basename(file_path)), ".md")
}

just_basename <- function(file_path) {
  file_path %>% basename %>% tools::file_path_sans_ext()
}

dir_in <- "_R"
dir_out <- "_posts"
dir_figs <- "figs/"
dir_cache <- "_caches"

knit_folder("_R", "_posts", "figs/", "_caches/")

dir_in <- "_R/_drafts"
dir_out <- "_drafts"
dir_figs <- "figs/drafts/"
knit_folder("_R/_drafts", "_drafts", "figs/drafts/", "_caches/")
