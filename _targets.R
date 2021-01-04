library(targets)
library(tarchetypes)
library(knitr)

list_rmds <- function(dir) {
  list.files(dir, full.names = TRUE, pattern = ".Rmd$")
}

# "figs/drafts/", "_caches/")

dir_in <- "_R"
dir_out <- "_posts"
dir_figs <- "figs"
dir_cache <- "_caches"

# knit_folder("_R", "_posts", "figs/", "_caches/")

# rmd_to_md

dir_in <- "_R/_drafts"
dir_out <- "_drafts"
dir_figs <- "figs/drafts"
# knit_folder("_R/_drafts", "_drafts", "figs/drafts/", "_caches/")

# path_in <- current_posts[1]

knit_post <- function(path_in, dir_out, dir_figs, dir_cache, base_url = "/") {
  just_basename <- function(file_path) {
    tools::file_path_sans_ext(basename(file_path))
  }
  rmd_to_md <- function(file_path, dir_md = ".") {
    paste0(file.path(dir_md, just_basename(file_path)), ".md")
  }

  path_out <- rmd_to_md(path_in, dir_out)
  # this function is a modified version of an example here:
  # http://jfisher-usgs.github.com/r/2012/07/03/knitr-jekyll/

  path_figs <-  file.path(dir_figs,  just_basename(path_in))
  path_cache <- file.path(dir_cache, just_basename(path_in))

  knit_it <- function(path_in, path_out, path_figs, path_cache, base_url) {
    library(knitr)
    opts_knit$set(base.url = base_url)
    opts_chunk$set(
      fig.asp = 0.618,
      fig.width = 6,
      dpi = 300,
      fig.align = "center",
      out.width = "80%",
      fig.path = path_figs,
      cache.path = path_cache,
      fig.cap = "center",
      comment = "#>",
      collapse = TRUE
    )
    render_markdown()
    knit(path_in, path_out, envir = new.env(), encoding = "UTF-8")
  }

  # knit in a clean session
  callr::r(
    knit_it,
    list(
      path_in = path_in,
      path_out = path_out,
      path_figs = path_figs,
      path_cache = path_cache,
      base_url = base_url
    )
  )

  path_out
}


paths_current_posts <- list_rmds("./_R")[1:2]
paths_draft_posts <- list_rmds("./_R/_drafts")





# Set target-specific options such as packages.
tar_option_set(packages = "knitr")

# End this file with a list of target objects.


#
# dir_in <- "_R"
# dir_out <- "_posts"
# dir_figs <- "figs"
# dir_cache <- "_caches"

list(
  tar_map(
    list(post = paths_current_posts),
    tar_target(in_current, post, format = "file"),
    tar_target(
      out_current,
      knit_post(in_current, "_posts", "figs", "_caches"),
      format = "file"
    )
  ),
  tar_map(
    list(post = paths_draft_posts),
    tar_target(in_draft, post, format = "file"),
    tar_target(
      out_draft,
      knit_post(in_draft, "_drafts", "figs/drafts", "_caches"),
      format = "file",
      error = "continue"
    )
  )
)
