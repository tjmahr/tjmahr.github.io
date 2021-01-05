library(targets)
library(tarchetypes)
library(stringr)
library(knitr)

list_rmds <- function(dir) {
  list.files(dir, full.names = TRUE, pattern = ".Rmd$")
}

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

  path_figs <-  file.path(dir_figs,  just_basename(path_in), "/")
  path_cache <- file.path(dir_cache, just_basename(path_in))

  knit_it <- function(path_in, path_out, path_figs, path_cache, base_url) {
    library(knitr)
    opts_knit$set(
      base.url = base_url,
      root.dir = here::here()
    )
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


paths_current_posts <- list_rmds("./_R")[1:10] %>%
  stringr::str_subset("_footer.Rmd", negate = TRUE)
paths_draft_posts <- list_rmds("./_R/_drafts")


# Set target-specific options such as packages.
tar_option_set(packages = "knitr")

posts <- list(
  post = paths_current_posts,
  name = basename(paths_current_posts),
  sym_name = rlang::syms(basename(paths_current_posts)),
  name_md = paths_current_posts %>%
    basename() %>%
    stringr::str_replace(".Rmd$", ".md")
)

drafts <- list(
  post = paths_draft_posts,
  name = basename(paths_draft_posts),
  name_md = paths_draft_posts %>%
    basename() %>%
    stringr::str_replace(".Rmd$", ".md")
)

targets_posts <- list(
  tar_target(footer, "_R/_footer.Rmd", format = "file"),
  tar_eval(tar_target(name, post, format = "file"), posts),
  tar_eval(
    tar_target(
      name_md,
      {
        list(footer) # name footer so that it appears in the graph
        knit_post(sym_name, "_posts", "figs", "_caches")
      },
      format = "file"
    ),
    posts
  ),
  targets::tar_target_raw(
    "post_rmds",
    rlang::expr(c(!!! posts$sym_name)),
    deps = posts$name
  )
)

targets_drafts <- list(
  tar_eval(tar_target(name, post, format = "file"), drafts),
  tar_eval(
    tar_target(
      name_md,
      knit_post(name, "_drafts", "figs/drafts", "_caches"),
      format = "file",
      error = "continue"
    ),
    drafts
  )
)


list(
  targets_posts,

  tar_target(
    spellcheck_exceptions,
    c(
      # abbreviations
      "APA", "PPV", "NPV", "btw", "DS",
      # code names
      "bayesplot", "dplyr", "gganimate", "ggplot", "GitHub", "lazyeval",
      "magrittr", "pandoc",
      "readr", "RMarkdown", "RStanARM",
      "RStudio", "setNames", "stringr",
      "md", "Rmd", "README",
      # words
      "eyetracking", "hotdog", "lookahead", "lookaround",
      "programmatically", "variadic", "hoth", "si", "th", "quosure",
      "toolset", "affordances", "unintuitive", "hmmm", "wavily",
      # names
      "Fernald", "Marchman"
    )
  ),

  tar_target(
    spellcheck_current,
    spelling::spell_check_files(
      post_rmds,
      ignore = spellcheck_exceptions
    )
  ),

  tar_force(
    spellcheck_current_results,
    command = print(spellcheck_current),
    force = nrow(spellcheck_current) > 0
  )
)
