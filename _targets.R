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
      collapse = TRUE,
      dev = "ragg_png"
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

paths_current_posts <- list_rmds("./_R") %>%
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
  sym_name = rlang::syms(basename(paths_draft_posts)),
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
  # tar_target(footer, "_R/_footer.Rmd", format = "file"),
  tar_eval(tar_target(name, post, format = "file"), drafts),
  tar_eval(
    tar_target(
      name_md,
      {
        list(footer) # name footer so that it appears in the graph
        knit_post(sym_name, "_drafts", "figs/drafts", "_caches")
      },
      format = "file",
      error = "continue"
    ),
    drafts
  ),
  targets::tar_target_raw(
    "draft_rmds",
    rlang::expr(c(!!! drafts$sym_name)),
    deps = drafts$name
  )
)


list(
  targets_posts,
  targets_drafts,
  tar_target(
    spellcheck_exceptions,
    c(
      # URLs / paths
      "shinyapps", "io", "https", "tristan", "icc", "jpg", "png", "embo",
      # Abbreviations
      "APA", "PPV", "NPV", "btw", "DS", "ICC",
      "AST",
      # Code names
      "bayesplot",
      "brms",
      "callr",
      "cowplot",
      "DiagrammeR",
      "dplyr", "dplyring",
      "dnorm", "qnorm",
      "flexdashboard",
      "gamlss",
      "gratia",
      "ggplot", "aes",
      "gganimate", "ggmcmc", "ggridges",
      "GitHub",
      "iccbot",
      "knitr",
      "lazyeval",
      "libPaths", "libpaths",
      "magrittr",
      "mcycle",
      "mgcv", "gam", "gamm", "s",
      "nlme", "lme",
      "pandoc",
      "polypoly",
      "printy",
      "plotmath",
      "ppoints",
      "probs",
      "purrr", "purrr's",
      "readr",
      "RMarkdown",
      "RStanARM",
      "rlang", "rlang's",
      "RStudio", "RStudio's",
      "setNames",
      "stringr",
      "ShinyApps",
      "Stanverse",
      "tidybayes",
      "withr",
      "WrapRmd",
      "tibble", "tribble",
      "md", "Rmd", "README",
      "Ctrl", "csvs",
      # Words
      "si", "th", "ish",
      "variadic",
      "affordances",
      "anterograde",
      "autocomplete", "autocompletion",
      "asymptote",
      "biomes",
      "blogpost",
      "bowtie",
      "coarticulation",
      "completionists",
      "copypasting",
      "detrend",
      "dextrality",
      "else's",
      "eyetracking", "eyetracker",
      "gnosis",
      "grey",
      "gridlines",
      "Hamilitonian",
      "hmmm",
      "hotdog",
      "hoth",
      "idemnotic",
      "intra",
      "intraclass",
      "interrater",
      "iteratively",
      "joyplots",
      "lookahead", "lookaround",
      "monoid",
      "na",
      "noncentered",
      "nibling",
      "offscreen",
      "ooooooh", "ooooh",
      "overfit",
      "posteriori",
      "pre",
      "premade",
      "programmatically",
      "qq",
      "quartile",
      "quosure",
      "recode", "recoding",
      "reknitted",
      "reparameterize", "reparameterization",
      "rewrap", "rewrapping",
      "reproducibility",
      "rescaled", "rescaling",
      "ridgeline",
      "snuck",
      "spline's",
      "ta", "da",
      "theming",
      "toolset",
      "ummm", "umm",
      "understandability",
      "unintuitive",
      "vaganotic",
      "walkthrough",
      "wavily",
      "wiggliness",
      # Names
      "Pokémon", "Unown", "TheSilphRoad", "unown",
      "Sitka",
      "Betancourt",
      "DeCrescenzo", "DeCrescenzo's",
      "Fernald",
      "Fleiss",
      "Foresman",
      "Gabry",
      "Gelman", "gelman",
      "Kross", "Kross's",
      "Hazen",
      "Marchman",
      "McElreath",
      "Mirman", "Mirman's",
      "Shrout",
      "Vehtari", "Avi",
      "Wickham", "Hadley",
      "Wilke",
      "TJ",
      # Math
      "arg", "dt", "lpdf", "sd", "sds", "StudentT", "Zb", "zs",
      # LaTeX / MathJax
      "cdot",
      "frac",
      "intercal",
      "mathbf", "mathrm", "mathtt",
      "operatorname",
      "propto",
      "textrm", "textsf", "texttt"
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
