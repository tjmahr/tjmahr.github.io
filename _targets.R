library(targets)
library(tarchetypes)
library(stringr)
library(knitr)

# dont_forget_these_packages <- c(
#   "ggmcmc", "ellipse", "ggrepel", "pryr", "lobstr", "conflicted",
#   "reticulate", "cowplot", "gamair", "microbenchmark", "polypoly",
#   "irr", "psych", "DiagrammeR", "babynames", "DiagrammeRsvg",
#   "uuid", "gratia", "lazyeval", "cowplot"
# )
#
# missing <- setdiff(dont_forget_these_packages, installed.packages()[, 1])
# if (length(missing)) {
#   install.packages(missing)
#   remotes::install_github("tjmahr/iccbot")
#   remotes::install_github("tjmahr/fillgaze")
#   remotes::install_github("tjmahr/solarizeddocx")
# }

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

make_post_name <- function(xs, prefix = "post") {
  candidates <- xs %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    paste0(prefix, "_", .) %>%
    stringr::str_replace_all("\\W", "_") %>%
    tolower()
  stopifnot(unique(candidates) == candidates)
  candidates
}

list2 <- tibble::lst

posts <- list2(
  post = paths_current_posts,
  name = make_post_name(post, "rmd"),
  sym_name = rlang::syms(name),
  # path_md = paths_current_posts %>%
  #   basename() %>%
  #   stringr::str_replace(".Rmd$", ".md"),
  name_md = paths_current_posts %>%
    basename() %>%
    stringr::str_replace(".Rmd$", ".md") %>%
    make_post_name("md")
)

drafts <- list2(
  post = paths_draft_posts,
  name = make_post_name(post, "draft_rmd"),
  sym_name = rlang::syms(basename(name)),
  name_md = post %>%
    basename() %>%
    stringr::str_replace(".Rmd$", ".md") %>%
    make_post_name("draft_md")
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
      # unicode,
      "Pokémon", "ツ", "café",
      "Sβ", "Xβ", "α", "β", "θ", "λ", "σ",
      # URLs / paths
      "shinyapps", "io", "https", "tristan", "icc", "jpg", "png", "embo",
      # Abbreviations
      "APA",
      "PPV", "NPV",
      "btw",
      "DS",
      "ICC",
      "AST",
      "uuid",
      "LOESS",
      "JSON", "YAML",
      # Code names
      "bayesplot",
      "brms",
      "callr",
      "Consolas",
      "cowplot",
      "DiagrammeR",
      "dplyr", "dplyring",
      "dnorm", "qnorm",
      "dotenv",
      "flexdashboard",
      "gamlss",
      "gratia",
      "ggplot", "aes",
      "gganimate", "ggmcmc", "ggridges",
      "GitHub",
      "iccbot",
      "Javascript",
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
      "reprex",
      "Renviron",
      "RMarkdown",
      "RStanARM",
      "rlang", "rlang's",
      "RStudio", "RStudio's",
      "setNames",
      "solarized", "solarizeddocx",
      "stringr",
      "ShinyApps",
      "Stanverse",
      "styler", "grkstyle",
      "tidybayes",
      "tidyverse",
      "withr",
      "WrapRmd",
      "tibble", "tribble",
      "md", "Rmd", "README",
      "Ctrl", "csv", "csvs", "docx",
      # Words
      "si", "th", "ish", "ing",
      "variadic",
      "avant", "la", "lettre",
      "affordances",
      "anterograde",
      "autocomplete", "autocompletion",
      "asymptote",
      "bandaid",
      "biomes",
      "blogpost",
      "bowtie",
      "coarticulation",
      "comboing",
      "completionists",
      "copypasting",
      "counterintuitive",
      "customization",
      "deadends",
      "debuff",
      "detrend",
      "dextrality",
      "else's",
      "epred", "pred", "ictions", "xpectations",
      "eyetracking", "eyetracker",
      "falsiness", "falsy", "fetishize",
      "gameplay",
      "gnosis",
      "grey",
      "gridded", "gridlines",
      "hacky",
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
      "lookahead", "lookaround", "lockdown",
      "monoid",
      "na",
      "noncentered",
      "nibling",
      "nullish",
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
      "ss",
      "synergistically",
      "ta", "da",
      "theming",
      "toolset", "truthy", "tryhard",
      "ummm", "umm",
      "understandability",
      "unintuitive",
      "unintrusive",
      "vaganotic",
      "vectorized", "vectorization",
      "walkthrough",
      "wavily",
      "wiggliness",
      # Puns?
      "mooooooo",
      "pwetty", "pwease",
      "qquines",
      "Snecko", "snecko", "steppo",
      # Names
      "Pokémon", "Unown", "TheSilphRoad", "unown",
      "Sitka",
      "Betancourt",
      "Buie",
      "Covid", "COVID",
      "Dahly",
      "DeCrescenzo", "DeCrescenzo's",
      "Fernald",
      "Fleiss",
      "Foresman",
      "Gabry",
      "Gelman", "gelman",
      "Jorbs",
      "Kross", "Kross's",
      "Hazen",
      "Marchman",
      "McElreath",
      "Mirman", "Mirman's",
      "Shrout",
      "usernameequalspants",
      "Vehtari", "Avi",
      "Whitfill",
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
