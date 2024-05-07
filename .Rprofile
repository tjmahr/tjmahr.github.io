options(WrapRmd.width = 72L)

list_images <- function() {
  fs::dir_ls("assets/images/")
}

tar_make_post_menu <- function(choice = NULL) {
  d <- targets::tar_manifest()
  p <- d |>
    subset(startsWith(name, "md")) |>
    sort_by(~name, decreasing = TRUE)

  if (is.null(choice)) {
    choice <- utils::menu(p$name, title = "Choose a post to make")
  }
  if (choice != 0) {
    targets::tar_make(p$name[choice])
  }
}
