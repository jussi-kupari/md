# Create knitting function
knit_notebook <- function(file) {
  ezknitr::ezknit(
    here::here(file),
    wd = here::here(),
    out_dir = here::here("Notebooks"),
    verbose = TRUE,
    keep_md = TRUE,
    keep_html = FALSE
  )
}

# Style all Notebooks
styler::style_dir(here::here())

# Knit all Notebooks
fs::dir_ls() |>
  stringr::str_subset(".Rmd$") |>
  purrr::walk(knit_notebook)
