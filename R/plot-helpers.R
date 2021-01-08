any_plot_export <- function(p) any(p$pdf, p$png, p$jpg, p$svg, p$gif)

create_dir_for_file <- function(file_name) {
  dir.create(dirname(file_name), showWarnings = FALSE, recursive = TRUE)
  dirname(file_name)
}