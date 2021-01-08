gridlines <- function(p) {
  print_debug_info(p)
    
  abline(h = if (p$turn) NULL else p$y_at, v = if (p$turn) p$x_at else NULL, lwd = p$grid_lines_lwd, col = p$grid_lines_col)

  p
}
