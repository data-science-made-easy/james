bold_axis <- function(p) {
  print_debug_info(p)

  if (is_yes(p$hline_bold_show)) {
    if (is_set(p$hline_bold)) {
      y_value <- p$hline_bold
    } else if (any(is_bar(p$type) | is_area_stack(p$type)) & p$y_l_lim[1] <= 0 & 0 <= p$y_l_lim[2]) {
      y_value <- 0
    } else {
      y_value <- if (p$turn) p$x_at[1] else p$y_at[1]
    }
    
    abline(h = if (p$turn) NULL else y_value, v = if (p$turn) y_value else NULL, lwd = p$hline_bold_lwd, col = get_col_from_p(p, p$hline_bold_col))
  }
  
  p
}