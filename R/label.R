is_label <- function(type) is.element(type, LABEL)

label_pre <- function(p) {
  print_debug_info(p)
  if (!is_set(p$label)) return(p)  
  
  # If only one value for aligment, then use this value everywhere
  if (1 == length(p$label_align)) p$label_align <- rep(p$label_align, length(p$label))
  if (1 == length(p$text_offset)) p$text_offset <- rep(p$text_offset, length(p$label))
    
  p
}

label <- function(p) {
  print_debug_info(p)
  if (!is_set(p$label)) return(p)
    
    
  for (i in seq_along(p$label)) {
    y <- p$y[, p$label_series_n]
    
    # handle turn
    x <- if (p$turn) y   else p$x
    y <- if (p$turn) p$x else y
    
    this_align <- if (0 == p$label_align[i]) NULL else p$label_align[i]
    
    text(x[i], y[i], p$label[i], family = p$font, pos = this_align, col = get_col_from_p(p, p$text_col), srt = p$text_rotation, font = p$text_font_style, cex = p$label_font_size, offset = p$label_offset)
  }
  
  p
}