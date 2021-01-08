axes <- function(p) {
  print_debug_info(p)
  
  # IF TURN, PREPARE
  if (p$turn) {
   q <- p
   q$x_at <- p$y_at
   q$y_at <- p$x_at
   q$x_lab <- p$y_lab
   q$y_lab <- p$x_lab
   q$x_lab_font_size <- p$y_lab_font_size
   q$y_lab_font_size <- p$x_lab_font_size
   q$x_lab_col <- p$y_lab_col
   q$y_lab_col <- p$x_lab_col
   q$x_top_lab_col <- p$y_r_lab_col
   p <- q
  }
  
  # x-axis ticks for years, months, weeks, days
  if (is_set(p$x_ticks_date)) {
    for (i in seq_along(p$x_ticks_date)) {
      sorted_units <- TIME_UNITES[which(is.element(TIME_UNITES, p$x_ticks_date))]
      
      # Determine right starting and ending point for ticks
      xl <- p$x_lim[1]
      xr <- p$x_lim[2]
      if ("years" == sorted_units[i]) {
        x_left  <- ceiling(xl)
        x_right <-   floor(xr)
      } else if ("quarters" == sorted_units[i]) {
        x_left  <- ceiling(4 * xl) / 4
        x_right <-   floor(4 * xr) / 4
      } else if ("months" == sorted_units[i]) {
        x_left  <- ceiling(12 * xl) / 12
        x_right <-   floor(12 * xr) / 12
      } else if ("weeks" == sorted_units[i]) {
        x_left  <- ceiling(52 * xl) / 52
        x_right <-   floor(52 * xr) / 52
      } else if ("days" == sorted_units[i]) {
        x_left  <- ceiling(365 * xl) / 365
        x_right <-   floor(365 * xr) / 365
      }
      x_at_ticks   <- seq(lubridate::date_decimal(x_left), lubridate::date_decimal(x_right), sorted_units[i])
      tick_length  <- if (1 < length(p$x_ticks_date)) p$x_ticks_length_date / i else p$x_ticks_length
      axis(if (p$turn) 2 else 1, at = lubridate::decimal_date(x_at_ticks), labels = NA, lwd = 0, lwd.ticks = p$x_ticks_lwd, line = p$x_ticks_vshift, tck = tick_length, xpd = T, p$x_ticks_col)
    }
  } else { # x-axis default ticks
    if (p$x_axis_show) axis(if (p$turn) 2 else 1, at = p$x_ticks, labels = NA, lwd = 0, lwd.ticks = p$x_ticks_lwd, line = p$x_ticks_vshift, tck = p$x_ticks_length, xpd = T, col = p$x_ticks_col)
  }
    
  # x-axis labels (don't confuse with x-title)
  if (p$x_axis_show) {
    text(x = p$x_at, par("usr")[3] - (diff(par("usr")[3:4]) / par("pin")[2] / cm(1)) * p$x_lab_v_shift, labels = set_newline(p$x_lab), srt = p$x_lab_rotation, xpd = TRUE, cex = p$x_lab_font_size, family = p$font, col = get_col_from_p(p, p$x_lab_col), adj = if (45 <= p$x_lab_rotation) 1 else c(.5, 1))
  }
  
  # x-axis at top
  # plot extra x-axis
  if (is.element("x-top", p$style)) {
    if (p$turn) {
      text(x = p$x_at, par("usr")[4] + (diff(par("usr")[3:4]) / par("pin")[2] / cm(1)) * p$x_lab_v_shift, labels = set_newline(p$y_r_lab), srt = p$x_lab_rotation, xpd = TRUE, cex = p$x_lab_font_size, adj = if (0 == p$x_lab_rotation) c(.5, 0) else 1, family = p$font, col = get_col_from_p(p, p$x_top_lab_col))
    } else error_msg("x-top withouth turn: TODO")
  }
  
  if (is.element("y-right", p$style)) {
    if (p$turn) {
      error_msg("y-right withouth turn: TODO")
    } else {
      axis(4, at = p$y_at, labels = set_newline(p$y_r_lab), las = 2, lwd = 0, mgp = c(0, p$y_lab_margin_right * p$width / cm(1) * 8.55, 0), cex.axis = p$y_lab_font_size, family = p$font, col.axis = get_col_from_p(p, p$y_r_lab_col)) # y_r_lab
    }
  }
  
  # y-axis labels (don't confuse with y-title)
  # axis(2, at = p$y_at, labels = set_newline(p$y_lab), las = 2, lwd = 0, lwd.ticks = 0, xpd = TRUE, cex.axis = p$y_lab_font_size, adj = p$labels_margin_left, mgp = c(0, p$y_lab_margin_right * p$width / cm(1) * 8.55, 0), family = p$font, col.axis = get_col_from_p(p, p$y_lab_col)) # TODO adj heeft geen functie hier en kan weg (naar verluidt << testen)
  tmp_axis <- function(...) axis(2, las = 2, lwd = 0, lwd.ticks = 0, xpd = TRUE, cex.axis = p$y_lab_font_size, adj = p$labels_margin_left, mgp = c(0, p$y_lab_margin_right * p$width / cm(1) * 8.55, 0), family = p$font, ...)
  
  if (p$y_axis_show) Map(tmp_axis, at = p$y_at, labels = set_newline(p$y_lab), col.axis = get_col_from_p(p, p$y_lab_col))
  
  if (is_set(p$group)) { #is.element(BOX, p$style) & 
    if (p$turn) {
      lheight <- par("csi") * cm(1)
      this_line <- (p$group_label_offset) / lheight
      mtext(side = 2, at = p$group_x, text = set_newline(unique(p$group)), col = get_col_from_p(p, p$group_col), las = 2, adj = 0, line = this_line, cex.axis = p$group_font_size, font = hack_font(p, p$group_font), family = p$font)
    } else {
      group_x_mean <- NULL
      unique_group_names <- unique(p$group)
      for (i in seq_along(unique_group_names)) {
        gr <- unique_group_names[i]
        index <- which(gr == p$group)
        group_x_mean[i] <- mean(p$x_at[index])
      }
      text(x = group_x_mean, par("usr")[3] - (diff(par("usr")[3:4]) / par("pin")[2] / cm(1)) * p$x_lab_group_v_shift, labels = set_newline(unique_group_names), xpd = TRUE, cex = p$group_font_size, font = hack_font(p, p$group_font), col = get_col_from_p(p, p$group_col), family = p$font, adj = c(.5, 1))
    }
  }
  
  p
}






























