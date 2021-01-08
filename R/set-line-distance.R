set_line_distance <- function(p) {
  print_debug_info(p)
  
  # Set line distance
  par(lheight = p$line_distance)
  
  p
}