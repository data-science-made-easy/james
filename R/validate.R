j_validate <- function(p) {
  print_debug_info(p)

  # Loop over given params and warn if not known
  unkown_params <- setdiff(names(p), globals$param)
  if (length(unkown_params)) print_warning("Uknown parameters: ", paste(unkown_params, collapse = ", "))
  
  
  if (any(!is.element(p$style, styles()))) error_msg("Not all of your styles, ", paste0(p$style, collapse = ", "), ", are part of the available styles: ", paste0(styles(), collapse = ", "))

  todo(p, "TODO make types()")
  todo(p, "TODO rename styles to style")
  existing_types_string <- get_param("type", style = "example")
  existing_types <- as_char_vec(existing_types_string, sep = SEP0)
  non_existing_type_index <- which(!(p$type %in% existing_types))
  if (length(non_existing_type_index)) error_msg("type '", p$type[non_existing_type_index], "' not found in the 'example'-column of james-base-settings.xlsx. Please update the xlsx-file or use one of the following: ", existing_types_string, ".")
    
  # hist
  if (is_hist(p)) {
    if (1 < ncol(p$data)) error_msg("Currently 'style = histogram' only allows one data column.")
  }
  
  n_whiskers <- length(which(is_whisker(p$type)))
  if (0 != n_whiskers %% 2) error_msg("'whisker' occurs ", length(n_whiskers), " times in as 'type'. This should be an even number.")
    
  n_areas <- length(which(is_area(p$type)))
  if (1 != n_areas & 0 != n_areas %% 2) error_msg("'area' occurs ", length(n_areas), " times in as 'type'. This should be an even number.")
 
  # AXES
  for (prefix in c("", "x_", "y_")) {
    align <- paste0(prefix, "title_align")
    if (!is.element(get_parsed(p, align), ALIGNMENT)) error_msg("'", align, "' (", get_parsed(p, align), ") should be one of ", paste(ALIGNMENT, collapse = ", "))
  }

  # Right axis: check areas and bars are on same axis
  if (any(is_yr(p))) { # we have right y-axis
    for (i in 1:2) {
      index <- list(which(is_bar_stack(p$type)), which(is_area_stack(p$type)))[[i]]
      if (length(index)) { # we have bars stacked; allow a stack only on same axis
        if (length(index) != length(which(is_yr(p, index))) & length(index) != length(which(is_yl(p, index)))) {
          terms <- c("bars", "areas")
          error_msg("Stacked ", terms[i], " should be all on either the left y-axis or all on the right y-axis. Please update parameter 'type' or parameter 'y_axis'.")
        }
      }
    }
  }
 
  # ID may not have spaces
  if (is_set(p$id)) if (grepl("\\s+", p$id, perl = T)) error_msg("Parameter 'id' does not allow a whitespace. Please remove the whitespace or replace it with e.g. '-'. Problematic id: '", p$id, "'.")
 
  # FONT
  # if (!is.element(p$font, extrafont::fonts())) error_msg("Your font '", p$font, "' is unkown. Please choose from: ", paste(extrafont::fonts(), collapse = ", "))
  # if (!is.element(p$font, names(quartzFonts()))) error_msg("Your font '", p$font, "' is unkown. Please choose from: ", paste(names(quartzFonts()), collapse = ", "))
 
  # ORDER
  if (is_set(p$order) & is_set(p$order_name)) error_msg("You have specified both 'order' (", p$order, ") as 'order_name' (", p$order_name, "). James doesn't know which to take. Please leave at least one empty.")
 
  # # DATA
  # series_name <- colnames(p$data)[-1]
  # series_name_duplicated <- series_name[duplicated(series_name)]
  # if (length(series_name_duplicated)) error_msg("Some series occur twice in your data tab '", p$tab, "' ('", paste(series_name_duplicated, collapse = ", "), "'). Please remove or rename.")
 
  return(p)
}