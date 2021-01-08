plot.character <- function(string, ...) {
  # string may be file or cbs-url
  if (file.exists(string)) { # try file
    lst <- import(xlsx = string, ...)
    
    if (0 == length(lst)) {
      print_warning("Nothing to do...")
      return(invisible(NULL)) # nothing to do
    }
    
    # If lst contains report(s), only handle report(s). Else, plot lst.
    index_report <- which(sapply(lst, function(p) is_report(p) & !is_no(p$create)))
    if (1 < length(index_report)) error_msg("Currently, James can only produce one report per xlsx-file.")
    if (length(index_report)) { # We want a report
      return(plot(lst[[index_report]]))
    } else { # We just want figures
      return(plot(lst))
    }
    
  } else { # try cbs
    return(plot(james(data = cbs(string), ...)))
  }
}

plot.matrix <- function(mat, ...) {
  plot(james(data = mat, ...))
}

plot.data.frame <- function(mat, ...) {
  plot(james(data = mat, ...))
}

plot.list <- function(lst, ...) {
  paths <- NULL
  for (p in lst) {
    # Overwrite parameters p
    P <- list(...)
    if (!missing(...)) for (i in seq_along(P)) p[[names(P)[i]]] <- P[[i]]
    
    paths <- c(paths, plot(p, figs = lst))
  }
  
  paths
}

plot.james <- function(p, ...) {
  print_debug_info(p)

  # Overwrite parameters p
  P <- list(...)
  if (!missing(...)) for (i in seq_along(P)) p[[names(P)[i]]] <- P[[i]]
  
  # Skip if !create
  if (is_no(p$create)) {
    print_progress(p, "Skipping '", p$id, "'.")
    return()
  }
  
  # Check for report
  if (is_report(p)) { # "Plot" the report
    print_progress(p, "Creating report...")
    path <- create_report(p)
    return(path)
  }

  # If creating report, (only) produce PDF
  if (creating_report_now()) p <- set_report_export(p) # TODO is dit nodig?
  
  # First do pre-processing according to function order in james-base-settings file
  if (!p$gif) p <- preprocess(p)

  if (p$pdf) {
    # p$pdf_active_hack_font <- TRUE
    print_progress(p, "Creating ", basename(p$pdf_file), "...")
    showtext::showtext_auto(enable = FALSE)
    plot_james_pdf(p)
    # p$pdf_active_hack_font <- FALSE
    print_progress(p, "Embedding fonts in pdf...")
    extrafont::embed_fonts(p$pdf_file)
    if (!creating_report_now() & (p$debug | is_yes(p$open))) {
      print_progress(p, p$pdf_file)
      system(paste("open", p$pdf_file), wait = FALSE)
    }
  }
  if (p$png) {
    print_progress(p, "Creating ", basename(p$png_file), "...")
    p <- init_font(p)
    showtext::showtext_auto(enable = TRUE)
    plot_james_png(p)
    showtext::showtext_auto(enable = FALSE) # double check
    if (!creating_report_now() & (p$debug | is_yes(p$open))) {
      print_progress(p, p$png_file)
      system(paste("open", p$png_file))
    }
  }
  if (p$jpg) {
    print_progress(p, "Creating ", basename(p$jpg_file), "...")
    p <- init_font(p)
    showtext::showtext_auto(enable = TRUE)
    plot_james_jpg(p)
    showtext::showtext_auto(enable = FALSE) # double check
    if (!creating_report_now() & (p$debug | is_yes(p$open))) {
      print_progress(p, p$jpg_file)
      system(paste("open", p$jpg_file))
    }
  }
  if (p$svg) plot_james_svg(p)
  if (p$gif) {
    print_progress(p, "Creating ", basename(p$gif_file), "...")
    if (is_set(p$format)) if (!is.element(p$format, c("html", "ioslides"))) error_msg("You try to create a gif-file for a report that is not 'html' or 'ioslides'. Please choose another format. E.g. set gif = F and png = T.")
    p <- init_font(p)
    showtext::showtext_auto(enable = TRUE)
    gif_file <- plot_james_gif(p)$gif_file
    p$gif_file <- gif_file
    showtext::showtext_auto(enable = FALSE) # double check
    if (!creating_report_now() & (p$debug | is_yes(p$open))) {
      print_progress(p, p$gif_file)
      system(paste("open", p$gif_file))
    }
  }

  if (!any_plot_export(p)) p <- plot_james_internal(p)
  
  paths <- NULL
  if (p$png) paths <- c(paths, p$png_file)
  if (p$pdf) paths <- c(paths, p$pdf_file)
  if (p$jpg) paths <- c(paths, p$jpg_file)    
  if (p$svg) paths <- c(paths, p$svg_file)
  if (p$gif) paths <- c(paths, p$gif_file)
  if (is.null(paths)) paths <- p$png_file

    # pp <<- p
  # for (i in seq_along(paths))
  #
  #   rmd_link_to_figure <- if ("pdf" == report$report_format) paste0("\\includegraphics{", p$figure_path, "}") else paste0("![](", p$figure_path, ")")

  return(paths)
}

plot_james_pdf <- function(p) {
  print_debug_info(p)
  create_dir_for_file(p$pdf_file)
  # extrafont::font_import() # ONLY ONCE
  extrafont::loadfonts(quiet = TRUE)
  # if ("windows" == .Platform$OS.type) loadfonts(device = "win")
  # if (!is.element("rijk", names(pdfFonts()))) {
  #   rijk <- pdfFonts()$RijksoverheidSansText
  #   rijk$metrics <- rijk$metrics[c(1,3,2,4,5)]
  #   pdfFonts(rijk = rijk)
  # }
  on.exit(dev.off())
  pdf(p$pdf_file, width = p$width / cm(1), height = p$height / cm(1), pointsize = p$font_size, useDingbats = FALSE, family = p$font)
  p <- plot_james_internal(p)
  
  todo(p, "Fix return value of plot")
  # return(p)
}

plot_james_png <- function(p) {
  print_debug_info(p)
  create_dir_for_file(p$png_file)
  on.exit({dev.off(); showtext::showtext_auto(enable = FALSE)})
  p$font_size <- p$font_size * 1.7
  png(p$png_file, width = p$width / cm(1), height = p$height / cm(1), pointsize = p$font_size, unit = "in", res = p$resolution, type = "cairo")
  
  p <- plot_james_internal(p)
 todo(p, "Fix return value of plot")
  # return(p)
}

plot_james_jpg <- function(p) {
  print_debug_info(p)
  create_dir_for_file(p$jpg_file)
  on.exit({dev.off(); showtext::showtext_auto(enable = FALSE)})
  jpeg(p$jpg_file, width = p$width / cm(1), height = p$height / cm(1), pointsize = p$font_size_jpeg, unit = "in", res = p$resolution, quality = p$quality, type = "cairo") # , family = p$font seems not to work here; instead inject everywhere apart
    # p <- preprocess(p)
    p <- plot_james_internal(p)
  # return(p)
}

plot_james_svg <- function(p) {
  print_debug_info(p)
  create_dir_for_file(p$svg_file)
  # extrafont::font_import() # ONLY ONCE
  extrafont::loadfonts(quiet = TRUE)
  # if ("windows" == .Platform$OS.type) loadfonts(device = "win")
  # if (!is.element("rijk", names(pdfFonts()))) {
  #   rijk <- pdfFonts()$RijksoverheidSansText
  #   rijk$metrics <- rijk$metrics[c(1,3,2,4,5)]
  #   pdfFonts(rijk = rijk)
  # }
  on.exit(dev.off())
  svg(p$svg_file, width = p$width / cm(1), height = p$height / cm(1), pointsize = p$font_size, family = p$font)
  p <- plot_james_internal(p)
  
  todo(p, "Fix return value of plot")
  # return(p)
}

plot_james_gif <- function(p) {
  print_debug_info(p)
  create_dir_for_file(p$gif_file)
  on.exit({if (length(dev.list())) dev.off();showtext::showtext_auto(enable = FALSE)})
  # jstop(p)
  # Make end figure to derive y_lim
  p$gif = F
  p$png = F
  data_orig <- p$data
  p_orig <- p
  p_gif_pngs <- p$gif_pngs
  p <- preprocess(p)
  y_lim <- p$y_lim
  
  p <- p_orig
  p$y_lim <- y_lim
  p$gif_pngs <- p_gif_pngs
  dir.create(file.path(p$destination_path, p$gif_dir, "pngs-for-gif"), showWarnings = F, recursive = T)
  if (!is_set(p$gif_pngs)) p$gif_pngs <- file.path(p$destination_path, p$gif_dir, "pngs-for-gif", paste0(p$file_base, "-", 1:p$gif_n_frames, ".png"))
  for (i in 1:p$gif_n_frames) {
    p$png_file <- p$gif_pngs[i]
    p$data <- (i - 1) / (p$gif_n_frames - 1) * data_orig
    plot(p, png = T, gif = F, open = F)
  }

  ## GIF name
  if (is.na(p$gif_file)) p$gif_file <- file.path(p$destination_path, p$gif_dir, paste0(p$file_base, ".gif"))
  
  gifski::gifski(p$gif_pngs, p$gif_file, delay = p$gif_delay, loop = p$gif_loop)
  
  todo(p, "gifff")
  
  p
}





























