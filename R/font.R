#quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", "Avenir Black Oblique"))


#Type1Font("RijksoverheidSansText", metrics = rep("/Users/mdijkstra/Library/Fonts/RijksoverheidSansText-Regular_2_0.ttf", 4))
# extrafont::loadfonts(quiet = TRUE)

# rijk <- Type1Font("RijksoverheidSansText", metrics = rep("M:/p_james/fonts/RijksoverheidSansText-Regular_2_0.ttf", 4))


# Alleen bij installatie extrafont::font_import(paths = "M:/p_io/Medewerkers/mdk/fonts") # PAD MOET TOEGANKELIJK ZIJN!  Het gaat volgens mij om de ttf files!
# extrafont::loadfonts(quiet = TRUE)

init_font <- function(p) {
  print_debug_info(p)
  # p <- list(font = "RijksoverheidSansText")

  # if (on_windows())     path_prefix <- "M:/p_james/fonts/RijksoverheidSansText-"
  # if (on_rekenserver()) path_prefix <- "~/m:/p_james/fonts/RijksoverheidSansText-"
  # if (on_mac())         path_prefix <- "~/Library/Fonts/RijksoverheidSansText-"
  # rijk_regular    <- paste0(path_prefix, "Regular_2_0.ttf")
  # rijk_italic     <- paste0(path_prefix, "Italic_2_0.ttf")
  # rijk_bold       <- paste0(path_prefix, "Bold_2_0.ttf")
  # rijk_bolditalic <- paste0(path_prefix, "BoldItalic_2_0.ttf")

  # For png(?):
  if ("RijksoverheidSansText" == p$font & !is.element(p$font, sysfonts::font_families())) {
    if (on_windows())     path_prefix <- "M:/p_james/fonts/RijksoverheidSansText-"
    if (on_rekenserver()) path_prefix <- "~/m:/p_james/fonts/RijksoverheidSansText-"
    if (on_mac())         path_prefix <- "~/Library/Fonts/RijksoverheidSansText-"
    rijk_regular    <- paste0(path_prefix, "Regular_2_0.ttf")
    rijk_italic     <- paste0(path_prefix, "Italic_2_0.ttf")
    rijk_bold       <- paste0(path_prefix, "Bold_2_0.ttf")
    rijk_bolditalic <- paste0(path_prefix, "BoldItalic_2_0.ttf")
    if (file.exists(rijk_regular)) sysfonts::font_add("RijksoverheidSansText", regular = rijk_regular, bold = rijk_bold, italic = rijk_italic, bolditalic = rijk_bolditalic)
  }
  
  # For pdf: TODO moet hier niet ook bold, italic, enz staan?
  if (on_windows() & !is.element("RijksoverheidSansText", names(pdfFonts()))) pdfFonts(RijksoverheidSansText = Type1Font("RijksoverheidSansText", metrics = rep("M:/p_james/fonts/RijksoverheidSansText-Regular_2_0.ttf", 4))) 
  
  # if (on_mac()) pdfFonts(RijksoverheidSansText = Type1Font("RijksoverheidSansText", metrics = c(rijk_regular, rijk_bold, rijk_italic, rijk_bolditalic)))
  
  if (!on_windows() & "RijksoverheidSansText" == p$font) quartzFonts(RijksoverheidSansText = c("RijksoverheidSansText-Regular", "RijksoverheidSansText-Bold", "RijksoverheidSansText-Italic", "RijksoverheidSansText-BoldItalic"))
    
  p
}
