set_file_name <- function(p) {
  print_debug_info(p)
  time_stamp <- time_stamp()

  # init names:
  if (!is_set(p$file_base) & is_set(p$id))
    p$file_base <- paste0(p$id, "-", time_stamp) # try id
  if (!is_set(p$file_base) & is_set(p$title))
    p$file_base <- paste0(p$title, "-", time_stamp) # try title
  if (!is_set(p$file_base) & is_set(p$tab))
    p$file_base <- paste0(p$tab, "-", time_stamp)  # try tab name
  if (!is_set(p$file_base)) {
    if (is_report(p)) {
      p$file_base <- paste0("james-report-", time_stamp) # just a name
    } else {
      p$file_base <- paste0("james-figure-", time_stamp) # just a name
    }
  }  

  p$file_base <- gsub('[[:punct:] ]+|\n','-', p$file_base) # for linux, OSx, remove punctuation, white space, and newlines
  p$file_base <- stringi::stri_trans_general(p$file_base, "Latin-ASCII")
  ## PDF name
  if (is.na(p$pdf_file)) p$pdf_file <- file.path(p$destination_path, p$pdf_dir, paste0(p$file_base, ".pdf"))
  ## PNG name
  if (is.na(p$png_file)) p$png_file <- file.path(p$destination_path, p$png_dir, paste0(p$file_base, ".png"))
  ## JPG name
  if (is.na(p$jpg_file)) p$jpg_file <- file.path(p$destination_path, p$jpg_dir, paste0(p$file_base, ".jpg"))
  ## SVG name
  if (is.na(p$svg_file)) p$svg_file <- file.path(p$destination_path, p$svg_dir, paste0(p$file_base, ".svg"))
  # ## GIF name
  # if (is.na(p$gif_file)) p$gif_file <- file.path(p$destination_path, p$gif_dir, paste0(p$file_base, ".gif"))


  # Report name
  if (is.na(p$report_file)) p$report_file <- file.path(p$destination_path, p$report_dir, paste0(p$file_base, ".Rmd"))

  return(p)
}