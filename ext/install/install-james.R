rm(list = ls()); if (file.exists("m:/")) setwd('m:/p_james/dev') else setwd('~/Dropbox/cpb/git/james')
cpblib::use_cpblib()
source("initialize-james.R")

# Set parameters in 'ext/james-base-settings.xlsx' tab 'install':
# install_local = n
# install_status_production = y

SAFE <- function() !(get_param("install_local") & !get_param("install_status_production")) # We don't want a local dev install.

if (SAFE()) print(paste("Install location:", fix_path(''))) else stopifnot(SAFE())


# (Re-)create installation dir
if (SAFE()) {
  unlink(fix_path('', use_local_path = T), recursive = T)
  dir.create(fix_path('', use_local_path = T), showWarnings = F, recursive = T)

  # Copy stuff to install folder
  file.copy(from = c("ext", "R"), to = fix_path('', use_local_path = T), recursive = T)
}


#
## The manual
#
# Let's first create a manual
report_output_format <- "html"
report_file_name <- paste0("james-", get_param("james_version"), "-manual", if (get_param("install_status_production")) "" else paste0("-", time_stamp()), ".", report_output_format)


#
#
#       # TODO james-manual.xlsx verplaatsen naar ext/report/manual/
#
#
generated_report_path <- plot("james-manual.xlsx", id = "report", debug = F, open = F) # added , id = "report"

# Copy the manual to installation path
file.copy(from = generated_report_path, to = file.path(fix_path('', use_local_path = T), report_file_name), overwrite = T)
# And copy its data (xlsx, RData, R-script) files
examples_from <- file.path(get_param("destination_path"), get_param("report_dir"), "examples")
examples_to   <- file.path(fix_path('', use_local_path = T))
dir.create(examples_to, showWarnings = F, recursive = T)
file.copy(from = examples_from, examples_to, recursive = T)

# Create scripts with right paths
for (f in get_param("manual_process_templates")) {
  script_file <- file.path(fix_path('', use_local_path = T), f)
  
  # inject version number for *.bat, *.sh
  is_bat <- "bat" == tools::file_ext(script_file)
  is_sh  <- "sh"  == tools::file_ext(script_file)
  if (is_bat | is_sh) {
    script_file <- paste0(tools::file_path_sans_ext(script_file), "-", get_param("james_version"), ".", tools::file_ext(script_file))
  }
  knitr::knit(paste0("ext/script/", f, ".template"), output = script_file)
  Sys.chmod(script_file, mode = "0755", use_umask = T)
  
  # Copy one dir higher for ease of use
  cat("Copy bat/sh one level above install root...\n\n")
  if (is_bat & !get_param("install_local")) file.copy(from = script_file, to = file.path(dirname(script_file), ".."), overwrite = T)
  if (is_sh & get_param("install_local")) file.copy(from = script_file, to = file.path(dirname(script_file), ".."), overwrite = T)
}









