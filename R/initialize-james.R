r_files <- list.files('R', pattern = "*.R", full.names = T)
r_files <- r_files[-which(r_files == "R/initialize-james.R")]
for (f in r_files) source(f)

#
## INIT: READ CONFIG FILE FROM PACKAGE
#
james_settings_file <- "ext/james-base-settings.xlsx"

globals <- NULL
for (sh in openxlsx::getSheetNames(james_settings_file)) globals <- rbind(globals, openxlsx::read.xlsx(james_settings_file, sheet = sh))

# CLEAN
unlink(get_param("destination_path"), recursive = T); dir.create(get_param("destination_path"))


# set_globals("dir_output_default", "./generated")
