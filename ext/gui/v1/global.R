url_to_manual <- "james-dev-2020-10-21-manual.html"

james_settings_file <- "~/Dropbox/cpb/git/james/ext/james-base-settings.xlsx"
globals <- NULL
for (sh in openxlsx::getSheetNames(james_settings_file)) globals <- rbind(globals, openxlsx::read.xlsx(james_settings_file, sheet = sh))