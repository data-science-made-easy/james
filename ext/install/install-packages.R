# Packages you need:
install.packages("devtools")
devtools::install_github("data-science-made-easy/cpblib")
install.packages("openxlsx")
install.packages("sysfonts")
install.packages("showtext")
install.packages("rmarkdown")
install.packages("lubridate")
install.packages("stringr")
install.packages("kableExtra") # report
install.packages("whoami")     # report
install.packages("sf")         # geo
install.packages("png")        # logo
install.packages("jpeg")       # jpg
install.packages("gifski")     # gifs
install.packages("cbsodataR")  # CBS
install.packages("dplyr")      # CBS
install.packages("plyr")       # Joint cbs with rbind to normal data
install.packages("tidyr")      # transform matrix
install.packages("circlize")   # colors

# GUI
install.packages("shiny")
install.packages("shinydashboard")
install.packages("rhandsontable")  # to edit table in place
install.packages("shinyglide")     # voor wizard

# FONT
# Alleen bij nieuwe R-versie op Windows:
# Admin op remote-desktop, dus voor alle R-users
install.packages("extrafont")    # Fonts in pdf
# install.packages("extrafontdb")  # of gaat deze vanzelf? ja is dependency
extrafont::font_import(paths = "M:/p_r_packages/fonts/")
extrafont::loadfonts(quiet = TRUE)
extrafont::loadfonts(device = "win")

# FONT op Mac
extrafont::font_import(paths = "~/Dropbox/cpb/RijksFont/Rijksoverheid_Fonts_2_0-OTF/RijksoverheidSans_2_0") 

# extrafont::font_import() # werkt wel