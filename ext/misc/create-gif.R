rm(list = ls()); if (file.exists("m:/")) setwd('m:/p_james/dev') else setwd('~/Dropbox/cpb/git/james')
cpblib::use_cpblib()
source("initialize-james.R")

get_walk <- function(xb, yb, xe, ye, n = 10) { # vectors xbegin, ybegin, xend, yend; n steps
  n <- n - 1 # begin is start
  dx <- (xe - xb) / n
  dy <- (ye - yb) / n
  
  x_mat <- x_cur <- xb
  y_mat <- y_cur <- yb
  
  for (i in 1:n) {
    x_cur <- x_cur + dx
    x_mat <- cbind(x_mat, x_cur)
    y_cur <- y_cur + dy
    y_mat <- cbind(y_mat, y_cur)    
  }
  
  return(list(x = x_mat, y = y_mat))
}

n = 30
j = get_walk(xb = 0:3 / 3, yb = (1 - .3 * 0/4) * 0:3 / 3, xe = 0.0 + .2 * c(.35, 1, 1, .35),         ye = 0.0 + .22 * c(0, 0.2, 1, 1), n)
a = get_walk(xb = 0:3 / 3, yb = (1 - .3 * 1/4) * 0:3 / 3, xe = 0.2 + .2 * c(0, .5, 1, .25),          ye = 0.2 + .2 * c(0, 1, 0, .5), n)
m = get_walk(xb = 0:4 / 4, yb = (1 - .3 * 2/4) * 0:4 / 4, xe = 0.4 + .2 * c(0, 0, .5, 1, 1),         ye = 0.4 + .2 * c(0, 1, .75, 1, 0), n)
e = get_walk(xb = 0:4 / 4, yb = (1 - .3 * 3/4) * 0:4 / 4, xe = 0.6 + .2 * c(1, 0, .25, 0, 1),        ye = 0.6 + .2 * c(0, 0, .5, 1, 1), n)
s = get_walk(xb = 0:5 / 5, yb = (1 - .3 * 4/4) * 0:5 / 5, xe = 0.8 + .2 * c(0, .95, .95, 0, 0, .95), ye = 0.8 + .18 * c(0, 0, .5, .5, 1, 1), n)

# mat <- matrix(NA, nrow = nrow(j$x) + nrow(a$x) + nrow(m$x) + nrow(e$x) + nrow(s$x), ncol = 6)
# offset = 0
# for (i_let in 1:5) {
#   let <- list(j,a,m,e,s)[[i_let]]
#   npoints = nrow(let$x)
#   mat[offset + 1:npoints, 1] <- let$x[, i]
#   mat[offset + 1:npoints, 1 + i_let] <- let$y[, i]
#   offset <- offset + npoints
# }

for (i in 1:n) {
  mat <- matrix(NA, nrow = nrow(j$x) + nrow(a$x) + nrow(m$x) + nrow(e$x) + nrow(s$x), ncol = 6)
  offset = 0
  for (i_let in 1:5) {
    let <- list(j,a,m,e,s)[[i_let]]
    npoints = nrow(let$x)
    mat[offset + 1:npoints, 1] <- let$x[, i] * (2020 - 1945) + 1945
    mat[offset + 1:npoints, 1 + i_let] <- let$y[, i]
    offset <- offset + npoints
  }

  colnames(mat) = c("year", "j", "a", "m", "e", "s")
  plot(mat, logo_show = T, title = "Your figures at the push of a button!", style = c("no-legend", "english"), x_lim = c(1940, 2020), y_lim = c(0, 1), x_title = "year", y_title = "degree of automation", custom = "rasterImage(image = png::readPNG('ext/logo/james-serves-90-110px-mirrored.png', T), xleft = .75  * (2020 - 1945) + 1945, xright = 2020, ybottom = 0, ytop = .5, interpolate = T)", line_lwd = 1.5 + (i - 1) / (n - 1) * 2.5, file_base = paste0("james-gif-", i))
}

png_files <- paste0("generated/png/james-gif-", 1:n, ".png")
png_files <- c(png_files, rep(tail(png_files, 1), 8), rev(png_files))
gif_file  <- "ext/logo/james.gif"
gifski::gifski(png_files, gif_file, delay = 0.1)

# rm(list = ls()); if (file.exists("m:/")) setwd('m:/p_james/dev') else setwd('~/Dropbox/cpb/git/james')
# cpblib::use_cpblib()
# source("initialize-james.R")
#
# plot("james-kmev2021.xlsx", report_output_format = "html")
#
