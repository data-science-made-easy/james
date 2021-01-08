get_pal <- function(palette = "all", trans = 0) {
  col_pal_cpb      <- as.data.frame(t(as.data.frame(sapply(get_param("cpb"), get_param))), stringsAsFactors = F)
  col_pal_cpb_3    <- as.data.frame(t(as.data.frame(sapply(get_param("cpb_3"), get_param))), stringsAsFactors = F)
  col_pal_kansrijk <- as.data.frame(t(as.data.frame(sapply(get_param("kansrijk"), get_param))), stringsAsFactors = F)
  col_pal_fan      <- as.data.frame(t(as.data.frame(sapply(get_param("fan"), get_param))), stringsAsFactors = F)
  col_pal_ppower   <- as.data.frame(t(as.data.frame(sapply(get_param("ppower"), get_param))), stringsAsFactors = F)
  # TODO NA's vervangen door hun colname

  # Prepare extra cols
  index_this_set_start <- which("# INDIVIDUAL COLORS" == globals$param)
  index_all_sets_start <- which("# " == stringr::str_sub(globals$param, 1, 2))
  index <- (1 + index_this_set_start):(index_all_sets_start[which(index_this_set_start < index_all_sets_start)[1]] - 1)
  col_name  <- setdiff(globals$param[index], c(names(col_pal_cpb), names(col_pal_cpb_3), names(col_pal_kansrijk), names(col_pal_fan), names(col_pal_ppower)))
  col_pal_extra    <- as.data.frame(t(as.data.frame(sort(sapply(col_name, get_param), decreasing = T))), stringsAsFactors = F)
  
  rownames(col_pal_cpb)      <- NULL
  rownames(col_pal_cpb_3)    <- NULL
  rownames(col_pal_kansrijk) <- NULL
  rownames(col_pal_fan)      <- NULL
  rownames(col_pal_ppower)   <- NULL
  rownames(col_pal_extra)    <- NULL

  piratepal.ls <- list(cpb.pal = col_pal_cpb, cpb_3.pal = col_pal_cpb_3, kansrijk.pal = col_pal_kansrijk, fan.pal = col_pal_fan, ppower.pal = col_pal_ppower, extra.pal = col_pal_extra,
    basel.pal = data.frame(blue1 = rgb(12, 
      91, 176, alpha = (1 - trans) * 255, maxColorValue = 255), 
      red = rgb(238, 0, 17, alpha = (1 - trans) * 255, 
          maxColorValue = 255), green = rgb(21, 152, 61, 
          alpha = (1 - trans) * 255, maxColorValue = 255), 
      pink = rgb(236, 87, 154, alpha = (1 - trans) * 255, 
          maxColorValue = 255), orange = rgb(250, 107, 
          9, alpha = (1 - trans) * 255, maxColorValue = 255), 
      blue2 = rgb(20, 155, 237, alpha = (1 - trans) * 255, 
          maxColorValue = 255), green2 = rgb(161, 199, 
          32, alpha = (1 - trans) * 255, maxColorValue = 255), 
      yellow = rgb(254, 193, 11, alpha = (1 - trans) * 
          255, maxColorValue = 255), turquoise = rgb(22, 
          160, 140, alpha = (1 - trans) * 255, maxColorValue = 255), 
      poop = rgb(154, 112, 62, alpha = (1 - trans) * 255, 
          maxColorValue = 255), stringsAsFactors = F), 
      pony.pal = data.frame(pink = rgb(235, 82, 145, alpha = (1 - 
          trans) * 255, maxColorValue = 255), orange = rgb(251, 
          187, 104, alpha = (1 - trans) * 255, maxColorValue = 255), 
          lpink = rgb(245, 186, 207, alpha = (1 - trans) * 
            255, maxColorValue = 255), lblue = rgb(157, 
            218, 245, alpha = (1 - trans) * 255, maxColorValue = 255), 
          purple1 = rgb(99, 81, 160, alpha = (1 - trans) * 
            255, maxColorValue = 255), gray = rgb(236, 
            241, 244, alpha = (1 - trans) * 255, maxColorValue = 255), 
          yellow = rgb(254, 247, 158, alpha = (1 - trans) * 
            255, maxColorValue = 255), dblue = rgb(23, 
            148, 206, alpha = (1 - trans) * 255, maxColorValue = 255), 
          purple2 = rgb(151, 44, 141, alpha = (1 - trans) * 
            255, maxColorValue = 255), stringsAsFactors = F), 
      xmen.pal = data.frame(blue = rgb(2, 108, 203, alpha = (1 - 
          trans) * 255, maxColorValue = 255), red = rgb(245, 
          30, 2, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(5, 177, 2, alpha = (1 - trans) * 
            255, maxColorValue = 255), orange = rgb(251, 
            159, 83, alpha = (1 - trans) * 255, maxColorValue = 255), 
          gray = rgb(155, 155, 155, alpha = (1 - trans) * 
            255, maxColorValue = 255), pink = rgb(251, 
            130, 190, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown = rgb(186, 98, 34, alpha = (1 - trans) * 
            255, maxColorValue = 255), yellow = rgb(238, 
            194, 41, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), decision.pal = data.frame(red = rgb(213, 
          122, 109, alpha = (1 - trans) * 255, maxColorValue = 255), 
          yellow = rgb(232, 183, 98, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue = rgb(156, 
            205, 223, alpha = (1 - trans) * 255, maxColorValue = 255), 
          gray = rgb(82, 80, 82, alpha = (1 - trans) * 
            255, maxColorValue = 255), tan = rgb(230, 206, 
            175, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown = rgb(186, 149, 112, alpha = (1 - trans) * 
            255, maxColorValue = 255), stringsAsFactors = F), 
      southpark.pal = data.frame(blue = rgb(47, 134, 255, 
          alpha = (1 - trans) * 255, maxColorValue = 255), 
          yellow = rgb(235, 171, 22, alpha = (1 - trans) * 
            255, maxColorValue = 255), red = rgb(222, 0, 
            18, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(34, 196, 8, alpha = (1 - trans) * 
            255, maxColorValue = 255), tan = rgb(254, 205, 
            170, alpha = (1 - trans) * 255, maxColorValue = 255), 
          orange = rgb(241, 72, 9, alpha = (1 - trans) * 
            255, maxColorValue = 255), stringsAsFactors = F), 
      google.pal = data.frame(blue = rgb(61, 121, 243, 
          alpha = (1 - trans) * 255, maxColorValue = 255), 
          red = rgb(230, 53, 47, alpha = (1 - trans) * 
            255, maxColorValue = 255), yellow = rgb(249, 
            185, 10, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(52, 167, 75, alpha = (1 - trans) * 
            255, maxColorValue = 255), stringsAsFactors = F), 
      eternal.pal = data.frame(purple1 = rgb(23, 12, 46, 
          alpha = (1 - trans) * 255, maxColorValue = 255), 
          red = rgb(117, 16, 41, alpha = (1 - trans) * 
            255, maxColorValue = 255), purple2 = rgb(82, 
            25, 76, alpha = (1 - trans) * 255, maxColorValue = 255), 
          purple3 = rgb(71, 59, 117, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue1 = rgb(77, 
            112, 156, alpha = (1 - trans) * 255, maxColorValue = 255), 
          tan = rgb(111, 118, 107, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue2 = rgb(146, 
            173, 196, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), evildead.pal = data.frame(brown = rgb(25, 
          24, 13, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(33, 37, 16, alpha = (1 - trans) * 
            255, maxColorValue = 255), red = rgb(46, 16, 
            11, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown2 = rgb(57, 46, 18, alpha = (1 - trans) * 
            255, maxColorValue = 255), brown3 = rgb(87, 
            81, 43, alpha = (1 - trans) * 255, maxColorValue = 255), 
          tan = rgb(150, 142, 76, alpha = (1 - trans) * 
            255, maxColorValue = 255), stringsAsFactors = F), 
      usualsuspects.pal = data.frame(gray1 = rgb(50, 51, 
          55, alpha = (1 - trans) * 255, maxColorValue = 255), 
          gray2 = rgb(83, 76, 83, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue = rgb(63, 81, 
            106, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown = rgb(155, 102, 89, alpha = (1 - trans) * 
            255, maxColorValue = 255), red = rgb(232, 59, 
            65, alpha = (1 - trans) * 255, maxColorValue = 255), 
          gray3 = rgb(159, 156, 162, alpha = (1 - trans) * 
            255, maxColorValue = 255), tan = rgb(234, 174, 
            157, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), ohbrother.pal = data.frame(brown1 = rgb(26, 
          15, 10, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown2 = rgb(61, 41, 26, alpha = (1 - trans) * 
            255, maxColorValue = 255), brown3 = rgb(113, 
            86, 57, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(116, 125, 109, alpha = (1 - trans) * 
            255, maxColorValue = 255), tan1 = rgb(173, 
            157, 11, alpha = (1 - trans) * 255, maxColorValue = 255), 
          blue = rgb(148, 196, 223, alpha = (1 - trans) * 
            255, maxColorValue = 255), tan2 = rgb(230, 
            221, 168, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), appletv.pal = data.frame(green = rgb(95, 
          178, 51, alpha = (1 - trans) * 255, maxColorValue = 255), 
          gray = rgb(106, 127, 147, alpha = (1 - trans) * 
            255, maxColorValue = 255), orange = rgb(245, 
            114, 6, alpha = (1 - trans) * 255, maxColorValue = 255), 
          red = rgb(235, 15, 19, alpha = (1 - trans) * 
            255, maxColorValue = 255), purple = rgb(143, 
            47, 139, alpha = (1 - trans) * 255, maxColorValue = 255), 
          blue = rgb(19, 150, 219, alpha = (1 - trans) * 
            255, maxColorValue = 255), stringsAsFactors = F), 
      brave.pal = data.frame(brown = rgb(168, 100, 59, 
          alpha = (1 - trans) * 255, maxColorValue = 255), 
          yellow = rgb(182, 91, 35, alpha = (1 - trans) * 
            255, maxColorValue = 255), red = rgb(148, 34, 
            14, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(39, 45, 23, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue = rgb(32, 33, 
            38, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), bugs.pal = data.frame(green1 = rgb(102, 
          120, 64, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green2 = rgb(186, 214, 168, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue = rgb(133, 
            199, 193, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown1 = rgb(165, 154, 107, alpha = (1 - trans) * 
            255, maxColorValue = 255), brown2 = rgb(103, 
            85, 63, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), cars.pal = data.frame(peach = rgb(231, 
          176, 143, alpha = (1 - trans) * 255, maxColorValue = 255), 
          purple = rgb(136, 76, 73, alpha = (1 - trans) * 
            255, maxColorValue = 255), red = rgb(224, 54, 
            58, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown = rgb(106, 29, 26, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue = rgb(157, 
            218, 230, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), nemo.pal = data.frame(yellow = rgb(251, 
          207, 53, alpha = (1 - trans) * 255, maxColorValue = 255), 
          orange = rgb(237, 76, 28, alpha = (1 - trans) * 
            255, maxColorValue = 255), brown = rgb(156, 
            126, 112, alpha = (1 - trans) * 255, maxColorValue = 255), 
          blue1 = rgb(90, 194, 241, alpha = (1 - trans) * 
            255, maxColorValue = 255), green = rgb(17, 
            119, 108, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), rat.pal = data.frame(brown = rgb(159, 
          77, 35, alpha = (1 - trans) * 255, maxColorValue = 255), 
          purple = rgb(146, 43, 73, alpha = (1 - trans) * 
            255, maxColorValue = 255), red = rgb(178, 29, 
            19, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(127, 134, 36, alpha = (1 - trans) * 
            255, maxColorValue = 255), yellow = rgb(241, 
            156, 31, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), up.pal = data.frame(blue1 = rgb(95, 
          140, 244, alpha = (1 - trans) * 255, maxColorValue = 255), 
          blue2 = rgb(220, 214, 252, alpha = (1 - trans) * 
            255, maxColorValue = 255), orange = rgb(226, 
            122, 72, alpha = (1 - trans) * 255, maxColorValue = 255), 
          brown = rgb(96, 86, 70, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue = rgb(67, 65, 
            89, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), espresso.pal = data.frame(blue = rgb(35, 
          102, 192, alpha = (1 - trans) * 255, maxColorValue = 255), 
          yellow = rgb(233, 215, 56, alpha = (1 - trans) * 
            255, maxColorValue = 255), red = rgb(185, 18, 
            38, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(163, 218, 75, alpha = (1 - trans) * 
            255, maxColorValue = 255), orange = rgb(255, 
            100, 53, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), ipod.pal = data.frame(lightgray = rgb(215, 
          215, 215, alpha = (1 - trans) * 255, maxColorValue = 255), 
          red = rgb(243, 174, 175, alpha = (1 - trans) * 
            255, maxColorValue = 255), darkgray = rgb(174, 
            173, 176, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(158, 217, 191, alpha = (1 - trans) * 
            255, maxColorValue = 255), blue = rgb(92, 203, 
            235, alpha = (1 - trans) * 255, maxColorValue = 255), 
          yellow = rgb(222, 235, 97, alpha = (1 - trans) * 
            255, maxColorValue = 255), background = rgb(242, 
            242, 242, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), info.pal = data.frame(red = rgb(231, 
          105, 93, alpha = (1 - trans) * 255, maxColorValue = 255), 
          darkblue = rgb(107, 137, 147, alpha = (1 - trans) * 
            255, maxColorValue = 255), creme = rgb(246, 
            240, 212, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(149, 206, 138, alpha = (1 - trans) * 
            255, maxColorValue = 255), gray1 = rgb(210, 
            210, 210, alpha = (1 - trans) * 255, maxColorValue = 255), 
          lightblue = rgb(148, 212, 212, alpha = (1 - trans) * 
            255, maxColorValue = 255), gray2 = rgb(150, 
            150, 150, alpha = (1 - trans) * 255, maxColorValue = 255), 
          background = rgb(241, 243, 232, alpha = (1 - 
            trans) * 255, maxColorValue = 255), brown = rgb(136, 
            119, 95, alpha = (1 - trans) * 255, maxColorValue = 255), 
          stringsAsFactors = F), fourteen.pal = data.frame(darkblue = rgb(0, 
          106, 64, alpha = (1 - trans) * 255, maxColorValue = 255), 
          pink = rgb(240, 136, 146, alpha = (1 - trans) * 
            255, maxColorValue = 255), lightgreen = rgb(117, 
            180, 30, alpha = (1 - trans) * 255, maxColorValue = 255), 
          lightgray = rgb(149, 130, 141, alpha = (1 - trans) * 
            255, maxColorValue = 255), grayblue = rgb(112, 
            140, 152, alpha = (1 - trans) * 255, maxColorValue = 255), 
          lightblue = rgb(138, 184, 207, alpha = (1 - trans) * 
            255, maxColorValue = 255), turquoise = rgb(0, 
            126, 127, alpha = (1 - trans) * 255, maxColorValue = 255), 
          green = rgb(53, 131, 89, alpha = (1 - trans) * 
            255, maxColorValue = 255), paleblue = rgb(139, 
            161, 188, alpha = (1 - trans) * 255, maxColorValue = 255), 
          purple = rgb(90, 88, 149, alpha = (1 - trans) * 
            255, maxColorValue = 255), orange = rgb(242, 
            153, 12, alpha = (1 - trans) * 255, maxColorValue = 255), 
          purple = rgb(90, 88, 149, alpha = (1 - trans) * 
            255, maxColorValue = 255), paleorange = rgb(229, 
            186, 58, alpha = (1 - trans) * 255, maxColorValue = 255), 
          salmon = rgb(216, 108, 79, alpha = (1 - trans) * 
            255, maxColorValue = 255), stringsAsFactors = F))
            
  if ("all" == palette) return(piratepal.ls) else return(piratepal.ls[[paste0(palette, ".pal")]])
}

show_col_pals <- function (palette = "all", plot.result = FALSE, trans = 0, mix.col = "white", mix.p = 0) 
{
  showtext::showtext_auto(enable = TRUE)
  on.exit({showtext::showtext_auto(enable = FALSE)})

  length.out = NULL
    
    if (trans < 0 | trans > 1) {
        stop("Problem: trans must be a number between 0 and 1")
    }
    {
      piratepal.ls = get_pal()
    }
    palette.names <- unlist(strsplit(names(piratepal.ls), ".pal", 
        TRUE))
    n.palettes <- length(palette.names)
    if (!(palette %in% c(palette.names, "random", "all", "names"))) {
        stop(c("You did not specify a valid palette. Run piratepal('names') to see all of the palette names."))
    }
    margin.o <- par("mar")
    if (palette == "all") {
        if (mix.p > 0) {
            for (pal.i in 1:length(piratepal.ls)) {
                for (col.i in 1:length(piratepal.ls[[pal.i]])) {
                  pal.fun <- circlize::colorRamp2(c(0, 1), colors = c(piratepal.ls[[pal.i]][col.i], 
                    mix.col), transparency = trans)
                  piratepal.ls[[pal.i]][col.i] <- pal.fun(mix.p)
                }
            }
        }
        output <- NULL
        par(mar = c(1, 6, 4, 0))
        n.palettes <- length(palette.names)
        plot(1, xlim = c(0, 15), ylim = c(0, 1), xaxt = "n", 
            yaxt = "n", bty = "n", type = "n", xlab = "", ylab = "", 
            main = "Color palettes to choose from", family = "RijksoverheidSansText")
        mtext(text = "Default palette: cpb", side = 3, family = "RijksoverheidSansText")
        y.locations <- seq(1, 0, length.out = n.palettes)
        for (i in 1:n.palettes) {
            palette.df <- unlist(piratepal.ls[[paste(palette.names[i], 
                ".pal", sep = "")]])
            n.colors <- length(palette.df)
            rect(0:(n.colors - 1), rep(y.locations[i], n.colors) - 
                1/(n.palettes * 2.2), 1:(n.colors), rep(y.locations[i], 
                n.colors) + 1/(n.palettes * 2.2), col = palette.df, 
                border = NA)
            mtext(unlist(strsplit(palette.names[i], fixed = T, 
                split = "."))[1], side = 2, at = y.locations[i], 
                las = 1, cex = 0.9, line = 0, family = "RijksoverheidSansText")
        }
    }
    if (palette == "random") {
        palette <- sample(palette.names[palette.names != "random"], 
            1)
        palette <- unlist(strsplit(palette, ".", fixed = T))[1]
        message(paste("Here's the", palette, "palette"))
    }
    if (palette == "names") {
        output <- palette.names
    }
    if (palette %in% c("all", "random", "names") == FALSE) {
        if (mix.p > 0) {
            for (pal.i in which(palette == palette.names)) {
                for (col.i in 1:length(piratepal.ls[[pal.i]])) {
                  pal.fun <- circlize::colorRamp2(c(0, 1), colors = c(piratepal.ls[[pal.i]][col.i], 
                    mix.col), transparency = trans)
                  piratepal.ls[[pal.i]][col.i] <- pal.fun(mix.p)
                }
            }
        }
        palette.df <- piratepal.ls[[paste(palette, ".pal", sep = "")]]
        if (is.null(length.out)) {
            output <- unlist(palette.df)
        }
        if (is.null(length.out) == F) {
            output <- rep(unlist(palette.df), length.out = length.out)
        }
    }
    if (plot.result & palette %in% palette.names) {
        palette.df <- piratepal.ls[[paste(palette, ".pal", sep = "")]]
        col.vec <- unlist(palette.df)
        n.colors <- length(col.vec)
        par(mar = c(1, 1, 1, 1))
        plot(1, xlim = c(0, 1), ylim = c(0, 1), type = "n", xaxs = "i", 
            xaxt = "n", yaxt = "n", bty = "n", yaxs = "i", xlab = "", 
            ylab = "")
        if (system.file(paste(palette, ".jpg", sep = ""), package = "yarrr") != 
            "") {
            point.heights <- 0.3
            text.heights <- 0.05
            pic.center <- c(0.5, 0.65)
            jpg <- jpeg::readJPEG(system.file(paste(palette, 
                ".jpg", sep = ""), package = "yarrr"), native = T)
            res <- dim(jpg)[1:2]
            ar <- res[2]/res[1]
            if (res[2] >= res[1]) {
                desired.width <- 0.6
                required.height <- desired.width/ar
                rasterImage(jpg, pic.center[1] - desired.width/2, 
                  pic.center[2] - required.height/2, pic.center[1] + 
                    desired.width/2, pic.center[2] + required.height/2)
            }
            if (res[2] < res[1]) {
                desired.height <- 0.4
                required.width <- desired.height * ar
                rasterImage(jpg, pic.center[1] - required.width/2, 
                  pic.center[2] - desired.height/2, pic.center[1] + 
                    required.width/2, pic.center[2] + desired.height/2)
            }
        }
        if (floor(n.colors/2) != n.colors/2) {
            possible.locations <- seq(0, 1, 1/16)
            start.location <- ceiling(0.5 * length(possible.locations)) - 
                floor(n.colors/2)
        }
        if (floor(n.colors/2) == n.colors/2) {
            possible.locations <- seq(0, 1, 1/15)
            start.location <- 0.5 * length(possible.locations) - 
                n.colors/2 + 1
        }
        end.location <- start.location + n.colors - 1
        locations.to.use <- possible.locations[start.location:end.location]
        if (system.file(paste(palette, ".jpg", sep = ""), package = "yarrr") == 
            "") {
            point.heights <- 0.6
            text.heights <- 0.25
        }
        segments(locations.to.use, text.heights + 0.05, locations.to.use, 
            point.heights, lwd = 1, lty = 2)
        points(x = locations.to.use, y = rep(point.heights, length(col.vec)), 
            pch = 16, col = col.vec, cex = 10)
        text(locations.to.use, text.heights, names(col.vec), 
            srt = 45, family = "RijksoverheidSansText")
        text(0.5, 0.95, palette, cex = 2, family = "RijksoverheidSansText")
        # text(0.5, 0.9, paste("trans = ", trans, sep = ""))
    }
    if (is.null(output) == F & plot.result == F) {
        return(output)
    }
    par(mar = margin.o)
}