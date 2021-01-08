on_windows <- function() "windows" == .Platform$OS.type
on_mac         <- function() "mac" == stringr::str_sub(.Platform$pkgType, end = 3)
on_rekenserver <- function() "x86_64-pc-linux-gnu" == R.version$platform
creating_png_now <- function() is.element("plot_james_png", unlist(lapply(lapply(sys.calls(), as.character), function(vec) vec[1])))
creating_pdf_now <- function() is.element("plot_james_pdf", unlist(lapply(lapply(sys.calls(), as.character), function(vec) vec[1])))
creating_report_now <- function() isTRUE(getOption('knitr.in.progress'))

time_stamp <- function() format(Sys.time(), "%Y-%m-%d-%H.%M.%S")

hack_font <- function(p, x) {
  # if (!creating_pdf_now() | "RijksoverheidSansText" != p$font) return(x)
  # if (2 == x) return(3)
  # if (3 == x) return(2)
  return(x)
}

# append multiple values
mappend <- function(x, values, after = length(x)) {
  vec <- rep(x, (1:2)[(1:length(x) %in% after) + 1])
  vec[after + 1:length(after)] <- values
  vec
}

get_param <- function(name, style = DEFAULT) {
  index <- which(name == globals$param)
  if (!length(index)) {
    return(NA)
  } else {
    val <- globals[index, style] 

    # Cast value
    type <- globals[index, TYPE]
    type_original <- type
    if (is.element(type, LIST_SEPS)) {
      type <- globals[index, LIST_TYPE]
    }
    
    if (type == NUMERIC) val <- as_native_vec(val, type_original) #as.numeric(val)     # TODO tryCatch
    if (type == STRING)  val <- as_char_vec(val, sep = type_original, trim = REPORT_TEXT != name) # Leave report intact.
    if (type == PATH)    val <- as_char_vec(val, sep = type_original)
    if (type == BOOL)    { # Let NA intact, transform rest to T, F
      index_NA  <- which(is.na(val))
      val       <- is_yes(val)
      if (length(index_NA)) val[index_NA] <- NA # Set NA's back
    }  
    if (type == DATE) {
      if ("Date" != class(val)) {
        val <- as.Date(as.numeric(val), origin = "1899-12-30")
      }
    }
    
    return(val)
  } 
}

get_param_value_type <- function(name) {
  index <- which(name == globals$param)
  if (!length(index)) {
    return("unknown")
  }
  type <- globals[index, TYPE]
  if (is.element(type, LIST_SEPS)) type <- globals[index, LIST_TYPE]

  return(type)
}


get_param_replicate <- function() globals$param[which(is_yes(globals[[REPLICATE]]))]

#' Indicate whether all values in vec are really char
#' @param vec vector
#' @import utils
#' @keywords internal
is_really_character <- function(vec) {
  if ("character" == class(vec)) {
    return("character" == class(type.convert(vec, as.is = TRUE)))
  } else return(FALSE)
}

set_newline <- function(string) stringr::str_replace_all(string, "\\\\n", "\n")

has_value <- function(x) { # Considers NA to be a value
  if (0 == length(x)) # NB 0 == length(NULL)
    return(FALSE)
  x <- paste(x, collapse="")
  if (is.na(x))
    return(TRUE)
  return("" != stringr::str_trim(x))
}

is_set <- function(x) { # Returns TRUE for c(1, NA)
  if (0 == length(x)) return(FALSE) else if (1 == length(x)) return(!is.na(x)) else return(TRUE)
}
is_set_vec <- function(vec) sapply(vec, is_set)

get_x <- function(p, x, name = "unknown") {
  if (any(!is_set_vec(suppressWarnings(as.numeric(rownames(p$data)))))) { # only proceed if any of the x's is not numeric
      corresponding_int <- which(as.character(x) == as.character(rownames(p$data)))
    if (1 != length(corresponding_int)) {
      error_casting_x_lab(param_name = name, problematic_value = x, possible_values = rownames(p$data))
    } else {
      x <- corresponding_int
    }
  } else {
    # ? IS THIS CHECK NOT TOO MUCH?!
    if (!is.element(x, rownames(p$data)))
      error_casting_x_lab(param_name = name, problematic_value = x, possible_values = rownames(p$data))
  }    
  x
}

fix_numbers <- function (vec, n_decimals, decimal_mark, big_mark) {
    vec <- as.numeric(vec)
    if (!is_set(n_decimals)) n_decimals <- max(n_digits(vec))
    if (is_no(big_mark)) big_mark <- ""
    vec <- format(round(vec, n_decimals), nsmall = n_decimals, big.mark = big_mark, decimal.mark = decimal_mark, scientific = F)
    vec <- trimws(vec)
    # if ("." != decimal_mark) vec <- stringr::str_replace(c(vec), "\\.", decimal_mark)

    return(vec)
}

n_digits <- function (vec) {
  if (1 < length(vec)) {
   return(sapply(vec, n_digits))
  } else {
    if (vec == round(vec)) return(0) else return(nchar(as.character(gsub("(.*)(\\.)|([0]*$)", "", vec))))
  }
}

get_str_dim <- function(p, x, horizontal = TRUE, ...) {
  h <- strwidth(x, ...)
  v <- strheight(x, ...)

  if (!horizontal) {
    x_lim_full <- diff(par()$usr[1:2])
    y_lim_full <- diff(par()$usr[3:4])
    x_inch     <- par("pin")[1]
    y_inch     <- par("pin")[2]
    
    h_  <- h
    h   <- v  * x_lim_full / y_lim_full * y_inch / x_inch
    v   <- h_ * y_lim_full / x_lim_full * x_inch / y_inch
  }    
  
  return(list(h = h, v = v))
}

#' @keywords internal
as_char_vec <- function(str, sep = SEP1, trim = TRUE) {
  if (!is.na(str) && !is.null(str) && str == sep) return(str) else {
    vec <- unlist(stringr::str_split(str, sep))
    if (trim) return(stringr::str_trim(vec)) else return(vec)
  }
}
as_numeric_vec <- function(str) as.numeric(as_char_vec(str))
as_native_vec <- function(str, sep = SEP0) {
  vec <- as_char_vec(str, sep = sep)
  if (is_really_character(vec))
    return(vec)
  else
    return(as.numeric(vec))
}

#' keywords internal
#' @param meta list with meta data
strings_to_vectors <- function(meta, skip_fields = NULL) {
  input_char <- is.character(meta)
  if (input_char) meta <- list(whatever = meta)
    
  for (i in seq_along(meta)) {
    val <- meta[[i]]
    if (!is.element(names(meta)[i], skip_fields) && "character" == class(val)) {
      meta[[i]] <- as_native_vec(val)
    }
  }
  
  if (input_char) meta <- unlist(meta)
  
  return(meta)
}

combine_lists <- function(high_prio, low_prio) {
  if (0 == length(low_prio)) return(high_prio)
  
  lst <- low_prio
  
  for (i in seq_along(high_prio)) {
    var <- names(high_prio)[i]
    value <- high_prio[[i]]
    
    # Only use NA to overwrite if var was non-existent
    if (is.null(value) || !is.na(value) || is.null(lst[[var]]))
      lst[[var]] <- value
  }
  
  return(lst)
}

get_params_from_col_as_list <- function(df, col_name) {
  index <- which(!is.na(df[, col_name]))
  meta  <- list()
  for (i in index)
    meta[[df$param[i]]] <- df[i, col_name]
  meta
}

#' @keywords internal
is_no <- function(val) {
  if (is.null(val))
    return(FALSE)
  else
    return(is.element(val, NO))
}

#' @keywords internal
is_yes <- function(val) {
  if (1 < length(val)) return(as.vector(sapply(val, is_yes)))
    
  if (is.null(val) || is.na(val))
    return(FALSE)
  else
    return(is.element(val, YES) || (is.logical(val) && val))
}

swap_xy <- function(p) {
  if (p$turn) {
    q <- p
    q$x_title <- p$y_title
    q$y_title <- p$x_title
    q$x_lim <- p$y_l_lim
    q$y_l_lim <- rev(p$x_lim) # Reverse because we want smallest above
    p <- q
  }
  
  p
}

get_example_names <- function() names(meta_pp)

j_get_example_meta <- function(example) {
  meta <- meta_pp[[example]]

  meta_sheet <- read.xlsx(paste("../../", get_param("james_example_xlsx", globals = globals), sep = ""), sheet = "meta", rowNames = TRUE, colNames = FALSE)
  
  # select col
  col_index <- which(example == meta_sheet["name", ])
  if (!length(col_index))
    col_index <- which(example == meta_sheet["tab", ] & is.na(meta_sheet["name", ]))
    
  index_meta <- which(!is.na(meta_sheet[, col_index]))
  meta_names <- rownames(meta_sheet)[index_meta]

  return(meta[c(meta_names, "data")])
}

make_transparent <- function(hex_col, alpha = .5) rgb(t(col2rgb(hex_col) / 255), alpha = alpha)

seqft <- function(ft) seq(from = ft[1], to = ft[2])
styles <- function() colnames(globals)[seqft(which(is.element(colnames(globals), c(DEFAULT, INTERACTIVE))))]
types <- function() get_param("type", style = EXAMPLE)

odd_elements  <- function(vec) {
  if (length(vec) < 1) NULL else vec[seq(from = 1, to = length(vec), by = 2)]
}

even_elements <- function(vec) {
  if (length(vec) < 2) NULL else vec[seq(from = 2, to = length(vec), by = 2)]
}





















