import <- function(xlsx, ...) { # filter = "create" or "publish" to speedup import
  p <- list(...)

  # Get meta tab
  meta_tab <- openxlsx::read.xlsx(xlsx, sheet = "meta", colNames = F, rowNames = T)

  #### If we see a report, then set create = F as default. So, figs will only be created if user explicitely wants to.
  ##### not_plot_figs_next_to_report <- not_plot_figs(meta_tab)

  # Load globals-tab if exists
  sheet_names <- openxlsx::getSheetNames(xlsx)
  g_list <- list()
  if (is.element("globals", sheet_names)) {
    g <- openxlsx::read.xlsx(xlsx, sheet = "globals", colNames = F, rowNames = T)
    if (!is.null(g)) {
      g_list <- list()
      for (i in 1:nrow(g)) g_list[[i]] <- g[i, 1]
      names(g_list) <- rownames(g)
      # p <- combine_lists(high_prio = p, low_prio = g_list) # NOT HERE
    }
  }

  if (is_set(p$id)) { # filter specific id
    stopifnot(!is_set(p$filter))
    p$filter <- "create"
    meta_tab[p$filter, ] <- "n"
    index <- which(p$id == meta_tab["id", ])
    meta_tab[p$filter, index] <- "y"
  }
  
  if (!is_set(p$filter)) p$filter <- "create"

  # Filter cols
  if (is_set(p$filter)) {
    filter <- p$filter
    if (is.element(filter, rownames(meta_tab))) {
      filter_default <- !is_no(p[[filter]])
      if (filter_default) {
        index <- which(!is_no(meta_tab[filter, ]))
      } else {
        index <- which(is_yes(meta_tab[filter, ]))
      }
    } else {
      index <- 1:ncol(meta_tab)
    }  
    meta_tab <- meta_tab[, index, drop = F]
  }
  
  if (0 == ncol(meta_tab)) return(list()) # nothing to do

  index_map          <- which(stringr::str_trim(meta_tab[STYLE, ]) == MAP)

  # Iterate meta_tab; TODO only import if there is data AND create == TRUE or publish == TRUE
  meta_lst <- list()
  meta_tab_params <- rownames(meta_tab)
  for (i in 1:ncol(meta_tab)) {
    this_list <- list()
    for (j in 1:nrow(meta_tab)) {
      this_list[[meta_tab_params[j]]] <- meta_tab[j, i]
    }
    this_list <- combine_lists(high_prio = this_list, low_prio = list(xlsx = xlsx)) # add imported file name

    # ADD DATA FROM TAB
    this_tabs <- stringr::str_trim(unlist(stringr::str_split(meta_tab[TAB, i], get_param("tab", style = TYPE))))
    for (this_tab in this_tabs) {
      # this_tab <- meta_tab[TAB, i]
      if (is.element(this_tab, sheet_names)) {
        if (is_set(this_list$type) && all(REPORT == this_list$type)) { # in case of report
          this_list[[REPORT_TEXT]] <- data_frame_to_string(openxlsx::read.xlsx(xlsx, sheet = as.character(this_tab), colNames = F, rowNames = F, skipEmptyRows = F))
        } else { # in case of data
          this_data <- openxlsx::read.xlsx(xlsx, sheet = as.character(this_tab), sep.names = " ", colNames = T, rowNames = F, detectDates = T)
          this_list[[DATA]] <- if (is_set(this_list[[DATA]])) plyr::rbind.fill(this_list[[DATA]], this_data) else this_data
        }
      } else if (!is.element(i, index_map) & is_set(this_tab)) error_msg("In column ", i, " of your meta tab, you refer to a non-existing tab '", this_tab, "'.")
    }
    
    # ADD DATA FROM CBS
    if (is_set(this_list[[CBS_URL]])) {
      this_list[[DATA]] <- plyr::rbind.fill(this_list[[DATA]], cbs(url = this_list[[CBS_URL]]))
    }
    
    meta_lst[[i]] <- james(combine_lists(combine_lists(high_prio = p, low_prio = this_list), g_list))
    
    ### Set create of figs default to FALSE if we also produce a report
    ### if (not_plot_figs_next_to_report & any(REPORT != meta_lst[[i]]$type)) meta_lst[[i]]$create_for_report <- FALSE

    # Give element name
    sheet_id <- meta_lst[[i]]$id
    sheet_id_set_by_user <- is_set(sheet_id)
    # ID may not have spaces
    if (sheet_id_set_by_user) if (grepl("\\s+", sheet_id, perl = T)) error_msg("Parameter 'id' does not allow a whitespace. Please remove the whitespace or replace it with e.g. '-'. Problematic id: '", sheet_id, "'.")
    if (!is_set(sheet_id)) sheet_id <- meta_lst[[i]]$tab
    if (1 < length(sheet_id)) sheet_id <- paste(sheet_id, collapse = "-") # if multiple data-tabs
    if (!is_set(sheet_id)) sheet_id <- meta_lst[[i]]$title
    if (!is_set(sheet_id)) sheet_id <- i #meta_lst[[i]][[CBS_URL]]
    if (!sheet_id_set_by_user) sheet_id <- gsub('[[:punct:] ]+|\n','-', sheet_id)
    names(meta_lst)[i]              <- sheet_id
    meta_lst[[i]]$id                <- sheet_id # Also give the ID to the element itself
  }
  
  meta_lst
}


























