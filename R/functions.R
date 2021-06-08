
# function to read all sheets from a given excel file
read_excel_all_sheets <- function(path, na.strings = "") {
  sheets <- readxl::excel_sheets(path = path)
  if (length(sheets) == 1L) {
    out <- readxl::read_excel(path = path, na = na.strings)
    out <- list(as.data.frame(out, stringsAsFactors = FALSE))
    if (sheets == 'Sheet 1') {
      names(out) <- path
    } else {
      names(out) <- sheets
    }
  } else {
    out <- purrr::map(sheets, ~ readxl::read_excel(path, sheet = .x, na = na.strings))
    out <- purrr::map(out, as.data.frame, stringsAsFactors = FALSE)
    names(out) <- sheets
  }
  out
}

# function to flatten a nested list, leaving data.frames as-is
flatten <- function(lst) {
  do.call(c, lapply(lst, function(x) if (!is.data.frame(x) && is.list(x)) flatten(x) else list(x)))
}

# function to ignore NAs in functions like paste
ignore_na <- function(X, f, ...) {
  purrr::modify_if(X, ~ !is.na(.x), f, ...)
}

# function to reformat plate maps to desired form
reformap <- function(pl) {
  if (!is.data.frame(pl))
    stop("input must be a data.frame")
  if (is_tibble(pl))
    pl <- as.data.frame(pl)
  # make first column into row names
  rownames(pl) <- pl[, 1]
  pl <- pl[, -1]
  
  rs <- nrow(pl)
  cs <- ncol(pl)
  
  if (rs == 16 && cs == 24)  # test for 384-well format (base case)
    return(pl)  # return map as-is
  else if (rs == 8 && cs == 12) {  # test for 96-well format
    pl2 <- purrr::map_df(pl, rep, each = 2, FUN.VALUE = character(16))
    pl2 <- as.data.frame(pl2, stringsAsFactors = FALSE)
    rownames(pl2) <- LETTERS[1:16]
    pl <- pl2[rep(1:12, each = 2)]
    colnames(pl) <- 1:24
    # now add EHNA label to the appropriate wells: every other column (row on plate), excluding standards
    evens <- 2*(1:8)
    pl[evens, -c(1:2)] <- modify(pl[evens, -c(1:2)], ignore_na, paste, "EHNA")
  } else warning("unrecognized format, plate passed as-is")
  pl
}

# function to tidy up data file and plate map names so that they are easier to match
string_clean <- function(string) {
  if (grepl("V3V4", string)) {
    key <- "\\d{3}$"
    clean <- paste0("V3V4_p", stringr::str_extract(string, key))
    return(clean)
  } else if (grepl("Plate\\d_", string)) {
    key <- "(?<=[Pp]late)\\d"
    clean <- paste0("p00", stringr::str_extract(string, key))
    return(clean)
  } else {
    key <- "\\d{3}$"
    clean <- paste0("p", stringr::str_extract(string, key))
    return(clean)
  }
}

# function to check difference between duplicates, or kick out outliers if there are more than 2 replicates
check_dif <- function(v) {
  if (length(v) < 2L) return(v)
  else if (length(v) == 2L) {
    if (is.na(v[1]) | is.na(v[2])) return(v)
    else if (v[2] / v[1] < 0.75) v[2] <- NA
  }
  else if (length(v) > 2L) {
    vhi <- mean(na.omit(v)) + sd(na.omit(v))
    vlo <- mean(na.omit(v)) - sd(na.omit(v))
    v[v > vhi | v < vlo] <- NA
  }
  v
}

# function to ggsave only if user specifies a save argument, called pred here
ggsaveif <- function(pred, filename, plot = last_plot(), device = NULL, path = NULL,
                     scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
                     dpi = 300, limitsize = TRUE, ...) {
  if (get0(pred, ifnotfound = FALSE))
    ggsave(filename = filename, plot = plot, device = device, path = path, 
           scale = scale, width = width, height = height, units = units, 
           dpi = dpi, limitsize = limitsize, ...)
  else
    return(NULL)
}

# wrapper to paste things together with "_" as the separator
paste_ <- function(...) {
  paste(..., sep = "_")
}

# function to reorder factor levels in one line
factorder <- function(factor, new_levels) {
  if (!is.factor(factor))
    stop("input must be a factor")
  if (length(levels(factor)) != length(new_levels))
    stop("length of new factor levels must match old length")
  
  levels(factor) <- new_levels
  factor
}

# alp_theme: a function to add a common theme to all figures
alp_theme <- function() theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 0.5)) +
  theme(text = element_text(size = 14)) + 
  theme(strip.text = element_text(color = "black"), 
        strip.text.y = element_text(angle = 180),
        strip.background = element_rect(fill = "white"), 
        strip.placement = "outside") +
  theme(legend.position = 'none')

print_png <- function(plot, plotname, path, date = NULL) {
  png(filename = paste0(path, '/', plotname, '/', date, '.png'), plot)
}
