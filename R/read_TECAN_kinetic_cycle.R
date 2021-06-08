read_TECAN_kinetic_cycle <- function(path) {
  suppressMessages({
    plate <- readxl::read_excel(path, na = c("", "NA"))
  })
  
  # trim leading metadata in a way that is consistent for different sheets
  beg <- grep("Cycle Nr\\.", unlist(plate[1]))
  if (length(beg) == 0L) 
    stop("error: can't find plate start")
  colnames(plate) <- as.character(plate[beg, ])
  colnames(plate)[1] <- "Location"
  plate <- plate[-c(1:beg), ]
  
  # trim trailing NAs
  end <- which(is.na(plate[1]))[1]
  plate <- plate[1:end-1, ]
  
  # extract time/temp/location info as separate vectors
  time <- as.double(plate[1, -1])
  temp <- as.double(plate[2, -1])
  loc <- unname(unlist(plate[-c(1:2), 1]))
  # format absorbance as a time series
  abs <- plate[-c(1:2), -1] %>%
    map_df(as.double) %>% 
    t() %>%
    as.ts()
  
  # return list of time, temp, location, and absorbance
  #TODO: make this into an object class for the other functions in this package
  list(time = time, temp = temp, location = loc, absorbance = abs)
}