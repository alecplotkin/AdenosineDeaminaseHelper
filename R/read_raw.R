# perform the initial qc steps for each plate, but instead of reporting absorbance deltas,
# report ratio between duplicates for each sample

# dependencies: readxl, dplyr/tidyverse, here
################################
# read in data
################################

read_raw <- function(platename, platemap, newname = NULL, path) {
  # p should be designated in calling environment, i.e. a for loop or a functional
  plate <- readxl::read_excel(paste0(path, "/", platename, ".xlsx"), na = c("", "NA"))

  # trim leading metadata in a way that is consistent for different sheets
  beg <- grep("Cycle Nr\\.", unlist(plate[1]))
  if (length(beg) == 0L) 
    stop("error: can't find plate start")
  names(plate) <- as.character(plate[beg, ])
  plate <- plate[-c(1:beg), ]

  # trim trailing NAs
  end <- which(is.na(plate[1]))[1]
  plate <- plate[1:end-1, ]

  ################################
  # temperature qc
  ################################

  time <- as.double(plate[1, -1])   # exclude first column, which functions as a row name
  temp <- as.double(plate[2, -1])

  # create list of time points to keep, based on whether temperature is within range
  keep <- which(temp >= 36 & temp < 38)

  # transfer time points that pass test (ie. those in keep) and well names to a new data frame, called data; exclude rows containing time and temp
  time <- time[keep]
  data <- plate[-c(1:2), c(1, keep + 1)]   # keep + 1 corrects for prior exclusion of first column
  wells <- data[1]
  data <- vapply(data[-1], as.double, FUN.VALUE = double(nrow(data)))  # make numeric
  data <- cbind(wells, data)
  colnames(data) <- c("well", paste("t", keep, sep = ""))   # update column names for ease of use

  ##################################
  # extract sample names from platemap
  ##################################
  
  # create data frame of well names and *empty* sample ids
  samples <- tibble(well = unlist(data[, 1]), sample_id = character(nrow(data)))
  
  # populate sample_id with names from platemap according to their position within the map
  for (l in rownames(platemap)){
    for (n in colnames(platemap)){
      w <- paste(l, n, sep = "")
      samples$sample_id[samples$well == w] <- platemap[l, n]
    }
  }

  # merge sample_id into data and eliminate rows corresponding to wells that did not have any samples
  data <- samples %>%
    merge(data, by = "well", sort = FALSE) %>%
    filter(!is.na(sample_id))
  
  #######################################
  # calculate avg absorbance change over time for each well
  #######################################
  
  # data frame to store avg delta of each well over time #
  deltas <- data[, -c(ncol(data))]
  
  # create vector corresponding to columns containing time points in deltas
  tps <- which(!names(deltas) %in% c("well", "sample_id"))
  
  # qc check to make sure values don't leave accepted range during run
  conseq <- rle(diff(tps))
  if (any(conseq$values > 1L))
    stop(paste("temperature drops below range during run for plate ", p))
  
  
  # populate deltas with delta between timepoints for each well in data, divided by the time difference
  # dividing by time difference allows non-adjacent timepoints to be compared in situations where entire columns were rejected due to temperature discrepancies
  for (t in tps){
    deltas[, t] <- (data[, t+1] - data[, t]) / (time[t-1] - time[t-2])
  }

  # calculate mean rate of absorbance change over time for each sample
  deltas$dabs <- deltas %>%
    select(-c(well, sample_id)) %>%
    transmute(dabs = rowMeans(.)) %>%
    unlist()

  #################################
  # output average absorbance deltas
  #################################
  avgs <- deltas %>%
    dplyr::select(well, sample_id, dabs) %>%
    dplyr::mutate(plate = ifelse(is.null(newname), platename, newname))
  
  avgs
}