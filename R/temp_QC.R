temp_QC <- function(temp_data, temp_range = c(36, 38)) {
  which(temp_data > temp_range[1] & temp_data < temp_range[2])
  # TODO make this work for a TECAN_kinetic_cycle object class as well
}