get_lod <- function(blank, low_conc_sample) {
  lob <- mean(blank, na.rm = TRUE) + 1.645*sd(blank, na.rm = TRUE)
  lod <- lob + 1.645*sd(low_conc_sample, na.rm = TRUE)
}