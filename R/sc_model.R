sc_model <- function(data, measurement, expected, transform = identity) {
  if (!is.data.frame(data))
    stop("`data` must be a data.frame")
  f <- formula(sprintf("%1s ~ %2s", measurement, expected))
  data[measurement] <- transform(data[measurement])
  data[expected] <- transform(data[expected])
  lm(f, data)
}