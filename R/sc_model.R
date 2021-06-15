sc_model <- function(data, actual, expected, transform = identity) {
  if (!is.data.frame(data))
    stop("`data` must be a data.frame")
  Y <- data[[actual]]
  X <- data[[expected]]
  if (!is.null(transform)) {
    Y <- transform(Y)
    X <- transform(X)
  }
  lm(Y ~ X)
}