calculate_delta <- function(data, time_points = seq_len(nrow(data))) {
  time_intervals <- diff(time_points)
  if (!is.ts(data)) 
    stop("data must be formatted as a time series.")
  delta <- diff(data[time_points, ])
  avg_delta <- apply(delta, 2, mean, na.rm = TRUE)
  sd_delta <- apply(delta, 2, sd, na.rm = TRUE)
  cv_delta <- sd_delta/avg_delta * 100
  # format output
  avg <- tibble(location = names(avg_delta), avg = avg_delta)
  cv <- tibble(location = names(cv_delta), cv = cv_delta)
  list(deltas = delta, avg = avg, cv = cv)
}