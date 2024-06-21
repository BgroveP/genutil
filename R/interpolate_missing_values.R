#' Interpolate missing data point (linearly)
#'
#' Linear interpolation function.
#'
#' @param x Variable with missing data-points
#' @param time A variable that denotes the spacing between npoints. If omitted, missing points will be assumed to be in the middle of observed values.
#' @param extrapolate Calculate values where only one adjacent value is known.
#' @return A vector of data with observed and interpolated values.
#' @export
#'

interpolate_missing_values <- function(x, time = NA, extrapolate = F) {
  y <- x
  positions <- 1:length(x)
  first_not_missing <- min(which(!is.na(x)))
  if (length(time) == 1) if (is.na(time)) time <- positions

  current_standpoint <- first_not_missing
  next_standpoint <- min(which(!is.na(x) & positions > current_standpoint))

  while (current_standpoint < max(positions) & next_standpoint <= max(positions)) {
    # If both values are present, and none is missing between them
    if ((next_standpoint - current_standpoint) == 1 & !is.na(x[next_standpoint])) {
      next_standpoint <- next_standpoint + 1
      current_standpoint <- current_standpoint + 1
    }

    # If next value is missing
    if (is.na(x[next_standpoint])) {
      next_standpoint <- next_standpoint + 1
    }

    # If both values are present, and something is missing between them
    if ((next_standpoint - current_standpoint) > 1 & !is.na(x[next_standpoint])) {
      regression_slope <- (x[next_standpoint] - x[current_standpoint]) / (time[next_standpoint] - time[current_standpoint])
      y[current_standpoint:next_standpoint] <- y[current_standpoint] + regression_slope * (time[current_standpoint:next_standpoint] - time[current_standpoint])
      current_standpoint <- next_standpoint
      next_standpoint <- next_standpoint + 1
    }
  }
  if (extrapolate) {
    # Any missing in the ends?
    missing_start <- is.na(y[1])
    missing_emd <- is.na(y[max(positions)])

    # Exstrapolate missing start
    first_two_values_positions <- head(which(!is.na(y)), 2)
    regression_slope <- (y[first_two_values_positions[2]] - y[first_two_values_positions[1]]) / (time[first_two_values_positions[2]] - time[first_two_values_positions[1]])
    y[1:first_two_values_positions[1]] <- y[first_two_values_positions[1]] + regression_slope * (time[1:first_two_values_positions[1]] - time[first_two_values_positions[1]])

    # Exstrapolate missing start
    last_two_values_positions <- tail(which(!is.na(y)), 2)
    regression_slope <- (y[last_two_values_positions[2]] - y[last_two_values_positions[1]]) / (time[last_two_values_positions[2]] - time[last_two_values_positions[1]])
    y[last_two_values_positions[2]:max(positions)] <- y[last_two_values_positions[2]] + regression_slope * (time[last_two_values_positions[2]:max(positions)] - time[last_two_values_positions[2]])
  }
  return(y)
}
