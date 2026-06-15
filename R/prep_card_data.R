#' Prepare data for bslib cards
#'
#' Function to prepare sum of vehicles on latest valid day and in last year for each location and direction
#'
#' @param days_over_years tibble with miv/velo data, one entry per day (and location and direction; the sum over that day)
#' @param current_year int, current year
#' @param min_n_per_day int, minimum number of observations per day for the day to be valid
#'
#' @return list of two dataframes, one for sum over latest day, the other for sum over year so far
#' @keywords internal
#'
#' @examples \dontrun{
#' cards_info <- prep_card_data(days_over_years, current_year, min_n_per_day)
#' }
prep_card_data <- function(days_over_years, current_year, min_n_per_day) {
  # TBD sum over invalid dates too for year? currently yes, removing NAs and including days with few measurements
  sum_current_year <- days_over_years |>
    filter(jahr == current_year) |>
    summarise(
      n_vehicles = sum(n_vehicles, na.rm = TRUE),
      .by = c(loc_id, loc_name, dir_id, dir_name, jahr)
    )

  # determine latest valid day for each location and direction
  latest_days <- days_over_years |>
    filter(
      n_timepoints >= min_n_per_day,
      jahr == current_year
    ) |>
    summarise(
      last_valid_date = max(timestamp_datum),
      .by = c(loc_id, loc_name, dir_id, dir_name)
    )

  # calculate sum of vehicles on latest valid day for each location and direction
  sum_latest_days <- days_over_years |>
    filter(
      n_timepoints >= min_n_per_day,
      jahr == current_year
    ) |>
    left_join(latest_days, by = c("loc_id", "loc_name", "dir_id", "dir_name")) |>
    filter(timestamp_datum == last_valid_date) |>
    select(-timestamp_datum)

  return(list(
    "sum_current_year" = sum_current_year,
    "sum_latest_days" = sum_latest_days
  ))
}
