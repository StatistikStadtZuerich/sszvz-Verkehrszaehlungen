#' aggregate_to_daily
#'
#' @param input_data velo/miv tibble with hourly data
#' @param min_n_per_day int, minimal number of hours to be present for a day to be valid/included
#'
#' @returns tibble with one entry per day for each direction
#'
#' @keywords internal
aggregate_to_daily <- function(input_data, min_n_per_day) {
  input_data |>
    mutate(
      timestamp_datum = date(timestamp),
      vehicle_na = is.na(n_vehicles)
    ) |>
    filter(!vehicle_na) |>
    summarise(
      n_vehicles = sum(n_vehicles),
      n_timepoints = n(),
      .by = c(loc_id, loc_name, dir_id, dir_name, jahr, timestamp_datum)
    ) |>
    filter(
      n_timepoints >= min_n_per_day,
      n_vehicles > 0
    )
}
