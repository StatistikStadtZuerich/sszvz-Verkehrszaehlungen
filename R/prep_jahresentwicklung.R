#' Prepare data for Jahresentwicklung
#'
#' Data to create one value per year and day category for each location and direction
#'
#' @param days_over_years miv/velo data tibble with one entry per day (for each location and direction; the sum over that day)
#' @param min_fraction_per_year minimal fraction of valid days in a year required for a year to be valid
#' @param feiertage tibble with bank holidays based on OGD
#' @param loc_dir tibble with all locations and directions
#'
#' @return tibble
#' @keywords internal
#'
#' @examples \dontrun{
#' prep_jahresentwicklung(days_over_years, min_fraction_per_year, feiertage, loc_dir)
#' }
prep_jahresentwicklung <- function(days_over_years, min_fraction_per_year, feiertage, loc_dir) {
  valid_years <- days_over_years |>
    summarise(
      n_days_year = n(),
      .by = c(loc_id, loc_name, dir_id, dir_name, jahr)
    ) |>
    filter(n_days_year >= (365 * min_fraction_per_year))

  # for each direction, get median per day category
  helper_function <- function(current_loc_id, current_dir_id, valid_years, days_over_years) {
    current_valid_days <- days_over_years |>
      filter(loc_id == current_loc_id, dir_id == current_dir_id) |>
      pull(timestamp_datum) |>
      unique()

    current_valid_years <- valid_years |>
      filter(loc_id == current_loc_id, dir_id == current_dir_id) |>
      pull(jahr) |>
      unique()

    days_over_years |>
      filter(
        loc_id == current_loc_id,
        dir_id == current_dir_id,
        jahr %in% current_valid_years,
        timestamp_datum %in% current_valid_days
      ) |>
      categorise_weekdays(feiertage, "timestamp_datum") |>
      summarise(
        n_vehicles = round(median(n_vehicles, na.rm = T), digits = 0),
        .by = c(loc_id, loc_name, dir_id, dir_name, jahr, day_category)
      ) |>
      mutate(
        data_id = jahr,
        tooltip_single = glue(as.character(div(
          class = "tooltip-row",
          span(class = "tooltip-label", "{day_category}:"),
          span(class = "tooltip-value", '{format(round(n_vehicles), big.mark = " ")}')
        )))
      )
  }

  # loop over all locations and directions
  # first create list to loop over
  list_loc_dir <- purrr::pmap(
    .l = loc_dir |> ungroup() |> select(loc_id, dir_id),
    .f = \(loc_id, dir_id) list("loc_id" = loc_id, "dir_id" = dir_id)
  )
  # then loop over it, using helper function
  df_jahresentwicklung_single_tooltip <- purrr::map(
    .x = list_loc_dir,
    .f = \(x) helper_function(x$loc_id, x$dir_id, valid_years, days_over_years)
  ) |>
    bind_rows()
  # todo figure out warnings?

  # create grouped tooltip column
  df_jahresentwicklung_grouped_tooltip <- df_jahresentwicklung_single_tooltip |>
    arrange(day_category) |> # sort to ensure order in tooltip
    summarise(
      tooltip_temp = glue_collapse(tooltip_single),
      .by = c(loc_id, loc_name, dir_id, dir_name, jahr)
    ) |>
    mutate(tooltip = glue(as.character(div(
      class = "tooltip-container",
      div(
        class = "tooltip-title",
        "Jahr: {jahr}"
      ),
      hr(),
      div(
        class = "tooltip-content",
        "{tooltip_temp}"
      )
    )))) |>
    select(-tooltip_temp)

  # join grouped timestamp back on
  df_jahresentwicklung_single_tooltip |>
    left_join(df_jahresentwicklung_grouped_tooltip, by = c("loc_id", "loc_name", "dir_id", "dir_name", "jahr")) |>
    select(-tooltip_single)
}
