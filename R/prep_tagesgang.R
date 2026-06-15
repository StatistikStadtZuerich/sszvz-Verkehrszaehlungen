#' prep_tagesgang
#'
#' prepare data as Tagesgang: one value per hour over 24h
#'
#' @param input_data miv/velo data, tibble
#' @param one_year_ago date/string, one year ago
#' @param feiertage tibble with OGD Feiertage
#'
#' @return tibble
#' @keywords internal
#'
prep_tagesgang <- function(input_data, one_year_ago, feiertage) {
  prep1 <- input_data |>
    filter(date(timestamp) >= one_year_ago) |>
    collect() |> # the next line does not work in arrow
    categorise_weekdays(feiertage = feiertage) |>
    summarise(
      n_vehicles = round(median(n_vehicles, na.rm = TRUE), digits = 0),
      .by = c(loc_id, loc_name, dir_id, dir_name, day_category, timestamp_hours_only)
    ) |> # remove nas? or remove days with too few entries?
    mutate(
      data_id = timestamp_hours_only,
      tooltip_single = glue(as.character(div(
        class = "tooltip-row",
        span(class = "tooltip-label", "{day_category}:"),
        span(class = "tooltip-value", "{n_vehicles}")
      )))
    )

  # create grouped tooltip column
  prep2 <- prep1 |>
    arrange(day_category) |> # sort to ensure order in tooltip
    summarise(
      tooltip_temp = glue_collapse(tooltip_single),
      .by = c(loc_id, loc_name, dir_id, dir_name, timestamp_hours_only)
    ) |>
    mutate(
      hour_two_digits = substr(as.character(timestamp_hours_only), 1, 2),
      tooltip = glue(as.character(div(
        class = "tooltip-container",
        div(
          class = "tooltip-title",
          "Tageszeit: {hour_two_digits}:00 - {hour_two_digits}:59"
        ),
        hr(),
        div(
          class = "tooltip-content",
          "{tooltip_temp}"
        )
      )))
    )

  # join
  prep1 |>
    left_join(prep2, by = c("loc_id", "loc_name", "dir_id", "dir_name", "timestamp_hours_only")) |>
    select(-tooltip_single, -tooltip_temp, -hour_two_digits)
}
