#' prep_data
#'
#' @param input_data velo/miv data, tibble
#'
#' @param min_n_per_day minimum number of measurements per day to be valid, default 22
#' @param min_fraction_per_year minimal fraction of valid days in a year required for a year to be valid, default 0.75
#' @param df_location tibble with locations, with loc_id and loc_name
#'
#' @keywords internal
#'
#' @return list of tibbles
prep_data <- function(input_data, df_location, min_n_per_day = 22, min_fraction_per_year = 0.75) {
  current_date <- Sys.Date()
  current_year <- year(current_date)
  last_year <- current_year - 1
  one_year_ago <- current_date - 365
  feiertage <- read_feiertage()

  # all locations and directions
  loc_dir <- input_data |>
    select(loc_id, loc_name, dir_id, dir_name) |>
    distinct()

  # keep only active locations
  df_location <- df_location |>
    filter(loc_id %in% loc_dir$loc_id) |>
    # prepare location data for map with tooltip
    add_tooltip_location()

  # Vorschlag für valide Tage implementiert
  days_over_years <- aggregate_to_daily(input_data, min_n_per_day)

  # cards
  cards_info <- prep_card_data(days_over_years, current_year, min_n_per_day)

  # tagesgang -------
  df_tagesgang <- prep_tagesgang(input_data, one_year_ago, feiertage)

  # jahresentwicklung -------
  df_jahresentwicklung <- prep_jahresentwicklung(days_over_years, min_fraction_per_year, feiertage, loc_dir)

  return(list(
    "loc_dir" = loc_dir,
    "df_location" = df_location,
    "df_tagesgang" = df_tagesgang,
    "cards_info" = cards_info,
    "df_jahresentwicklung" = df_jahresentwicklung
  ))
}
