#' get_current_ids
#'
#' @param arrow_data arrow dataset/dataframe with "timestamp" and "loc_id" columns
#'
#' @return vector with valid id_locs and date of their latest data
#' @keywords internal
#'
#' @examples \dontrun{
#' valid_zs <- get_current_ids(velo_all)
#' }
get_current_ids <- function(arrow_data) {
  one_year_ago <- Sys.Date() - 365
  two_years_ago <- year(Sys.Date()) - 2

  arrow_data |>
    # coarse filter first to run faster
    mutate(year = year(timestamp)) |>
    filter(year > two_years_ago) |>
    # finer filter: only keep last 365 days
    mutate(datum = date(timestamp)) |>
    filter(datum >= one_year_ago) |>
    select(loc_id) |>
    distinct() |>
    pull(loc_id)
}

#' add_extra_time_columns
#'
#' @param df arrow object/dataframe
#' @param timestamp_column optional name of the timestamp column, default is "timestamp"
#'
#' @return df/arrow object with added columns jahr, yday, timestamp_hours_only
#' @keywords internal
#'
#' @examples \dontrun{
#' add_extra_time_columns(miv_data)
#' add_extra_time_columns(miv_data, "name_of_timestamp_column")
#' }
add_extra_time_columns <- function(df, timestamp_column = "timestamp") {
  df |>
    mutate(
      jahr = year(.data[[timestamp_column]]),
      yday = yday(.data[[timestamp_column]])
    ) |>
    mutate(timestamp_hours_only = as_hms(.data[[timestamp_column]]))
}

#' read_feiertage
#'
#' read Feiertage from OGD and from local file (before 2018, saved as package data)
#'
#' @return tibble
#'
#' @export
#'
#' @examples
#' df_feier <- read_feiertage()
read_feiertage <- function() {
  f_ogd <- data.table::fread("https://data.stadt-zuerich.ch/dataset/ssd_schulferien/download/schulferien.csv") |>
    as_tibble() |>
    filter(
      !(stringr::str_detect(summary, "Schulen Stadt Zürich")),
      !(stringr::str_detect(summary, "Schuljahr"))
    ) |>
    select(start_date, summary)

  # da OGD erst im 2018 startet, hier noch die vom DAV gepflegte Liste aus Package Data verwenden (generiert in data-raw/feiertage_vor_2018.R)
  f_local <- feiertage_alt |>
    filter(feiertag == 1) |>
    select(datum, name) |>
    rename(
      start_date = datum,
      summary = name
    ) |>
    # nur die Daten vor 2018 behalten
    filter(start_date < min(f_ogd$start_date))

  # zusammensetzen und zurückgeben
  bind_rows(f_ogd, f_local) |>
    arrange(start_date) |>
    mutate(start_date = as_date(start_date))
}
