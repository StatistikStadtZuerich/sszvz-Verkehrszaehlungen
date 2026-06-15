#' read_miv_data
#'
#' function to read miv data; keep only locations that are still active (data within the last year)
#'
#' @return tibble
#'
#' @export
#'
#' @examples
#' read_miv_data()
read_miv_data <- function() {
  print("getting MIV data")

  parquet_link_miv <- "https://data.stadt-zuerich.ch/dataset/sid_dav_verkehrszaehlung_miv_od2031/download/sid_dav_verkehrszaehlung_miv_od2031_alle_jahre.parquet"
  miv_all <- arrow::read_parquet(parquet_link_miv) |>
    as_tibble() |>
    janitor::clean_names() |>
    # parse timestamps as timestamps (saved as strings in parquet file)
    mutate(
      messung_dat_zeit = lubridate::ymd_hms(messung_dat_zeit, tz = "Europe/Zurich"),
      liefer_dat = lubridate::ymd(liefer_dat, tz = "Europe/Zurich")
    ) |>
    # harmonise column names
    rename(
      loc_id = zsid,
      loc_name = zs_name,
      dir_id = msid,
      dir_name = richtung, # currently ignore "ms_name"
      timestamp = messung_dat_zeit,
      n_vehicles = anz_fahrzeuge
    )

  # get locations that are still active with values present in the last 365 days
  valid_zs <- get_current_ids(miv_all)
  # read the locations as they have a status saying whether they are active
  df_loc <- read_miv_loc_current()

  # filter by keeping only active locations and add timestamp columns
  miv_ready <- miv_all |>
    # filter active locations
    filter(
      loc_id %in% valid_zs,
      loc_id %in% df_loc$loc_id
    ) |>
    add_extra_time_columns("timestamp")

  # check all necessary variables are present
  required_vars <- c(
    "loc_id", "loc_name", "dir_id", "dir_name", "timestamp",
    "n_vehicles", "yday", "timestamp_hours_only", "jahr"
  )
  stopifnot(all(required_vars %in% names(miv_ready)))

  return(miv_ready)
}
