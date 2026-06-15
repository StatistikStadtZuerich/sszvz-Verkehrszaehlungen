#' read_velo_data
#'
#' function to read velo data of all years
#'
#' @return tibble with all velo data
#'
#' @export
#'
#' @examples
#' read_velo_data()
read_velo_data <- function() {
  print("getting velo data")

  # read the parquet file in a way that we can capture the warnings
  read_parquet_velo_quiet <- purrr::quietly(read_parquet_velo)
  velo_meta <- read_parquet_velo_quiet()

  # expect 1 warning about 3 parsing errors from timestamp parsing in 2019
  stopifnot(length(velo_meta$warnings) < 2)
  stopifnot(sum(is.na(velo_meta$result$datum)) < 4)

  # assign result of reading the parquet file
  velo_fuss_all <- velo_meta$result |>
    # Interpolate the three missing timestamps (convert to numeric first, then back)
    # this assumes again UTC, convert back to local time with tz
    mutate(
      datum = as_datetime(zoo::na.approx(as.numeric(datum)), tz = "Europe/Zurich")
    )
  # now there should be no missings anymore
  stopifnot(sum(is.na(velo_fuss_all$datum)) == 0)

  df_loc <- read_velo_loc_current()

  velo_ready <- velo_fuss_all |>
    # inner join to only keep active locations according to location data and only Velo
    inner_join(df_loc, by = c("fk_standort" = "id1")) |>
    # pivot the two directions to long
    pivot_vzs(type = "velo") |>
    # get rid of directions that do not exist (some locations only have 1 direction)
    filter(!is.na(dir_name)) |>
    filter(dir_name != "---") |>
    sum_and_correct()

  # check all necessary variables are present (some can only be added once all years are joined)
  required_vars <- c("loc_id", "loc_name", "dir_id", "dir_name", "n_vehicles", "timestamp", "yday", "jahr")
  stopifnot(all(required_vars %in% names(velo_ready)))

  return(velo_ready)
}

#' read_parquet_velo
#'
#' @returns tibble with latest velo data
#'
#' @keywords internal
read_parquet_velo <- function() {
  parquet_velo <- "https://data.stadt-zuerich.ch/dataset/ted_taz_verkehrszaehlungen_werte_fussgaenger_velo/download/verkehrszaehlungen_werte_fussgaenger_velo_alle_jahre.parquet"

  arrow::read_parquet(parquet_velo) |>
    as_tibble() |>
    janitor::clean_names() |>
    mutate(datum = lubridate::ymd_hm(datum, tz = "Europe/Zurich"))
}

#' sum_and_correct
#'
#' sum values from every 15min to hourly values, including applying the correction factor
#'
#' @param df Velo tibble with variables anzahl, fk_standort, id, fk_zaehler, korrekturfaktor, objectid,
#' loc_id, loc_name, dir_id, dir_name, datum
#'
#' @returns tibble with hourly and "corrected" data
#' @keywords internal
sum_and_correct <- function(df) {
  df |>
    sum_to_hourly() |>
    # use correction factor
    mutate(n_vehicles = anzahl * korrekturfaktor) |>
    add_extra_time_columns("timestamp")
}

#' pivot_vzs
#'
#' function to convert velo/fuss into longer format
#'
#' @param df tibble
#' @param type string, velo or fuss
#'
#' @return tibble
#' @keywords internal
pivot_vzs <- function(df, type = c("velo", "fuss")) {
  if (type == "velo") {
    drop <- "fuss"
    cols_to_pivot <- c("velo_in", "velo_out")
  } else if (type == "fuss") {
    drop <- "velo"
    cols_to_pivot <- c("fuss_in", "fuss_out")
  }

  df |>
    select(-starts_with(drop)) |>
    pivot_longer(all_of(cols_to_pivot),
      names_to = "dir_id",
      values_to = "anzahl"
    ) |>
    mutate(dir_name = if_else(stringr::str_detect(dir_id, "out"),
      richtung_out,
      richtung_in
    )) |> # Anzahl pro Stunde
    select(-richtung_out, -richtung_in)
}

#' sum_to_hourly
#'
#' sum the values from every 15min to hourly values; if one value is NA, the hourly value will be NA
#'
#' @details
#' round to the nearest hour, see https://lubridate.tidyverse.org/reference/round_date.html
#' xx.00 = measurements from minutes 0-14
#' xx.15 = measurements from minutes 15-29
#' xx.30 = measurements from minutes 30-44
#' xx.45 = measurements from minutes 45-59
#' round everything down --> e.g. 8.00 will have measurements from 08:00-08:59
#'
#' @param df arrow velo object
#'
#' @return arrow velo but with hourly data
#'
#' @keywords internal
sum_to_hourly <- function(df) {
  columns_to_keep <- c("fk_standort", "id", "fk_zaehler", "korrekturfaktor", "objectid")
  df |>
    #
    mutate(timestamp = floor_date(datum, unit = "hour")) |>
    summarise(
      anzahl = sum(anzahl), # NAs are propagated/kept
      across(all_of(columns_to_keep), .fns = \(x) unique(x)),
      .by = c(loc_id, loc_name, dir_id, dir_name, timestamp)
    )
}
