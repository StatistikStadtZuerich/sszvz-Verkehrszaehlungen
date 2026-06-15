# test preparation: download some data
parquet_link_miv <- "https://data.stadt-zuerich.ch/dataset/sid_dav_verkehrszaehlung_miv_od2031/download/sid_dav_verkehrszaehlung_miv_od2031_alle_jahre.parquet"
miv_all <- arrow::read_parquet(parquet_link_miv) |>
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

test_that("getting current ids works as expected", {
  valid_zs <- get_current_ids(miv_all)
  # there should be quite a few active locations
  expect_gt(length(valid_zs), 100)
  # locations should be a vector of strings
  expect_vector(valid_zs, ptype = character())
  # first letter should be "Z" for each location
  expect_true(all(stringr::str_starts(valid_zs, "Z")))
})

test_that("adding timestamp columns works", {
  miv_with_ts_cols <- add_extra_time_columns(miv_all)

  # number of observations should be the same
  expect_equal(nrow(miv_with_ts_cols), nrow(miv_all))
  # existing columns should be the same
  expect_equal(select(miv_with_ts_cols, names(miv_all)), miv_all)
  # exactly these 3 new columns added
  expect_equal(setdiff(names(miv_with_ts_cols), names(miv_all)), c("jahr", "yday", "timestamp_hours_only"))
})

test_that("read_feiertage works", {
  feiert <- read_feiertage()

  # should be a tibble
  expect_s3_class(feiert, "tbl")

  # should have quite a few rows
  expect_gt(nrow(feiert), 200)

  # should have two columns
  expect_named(feiert, c("start_date", "summary"))

  # should have a date column (of lubridate::date type!)
  expect_s3_class(feiert$start_date, "Date")

  # dates should contain years from 2009 (start of Velozählungen) and go at least to current
  expect_contains(unique(year(feiert$start_date)), 2009:year(Sys.Date()))
})
