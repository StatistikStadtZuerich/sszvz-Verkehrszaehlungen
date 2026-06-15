test_that("aggregate_to_daily works", {
  # prepare input data
  velo_clean <- readRDS(test_path("fixtures", "velo_all.rds")) |>
    clean_velo_directions()

  # call function
  velo_day <- aggregate_to_daily(velo_clean, 22)

  # expect aggregation --> fewer number of rows (from 24h/day to daily plus some not valid)
  expect_gt(nrow(velo_clean), nrow(velo_day) * 24)

  # output type
  expect_s3_class(velo_day, "tbl")

  # location and direction columns originally present should still be there
  expect_contains(names(velo_day), c("loc_id", "loc_name", "dir_id", "dir_name", "jahr"))
  # we have a new timestamp_datum column
  expect_contains(names(velo_day), "timestamp_datum")
  # that column should be of type date
  expect_s3_class(velo_day$timestamp_datum, "Date")

  # check one daily sum
  expect_equal(
    velo_clean |>
      filter(
        loc_id == "VZS_MUEH",
        dir_id == "velo_in",
        jahr == 2024,
        date(timestamp) == ymd("2024-07-02")
      ) |>
      pull(n_vehicles) |>
      sum(),
    velo_day |>
      filter(
        loc_id == "VZS_MUEH",
        dir_id == "velo_in",
        timestamp_datum == ymd("2024-07-02")
      ) |>
      pull(n_vehicles)
  )
})
