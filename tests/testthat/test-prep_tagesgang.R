test_that("prep_tagesgang works", {
  # prepare input
  velo_clean <- readRDS(test_path("fixtures", "velo_all.rds")) |>
    clean_velo_directions()
  one_year_ago <- Sys.Date() - 365
  # all locations and directions
  loc_dir <- velo_clean |>
    select(loc_id, loc_name, dir_id, dir_name) |>
    distinct()
  # call function
  df_tage <- prep_tagesgang(velo_clean, one_year_ago, read_feiertage())

  # output should be a tibble
  expect_s3_class(df_tage, "tbl")

  # aggregated hours over a day --> should have fewer than number of locations x 24 hours x 3 day_categories
  expect_lte(nrow(df_tage), nrow(loc_dir) * 24 * 3)

  # expect these columns
  expect_named(
    df_tage,
    c("loc_id", "loc_name", "dir_id", "dir_name", "day_category", "timestamp_hours_only", "n_vehicles", "data_id", "tooltip")
  )

  # check one hourly sum: rounded median per day_category
  example_loc <- unique(df_tage$loc_id)[[3]]
  expect_equal(
    df_tage |>
      filter(
        loc_id == example_loc,
        dir_id == "velo_in"
      ) |>
      select(timestamp_hours_only, day_category, n_vehicles) |>
      arrange(day_category, timestamp_hours_only),
    velo_clean |>
      filter(
        loc_id == example_loc,
        dir_id == "velo_in",
        timestamp >= one_year_ago
      ) |>
      categorise_weekdays(read_feiertage()) |>
      summarise(
        .by = c(timestamp_hours_only, day_category),
        n_vehicles = round(median(n_vehicles, na.rm = TRUE))
      ) |>
      arrange(day_category, timestamp_hours_only)
  )
})
