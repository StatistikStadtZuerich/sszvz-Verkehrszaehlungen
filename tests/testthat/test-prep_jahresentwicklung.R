test_that("prep_jahresentwicklung works", {
  # prepare input
  min_per_day <- 22
  velo_day <- readRDS(test_path("fixtures", "velo_all.rds")) |>
    clean_velo_directions() |>
    aggregate_to_daily(min_per_day)
  # all locations and directions
  loc_dir <- velo_day |>
    select(loc_id, loc_name, dir_id, dir_name) |>
    distinct()
  # call function
  df_jahre <- prep_jahresentwicklung(velo_day, 0.75, read_feiertage(), loc_dir)

  # output should be a tibble
  expect_s3_class(df_jahre, "tbl")

  # aggregated to years --> should have fewer than number of locations x max. number of years x 3 day_categories
  expect_lt(nrow(df_jahre), nrow(loc_dir) * (year(now()) - 2009) * 3)

  # expect these columns
  expect_named(
    df_jahre,
    c("loc_id", "loc_name", "dir_id", "dir_name", "jahr", "day_category", "n_vehicles", "data_id", "tooltip")
  )

  # check one yearly sum: rounded median per day_category
  example_index <- 3
  last_year <- year(now()) - 1
  expect_equal(
    df_jahre |>
      filter(
        loc_id == loc_dir$loc_id[[example_index]],
        dir_id == loc_dir$dir_id[[example_index]],
        jahr == last_year
      ) |>
      select(day_category, n_vehicles) |>
      arrange(day_category),
    velo_day |>
      filter(
        loc_id == loc_dir$loc_id[[example_index]],
        dir_id == loc_dir$dir_id[[example_index]],
        jahr == last_year
      ) |>
      categorise_weekdays(read_feiertage(), "timestamp_datum") |>
      summarise(.by = day_category, n_vehicles = round(median(n_vehicles))) |>
      arrange(day_category)
  )
})
