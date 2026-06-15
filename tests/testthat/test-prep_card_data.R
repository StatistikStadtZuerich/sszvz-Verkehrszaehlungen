test_that("prep_card_data works", {
  # prepare input
  min_per_day <- 22
  velo_day <- readRDS(test_path("fixtures", "velo_all.rds")) |>
    clean_velo_directions() |>
    aggregate_to_daily(min_per_day)
  # call function
  card_ready <- prep_card_data(velo_day, year(now()), min_per_day)

  # expect list of two tibbles
  expect_named(card_ready, c("sum_current_year", "sum_latest_days"))
  expect_s3_class(card_ready$sum_current_year, "tbl")
  expect_s3_class(card_ready$sum_latest_days, "tbl")

  # check one example
  example_loc_id <- "VZS_LUXG"
  example_dir_id <- "velo_in"
  # same number of vehicles for latest valid day
  expect_equal(
    card_ready$sum_latest_days |>
      filter(
        loc_id == example_loc_id,
        dir_id == example_dir_id
      ) |>
      pull(n_vehicles),
    velo_day |>
      filter(
        loc_id == example_loc_id,
        dir_id == example_dir_id
      ) |>
      slice_max(timestamp_datum) |>
      pull(n_vehicles)
  )
  # same number of vehicles for in current (calendar) year
  expect_equal(
    card_ready$sum_current_year |>
      filter(
        loc_id == example_loc_id,
        dir_id == example_dir_id,
        jahr == year(now())
      ) |>
      pull(n_vehicles),
    velo_day |>
      filter(
        loc_id == example_loc_id,
        dir_id == example_dir_id,
        jahr == year(now())
      ) |>
      pull(n_vehicles) |>
      sum()
  )
})
