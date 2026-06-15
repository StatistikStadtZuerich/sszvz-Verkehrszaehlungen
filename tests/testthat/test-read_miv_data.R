test_that("read_miv_data works", {
  miv_all <- read_miv_data()

  # should be a dataframe
  expect_s3_class(miv_all, "tbl_df")

  # should have lots and lots of rows
  expect_gt(nrow(miv_all), 20000)

  # should (at least) have these columns
  min_expected_cols <- c(
    "loc_id", "loc_name", "dir_id", "dir_name", "timestamp",
    "n_vehicles", "yday", "timestamp_hours_only", "jahr"
  )
  expect_contains(names(miv_all), min_expected_cols)

  # should have quite a few different locations
  expect_gt(length(unique(miv_all$loc_id)), 100)
})
