test_that("read_velo_data works", {
  velo_all <- read_velo_data()

  # should be a dataframe
  expect_s3_class(velo_all, "tbl_df")

  # should have lots and lots of rows
  expect_gt(nrow(velo_all), 20000)

  # should (at least) have these columns
  min_expected_cols <- c(
    "loc_id", "loc_name", "dir_id", "dir_name", "timestamp",
    "n_vehicles", "yday", "timestamp_hours_only", "jahr"
  )
  expect_contains(names(velo_all), min_expected_cols)

  # should have quite a few different locations
  expect_gt(length(unique(velo_all$loc_id)), 15)

  # should have no unknown timestamp values
  expect_equal(sum(is.na(velo_all$timestamp)), 0)
  expect_equal(sum(is.na(velo_all$timestamp_hours_only)), 0)

  # in fact, we should have no NAs at all
  expect_equal(sum(is.na(velo_all)), 0)
})
