test_that("read_velo_standorte works", {
  df_loc <- read_velo_standorte()

  # should be an sf object / geo data frame
  expect_s3_class(df_loc, "sf")

  # should have quite a few entries
  expect_gt(nrow(df_loc), 50)

  # variables loc_id and loc_name should be present
  expect_contains(names(df_loc), c("loc_id", "loc_name"))
})

test_that("read_velo_loc_current works", {
  df_loc_current <- read_velo_loc_current()

  # should be a data frame
  expect_s3_class(df_loc_current, "data.frame")

  # should have maybe 2 dozen entries
  expect_gt(nrow(df_loc_current), 20)

  # variables loc_id and loc_name should be present
  expect_contains(names(df_loc_current), c("loc_id", "loc_name"))

  # should contain fewer locations than reading all
  expect_lt(nrow(df_loc_current), nrow(read_velo_standorte()))
})
