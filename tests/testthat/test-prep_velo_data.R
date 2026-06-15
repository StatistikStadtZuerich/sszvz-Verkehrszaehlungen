test_that("prep_velo_data works", {
  # Test dauert ziemlich lange und bringt mittel viel? deshalb momentan auskommentiert
  # prepare input
  velo_all <- readRDS(test_path("fixtures", "velo_all.rds"))
  # check wrapper calls function correctly
  expect_equal(
    prep_velo_data(velo_all),
    velo_all |>
      clean_velo_directions() |>
      prep_data(read_velo_standorte())
  )
})
