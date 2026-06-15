test_that("location plot with Stadtkreise works", {
  # prepare input data
  common_shapes <- read_common_shapes()
  df_loc <- read_velo_standorte()

  # function should check inputs
  expect_error(plot_locations_stadtkreise(df_loc, common_shapes$see))
  expect_error(plot_locations_stadtkreise(df_loc |> select(-loc_id), common_shapes))
  expect_error(plot_locations_stadtkreise(df_loc |> st_drop_geometry(), common_shapes))
  expect_no_error(plot_locations_stadtkreise(df_loc, common_shapes))
  expect_no_error(plot_locations_stadtkreise(df_loc, common_shapes, duplicates_possible = TRUE))

  # check ggplot is returned value (interactivity with ggiraph does not change the class)
  expect_s3_class(plot_locations_stadtkreise(df_loc, common_shapes, duplicates_possible = TRUE), "ggplot")
  expect_s3_class(plot_locations_stadtkreise(df_loc |> add_tooltip_location(), common_shapes, duplicates_possible = TRUE), "ggplot")
})
