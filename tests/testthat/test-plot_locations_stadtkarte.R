test_that("location plot with raster map works", {
  # prepare input data
  common_shapes <- read_common_shapes()
  df_loc <- read_velo_standorte() |>
    add_tooltip_location() # need tooltip!

  # function should check inputs
  expect_error(plot_locations_stadtkarte(df_loc |> select(), common_shapes$kreise))
  expect_error(plot_locations_stadtkarte(df_loc, common_shapes$kreise, raster_color = "blue"))
  expect_error(plot_locations_stadtkarte(df_loc |> st_drop_geometry(), common_shapes$kreise))

  # allow different input options (minimal tests: no error)
  expect_no_error(plot_locations_stadtkarte(df_loc, common_shapes$kreise, duplicates_possible = TRUE))
  expect_no_error(plot_locations_stadtkarte(df_loc, common_shapes$kreise, raster_color = "grey"))

  # check output class
  expect_s3_class(plot_locations_stadtkarte(df_loc, common_shapes$kreise, duplicates_possible = TRUE), "maplibregl")

  # same same with miv locations to see whether it becomes too crowded
  df_loc_miv <- read_miv_standorte() |>
    filter(status == "aktiv") |>
    add_tooltip_location()

  # allow different input options (minimal tests: no error)
  expect_no_error(plot_locations_stadtkarte(df_loc_miv, common_shapes$kreise, raster_color = "grey"))

  # duplicate filtering only works for Velos
  expect_error(plot_locations_stadtkarte(df_loc_miv, common_shapes$kreise, duplicates_possible = TRUE))
})
