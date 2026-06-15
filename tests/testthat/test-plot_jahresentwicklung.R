test_that("plot_jahresentwicklung works", {
  # prepare inputs
  velo_ready <- readRDS(test_path("fixtures", "velo_ready.rds"))
  # select example
  current_loc_id <- "VZS_TOED"
  current_dir_id <- "velo_in"
  selected_jahre <- velo_ready$df_jahresentwicklung |>
    filter(
      loc_id == current_loc_id,
      dir_id == current_dir_id
    )

  # function must check inputs
  expect_error(plot_jahresentwicklung(selected_jahre |> select(-dir_name)))
  expect_error(plot_jahresentwicklung(selected_jahre |> select(-n_vehicles)))
  expect_error(plot_jahresentwicklung(selected_jahre, vehicle_type = "E-Bikes"))

  # these calls should be ok
  expect_no_error(plot_jahresentwicklung(selected_jahre))
  expect_no_error(plot_jahresentwicklung(selected_jahre |> select(-data_id, -tooltip)))
  expect_no_error(plot_jahresentwicklung(selected_jahre, vehicle_type = "Velos"))
  expect_no_error(plot_jahresentwicklung(selected_jahre, font_name = "SSZ"))

  # a ggplot should be returned
  expect_s3_class(plot_jahresentwicklung(selected_jahre), "ggplot")
})
