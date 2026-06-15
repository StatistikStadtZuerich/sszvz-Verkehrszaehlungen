test_that("plot_tagesgang works", {
  # prepare inputs
  velo_ready <- readRDS(test_path("fixtures", "velo_ready.rds"))
  # select example
  current_loc_id <- "VZS_BASL"
  current_dir_id <- "velo_in"
  selected_tagesgang <- velo_ready$df_tagesgang |>
    filter(
      loc_id == current_loc_id,
      dir_id == current_dir_id
    )

  # function must check inputs
  expect_error(plot_tagesgang(selected_tagesgang |> select(-dir_name)))
  expect_error(plot_tagesgang(selected_tagesgang |> select(-n_vehicles)))
  expect_error(plot_tagesgang(selected_tagesgang, vehicle_type = "E-Bikes"))

  # these calls should be ok
  expect_no_error(plot_tagesgang(selected_tagesgang))
  expect_no_error(plot_tagesgang(selected_tagesgang |> select(-data_id, -tooltip)))
  expect_no_error(plot_tagesgang(selected_tagesgang, vehicle_type = "Velos"))
  expect_no_error(plot_tagesgang(selected_tagesgang, font_name = "SSZ"))

  # a ggplot should be returned
  expect_s3_class(plot_tagesgang(selected_tagesgang), "ggplot")
})
