test_that("clean_velo_directions works", {
  # prep: read data
  velo_all <- readRDS(test_path("fixtures", "velo_all.rds"))
  # run cleaning function
  velo_clean <- clean_velo_directions(velo_all)

  # number of entries should be the same
  expect_equal(nrow(velo_all), nrow(velo_clean))

  # output should be a tibble
  expect_s3_class(velo_clean, "tbl")

  # content check: need to find locations to be switched
  dir_check <- velo_all |>
    count(loc_id, dir_id, dir_name) |>
    count(loc_id) |>
    filter(n > 2)

  # all other locations should stay the same
  expect_equal(
    velo_all |> filter(!(loc_id %in% dir_check$loc_id)),
    velo_clean |> filter(!(loc_id %in% dir_check$loc_id))
  )

  # switched locations should be the same except for directions
  expect_equal(
    velo_all |>
      filter(loc_id %in% dir_check$loc_id) |>
      select(-dir_id, -dir_name) |>
      arrange(loc_id, timestamp),
    velo_clean |>
      filter(loc_id %in% dir_check$loc_id) |>
      select(-dir_id, -dir_name) |>
      arrange(loc_id, timestamp)
  )

  # direction names present should still be the same
  expect_setequal(
    velo_all |>
      filter(loc_id %in% dir_check$loc_id) |>
      pull(dir_name) |>
      unique(),
    velo_clean |>
      filter(loc_id %in% dir_check$loc_id) |>
      pull(dir_name) |>
      unique()
  )
})
