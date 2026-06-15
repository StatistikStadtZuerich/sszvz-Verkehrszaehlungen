test_that("categorise_weekdays works", {
  # prepare dummy df
  df <- tibble(
    ts = seq(ymd("2026-01-01"), ymd("2026-12-31"), by = "days")
  )
  df_feier <- read_feiertage()
  df_cat <- categorise_weekdays(df, df_feier, "ts")

  # row number should be the same
  expect_equal(nrow(df), nrow(df_cat))

  # new day_category column
  expect_contains(names(df_cat), "day_category")

  # check some specific dates
  # 2026-02-17 is a normal weekday
  expect_equal(df_cat |> filter(ts == ymd("2026-02-17")) |> pull(day_category) |> as.character(), "Werktag")
  # 2026-02-01 is a Sunday
  expect_equal(df_cat |> filter(ts == ymd("2026-02-01")) |> pull(day_category) |> as.character(), "Sonn-/Feiertag")
  # 2026-04-11 is a Saturday
  expect_equal(df_cat |> filter(ts == ymd("2026-04-11")) |> pull(day_category) |> as.character(), "Samstag")
  # 2026-04-03 is a bank holiday (Karfreitag)
  expect_equal(df_cat |> filter(ts == ymd("2026-04-03")) |> pull(day_category) |> as.character(), "Sonn-/Feiertag")

  # check overall numbers
  # more than 50 Saturdays
  expect_gt(df_cat |> filter(day_category == "Samstag") |> nrow(), 50)
  # more than 60 Sundays and bank holidays
  expect_gt(df_cat |> filter(day_category == "Sonn-/Feiertag") |> nrow(), 60)
  # plenty of weekdays
  expect_gt(df_cat |> filter(day_category == "Werktag") |> nrow(), 220)
})

test_that("add_tooltip_location works", {
  df_location <- read_velo_standorte()
  loc_with_tooltip <- add_tooltip_location(df_location)

  # same number of rows
  expect_equal(nrow(df_location), nrow(loc_with_tooltip))
  # one more column
  expect_equal(length(df_location) + 1, length(loc_with_tooltip))
  # new tooltip column
  expect_contains(names(loc_with_tooltip), "tooltip")
  # could also test whether the string contains valid html?
})
