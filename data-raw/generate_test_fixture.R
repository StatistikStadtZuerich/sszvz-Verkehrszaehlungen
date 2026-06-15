## code to prepare data to be used in tests, so they run faster
devtools::load_all()
velo_all <- read_velo_data()
velo_ready <- prep_velo_data(velo_all)
saveRDS(velo_all, here::here("tests", "testthat", "fixtures", "velo_all.rds"))
saveRDS(velo_ready, here::here("tests", "testthat", "fixtures", "velo_ready.rds"))
