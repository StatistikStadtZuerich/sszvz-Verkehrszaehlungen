## code to prepare Feiertage vor 2018 dataset goes here
# Feiertage sind erst ab 2018 in OGD, vorherige Feiertage müssen wir separat laden

feiertage_alt <- readxl::read_excel(here::here("dev", "Liste_Feiertage_Schulferien.xlsx")) |>
  janitor::clean_names()

usethis::use_data(feiertage_alt, overwrite = TRUE, internal = TRUE)
