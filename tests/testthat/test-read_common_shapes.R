test_that("read_common_shapes works", {
  ssz_shapes <- read_common_shapes()

  # expect 3 items in list with names "kreise", "quartiere", "see"
  expect_length(ssz_shapes, 3)
  expect_named(ssz_shapes, c("kreise", "quartiere", "see"))

  # these items should be geodata
  expect_s3_class(ssz_shapes$kreise, "sf")
  expect_s3_class(ssz_shapes$quartiere, "sf")
  expect_s3_class(ssz_shapes$see, "sf")

  # example content check: read quartiere
  expect_equal(
    ssz_shapes$quartiere,
    read_sf("https://www.ogd.stadt-zuerich.ch/wfs/geoportal/Statistische_Quartiere?service=WFS&version=1.1.0&request=GetFeature&outputFormat=application/json&typename=adm_statistische_quartiere_v")
  )
})
