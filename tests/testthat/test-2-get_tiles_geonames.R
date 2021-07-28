test_that("get_tiles gets the same geonames tiles twice", {
  skip_on_cran()
  dl_loc <- data.frame(
    lat = c(44.04905, 44.04911),
    lng = c(-74.01188, -74.01179)
  )
  dl_loc <- sf::st_as_sf(dl_loc, coords = c("lng", "lat"))
  sf::st_crs(dl_loc) <- sf::st_crs(4326)
  output_tif <- get_tiles(dl_loc,
    services = c("geonames"),
    georeference = FALSE
  )

  expect_equal(names(output_tif), "geonames")

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  expect_equal(
    png::readPNG(output_tif[[1]]),
    png::readPNG("testdata/geonames.png")
  )
})
