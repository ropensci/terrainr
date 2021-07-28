test_that("get_tiles gets the same ortho tiles twice", {
  skip_on_cran()
  dl_loc <- data.frame(
    lat = c(44.04905, 44.04911),
    lng = c(-74.01188, -74.01179)
  )
  dl_loc <- sf::st_as_sf(dl_loc, coords = c("lng", "lat"))
  sf::st_crs(dl_loc) <- sf::st_crs(4326)
  output_tif <- get_tiles(dl_loc,
    services = c("ortho", "USGSNAIPPlus"),
    georeference = FALSE
  )

  expect_equal(names(output_tif), "ortho")

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  expect_equal(
    png::readPNG(output_tif[[1]]),
    png::readPNG("testdata/NewNAIPPlus.png")
  )
})

test_that("get_tiles gets the same georeferenced ortho tiles twice", {
  skip_on_cran()
  dl_loc <- data.frame(
    lat = c(44.04905, 44.04911),
    lng = c(-74.01188, -74.01179)
  )
  dl_loc <- sf::st_as_sf(dl_loc, coords = c("lng", "lat"))
  sf::st_crs(dl_loc) <- sf::st_crs(4326)
  output_tif <- get_tiles(dl_loc,
    services = c("ortho", "USGSNAIPPlus")
  )

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  stored_raster <- raster::raster("testdata/NewNAIPPlus_gr.tif")
  test_raster <- raster::raster(output_tif[[1]])

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
  expect_equal(
    raster::cellStats(stored_raster, "max"),
    raster::cellStats(test_raster, "max")
  )
})
