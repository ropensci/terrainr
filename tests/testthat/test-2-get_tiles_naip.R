test_that("get_tiles gets the same ortho tiles twice", {
  skip_on_cran()
  output_tif <- get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("ortho", "USGSNAIPPlus"),
  georeference = FALSE
  )

  expect_equal(names(output_tif), "ortho")

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  expect_equal(
    png::readPNG(output_tif[[1]]),
    png::readPNG("testdata/NAIPPlus.png")
  )
})

test_that("get_tiles gets the same georeferenced ortho tiles twice", {
  skip_on_cran()
  skip_on_travis() # for now
  output_tif <- get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("ortho", "USGSNAIPPlus")
  )

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  stored_raster <- raster::raster("testdata/NAIPPlus_gr.tif")
  test_raster <- raster::raster(output_tif[[1]])

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
  expect_equal(
    raster::cellStats(stored_raster, "max"),
    raster::cellStats(test_raster, "max")
  )
})
