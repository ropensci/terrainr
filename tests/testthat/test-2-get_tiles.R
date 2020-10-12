test_that("get_tiles gets the same tiles twice", {
  skip_on_cran()
  output_tif <- get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("elevation", "ortho", "USGSNAIPPlus", "hydro"),
  georeference = FALSE
  )

  expect_equal(length(output_tif), 3)
  expect_equal(length(output_tif[[1]]), 1)

  stored_raster <- raster::raster("testdata/3DEP.tif")
  test_raster <- raster::raster(output_tif[[1]])

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
  expect_equal(
    raster::cellStats(stored_raster, "max"),
    raster::cellStats(test_raster, "max")
  )

  expect_equal(
    png::readPNG(output_tif[[2]]),
    png::readPNG("testdata/NAIPPlus.png")
  )

  expect_equal(
    png::readPNG(output_tif[[3]]),
    png::readPNG("testdata/nhd.png")
  )
})

test_that("get_tiles gets the same tiles twice -- with georeferencing!", {
  skip_on_cran()
  output_tif <- get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("elevation", "ortho", "USGSNAIPPlus", "hydro"),
  georeference = TRUE
  )

  expect_equal(length(output_tif), 3)
  expect_equal(length(output_tif[[1]]), 1)

  stored_raster <- raster::raster("testdata/3DEP_gr.tif")
  test_raster <- raster::raster(output_tif[[1]])

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
  expect_equal(
    raster::cellStats(stored_raster, "max"),
    raster::cellStats(test_raster, "max")
  )

  stored_raster <- raster::raster("testdata/NAIPPlus_gr.tif")
  test_raster <- raster::raster(output_tif[[2]])

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
  expect_equal(
    raster::cellStats(stored_raster, "max"),
    raster::cellStats(test_raster, "max")
  )
})

test_that("get_tiles fails as expected", {
  expect_error(get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("elevation", "ortho", "USGSNAIPPlus"),
  side_length = 4097
  ))

  expect_error(get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("exactly_the_data_i_want")
  ))

  expect_error(get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("elevation"),
  side_length = 8001
  ))
})
