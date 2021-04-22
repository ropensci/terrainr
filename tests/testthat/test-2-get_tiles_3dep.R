test_that("get_tiles gets the same elevation tiles twice", {
  skip_on_cran()
  output_tif <- get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("elevation", "3DEPElevation"),
  georeference = FALSE
  )

  expect_equal(names(output_tif), "elevation")

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  stored_raster <- raster::raster("testdata/3DEP.tif")
  test_raster <- raster::raster(output_tif[[1]])

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

test_that("specifying SR works", {

  dat <- data.frame(
    lat = c(44.04905, 44.04911),
    lng = c(-74.01188, -74.01179)
  )

  dat <- sf::st_as_sf(dat, coords = c("lng", "lat"))
  dat <- sf::st_set_crs(dat, 4326)

  output_tif <- vector("list", 4)
  output_tif[1] <- get_tiles(dat)
  output_tif[2] <- get_tiles(dat, bboxSR = 4326)
  output_tif[3] <- get_tiles(dat, imageSR = 4326)
  output_tif[4] <- get_tiles(dat, bboxSR = 4326, imageSR = 4326)

  expect_equal(
    brio::read_file_raw(output_tif[[1]]),
    brio::read_file_raw(output_tif[[2]])
  )

  expect_equal(
    brio::read_file_raw(output_tif[[1]]),
    brio::read_file_raw(output_tif[[3]])
  )

  expect_equal(
    brio::read_file_raw(output_tif[[1]]),
    brio::read_file_raw(output_tif[[4]])
  )

})
