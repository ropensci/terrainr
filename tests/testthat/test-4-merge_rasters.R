test_that("merge_raster files are identical no matter the filename", {
  skip_on_cran()
  df <- data.frame(
    lat = c(44.050030001, 44.05003),
    lng = c(-74.01164, -74.011640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
  df_sf <- sf::st_set_crs(df_sf, 4326)
  first_tile <- add_bbox_buffer(df_sf, 10)
  ft_bbox <- sf::st_bbox(first_tile)

  df <- data.frame(
    lat = c(44.051030001, 44.05103),
    lng = c(-74.01264, -74.012640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
  df_sf <- sf::st_set_crs(df_sf, 4326)
  second_tile <- add_bbox_buffer(df_sf, 10)
  # assign the output tile filenames...
  tmptif <- vector("list")
  tmptif[[1]] <- get_tiles(first_tile)[[1]]
  tmptif[[2]] <- get_tiles(second_tile)[[1]]

  # create two outputs, one that needs fix_tif and one that doesn't:
  tmptif[[3]] <- tempfile(fileext = ".tif")
  tmptif[[4]] <- tempfile(fileext = ".tiff")

  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[3]])
  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[4]])

  expect_equal(
    raster::raster(tmptif[[3]])@extent,
    raster::raster(tmptif[[4]])@extent
  )

  stored_raster <- raster::raster("testdata/merge_dem.tif")
  test_raster <- raster::raster(tmptif[[4]])

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
})
