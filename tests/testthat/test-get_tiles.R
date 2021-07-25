# Note: Individual data sources also have tests as test-2-get_tiles_<data>.R

test_that("raster method is consistent", {
  tmp_raster <- raster::raster("testdata/merge_rasters_test.tif")
  rstr_tile <- get_tiles(tmp_raster)
  downloaded_raster <- raster::raster(rstr_tile[["elevation"]])
  test_raster <- raster::raster("testdata/raster_tile.tif")
  expect_equal(downloaded_raster@crs, test_raster@crs)
  expect_equal(downloaded_raster@extent, test_raster@extent)
})
