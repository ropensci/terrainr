test_that("merge_raster expected errors error expectedly", {
  expect_error(merge_rasters("dummy_input.tif", "dummy_output.R"))
})

test_that("merge_raster files are identical no matter the filename", {
  skip_on_cran()
  tmptif <- lapply(1:3, function(x) tempfile(fileext = ".tif"))
  tmptif[[4]] <- tempfile(fileext = ".tiff")
  first_tile <- add_bbox_buffer(get_coord_bbox(lat = 44.05003,
                                               lng = -74.01164), 10)
  second_tile <- terrainr_bounding_box(
    bl = c(first_tile@bl@lat, first_tile@tr@lng),
    tr = point_from_distance(first_tile@tr, 10, 90)
  )
  get_heightmap(
    first_tile,
    tmptif[[1]]
  )
  get_heightmap(
    second_tile,
    tmptif[[2]]
  )

  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[3]])
  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[4]])

  expect_equal(
    raster::raster(tmptif[[3]])@extent,
    raster::raster(tmptif[[4]])@extent
  )
})
