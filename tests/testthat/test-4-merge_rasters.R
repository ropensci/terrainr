test_that("merge_raster expected errors error expectedly", {
  expect_error(merge_rasters("dummy_input.tif", "dummy_output.R"))
  expect_error(merge_rasters(
    "dummy_input.tif",
    "dummy_output.tif",
    "dummy_input.png"
  ))
  expect_error(merge_rasters(
    "dummy_input.tif",
    "dummy_output.tif",
    c("dummy_input.png", "dummy_input.png"),
    "dummy_output.tif"
  ))

  expect_error(merge_rasters("dummy_input.tif",
    NULL,
    "dummy_input.png",
    "dummy_output.png",
    merge_raster = FALSE
  ))
  expect_error(merge_rasters(
    "dummy_input.tif",
    "dummy_output.png",
    "dummy_input.png",
    "dummy_output.tif"
  ))
})

test_that("merge_raster files are identical no matter the filename", {
  skip_on_cran()
  first_tile <- add_bbox_buffer(get_coord_bbox(
    lat = 44.05003,
    lng = -74.01164
  ), 10)
  second_tile <- terrainr_bounding_box(
    bl = c(first_tile@bl@lat, first_tile@tr@lng),
    tr = point_from_distance(first_tile@tr, 10, 90)
  )
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
})

test_that("merge_raster works with orthos", {
  skip_on_cran()
  tmpf <- get_tiles(add_bbox_buffer(get_coord_bbox(
    lat = 44.05003,
    lng = -74.01164
  ), 10),
  services = c("elevation", "ortho"),
  georeference = FALSE
  )
  merge_orth <- tempfile(fileext = ".tiff")
  merge_rasters(tmpf[[1]], tempfile(fileext = ".tif"), tmpf[[2]], merge_orth)

  stored_raster <- raster::raster("testdata/merge_rasters_test.tif")
  test_raster <- raster::raster(merge_orth)

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
})

test_that("merging rasters with different nbands works", {
  skip_on_cran()

  tile1 <- tempfile(fileext = ".tif")
  tile2 <- tempfile(fileext = ".tif")
  merged <- tempfile(fileext = ".tif")

  download.file(
    "https://github.com/mikemahoney218/common-files/releases/download/rasters-different-bands/tile1.tif", # nolint
    tile1
    )
  download.file(
    "https://github.com/mikemahoney218/common-files/releases/download/rasters-different-bands/tile2.tif", # nolint
    tile2
  )
  download.file(
    "https://github.com/mikemahoney218/common-files/releases/download/rasters-different-bands/merged.tif", # nolint
    merged
  )

  out_merge <- merge_rasters(c(tile1, tile2))
  out_merge <- raster::raster(out_merge[[1]])
  out_merge@file@name <- ""
  out_merge@data@names <- ""

  ref_rst <- raster::raster(merged)
  ref_rst@file@name <- ""
  ref_rst@data@names <- ""

  expect_equal(out_merge, ref_rst)

})
