test_that("raster_to_raw runs without errors", {
  expect_error(
    raster_to_raw_tiles(
      input_file = "testdata/merge_rasters_test.tif",
      output_prefix = tempfile(),
      side_length = 4097,
      raw = TRUE
    ),
    NA
  )
})

test_that("raster_to_raw generates the same files twice", {
  skip_on_os(c("windows", "mac"))
  outputs <- raster_to_raw_tiles(
    input_file = "testdata/merge_rasters_test.tif",
    output_prefix = tempfile(),
    side_length = 4097,
    raw = FALSE
  )

  expect_equal(
    png::readPNG(outputs[[1]]),
    png::readPNG("testdata/raster_to_raw_1.png")
  )
})
