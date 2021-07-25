test_that("raster_to_raw warns about deprecation", {
  # on GitHub, this test fails to find temp files used in the middle of
  # raster_to_raw on windows and mac devices
  #
  # this does not occur on the windows machine I have access to, though I am
  # yet to test it on a mac. As such, I believe this may be an issue with the
  # GH environment rather than using magick to open a tempfile.
  skip_on_os(c("windows", "mac"))
  skip_on_cran()

  expect_warning(
    raster_to_raw_tiles(
      input_file = "testdata/merge_rasters_test.tif",
      output_prefix = tempfile(),
      side_length = 4097,
      raw = FALSE
    )
  )

  outputs <- suppressWarnings(
    raster_to_raw_tiles(
      input_file = "testdata/merge_rasters_test.tif",
      output_prefix = tempfile(),
      side_length = 4097,
      raw = FALSE
    )
  )

  expect_equal(
    png::readPNG(outputs[[1]]),
    png::readPNG("testdata/raster_to_raw_1.png")
  )
})
