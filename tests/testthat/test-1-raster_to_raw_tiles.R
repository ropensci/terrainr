test_that("raster_to_raw runs without errors", {
  # These files aren't included in the repo or package, so can't run these tests
  # But important to validate they pass locally!
  skip("Not running raster_to_raw tests")

  outputs <- raster_to_raw_tiles(
    input_file = "testdata/test_ortho.tif",
    output_prefix = tempfile(),
    side_length = 4097,
    raw = FALSE
  )

  expect_equal(png::readPNG(outputs[[1]]), "testdata/test_final_1_1.png")

  expect_error(
    raster_to_raw_tiles(
      input_file = "testdata/test_merge.tif",
      output_prefix = tempfile(),
      side_length = 4097,
      raw = TRUE
    ),
    NA
  )
})
