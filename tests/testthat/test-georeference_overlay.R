test_that("georeference_overlay edge cases work", {

  # change input file name and load raster
  expect_equal(
    tiff::readTIFF(georeference_overlay(
      "testdata/NAIPPlus_gr.tif",
      "testdata/NAIPPlus_gr.tif",
      tempfile(fileext = ".tif")
    )),
    tiff::readTIFF("testdata/NAIPPlus_gr.tif")
  )
})

test_that("georeference_overlay produces the same file twice", {
  gr_rst <- terra::rast(georeference_overlay(
    "testdata/NAIPPlus.png",
    "testdata/NAIPPlus_gr.tif",
    tempfile(fileext = ".tif")
  ))
  ref_rst <- terra::rast("testdata/NAIPPlus_gr.tif")
  expect_equal(
    terra::crs(gr_rst),
    terra::crs(ref_rst)
  )
  expect_equal(
    as.vector(terra::ext(gr_rst)),
    as.vector(terra::ext(ref_rst))
  )
})
