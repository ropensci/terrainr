test_that("georeference_overlay edge cases work", {

  # change input file name and load raster
  expect_equal(
    tiff::readTIFF(georeference_overlay(
      "testdata/NAIPPlus_gr.tif",
      tempfile(fileext = ".tif"),
      "testdata/NAIPPlus_gr.tif"
    )),
    tiff::readTIFF("testdata/NAIPPlus_gr.tif")
  )
})

test_that("georeference_overlay produces the same file twice", {
  gr_rst <- raster::raster(georeference_overlay(
    "testdata/NAIPPlus.png",
    tempfile(fileext = ".tif"),
    "testdata/NAIPPlus_gr.tif"
  ))
  gr_rst@file@name <- ""
  gr_rst@data@names <- ""

  ref_rst <- raster::raster("testdata/NAIPPlus_gr.tif")
  ref_rst@file@name <- ""
  ref_rst@data@names <- ""

  expect_equal(
    gr_rst,
    ref_rst
  )
})
