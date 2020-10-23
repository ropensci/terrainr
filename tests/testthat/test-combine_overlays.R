test_that("combine_overlays works", {
  x_tmp <- tempfile(fileext = ".png")
  y_tmp <- tempfile(fileext = ".png")

  x <- combine_overlays("testdata/NAIPPlus.png",
    "testdata/vto_point.png",
    output_file = x_tmp
  )
  y <- combine_overlays("testdata/NAIPPlus_gr.tif",
    "testdata/vto_point.png",
    output_file = y_tmp
  )

  expect_equal(
    png::readPNG(x_tmp),
    png::readPNG(y_tmp)
  )

  x <- combine_overlays("testdata/NAIPPlus.png",
    "testdata/vto_point.png",
    output_file = x_tmp,
    transparency = 18
  )
  y <- combine_overlays("testdata/NAIPPlus_gr.tif",
    "testdata/vto_point.png",
    output_file = y_tmp,
    transparency = 0.18
  )

  expect_equal(
    png::readPNG(x_tmp),
    png::readPNG(y_tmp)
  )
})
