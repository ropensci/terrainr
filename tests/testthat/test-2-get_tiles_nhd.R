test_that("get_tiles gets the same hydro tiles twice", {
  skip_on_cran()
  output_tif <- get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("hydro", "nhd", "NHDPlus_HR"),
  georeference = FALSE
  )

  expect_equal(names(output_tif), c("hydro", "NHDPlus_HR"))

  expect_equal(length(output_tif), 2)
  expect_equal(length(output_tif[[1]]), 1)

  expect_equal(
    png::readPNG(output_tif[[1]]),
    png::readPNG("testdata/nhd.png")
  )

  expect_equal(
    png::readPNG(output_tif[[2]]),
    png::readPNG("testdata/NHDPlus_HR.png")
  )
})
