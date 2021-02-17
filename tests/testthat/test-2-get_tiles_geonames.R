test_that("get_tiles gets the same geonames tiles twice", {
  skip_on_cran()
  output_tif <- get_tiles(list(
    c(lat = 44.04905, lng = -74.01188),
    c(lat = 44.04911, lng = -74.01179)
  ),
  services = c("geonames"),
  georeference = FALSE
  )

  expect_equal(names(output_tif), "geonames")

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  expect_equal(
    png::readPNG(output_tif[[1]]),
    png::readPNG("testdata/geonames.png")
  )
})
