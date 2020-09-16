test_that("get_heightmap doesn't care about classes", {
  expect_error(get_heightmap(
    list(
      c(lat = 44.04905, lng = -74.01188),
      c(lat = 44.04911, lng = -74.01179)
    ),
    tempfile(fileext = ".tif")
  ), NA)
})
