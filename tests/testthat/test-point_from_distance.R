test_that("point_from_distance doesn't care about classes", {
  expect_error(point_from_distance(c(
    lat = 44.05003,
    lng = -74.01164
  ), 100, 90), NA)
})
