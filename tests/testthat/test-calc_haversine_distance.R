test_that("Distance calculations make sense", {
  expect_equal(
    calc_haversine_distance(
      c(lat = 44.121268, lng = -73.903734),
      c(lat = 43.121268, lng = -74.903734)
    ),
    137270.5,
    tolerance = 0.005
  )
})
