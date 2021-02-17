test_that("classes are stable", {
  expect_warning(terrainr_coordinate_pair(c(44.04905, -74.01188)))
  expect_equal(
    terrainr_coordinate_pair(c(lat = 44.04905, lng = -74.01188)),
    terrainr_coordinate_pair(
      deg_to_rad(c(lat = 44.04905, lng = -74.01188)),
      "radians"
    )
  )

  expect_equal(
    terrainr_bounding_box(
      tr = c("lat" = 44.05003, "lng" = -74.01164),
      bl = c("lat" = 44.17538, "lng" = -73.83500)
    ),
    terrainr_bounding_box(
      bl = c("lat" = 44.05003, "lng" = -74.01164),
      tr = c("lat" = 44.17538, "lng" = -73.83500)
    )
  )

  expect_error(
    terrainr_bounding_box(
      bl = c(lat = 44.05003, lng = -74.01164),
      tr = c(lat = 42.17538, lng = -73.83500)
    )
  )
  expect_error(terrainr_coordinate_pair(c(foo = 44.04905, bar = -74.01188)))
})

test_that("terrainr_coordinate_pair is case-insensitive", {
  expect_equal(
    terrainr_bounding_box(
      tr = c("lat" = 44.05003, "lng" = -74.01164),
      bl = c("lat" = 44.17538, "lng" = -73.83500)
    ),
    terrainr_bounding_box(
      bl = c("LAT" = 44.05003, "LNG" = -74.01164),
      tr = c("Y" = 44.17538, "LON" = -73.83500)
    )
  )
})
