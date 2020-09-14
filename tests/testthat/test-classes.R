test_that("classes are stable", {
  expect_message(terrainr_coordinate_pair(c(44.04905, -74.01188)))
  expect_equal(
    terrainr_coordinate_pair(c(44.04905, -74.01188)),
    terrainr_coordinate_pair(deg_to_rad(c(44.04905, -74.01188)), "radians")
  )

  expect_error(
    terrainr_bounding_box(
      bl = c("lat" = 44.05003, "lng" = -74.01164),
      tr = c("lat" = 42.17538, "lng" = -73.83500)
    )
  )
  expect_error(terrainr_coordinate_pair(c(foo = 44.04905, bar = -74.01188)))
})

test_that("export functions are stable", {
  expect_warning(export_coord_pair("not a coord pair"))
  expect_warning(export_bounding_box("not a bounding box"))

  expect_equal(
    export_coord_pair(terrainr_coordinate_pair(c(44.04905, -74.01188))),
    c(44.04905, -74.01188)
  )

  expect_equal(
    export_bounding_box(terrainr_bounding_box(
      bl = c("lat" = 44.05003, "lng" = -74.01164),
      tr = c("lat" = 44.17538, "lng" = -73.83500)
    )),
    list(
      bl = c("lat" = 44.05003, "lng" = -74.01164),
      tr = c("lat" = 44.17538, "lng" = -73.83500)
    )
  )
})
