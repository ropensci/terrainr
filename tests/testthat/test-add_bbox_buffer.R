test_that("add_bbox_buffer doesn't care about classes", {
  expect_equal(
    add_bbox_buffer(list(
      c(lat = 44.04905, lng = -74.01188),
      c(lat = 44.17609, lng = -73.83493)
    ), 10),
    add_bbox_buffer(
      terrainr_bounding_box(
        c(lat = 44.04905, lng = -74.01188),
        c(lat = 44.17609, lng = -73.83493)
      ), 10
    )
  )
})

test_that("divisible works, ish", {
  divide_me <- add_bbox_buffer(
    terrainr_bounding_box(
      c(lat = 44.04905, lng = -74.01188),
      c(lat = 44.17609, lng = -73.83493)
      ), 10, divisible = 4096
  )
  tl <- c(divide_me@tr@lat, divide_me@bl@lng)
  expect_equal(calc_haversine_distance(tl, divide_me@tr) / 4096,
               4,
               tolerance = 0.005)
  expect_equal(calc_haversine_distance(tl, divide_me@bl) / 4096,
               5,
               tolerance = 0.005)
})
