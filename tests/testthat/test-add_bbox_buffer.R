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
