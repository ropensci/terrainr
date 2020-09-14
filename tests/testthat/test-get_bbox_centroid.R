test_that("get_bbox_centroid doesn't care about classes", {
  expect_equal(
    get_bbox_centroid(list(
      c(lat = 44.04905, lng = -74.01188),
      c(lat = 44.17609, lng = -73.83493)
    )),
    get_bbox_centroid(
      terrainr_bounding_box(
        c(lat = 44.04905, lng = -74.01188),
        c(lat = 44.17609, lng = -73.83493)
      )
    )
  )
})
