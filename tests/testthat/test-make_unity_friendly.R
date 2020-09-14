test_that("classes aren't necessary", {
  expect_equal(
    make_unity_friendly(
      terrainr_bounding_box(
        c(lat = 44.04905, lng = -74.01188),
        c(lat = 44.17609, lng = -73.83493)
      )
    ),
    make_unity_friendly(
      list(
        c(lat = 44.04905, lng = -74.01188),
        c(lat = 44.17609, lng = -73.83493)
      )
    )
  )
})
