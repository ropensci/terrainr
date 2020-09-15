test_that("heightmap retrieval works", {
  skip_on_cran()
  expect_error(
    hit_heightmap_api(
      list(
        c(lat = 44.04905, lng = -74.01188),
        c(lat = 44.17609, lng = -73.83493)
      ),
      100,
      100,
      verbose = TRUE
    ),
    NA
  )
})
