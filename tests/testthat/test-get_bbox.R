test_that("single coordinate bounding boxes are allowed", {
  expect_error(get_coord_bbox(lat = 44.05003, lng = -74.01164), NA)
})

test_that("NA coordinates get a warning", {
  expect_warning(
    get_coord_bbox(
      lat = c(44.04905, 44.17609, NA),
      lng = c(-74.01188, NA, -73.83493)
    )
  )
})
