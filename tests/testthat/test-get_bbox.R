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
  expect_warning(
    get_coord_bbox(
      lat = c(44.04905, 44.17609, NA),
      lng = c(-74.01188, NA, -73.83493),
      na.rm = TRUE
    ),
    NA
  )
  expect_error(
    get_coord_bbox(
      lat = c(44.04905, 44.17609, NA),
      lng = c(-74.01188, NA, -73.83493),
      na.rm = FALSE
    )
  )
})

test_that("tidy framework bounding boxes work", {
  tidy_df <- data.frame(
    pt1 = c(44.04905, 44.17609),
    pt2 = c(-74.01188, -73.83493)
  )
  expect_equal(
    get_coord_bbox(tidy_df, "pt1", "pt2"),
    get_coord_bbox(lat = tidy_df$pt1, lng = tidy_df$pt2)
  )
  expect_equal(
    get_coord_bbox(tidy_df, "pt1", "pt2"),
    get_bbox(tidy_df, "pt1", "pt2")
  )
  expect_equal(
    get_coord_bbox(tidy_df, pt1, pt2),
    get_bbox(tidy_df, "pt1", "pt2")
  )
})
