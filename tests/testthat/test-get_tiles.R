# Note: Individual data sources also have tests as test-2-get_tiles_<data>.R
test_that("split_bbox works consistently", {
  simulated_data <- data.frame(
    id = seq(1, 100, 1),
    lat = runif(100, 44.04905, 44.17609),
    lng = runif(100, -74.01188, -73.83493)
  )

  side_length <- 4096

  bbox <- get_coord_bbox(lat = simulated_data$lat, lng = simulated_data$lng)
  bbox <- add_bbox_buffer(bbox, 100)
  splits <- split_bbox(bbox, side_length)


  expect_equal(splits[[2]], 4)
  expect_equal(splits[[3]], 4)
  expect_equal(splits[[2]], splits[[3]])

  bbox_tiles <- splits[[1]]
  first_tile <- bbox_tiles[[1]][[1]][[1]]

  tl <- c(first_tile@tr@lat, first_tile@bl@lng)
  expect_equal(terrainr::calc_haversine_distance(tl, first_tile@bl),
               side_length,
               tolerance = side_length * 0.005)
  expect_equal(terrainr::calc_haversine_distance(tl, first_tile@tr),
               side_length,
               tolerance = side_length * 0.005)

  middle_tile <- bbox_tiles[[2]][[3]][[1]]
  tl <- c(middle_tile@tr@lat, middle_tile@bl@lng)
  expect_equal(terrainr::calc_haversine_distance(tl, middle_tile@bl),
               side_length,
               tolerance = side_length * 0.005)
  expect_equal(terrainr::calc_haversine_distance(tl, middle_tile@tr),
               side_length,
               tolerance = side_length * 0.005)

})
