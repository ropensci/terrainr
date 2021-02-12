# Note: Individual data sources also have tests as test-2-get_tiles_<data>.R
test_that("split_bbox works consistently", {
  simulated_data <- data.frame(
    id = seq(1, 100, 1),
    lat = runif(100, 44.04905, 44.17609),
    lng = runif(100, -74.01188, -73.83493)
  )

  side_length <- 4096

  bbox <- terrainr_bounding_box(
    bl = c(lat = min(simulated_data$lat), lng = min(simulated_data$lng)),
    tr = c(lat = max(simulated_data$lat), lng = max(simulated_data$lng))
  )
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
    tolerance = side_length * 0.005
  )
  expect_equal(terrainr::calc_haversine_distance(tl, first_tile@tr),
    side_length,
    tolerance = side_length * 0.005
  )

  middle_tile <- bbox_tiles[[2]][[3]][[1]]
  tl <- c(middle_tile@tr@lat, middle_tile@bl@lng)
  expect_equal(terrainr::calc_haversine_distance(tl, middle_tile@bl),
    side_length,
    tolerance = side_length * 0.005
  )
  expect_equal(terrainr::calc_haversine_distance(tl, middle_tile@tr),
    side_length,
    tolerance = side_length * 0.005
  )

  secondary_splits <- split_bbox(bbox, side_length, resolution = 2)
  tertiary_splits <- split_bbox(bbox, side_length, resolution = 3)

  secondary_bbox_tiles <- secondary_splits[[1]]
  x_dir <- length(secondary_bbox_tiles)
  y_dir <- length(secondary_bbox_tiles[[x_dir]])
  secondary_img_width <- (4096 * (x_dir - 1)) +
    secondary_bbox_tiles[[x_dir]][[y_dir]]$img_width

  x_dir <- length(bbox_tiles)
  y_dir <- length(bbox_tiles[[x_dir]])
  img_width <- (4096 * (x_dir - 1)) +
    bbox_tiles[[x_dir]][[y_dir]]$img_width

  expect_equal(secondary_img_width * 2,
    img_width,
    tolerance = 1
  )

  tertiary_bbox_tiles <- tertiary_splits[[1]]
  x_dir <- length(tertiary_bbox_tiles)
  y_dir <- length(tertiary_bbox_tiles[[x_dir]])
  tertiary_img_width <- (4096 * (x_dir - 1)) +
    tertiary_bbox_tiles[[x_dir]][[y_dir]]$img_width

  expect_equal(tertiary_img_width * 2,
    img_width,
    tolerance = 1
  )
})
