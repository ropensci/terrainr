test_that("set_bbox_side_length works within 1%", {
  df <- data.frame(
    lat = c(44.04905, 44.17609),
    lng = c(-74.01188, -73.83493)
  )

  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
  df_sf <- sf::st_set_crs(df_sf, 4326)

  bbox <- set_bbox_side_length(df_sf, 8000)
  bbox <- sf::st_bbox(bbox)

  tl <- c(lat = bbox[["ymax"]], lng = bbox[["xmin"]])
  expect_equal(calc_haversine_distance(
    c(lat = bbox[["ymax"]], lng = bbox[["xmax"]]),
    tl
  ),
  8000,
  tolerance = 8000 * 0.005
  )
  expect_equal(calc_haversine_distance(
    c(lat = bbox[["ymin"]], lng = bbox[["xmin"]]),
    tl
  ),
  8000,
  tolerance = 8000 * 0.005
  )
})
