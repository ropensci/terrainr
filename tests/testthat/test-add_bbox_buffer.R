test_that("set_bbox_side_length works within 1%", {
  df <- data.frame(
    lat = c(44.04905, 44.17609),
    lng = c(-74.01188, -73.83493)
  )

  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))

  expect_warning(
    set_bbox_side_length(df_sf, 8000),
    "No CRS associated with input data. Assuming EPSG:4326.\n"
  )
  expect_error(
    set_bbox_side_length(df_sf, 8000, error_crs = TRUE),
    "No CRS associated with input data."
  )
  bbox_default <- set_bbox_side_length(df_sf, 8000, error_crs = FALSE)

  df_sf <- sf::st_set_crs(df_sf, 4326)

  bbox <- set_bbox_side_length(df_sf, 8000)

  expect_equal(
    sf::st_crs(bbox_default)$wkt,
    sf::st_crs(bbox)$wkt
  )

  expect_equal(
    sf::st_set_crs(bbox_default, sf::NA_crs_),
    sf::st_set_crs(bbox, sf::NA_crs_)
  )

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

  tmp_raster <- terra::rast("testdata/merge_rasters_test.tif")
  rstr_bbox <- set_bbox_side_length(tmp_raster, 8000)
  rstr_bbox <- sf::st_bbox(rstr_bbox)

  tl <- c(lat = rstr_bbox[["ymax"]], lng = rstr_bbox[["xmin"]])
  expect_equal(calc_haversine_distance(
    c(lat = rstr_bbox[["ymax"]], lng = rstr_bbox[["xmax"]]),
    tl
  ),
  8000,
  tolerance = 8000 * 0.005
  )
  expect_equal(calc_haversine_distance(
    c(lat = rstr_bbox[["ymin"]], lng = rstr_bbox[["xmin"]]),
    tl
  ),
  8000,
  tolerance = 8000 * 0.005
  )

  expect_equal(
    rstr_bbox,
    sf::st_bbox(add_bbox_buffer(
      tmp_raster,
      sqrt((8000^2) * 2) / 2
    )),
    tolerance = 0.0001
  )
})
