test_that("the generalized pipeline runs", {
  testthat::skip_on_cran()
  expect_error(generate_raw_tiles(
    lat = c(44.04911, 44.04905),
    lng = c(-74.01188, -74.01179),
    buffer_distance = 1,
    unity_friendly = TRUE,
    tif_filename = NULL,
    raw_output_prefix = tempfile(),
    save_tif = FALSE
  ), NA)
})

test_that("sensible errors occur", {
  simulated_data <- data.frame(
    id = seq(1, 100, 1),
    lat = runif(100, 44.04905, 44.17609),
    lng = runif(100, -74.01188, -73.83493)
  )
  expect_error(generate_raw_tiles(
    data = simulated_data,
    lat = lat,
    lng = lng,
    bbox = NULL,
    buffer_distance = 100,
    unity_friendly = TRUE,
    tif_filename = NULL,
    raw_output_prefix = tempfile(),
    save_tif = TRUE
  ))
})
