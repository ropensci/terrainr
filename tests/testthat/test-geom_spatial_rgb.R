test_that("all methods of geom_spatial_rgb are equivalent", {
  skip_on_cran()
  simulated_data <- data.frame(id = seq(1, 100, 1),
                               lat = runif(100, 44.04905, 44.17609),
                               lng = runif(100, -74.01188, -73.83493))

  simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
  simulated_data <- sf::st_set_crs(simulated_data, 4326)

  output_tiles <- get_tiles(simulated_data,
                            services = c("ortho"),
                            resolution = 120)

  merged_ortho <- tempfile(fileext = ".tif")
  merge_rasters(output_tiles[["ortho"]], merged_ortho)

  test <- raster::stack(merged_ortho)
  test_df <- raster::as.data.frame(test, xy = TRUE)
  test_df <- setNames(test_df, c("x", "y", "red", "green", "blue"))

  plots <- vapply(1:6, function(x) tempfile(fileext = ".png"), character(1))

  ggplot2::ggplot() +
    geom_spatial_rgb(data = test_df,
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            r = red,
                                            g = green,
                                            b = blue)) +
    ggplot2::geom_sf(data = simulated_data) +
    ggplot2::ggsave(plots[[1]])

  ggplot2::ggplot() +
    geom_spatial_rgb(data = test,
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            r = red,
                                            g = green,
                                            b = blue)) +
    ggplot2::geom_sf(data = simulated_data) +
    ggplot2::ggsave(plots[[2]])

  ggplot2::ggplot() +
    geom_spatial_rgb(data = merged_ortho,
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            r = red,
                                            g = green,
                                            b = blue)) +
    ggplot2::geom_sf(data = simulated_data) +
    ggplot2::ggsave(plots[[3]])

  ggplot2::ggplot() +
    stat_spatial_rgb(data = test_df,
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            r = red,
                                            g = green,
                                            b = blue),
                     scale = 1) +
    ggplot2::geom_sf(data = simulated_data) +
    ggplot2::ggsave(plots[[4]])

  ggplot2::ggplot() +
    stat_spatial_rgb(data = test,
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            r = red,
                                            g = green,
                                            b = blue)) +
    ggplot2::geom_sf(data = simulated_data) +
    ggplot2::ggsave(plots[[5]])

  ggplot2::ggplot() +
    stat_spatial_rgb(data = merged_ortho,
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            r = red,
                                            g = green,
                                            b = blue)) +
    ggplot2::geom_sf(data = simulated_data) +
    ggplot2::ggsave(plots[[6]])

  expect_identical(
    brio::read_file_raw(plots[[1]]),
    brio::read_file_raw(plots[[2]]),
  )

  expect_identical(
    brio::read_file_raw(plots[[3]]),
    brio::read_file_raw(plots[[4]]),
  )

  expect_identical(
    brio::read_file_raw(plots[[5]]),
    brio::read_file_raw(plots[[6]]),
  )

  expect_identical(
    brio::read_file_raw(plots[[2]]),
    brio::read_file_raw(plots[[3]]),
  )

  expect_identical(
    brio::read_file_raw(plots[[1]]),
    brio::read_file_raw(plots[[6]]),
  )

})
