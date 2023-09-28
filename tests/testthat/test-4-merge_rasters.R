test_that("merge_raster files are identical no matter the filename", {
  skip_on_cran()
  skip_if_offline()
  df <- data.frame(
    lat = c(44.050030001, 44.05003),
    lng = c(-74.01164, -74.011640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
  first_tile <- add_bbox_buffer(df_sf, 10)
  ft_bbox <- sf::st_bbox(first_tile)

  df <- data.frame(
    lat = c(44.051030001, 44.05103),
    lng = c(-74.01264, -74.012640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
  second_tile <- add_bbox_buffer(df_sf, 10)
  # assign the output tile filenames...
  tmptif <- vector("list")
  tmptif[[1]] <- get_tiles(first_tile)[[1]]
  tmptif[[2]] <- get_tiles(second_tile)[[1]]

  # create two outputs, one that needs fix_tif and one that doesn't:
  tmptif[[3]] <- tempfile(fileext = ".tif")
  tmptif[[4]] <- tempfile(fileext = ".tiff")

  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[3]])
  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[4]])

  expect_equal(
    as.vector(terra::ext(terra::rast(tmptif[[3]]))),
    as.vector(terra::ext(terra::rast(tmptif[[4]])))
  )

  stored_raster <- terra::rast("testdata/merge_dem.tif")
  test_raster <- terra::rast(tmptif[[4]])

  expect_equal(as.vector(terra::crs(stored_raster)),
               as.vector(terra::crs(test_raster)))
  expect_equal(as.vector(terra::ext(stored_raster)),
               as.vector(terra::ext(test_raster)))

})

test_that("fallback method works", {
  skip_on_cran()
  skip_if_offline()
  # Just for time savings
  skip_on_os(c("windows", "mac"))
  df <- data.frame(
    lat = c(44.050030001, 44.05003),
    lng = c(-74.01164, -74.011640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
  df_sf <- sf::st_set_crs(df_sf, 4326)
  first_tile <- add_bbox_buffer(df_sf, 10)
  ft_bbox <- sf::st_bbox(first_tile)

  df <- data.frame(
    lat = c(44.051030001, 44.05103),
    lng = c(-74.01264, -74.012640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
  df_sf <- sf::st_set_crs(df_sf, 4326)
  second_tile <- add_bbox_buffer(df_sf, 10)
  # assign the output tile filenames...
  tmptif <- vector("list")
  tmptif[[1]] <- get_tiles(first_tile)[[1]]
  tmptif[[2]] <- get_tiles(second_tile)[[1]]

  # create two outputs, one that needs fix_tif and one that doesn't:
  tmptif[[3]] <- tempfile(fileext = ".tif")
  tmptif[[4]] <- tempfile(fileext = ".tiff")

  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[3]], force_fallback = TRUE)
  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[4]], force_fallback = TRUE)

  expect_equal(
    as.vector(terra::ext(terra::rast(tmptif[[3]]))),
    as.vector(terra::ext(terra::rast(tmptif[[4]])))
  )

  stored_raster <- terra::rast("testdata/merge_dem.tif")
  test_raster <- terra::rast(tmptif[[4]])

  expect_equal(as.vector(terra::crs(stored_raster)),
               as.vector(terra::crs(test_raster)))
  expect_equal(as.vector(terra::ext(stored_raster)),
               as.vector(terra::ext(test_raster)))
})

test_that("overwrite works as expected", {
  skip_on_cran()
  skip_if_offline()
  test_file <- tempfile(fileext = ".tif")
  test_copy <- tempfile(fileext = ".tif")

  df <- data.frame(
    lat = c(44.050030001, 44.05003),
    lng = c(-74.01164, -74.011640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
  first_tile <- add_bbox_buffer(df_sf, 10)
  ft_bbox <- sf::st_bbox(first_tile)

  df <- data.frame(
    lat = c(44.051030001, 44.05103),
    lng = c(-74.01264, -74.012640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
  second_tile <- add_bbox_buffer(df_sf, 10)

  tmptif <- vector("list")
  tmptif[[1]] <- get_tiles(first_tile)[[1]]
  tmptif[[2]] <- get_tiles(second_tile)[[1]]

  merge_rasters(c(tmptif[[1]], tmptif[[2]]), test_file)
  file.copy(test_file, test_copy)
  expect_error(
    merge_rasters(c(tmptif[[1]], tmptif[[2]]), test_file),
    "File exists at"
  )
  expect_warning(
    merge_rasters(c(tmptif[[1]], tmptif[[2]]), test_file, overwrite = TRUE),
    NA
  )
  expect_warning(
    merge_rasters(c(tmptif[[1]], tmptif[[2]]),
      test_file,
      options = "-overwrite"
    ),
    NA
  )
  expect_warning(
    merge_rasters(c(tmptif[[1]], tmptif[[2]]),
      test_file,
      overwrite = TRUE, options = "-overwrite"
    ),
    NA
  )

  merge_rasters(c(tmptif[[2]]), output_raster = test_file, overwrite = TRUE)

  expect_false(
    all(
      as.vector(terra::ext(terra::rast(test_file))) ==
        as.vector(terra::ext(terra::rast(test_copy)))
    )
  )
})
