# Note: Individual data sources also have tests as test-2-get_tiles_<data>.R

test_that("SpatRast method is consistent", {
  skip_on_cran()
  skip_if_offline()
  tmp_raster <- terra::rast("testdata/merge_rasters_test.tif")
  rstr_tile <- get_tiles(tmp_raster, bboxSR = 4326, imageSR = 4326)
  downloaded_raster <- terra::rast(rstr_tile[["elevation"]])
  test_raster <- terra::rast("testdata/raster_tile.tif")
  expect_equal(as.vector(terra::crs(downloaded_raster)),
               as.vector(terra::crs(test_raster)))
  expect_equal(as.vector(terra::ext(downloaded_raster)),
               as.vector(terra::ext(test_raster)))
})

test_that("warnings fire appropriately", {
  skip_on_cran()
  tmp_raster <- terra::rast("testdata/merge_rasters_test.tif")
  terra::crs(tmp_raster) <- NA
  expect_warning(
    expect_warning(
      get_tiles(tmp_raster),
      "Assuming geographic CRS"
    ),
    "Assuming CRS of"
  )
})

test_that("The deprecated list method still works", {
  skip_on_cran()
  skip_if_offline()
  output_tif <- suppressWarnings(
    get_tiles(list(
      c(lat = 44.04905, lng = -74.01188),
      c(lat = 44.04911, lng = -74.01179),
      tolerance = 0.01
    ),
    services = c("elevation", "3DEPElevation"),
    georeference = FALSE
    )
  )

  expect_equal(names(output_tif), "elevation")

  expect_equal(length(output_tif), 1)
  expect_equal(length(output_tif[[1]]), 1)

  stored_raster <- terra::rast("testdata/3DEP.tif")
  test_raster <- terra::rast(output_tif[[1]])

  expect_equal(as.vector(terra::crs(stored_raster)),
               as.vector(terra::crs(test_raster)))
  expect_equal(as.vector(terra::ext(stored_raster)),
               as.vector(terra::ext(test_raster)))
  expect_equal(
    as.vector(terra::global(stored_raster, max))[[1]],
    as.vector(terra::global(test_raster, max))[[1]],
    tolerance = 0.01
  )
})

test_that("projected returns are consistent", {
  skip_on_cran()
  skip_if_offline()
  # For some reason, the exact boundaries appear to be system-specific
  # They're very close across systems, but the API returns a slightly wider
  # area for Mac and Windows than Linux
  skip_on_os(c("windows", "mac"))
  dl_loc <- data.frame(
    lat = c(44.04905, 44.04911),
    lng = c(-74.01188, -74.01179)
  )
  dl_loc <- sf::st_as_sf(dl_loc, coords = c("lng", "lat"))
  sf::st_crs(dl_loc) <- sf::st_crs(4326)
  dl_loc <- sf::st_transform(dl_loc, 5071)
  dl_save <- get_tiles(dl_loc)

  stored_raster <- terra::rast("testdata/projected.tif")
  test_raster <- terra::rast(dl_save[[1]])

  expect_equal(as.vector(terra::crs(stored_raster)),
               as.vector(terra::crs(test_raster)))
  expect_equal(
    as.vector(terra::global(stored_raster, max))[[1]],
    as.vector(terra::global(test_raster, max))[[1]],
    tolerance = 0.01
  )
})
