test_that("vector_to_overlay fails as expected", {
  expect_error(
    vector_to_overlay(
      system.file("shape/nc.shp", package = "sf"),
      100
    ),
    "Raster"
  )

  expect_error(
    vector_to_overlay(100),
    "sf"
  )
})

test_that("vector_to_overlay generates the same tiles", {
  skip_on_cran()
  # need to generate these images on other OS
  skip_on_os(c("windows", "mac"))
  # Generate points to download raster tiles for
  load("testdata/sim_dat.rds")

  # Download raster tiles and merge them into a single raster
  simulated_data_sf <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
  sds_cache <- simulated_data_sf
  sf::st_crs(simulated_data_sf) <- sf::st_crs(4326)
  downloaded_tiles <- get_tiles(simulated_data_sf, tempfile(fileext = ".tif"))
  merged_file <- merge_rasters(
    downloaded_tiles[[1]],
    tempfile(fileext = ".tif")
  )

  #' Overlay image for points
  agreement <- sum(
    png::readPNG(
      vector_to_overlay(
        simulated_data_sf,
        merged_file[[1]],
        size = 5,
        color = "red",
        na.rm = TRUE
      )
    ) == png::readPNG("testdata/vto_point.png")
  ) / length(png::readPNG("testdata/vto_point.png"))

  expect_true(
    agreement > 0.95
  )

  #' CRS warns
  expect_warning(
    png::readPNG(vector_to_overlay(
      sds_cache,
      merged_file[[1]],
      size = 5,
      color = "red",
      na.rm = TRUE
    ))
  )

  #' No CRS necessary
  expect_equal(
    png::readPNG(vector_to_overlay(
      simulated_data_sf,
      merged_file[[1]],
      size = 5,
      color = "red",
      na.rm = TRUE
    )),
    png::readPNG(vector_to_overlay(
      sds_cache,
      merged_file[[1]],
      size = 5,
      color = "red",
      na.rm = TRUE,
      error_crs = FALSE
    ))
  )

  # Overlay image for lines
  agreement <- sum(
    png::readPNG(
      vector_to_overlay(
        sf::st_cast(sf::st_union(simulated_data_sf), "LINESTRING"),
        merged_file[[1]],
        linewidth = 5,
        color = "red",
        na.rm = TRUE
      )
    ) == png::readPNG("testdata/vto_line.png")
  ) / length(png::readPNG("testdata/vto_line.png"))

  expect_true(
    agreement > 0.95
  )

  # Overlay image for polygons
  agreement <- sum(
    png::readPNG(
      vector_to_overlay(
        sf::st_cast(sf::st_union(simulated_data_sf), "POLYGON"),
        merged_file[[1]],
        linewidth = 5,
        color = "red",
        na.rm = TRUE
      )
    ) == png::readPNG("testdata/vto_poly.png")
  ) / length(png::readPNG("testdata/vto_poly.png"))

  expect_true(
    agreement > 0.95
  )
})
