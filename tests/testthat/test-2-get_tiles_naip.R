test_that("get_tiles gets the same ortho tiles twice", {
  skip_on_cran()

  location_of_interest <- data.frame(
    x = -123.45254,
    y = 40.61736
  )

  location_of_interest <- sf::st_as_sf(
    location_of_interest,
    coords = c("x", "y"),
    crs = 4326
  )

  location_of_interest <- set_bbox_side_length(location_of_interest, 8000)

  output_tiles <- get_tiles(location_of_interest,
                            services = c("ortho", "USGSNAIPPlus"),
                            resolution = 30,
                            georeference = FALSE
                            )

  expect_equal(names(output_tiles), "ortho")

  expect_equal(length(output_tiles), 1)
  expect_equal(length(output_tiles[[1]]), 1)

  agreement <-
    brio::read_file_raw(output_tiles[[1]]) ==
    brio::read_file_raw("testdata/NewestNaip.png")

  expect_equal(
      sum(agreement),
      length(agreement),
      tolerance = 0.95
  )
})

test_that("get_tiles gets the same georeferenced ortho tiles twice", {
  skip_on_cran()
  location_of_interest <- data.frame(
    x = -123.45254,
    y = 40.61736
  )

  location_of_interest <- sf::st_as_sf(
    location_of_interest,
    coords = c("x", "y"),
    crs = 4326
  )

  location_of_interest <- set_bbox_side_length(location_of_interest, 8000)

  output_tiles <- get_tiles(location_of_interest,
                            services = c("ortho", "USGSNAIPPlus"),
                            resolution = 30
  )

  expect_equal(names(output_tiles), "ortho")

  expect_equal(length(output_tiles), 1)
  expect_equal(length(output_tiles[[1]]), 1)

  agreement <- suppressWarnings({
    brio::read_file_raw(output_tiles[[1]]) ==
          brio::read_file_raw("testdata/NewestNaip_gr.tif")
    })

  expect_equal(
    sum(agreement),
    length(agreement),
    tolerance = 0.95
  )

})
