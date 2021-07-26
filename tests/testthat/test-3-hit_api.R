test_that("dots replace their arguments", {
  skip_on_cran()
  expect_equal(
    hit_national_map_api(
      list(
        c(lat = 44.04905, lng = -74.01188),
        c(lat = 44.04911, lng = -74.01179)
      ),
      100,
      100,
      "3DEPElevation"
    ),
    hit_national_map_api(list(
      c(lat = 44.04905, lng = -74.01188),
      c(lat = 44.04911, lng = -74.01179)
    ),
    8000,
    8000,
    "3DEPElevation",
    verbose = TRUE,
    size = "100,100"
    )
  )
})
