test_that("distance conversions make sense", {
  expect_equal(
    round(convert_distance(0.621371, "miles")),
    1000
  )
  expect_equal(
    round(convert_distance(0.621371, "mi")),
    1000
  )
  expect_equal(
    convert_distance(1, "kilometers"),
    1000
  )
  expect_equal(
    convert_distance(1, "km"),
    1000
  )
  expect_equal(
    round(convert_distance(3280.84, "feet")),
    1000
  )
  expect_equal(
    round(convert_distance(3280.84, "ft")),
    1000
  )
  expect_equal(terrainr::convert_distance(100, "feet"),
               terrainr::convert_distance(1200, "inches"),
               tolerance = 0.001)
})
