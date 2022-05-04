test_that("make_unity is stable", {

  Sys.setenv("unifir_debugmode" = TRUE)

  outputs <- make_unity(
    file.path(tempdir(), "make_unity"),
    "testdata/3DEP_gr.tif",
    "testdata/NAIPPlus_gr.tif",
    action = FALSE,
    unity = unifir::waiver()
  )

  expect_equal(
    length(outputs$props),
    nrow(outputs$beats)
  )

  expect_equal(
    length(outputs$props),
    6
  )

  expect_match(
    outputs$scene_name,
    "terrainr_scene"
  )

  Sys.setenv("unifir_debugmode" = "")

})
