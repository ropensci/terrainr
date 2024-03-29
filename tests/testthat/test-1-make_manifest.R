test_that("make_manifest reproduces the same tiles", {
  # on GitHub, this test fails to find temp files used in the middle of
  # raster_to_raw on windows and mac devices
  #
  # this does not occur on the windows machine I have access to, though I am
  # yet to test it on a mac. As such, I believe this may be an issue with the
  # GH environment rather than using magick to open a tempfile.
  skip_on_os(c("windows", "mac"))
  skip_on_cran()

  outputs <- make_manifest(
    "testdata/3DEP_gr.tif",
    "testdata/NAIPPlus_gr.tif",
    output_prefix = tempfile(),
    manifest_path = tempfile(),
    importer_path = NULL
  )

  outputs_table <- read.table(outputs, sep = "\t")

  expect_equal(
    ncol(outputs_table),
    ncol(read.table("testdata/example.manifest", sep = "\t"))
  )

  expect_equal(
    png::readPNG(outputs_table$V8),
    png::readPNG("testdata/manifest_ort.png")
  )
})

test_that("prep_table warns when expected", {

  expect_warning(
    prep_table("testdata/3DEP_gr.tif",
               10,
               "import",
               type = "elevation"
    ),
    "Side lengths must be equal"
  )

})
