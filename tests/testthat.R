library(testthat)
library(terrainr)

if (Sys.getenv("NOT_CRAN") == "") {
  Sys.sleep(2)
} else {
  test_check("terrainr")
}
