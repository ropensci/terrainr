
<!-- README.md is generated from README.Rmd. Please edit that file -->

# terrainr

<!-- badges: start -->

<!-- badges: end -->

## Overview

terrainr makes it easy to identify your area of interest from point
data, retrieve data for that area from the National Map API, and then
process that data into larger, joined images or crop it into tiles that
can be imported into the Unity rendering engine.

At the absolute simplest level, terrainr provides a convenient and
consistent API to downloading data from the National Map, currently
supporting downloading DEMs from the 3D Elevation Program and
orthoimages from the National Agriculture Imagery Program.

``` r
library(terrainr)
output_tiles <- get_tiles(bbox = list(c(lat = 44.13926, lng = -74.01099),
                                      c(lat = 44.17609, lng = -73.95974)),
                          services = c("elevation", "ortho"))

# output_tiles is now a list of two vectors pointing to the elevation and 
# orthoimagery tiles we just downloaded -- here we're displaying the first
# of the ortho tiles
knitr::include_graphics(output_tiles[[2]][[1]])
```

<img src="/tmp/RtmpkYRD2t/fileac0a57819d51_USGSNAIPPlus_1_1.png" width="100%" />

Once downloaded, these images are in standard GeoTIFF or PNG formats and
can be used as expected with other utilities:

``` r
raster_read <- raster::raster(output_tiles[[1]][[1]])
raster::plot(raster_read)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Additionally, terrainr provides functions to transform these tiles into
RAW images ready to be imported into the Unity rendering engine,
allowing you to fly or walk through your downloaded data sets:

``` r
# We need to convert our PNG orthoimage to a TIFF for raster_to_raw_tiles

# When working with larger areas than our toy example here, this happens
# naturally when using merge_rasters(). We'll fake that step of the 
# process here:
output_tiles[[3]] <- tempfile(fileext = ".tif")
merge_rasters(output_tiles[[1]], 
              output_tiles[[1]],
              output_tiles[[2]],
              output_tiles[[3]])

mapply(
  function(x, y) raster_to_raw_tiles(input_file = x, 
                                     output_prefix = "marcy", 
                                     side_length = 4096, 
                                     raw = y),
  c(output_tiles[[1]], output_tiles[[3]]),
  c(TRUE, FALSE)
  )
```

``` r
# With about two minutes of movie magic (loading the files into Unity), 
# we can turn that into:
```

terrainr also includes functionality to merge and crop the files you’ve
downloaded, and to resize your area of interest so you’re sure to
download exactly the area you want. Additionally, the more time
intensive processing steps can all be monitored via the
[progressr](https://github.com/HenrikBengtsson/progressr) package, so
you’ll be more confident that your computer is still churning along and
not just hung. For more information, check out [the introductory
vignette\!](https://mikemahoney218.github.io/terrainr/articles/overview.html)

## Installation

You can install
<!-- the released version of terrainr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("terrainr")
```

And --> the development version of terrainr from
[GitHub](https://github.com/mikemahoney218/terrainr) with:

``` r
# install.packages("devtools")
devtools::install_github("mikemahoney218/terrainr")
```

If you’re planning on using `raster_to_raw_tiles()`, you’ll also need to
install the development version of the `magick` package for the time
being:

``` r
devtools::install_github("ropensci/magick")
```
