% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_manifest.R
\name{make_manifest}
\alias{make_manifest}
\alias{transform_elevation}
\alias{transform_overlay}
\title{Transform rasters and write manifest file for import into Unity}
\usage{
make_manifest(
  heightmap,
  overlay = NULL,
  output_prefix = "import",
  manifest_path = "terrainr.manifest",
  importer_path = "import_terrain.cs"
)

transform_elevation(heightmap, side_length = 4097, output_prefix = "import")

transform_overlay(overlay, side_length = 4097, output_prefix = "import")
}
\arguments{
\item{heightmap}{File path to the heightmap to transform.}

\item{overlay}{File path to the image overlay to transform. Optional for
\link{make_manifest}.}

\item{output_prefix}{The file path to prefix output tiles with.}

\item{manifest_path}{File path to write the manifest file to.}

\item{importer_path}{File name to write the importer script to. Set to NULL
to not copy the importer script. Will overwrite any file at the same path.}

\item{side_length}{Side length, in pixels, of each output tile. If the raster
has dimensions not evenly divisible by \code{side_length}, tiles will be generated
with overhanging pieces set to 0 units of elevation or RGB 0 (pure black).
Side lengths not equal to 2^x + 1 (for x <= 12) will cause a warning, as
tiles must be this size for import into Unity.}
}
\value{
\code{manifest_path}, invisibly.
}
\description{
These functions crop input raster files into smaller square tiles and then
converts them into either .png or .raw files which are ready to be imported
into the Unity game engine. \link{make_manifest} also writes a "manifest" file and
importer script which may be used to automatically import the tiles into
Unity.
}
\examples{
\dontrun{
if (!isTRUE(as.logical(Sys.getenv("CI")))) {
  simulated_data <- data.frame(
    id = seq(1, 100, 1),
    lat = runif(100, 44.04905, 44.17609),
    lng = runif(100, -74.01188, -73.83493)
  )
  simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
  output_files <- get_tiles(simulated_data)
  temptiff <- tempfile(fileext = ".tif")
  merge_rasters(output_files["elevation"][[1]], temptiff)
  make_manifest(temptiff, output_prefix = tempfile(), importer_path = NULL)
}
}

}
