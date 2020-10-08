#' Hit the USGS 3DEP API and retrieve an elevation heightmap
#'
#' @param bbox The bounding box (bottom left and top left coordinate pairs)
#' @param img.width The number of pixels in the x direction to retrieve
#' @param img.height The number of pixels in the y direction to retrieve
#' @param verbose Logical: Print out the number of tries required to pull each
#' tile? Default \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' hit_national_map_api(
#'                      bbox = list(c(44.10438, -74.01231),
#'                                  c(44.17633, -73.91224)),
#'                      img.width = 8000,
#'                      img.height = 8000,
#'                      service = "3DEPElevation")
#' }
#'
#' @export
hit_national_map_api <- function(bbox,
                                 img.width,
                                 img.height,
                                 service,
                                 verbose = FALSE,
                                 ...) {

  dots <- list(...)

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  first_corner <- bbox@bl
  second_corner <- bbox@tr

  # API endpoint for elevation mapping:
  url <- httr::parse_url(switch(service,
                                "3DEPElevation" = "https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage",
                                "USGSNAIPPlus" = "https://services.nationalmap.gov/arcgis/rest/services/USGSNAIPPlus/MapServer/export"))

  bbox_arg <- list(bbox = paste(
    min(first_corner@lng, second_corner@lng),
    min(first_corner@lat, second_corner@lat),
    max(second_corner@lng, first_corner@lng),
    max(second_corner@lat, first_corner@lat),
    sep = ",")
    )

  query_arg <- switch(service,
                      "3DEPElevation" = list(bboxSR = 4326,
                                             imageSR = 4326,
                                             size = paste(img.width, img.height, sep = ","),
                                             format = "tiff",
                                             pixelType = "F32",
                                             noDataInterpretation = "esriNoDataMatchAny",
                                             interpolation = "+RSP_BilinearInterpolation",
                                             f = "json"),
                      "USGSNAIPPlus" = list(bboxSR = 4326,
                                            imageSR = 4326,
                                            size = paste(img.width, img.height, sep = ","),
                                            format = "png",
                                            f = "json"))

  if (length(dots) > 0) {
    if (any(names(dots) %in% names(query_arg))) {
      replacements <- which(names(dots) %in% names(query_arg))
      query_arg[names(unlist(dots[replacements]))] <- dots[replacements]
      dots <- dots[-replacements]
    }
  }

  if (length(dots) > 0) query_arg <- c(query_arg, dots)

  res <- httr::GET(url, query = c(bbox_arg, query_arg))

  body <- httr::content(res, type = "application/json")
  img_res <- httr::RETRY("GET", url = body$href, times = 15, quiet = !verbose)

  if (httr::status_code(img_res) != 200) {
    # nocov start
    stop("Map server returned error code ", httr::status_code(img_res))
    # nocov end
  }

  httr::content(img_res, "raw")
}
