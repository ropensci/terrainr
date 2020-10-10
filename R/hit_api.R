#' Hit the USGS 3DEP API and retrieve an elevation heightmap
#'
#' This function retrieves a single tile of data from a single National Map
#' service and returns the raw response. End users are recommended to use
#' \code{\link{get_tiles}} instead, as it does much more validation and provides
#' a more friendly interface.
#'
#' @param bbox The bounding box (bottom left and top left coordinate pairs)
#' @param img.width The number of pixels in the x direction to retrieve
#' @param img.height The number of pixels in the y direction to retrieve
#' @param verbose Logical: Print out the number of tries required to pull each
#' tile? Default \code{FALSE}.
#' @param ... Additional arguments passed to the National Map API.
#' These can be used to change default query parameters or as additional options
#' for the National Map services, but are at no point validated, so use at your
#' own risk!
#'
#' @keywords internal
#'
#' @seealso \code{\link{get_tiles}} for a friendlier interface to the National
#' Map API.
#' @family data retrieval functions
#'
#' @return A raw vector.
#'
#' @examples
#' \dontrun{
#' hit_national_map_api(
#'                      bbox = list(c(lat = 44.10438, lng = -74.01231),
#'                                  c(lat = 44.17633, lng = -73.91224)),
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

  # length of dots changes after that last step, so check again
  if (length(dots) > 0) query_arg <- c(query_arg, dots) # nocov

  # periodically res is a HTML file instead of the JSON
  # I haven't been able to capture this happening, it just crashes something
  # like 3% of the time
  # But it's non-deterministic, so we can just retry
  get_href <- function(url, query, counter = 0) {
    if (counter < 15) {
      backoff <- stats::runif(n=1, min=0, max=2^counter - 1)
      if (verbose) message(sprintf("Attempt %d, retrying after %d seconds",
                                   counter + 1,
                                   backoff))
      Sys.sleep(backoff)
      res <- httr::GET(url, query = query)
      tryCatch(httr::content(res, type = "application/json"),
               error = function(e) {
                 get_href(url, query, counter = counter + 1) # nocov
               })
    } else {
      stop("Server returned malformed JSON") # nocov
    }
  }

  body <- get_href(url, query = c(bbox_arg, query_arg))

  img_res <- httr::RETRY("GET", url = body$href, times = 15, quiet = !verbose)

  if (httr::status_code(img_res) != 200) {
    # nocov start
    stop("Map server returned error code ", httr::status_code(img_res))
    # nocov end
  }

  httr::content(img_res, "raw")
}
