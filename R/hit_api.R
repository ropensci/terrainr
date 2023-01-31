#' Hit the USGS 3DEP API and retrieve an elevation heightmap
#'
#' This function retrieves a single tile of data from a single National Map
#' service and returns the raw response. End users are recommended to use
#' [get_tiles] instead, as it does much more validation and provides
#' a more friendly interface. For a description of the datasets provided by the
#' National Map, see \url{https://apps.nationalmap.gov/services}
#'
#' @param bbox An object from [sf::st_bbox].
#' @param img_width The number of pixels in the x direction to retrieve
#' @param img_height The number of pixels in the y direction to retrieve
#' @param verbose Logical: Print out the number of tries required to pull each
#' tile? Default \code{FALSE}.
#' @param service A string indicating what service API to use. For a full list
#' of available services, see [get_tiles]. Short codes are not accepted by this
#' function.
#' @param ... Additional arguments passed to the National Map API.
#' These can be used to change default query parameters or as additional options
#' for the National Map services. See below for more information.
#'
#' @keywords internal
#'
#' @section Additional Arguments:
#' The \code{...} argument can be used to pass additional arguments to the
#' National Map API or to edit the hard-coded defaults used by this function.
#' Some of the most useful options that can be changed include:
#'
#' * `bboxSR`: The spatial reference of the bounding box given to this function.
#'   If not specified, assumed to be
#'   [4326](https://spatialreference.org/ref/epsg/wgs-84/).
#'
#' * `imageSR`: The spatial reference of the image downloaded.
#'   If not specified, assumed to be
#'   [4326](https://spatialreference.org/ref/epsg/wgs-84/).
#'
#' * layers: Which data layers to download. If the National Map API returns data
#'   without specifying layers, this argument isn't used by default. When the
#'   National Map requires this argument, the default value is 0.
#'
#' * format: The image format to be downloaded. Defaults depend on the service
#'   being used and are set to be compatible with [get_tiles].
#'
#' Pass these arguments to \code{hit_national_map_api} like you would any other
#' argument to substitute new values. Note that \code{...} values are never
#' validated before being used; passing invalid parameters to \code{...} will
#' cause data retrieval to fail.
#'
#' @seealso [get_tiles] for a friendlier interface to the National
#' Map API.
#' @family data retrieval functions
#'
#' @return A raw vector.
#'
#' @examples
#' \dontrun{
#' hit_national_map_api(
#'   bbox = list(
#'     c(lat = 44.10438, lng = -74.01231),
#'     c(lat = 44.17633, lng = -73.91224)
#'   ),
#'   img_width = 8000,
#'   img_height = 8000,
#'   service = "3DEPElevation"
#' )
#' }
#'
#' @export
#' @md
hit_national_map_api <- function(bbox,
                                 img_width,
                                 img_height,
                                 service,
                                 verbose = FALSE,
                                 ...) {
  dots <- list(...)

  if (is.list(bbox) && length(bbox) == 2) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  if (methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_st_bbox(bbox)
  }
  stopifnot(methods::is(bbox, "bbox"))

  # nolint start
  available_endpoints <- c(
    "3DEPElevation" = "https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage",
    "USGSNAIPPlus" = "https://imagery.nationalmap.gov/arcgis/rest/services/USGSNAIPPlus/ImageServer/exportImage",
    "USGSNAIPImagery" = "https://imagery.nationalmap.gov/arcgis/rest/services/USGSNAIPImagery/ImageServer/exportImage",
# Currently offline
    #    "HRO" = "https://imagery.nationalmap.gov/arcgis/rest/services/HRO/ImageServer/exportImage",
    "nhd" = "https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer/export",
    "govunits" = "https://carto.nationalmap.gov/arcgis/rest/services/govunits/MapServer/export",
    "contours" = "https://cartowfs.nationalmap.gov/arcgis/rest/services/contours/MapServer/export",
    "geonames" = "https://carto.nationalmap.gov/arcgis/rest/services/geonames/MapServer/export",
    "NHDPlus_HR" = "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/export",
    "structures" = "https://carto.nationalmap.gov/arcgis/rest/services/structures/MapServer/export",
    "transportation" = "https://carto.nationalmap.gov/arcgis/rest/services/transportation/MapServer/export",
    "wbd" = "https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/export",
    "ecosystems" = "https://rmgsc.cr.usgs.gov/arcgis/rest/services/contUS/MapServer/export",
    "USGSTopo" = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/export",
    "USGSShadedReliefOnly" = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSShadedReliefOnly/MapServer/export",
    "USGSImageryOnly" = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/export",
    "USGSHydroCached" = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer/export",
    "USGSTNMBlank" = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTNMBlank/MapServer/export"
  )
  # nolint end

  # API endpoint for elevation mapping:
  url <- httr::parse_url(available_endpoints[[service]])

  standard_png_args <- list(
    bboxSR = 4326,
    imageSR = 4326,
    size = paste(img_width, img_height, sep = ","),
    format = "png",
    transparent = "true",
    f = "json"
  )

  query_arg <- switch(service,
    "3DEPElevation" = list(
      bboxSR = 4326,
      imageSR = 4326,
      size = paste(img_width, img_height, sep = ","),
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    ),
    # purposefully exclude transparency from NAIP downloads
    "USGSNAIPPlus" = c(
      standard_png_args[names(standard_png_args) != "transparent"],
      transparent = "false"
    ),
    "nhd" = c(layers = 0, standard_png_args),
    standard_png_args
  )

  bbox_arg <- list(bbox = paste(
    bbox[["xmin"]],
    bbox[["ymin"]],
    bbox[["xmax"]],
    bbox[["ymax"]],
    sep = ","
  ))

  if (length(dots) > 0) {
    if (any(names(dots) %in% names(query_arg))) {
      replacements <- which(names(dots) %in% names(query_arg))
      query_arg[names(unlist(dots[replacements]))] <- dots[replacements]
      dots <- dots[-replacements]
    }
  }

  # length of dots changes after that last step, so check again
  if (length(dots) > 0) query_arg <- c(query_arg, dots) # nocov

  agent <- httr::user_agent("https://github.com/mikemahoney218/terrainr")

  # periodically res is a HTML file instead of the JSON
  # I haven't been able to capture this happening, it just crashes something
  # like 3% of the time
  # But it's non-deterministic, so we can just retry
  get_href <- function(counter = 0) {
    backoff <- stats::runif(
      n = 1,
      min = 0,
      max = floor(c(2^counter - 1, 30))
    )
    Sys.sleep(backoff)
    if (verbose) message(sprintf("API call 1 attempt %d", counter + 1))

    res <- httr::GET(url, agent, query = c(bbox_arg, query_arg))

    if (!httr::http_error(res)) {
      body <- tryCatch(
        {
          if (verbose) rlang::inform("Interpreting JSON attempt 1")
          httr::content(res, type = "application/json")
        },
        # nocov start
        # Hard to force temporary API errors
        # Rather than have code coverage improve when servers go down,
        # I just exclude error handling from coverage
        error = function(e) {
          tryCatch(
            {
              if (verbose) rlang::inform("Interpreting JSON attempt 2")
              res <- httr::GET(url, agent, query = c(bbox_arg, query_arg))
              httr::content(res, type = "application/json")
            },
            error = function(e) {
              if (verbose) rlang::inform("Interpreting JSON attempt 3")
              res <- httr::GET(url, agent, query = c(bbox_arg, query_arg))
              httr::content(res, type = "application/json")
            }
          )
        }
        # nocov end
      )

      if (counter < 5 && !is.null(body$error)) {
        get_href(counter = counter + 1) # nocov
      } else {
        return(body)
      }
    } else if (counter < 5) {
      get_href(counter = counter + 1)
    } else { # nocov start
      stop(
        "Map server returned error code ",
        httr::status_code(img_res)
      )
    } # nocov end
  }

  body <- get_href()

  if (!is.null(body$href)) {
    if (verbose) rlang::inform(sprintf("API call 2 attempt %d", 1))
    img_res <- httr::GET(body$href, agent)
    counter <- 0
    for (counter in seq_len(5)) {
      if (!httr::http_error(img_res)) break

      if (verbose) rlang::inform(sprintf("API call 2 attempt %d", counter))
      backoff <- stats::runif(n = 1, min = 0, max = floor(c(
        2^counter - 1,
        30
      )))
      Sys.sleep(backoff)
      img_res <- httr::GET(body$href, agent)
    }

    if (httr::http_error(img_res)) {
      # nocov start
      stop("Map server returned error code ", httr::status_code(img_res))
      # nocov end
    }

    return(list(
      imageData = httr::content(img_res, "raw"),
      extent = body$extent
    ))
  } else {
    return(body)
  }
}
