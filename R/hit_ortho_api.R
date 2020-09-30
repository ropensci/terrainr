#' @export
hit_ortho_api <- function(bbox, overlay, img.width, img.height, verbose = FALSE) {

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  first_corner <- bbox@bl
  second_corner <- bbox@tr

  url <- httr::parse_url("https://services.nationalmap.gov/arcgis/rest/services/USGSNAIPPlus/MapServer/export")

  res <- httr::GET(url,
                   query = list(
                     bbox = paste(min(
                       first_corner@lng,
                       second_corner@lng
                     ),
                     min(
                       first_corner@lat,
                       second_corner@lat
                     ),
                     max(
                       second_corner@lng,
                       first_corner@lng
                     ),
                     max(
                       second_corner@lat,
                       first_corner@lat
                     ),
                     sep = ","
                     ),
                     bboxSR = 4326,
                     imageSR = 4326,
                     size = paste(img.width, img.height, sep = ","),
                     format = "png",
                     f = "json"
                   )
  )

  body <- httr::content(res, type = "application/json")
  img_res <- httr::RETRY("GET", url = body$href, times = 15, quiet = !verbose)

  if (httr::status_code(img_res) != 200) {
    # nocov start
    stop("Map server returned error code ", httr::status_code(img_res))
    # nocov end
  }

  httr::content(img_res, "raw")

}
