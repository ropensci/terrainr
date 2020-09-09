# TODO: document
#' @export
make_unity_friendly <- function(bbox) {

  tr <- bbox[["tr"]]
  bl <- bbox[["bl"]]

  tl <- c(lat = tr[["lat"]],
          lng = bl[["lng"]])

  x_tiles <- ceiling((calc_haversine_distance(tl, tr) / 4097))
  y_tiles <- ceiling(calc_haversine_distance(tl, bl) / 4097)

  bbox[["tr"]] <- point_from_distance(tl, x_tiles * 4097, 90)
  bbox[["bl"]] <- point_from_distance(bl, y_tiles * 4097, 180)

  return(bbox)

}
