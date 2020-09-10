get_bbox_centroid <- function(bbox) {

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  lat <- c(bbox@bl@lat, bbox@tr@lat)
  lng <- c(bbox@bl@lng, bbox@tr@lng)

  lat <- deg_to_rad(lat)
  lng <- deg_to_rad(lng)

  x <- sum(cos(lat) * cos(lng))/length(lat)
  y <- sum(cos(lat) * sin(lng))/length(lat)
  z <- sum(sin(lat))/length(lat)
  lng <- atan2(y, x)
  lat <- atan2(z, sqrt(x * x + y * y))

  lat <- rad_to_deg(lat)
  lng <- rad_to_deg(lng)

  return(terrainr_coordinate_pair(c(lat = lat, lng = lng)))
}
