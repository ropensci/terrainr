get_bbox_centroid <- function(bbox) {

  lat <- c(bbox[[1]]["lat"], bbox[[2]]["lat"])
  lng <- c(bbox[[1]]["lng"], bbox[[2]]["lng"])

  lat <- deg_to_rad(lat)
  lng <- deg_to_rad(lng)

  x <- sum(cos(lat) * cos(lng))/length(lat)
  y <- sum(cos(lat) * sin(lng))/length(lat)
  z <- sum(sin(lat))/length(lat)
  lng <- atan2(y, x)
  lat <- atan2(z, sqrt(x * x + y * y))

  lat <- rad_to_deg(lat)
  lng <- rad_to_deg(lng)

  return(c(lat = lat, lng = lng))
}
