# TODO: document
#' @export
make_unity_friendly <- function(bbox, size = NULL) {

  if (!is.null(size)) {
    stopifnot((size - 1) %% 2 == 0 && size <= 4097)
  }

  adjusted <- 0

  if (is.null(size)) {
    # quite possible we need no expansion
    size <- 0
    adjusted <- 1
  }

  tl <- c(lat = bbox[["tr"]][["lat"]],
          lng = bbox[["bl"]][["lng"]])

  tr <- bbox[["tr"]]
  bl <- bbox[["bl"]]

  x_partial <- calc_haversine_distance(tl, tr) %% 4097
  y_partial <- calc_haversine_distance(tl, bl) %% 4097

  while (x_partial > size || y_partial > size) {
    # we know we need SOME expansion so initialize to smallest possible value
    if (size == 0) size <- 3
    size <- 2^(log(size - 1, 2) + 1) + 1
    adjusted <- 1
  }
  if (adjusted) message("Note: expanding last row and column to ",
                        size,
                        " pixels to avoid clipping")
  if ((size == 0 || size == 4097) & is.null(size)) {
    message("Note: all tiles will be 4097x4097.")
  }

  bbox[["tr"]] <- point_from_distance(tr, size, 90)
  bbox[["bl"]] <- point_from_distance(bl, size, 180)

  return(bbox)

}
