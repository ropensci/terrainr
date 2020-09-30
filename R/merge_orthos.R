#' @export
merge_orthos <- function(input_files, output_file, overwrite = TRUE) {

  for (i in seq_along(input_files)) {
    for (j in seq_along(input_files[[i]])) {
      input_files[[i]][[j]] <- magick::image_read(input_files[[i]][[j]])
    }
    input_files[[i]] <- unlist(input_files[[i]])
    input_files <- magick::image_append(input_files[[i]])
  }

  input_files <- unlist(input_files)
  magick::image_append(input_files, stack = TRUE)

  return(NULL)
}
