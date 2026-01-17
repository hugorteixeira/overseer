#' @export
#' @noRd
read_text_file <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a single string.")
  }
  path <- path.expand(path)
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  text <- readLines(path, encoding = "UTF-8", warn = FALSE)
  paste(text, collapse = "\n")
}
