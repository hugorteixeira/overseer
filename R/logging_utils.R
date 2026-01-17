.format_log_entry <- function(text, timestamp = Sys.time()) {
  paste0("[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ", text)
}

.data_dir <- function(output_dir, timestamp = Sys.time()) {
  file.path(output_dir, "data", format(timestamp, "%Y"), format(timestamp, "%m"))
}

.ensure_data_dir <- function(output_dir, timestamp = Sys.time()) {
  data_dir <- .data_dir(output_dir, timestamp)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  data_dir
}

.log_dir <- function(output_dir, timestamp = Sys.time()) {
  file.path(output_dir, "data", format(timestamp, "%Y"))
}

.ensure_log_dir <- function(output_dir, timestamp = Sys.time()) {
  log_dir <- .log_dir(output_dir, timestamp)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  log_dir
}

.unique_path <- function(path) {
  if (!file.exists(path)) {
    return(path)
  }

  directory <- dirname(path)
  filename <- basename(path)
  ext <- tools::file_ext(filename)
  base <- if (nzchar(ext)) {
    substring(filename, 1, nchar(filename) - nchar(ext) - 1)
  } else {
    filename
  }

  for (i in seq_len(9999)) {
    suffix <- paste0("_", sprintf("%03d", i))
    candidate <- if (nzchar(ext)) {
      file.path(directory, paste0(base, suffix, ".", ext))
    } else {
      file.path(directory, paste0(base, suffix))
    }
    if (!file.exists(candidate)) {
      return(candidate)
    }
  }

  stop("Unable to create a unique file path for ", path)
}

.write_log_entry <- function(output_dir, entry, timestamp = Sys.time()) {
  log_dir <- .ensure_log_dir(output_dir, timestamp)
  filename <- paste0("data_", format(timestamp, "%Y%m%d_%H%M%S"), ".txt")
  log_path <- .unique_path(file.path(log_dir, filename))
  writeLines(entry, log_path)
  log_path
}

.update_yearly_aggregate <- function(output_dir, timestamp = Sys.time()) {
  log_dir <- .ensure_log_dir(output_dir, timestamp)
  year <- format(timestamp, "%Y")
  pattern <- paste0("^data_", year, "\\d{4}_\\d{6}(_\\d{3})?\\.txt$")
  files <- list.files(log_dir, pattern = pattern, full.names = TRUE)
  files <- sort(files)
  entries <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)

  aggregate_path <- file.path(log_dir, paste0("data_", year, ".txt"))
  writeLines(entries, aggregate_path)
  aggregate_path
}

.extract_response_text <- function(response) {
  if (is.null(response)) {
    return(NULL)
  }
  if (is.list(response)) {
    if (!is.null(response$response_value)) {
      return(paste(as.character(response$response_value), collapse = "\n"))
    }
    if (!is.null(response$value)) {
      return(paste(as.character(response$value), collapse = "\n"))
    }
    return(NULL)
  }
  if (is.character(response)) {
    return(paste(response, collapse = "\n"))
  }
  as.character(response)
}
