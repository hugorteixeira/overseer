#' @export
#' @noRd
overseer <- function(
  instructions,
  output_dir = "~/.overseer",
  min_interval = 5,
  max_interval = 300,
  change_threshold = 0.05,
  image_quality = 85,
  max_width = 1920,
  save_screenshots = FALSE,
  service = "openai",
  model = "gpt-5-mini",
  temp = 1,
  reasoning = NULL,
  genflow_label = NULL,
  verbose = TRUE,
  debug = FALSE,
  debug_every = 1,
  debug_save = FALSE,
  genflow_args = list()
) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required.")
  }
  if (!requireNamespace("genflow", quietly = TRUE)) {
    stop("Package 'genflow' is required.")
  }
  if (is.null(output_dir)) {
    output_dir <- "~/.overseer"
  }
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("`output_dir` must be a single string.")
  }
  if (is.na(output_dir) || !nzchar(output_dir)) {
    output_dir <- "~/.overseer"
  }
  if (!is.numeric(min_interval) || length(min_interval) != 1 || min_interval <= 0) {
    stop("`min_interval` must be a positive number.")
  }
  if (!is.numeric(max_interval) || length(max_interval) != 1 || max_interval <= 0) {
    stop("`max_interval` must be a positive number.")
  }
  if (min_interval > max_interval) {
    stop("`min_interval` must be less than or equal to `max_interval`.")
  }
  if (!is.numeric(change_threshold) || length(change_threshold) != 1 ||
      change_threshold <= 0 || change_threshold >= 1) {
    stop("`change_threshold` must be a number between 0 and 1.")
  }
  if (!is.numeric(image_quality) || length(image_quality) != 1 ||
      image_quality <= 0 || image_quality > 100) {
    stop("`image_quality` must be a number between 1 and 100.")
  }
  if (!is.logical(save_screenshots) || length(save_screenshots) != 1) {
    stop("`save_screenshots` must be TRUE or FALSE.")
  }
  if (!is.list(genflow_args)) {
    stop("`genflow_args` must be a list.")
  }
  if (!is.logical(debug) || length(debug) != 1) {
    stop("`debug` must be TRUE or FALSE.")
  }
  if (!is.numeric(debug_every) || length(debug_every) != 1 || debug_every < 1) {
    stop("`debug_every` must be a positive number.")
  }
  if (!is.logical(debug_save) || length(debug_save) != 1) {
    stop("`debug_save` must be TRUE or FALSE.")
  }

  scrot_path <- .ensure_scrot("install", verbose = verbose)
  if (is.null(scrot_path)) {
    return(invisible(FALSE))
  }

  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir)) {
    created <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose && created) {
      message("Created directory: ", output_dir)
    }
  }
  if (!dir.exists(output_dir)) {
    stop("Unable to create output directory: ", output_dir)
  }
  if (is.na(file.access(output_dir, 2)) || file.access(output_dir, 2) != 0) {
    stop("`output_dir` is not writable: ", output_dir)
  }

  temp_dir <- file.path(output_dir, "tmp")
  if (!dir.exists(temp_dir)) {
    created <- dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose && created) {
      message("Created temp directory: ", temp_dir)
    }
  }
  if (!dir.exists(temp_dir)) {
    stop("Unable to create temp directory: ", temp_dir)
  }

  previous_path <- file.path(temp_dir, "prev.png")
  current_path <- file.path(temp_dir, "current.png")

  scrot_status <- system2(scrot_path, c("-o", previous_path))
  if (!is.null(scrot_status) && scrot_status != 0) {
    stop("Failed to create initial screenshot (scrot exit status ", scrot_status, ").")
  }
  if (!file.exists(previous_path)) {
    status_note <- if (!is.null(scrot_status) && scrot_status != 0) {
      paste0(" (scrot exit status ", scrot_status, ")")
    } else {
      ""
    }
    stop("Failed to create initial screenshot at ", previous_path, status_note)
  }
  previous_info <- file.info(previous_path)
  if (is.na(previous_info$size) || previous_info$size <= 0) {
    stop("Failed to create initial screenshot at ", previous_path, " (empty file).")
  }

  inactivity_seconds <- 0
  interval <- max_interval
  iteration <- 0
  compare_size <- "800x600"

  if (verbose) {
    message("Starting screenshot watch.")
  }

  repeat {
    tryCatch(
      {
        scrot_status <- system2(scrot_path, c("-o", current_path))
        if (!is.null(scrot_status) && scrot_status != 0) {
          if (verbose) {
            message("Error: scrot exit status ", scrot_status, ".")
          }
          next
        }

        if (!file.exists(current_path)) {
          if (verbose) {
            message("Error: screenshot was not captured.")
          }
          next
        }
        current_info <- file.info(current_path)
        if (is.na(current_info$size) || current_info$size <= 0) {
          if (verbose) {
            message("Error: screenshot file is empty.")
          }
          next
        }

        previous_image <- magick::image_read(previous_path)
        current_image <- magick::image_read(current_path)

        change_result <- .compute_change_fraction(
          previous_image,
          current_image,
          compare_size,
          change_threshold
        )
        change_fraction <- change_result$fraction
        if (is.na(change_fraction)) {
          if (verbose) {
            message("Error: invalid image dimensions.")
          }
          next
        }
        iteration <- iteration + 1

        if (debug && (iteration %% debug_every == 0)) {
          prev_info <- tryCatch(file.info(previous_path), error = function(e) NULL)
          curr_info <- tryCatch(file.info(current_path), error = function(e) NULL)
          prev_md5 <- tryCatch(tools::md5sum(previous_path), error = function(e) NULL)
          curr_md5 <- tryCatch(tools::md5sum(current_path), error = function(e) NULL)
          message(
            "Debug: change_fraction=",
            sprintf("%.6f", change_fraction),
            " threshold=",
            change_threshold,
            " compare_size=",
            compare_size
          )
          message(
            "Debug: method=",
            change_result$method,
            " magick_fraction=",
            sprintf("%.6f", change_result$magick_fraction),
            " pixel_fraction=",
            sprintf("%.6f", change_result$pixel_fraction)
          )
          if (!is.null(prev_info) && !is.null(curr_info)) {
            message(
              "Debug: prev_size=",
              prev_info$size,
              " curr_size=",
              curr_info$size
            )
          }
          if (!is.null(prev_md5) && !is.null(curr_md5)) {
            message(
              "Debug: prev_md5=",
              unname(prev_md5[[1]]),
              " curr_md5=",
              unname(curr_md5[[1]])
            )
          }
          if (debug_save) {
            debug_dir <- file.path(output_dir, "debug")
            if (!dir.exists(debug_dir)) {
              dir.create(debug_dir, recursive = TRUE, showWarnings = FALSE)
            }
            debug_path <- file.path(
              debug_dir,
              paste0("debug_current_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
            )
            file.copy(current_path, debug_path, overwrite = TRUE)
          }
        }

        if (change_fraction > change_threshold) {
          inactivity_seconds <- 0
          timestamp <- Sys.time()

          mask <- .create_change_mask(previous_image, current_image, change_threshold)
          result_image <- magick::image_composite(
            current_image,
            mask,
            operator = "copyopacity"
          )
          result_image <- .resize_if_needed(result_image, max_width)
          result_image <- magick::image_strip(result_image)
          result_image <- magick::image_normalize(result_image)

          screenshot_name <- paste0(
            "screenshot_",
            format(timestamp, "%Y%m%d_%H%M%S"),
            ".png"
          )
          if (save_screenshots) {
            data_dir <- .ensure_data_dir(output_dir, timestamp)
            screenshot_path <- .unique_path(file.path(data_dir, screenshot_name))
          } else {
            screenshot_path <- .unique_path(file.path(temp_dir, screenshot_name))
          }
          magick::image_write(result_image, screenshot_path, quality = image_quality)

          response <- tryCatch(
            {
              base_args <- list(
                context = instructions,
                res_context = FALSE,
                add = NULL,
                add_img = screenshot_path,
                label = genflow_label,
                service = service,
                model = model,
                temp = temp,
                reasoning = reasoning
              )
              call_args <- utils::modifyList(base_args, genflow_args)
              do.call(genflow::gen_txt, call_args)
            },
            error = function(e) {
              if (verbose) {
                message("Error: gen_txt failed: ", e$message)
              }
              NULL
            }
          )

          response_text <- .extract_response_text(response)
          if (!is.null(response_text) && nzchar(response_text)) {
            entry <- .format_log_entry(response_text, timestamp = timestamp)
            .write_log_entry(output_dir, entry, timestamp = timestamp)
            .update_yearly_aggregate(output_dir, timestamp = timestamp)
          }

          if (verbose) {
            if (save_screenshots) {
              message(
                "Change detected (",
                round(change_fraction * 100, 2),
                "%). Saved: ",
                screenshot_path
              )
            } else {
              message(
                "Change detected (",
                round(change_fraction * 100, 2),
                "%). Sent to model."
              )
            }
          }

          interval <- max(
            min_interval,
            max_interval * (1 - min(change_fraction, 1))
          )
          if (!save_screenshots && file.exists(screenshot_path)) {
            unlink(screenshot_path)
          }
        } else {
          inactivity_seconds <- inactivity_seconds + interval
          if (inactivity_seconds > 300) {
            interval <- max_interval
            if (verbose) {
              message("Inactive. Increasing interval to ", interval, " seconds.")
            }
          }
        }

        file.rename(current_path, previous_path)
        previous_image <- current_image
      },
      error = function(e) {
        if (verbose) {
          message("Error: ", e$message)
        }
        interval <- max_interval
      }
    )

    Sys.sleep(interval)
  }
}
