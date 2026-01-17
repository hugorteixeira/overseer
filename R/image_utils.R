.extract_distortion_value <- function(distortion_result) {
  if (is.list(distortion_result) && !is.null(distortion_result$distortion)) {
    return(as.numeric(distortion_result$distortion))
  }
  if (is.numeric(distortion_result)) {
    return(as.numeric(distortion_result))
  }
  NA_real_
}

.create_change_mask <- function(previous_image, current_image, threshold) {
  diff_image <- magick::image_composite(previous_image, current_image, operator = "difference")
  diff_gray <- magick::image_convert(diff_image, colorspace = "gray")
  diff_gray <- magick::image_blur(diff_gray, radius = 1, sigma = 0.5)
  magick::image_threshold(diff_gray, threshold = threshold * 100)
}

.resize_if_needed <- function(image, max_width) {
  info <- magick::image_info(image)
  if (!is.numeric(max_width) || length(max_width) != 1 || max_width <= 0) {
    stop("`max_width` must be a positive number.")
  }
  if (info$width > max_width) {
    ratio <- max_width / info$width
    new_height <- floor(info$height * ratio)
    return(magick::image_scale(image, paste0(max_width, "x", new_height)))
  }
  image
}

.compute_change_fraction <- function(previous_image, current_image, compare_size, change_threshold) {
  previous_small <- magick::image_scale(previous_image, compare_size)
  current_small <- magick::image_scale(current_image, compare_size)

  distortion_result <- magick::image_compare_dist(
    previous_small,
    current_small,
    metric = "AE"
  )
  distortion_value <- .extract_distortion_value(distortion_result)

  image_info <- magick::image_info(previous_small)
  total_pixels <- image_info$width * image_info$height
  if (is.na(total_pixels) || total_pixels == 0) {
    return(list(
      fraction = NA_real_,
      method = "magick",
      magick_fraction = NA_real_,
      pixel_fraction = NA_real_
    ))
  }

  magick_fraction <- if (!is.na(distortion_value)) {
    distortion_value / total_pixels
  } else {
    NA_real_
  }

  if (!is.na(magick_fraction) && magick_fraction >= change_threshold) {
    return(list(
      fraction = magick_fraction,
      method = "magick",
      magick_fraction = magick_fraction,
      pixel_fraction = NA_real_
    ))
  }

  previous_data <- magick::image_data(previous_small, channels = "rgb")
  current_data <- magick::image_data(current_small, channels = "rgb")
  if (!identical(dim(previous_data), dim(current_data))) {
    return(list(
      fraction = if (is.na(magick_fraction)) 0 else magick_fraction,
      method = "magick",
      magick_fraction = magick_fraction,
      pixel_fraction = NA_real_
    ))
  }

  diff_mask <- previous_data != current_data
  if (length(dim(diff_mask)) != 3) {
    return(list(
      fraction = if (is.na(magick_fraction)) 0 else magick_fraction,
      method = "magick",
      magick_fraction = magick_fraction,
      pixel_fraction = NA_real_
    ))
  }

  any_diff <- diff_mask[1, , ] | diff_mask[2, , ] | diff_mask[3, , ]
  changed_pixels <- sum(any_diff)
  pixel_fraction <- changed_pixels / length(any_diff)
  if (is.na(magick_fraction)) {
    magick_fraction <- 0
  }

  if (pixel_fraction >= magick_fraction) {
    return(list(
      fraction = pixel_fraction,
      method = "pixel",
      magick_fraction = magick_fraction,
      pixel_fraction = pixel_fraction
    ))
  }

  list(
    fraction = magick_fraction,
    method = "magick",
    magick_fraction = magick_fraction,
    pixel_fraction = pixel_fraction
  )
}
