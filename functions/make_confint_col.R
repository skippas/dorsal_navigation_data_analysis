make_confint_col <- function(
    data,
    lower_col = "conf.low",
    upper_col = "conf.high",
    out_col = "confint",
    digits = 3,
    prefix = "(",
    sep = ", ",
    suffix = ")"
) {
  stopifnot(is.data.frame(data))
  if (!lower_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data.", lower_col), call. = FALSE)
  }
  if (!upper_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data.", upper_col), call. = FALSE)
  }

  lower_vals <- data[[lower_col]]
  upper_vals <- data[[upper_col]]

  fmt <- function(x) {
    if (is.numeric(x)) sprintf(paste0("%.", digits, "f"), x) else as.character(x)
  }

  data[[out_col]] <- paste0(prefix, fmt(lower_vals), sep, fmt(upper_vals), suffix)
  data
}
