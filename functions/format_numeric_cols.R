# function to format a table:
fmtNumCols <- function(x, digits = 3, keep_whole_cols = TRUE, tol = 1e-9) {
  x[] <- lapply(x, function(col) {
    if (!is.numeric(col)) return(col)

    # Keep whole-number columns (e.g., trial indices) unformatted by default.
    if (keep_whole_cols) {
      idx <- is.finite(col)
      if (any(idx) && all(abs(col[idx] - round(col[idx])) < tol)) {
        return(col)
      }
    }

    sprintf(paste0("%.", digits, "f"), col)
  })
  x
}
