# function to format a table:
fmtNumCols <- function(x, digits = 3) {
  x[] <- lapply(x, function(col) {
    if (is.numeric(col)) sprintf(paste0("%.", digits, "f"), col) else col
  })
  x
}