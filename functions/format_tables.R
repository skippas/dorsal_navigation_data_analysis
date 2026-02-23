formatSlopeTable <- function(df, caption = NULL) {
  stopifnot(is.data.frame(df))

  if (requireNamespace("kableExtra", quietly = TRUE)) {
    knitr::kable(
      df,
      caption = caption,
      booktabs = TRUE,
      align = "lcccc"
    ) %>%
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "condensed")
      )
  } else {
    knitr::kable(
      df,
      caption = caption,
      booktabs = TRUE,
      align = "lcccc"
    )
  }
}
