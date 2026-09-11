# Add one letter (A, B, C, ...) per facet ROW of a faceted ggplot, placed
# outside the panels: to the left of the shared y-axis title, top-aligned with
# each row of panels.
#
# ggplot2's facet_grid() has no per-row tagging, and patchwork's
# plot_annotation(tag_levels) tags whole plots, not facet rows. This helper
# builds a narrow "letter column" plot with the same number of facet rows and
# the same panel.spacing.y as `p`, then stitches it to the left of `p` with
# patchwork. patchwork aligns the panel areas of the two plots, so each letter
# lines up with the top of its row regardless of axis/strip sizes.
#
# Returns a patchwork object: it prints and ggsave()s like a ggplot and can be
# nested inside other patchwork layouts.
#
# Arguments
#   p        a faceted ggplot (facet_grid or facet_wrap).
#   tags     character vector of labels, one per row (default LETTERS).
#   width    width of the letter column (grid unit).
#   size     font size in points. NULL (default) = the plot's `plot.tag`
#            theme size, so letters match patchwork's plot_annotation() tags.
#   fontface font face. NULL (default) = the plot's `plot.tag` face.
#   vjust    vertical justification relative to the top of each row.
add_row_letters <- function(p, tags = LETTERS, width = grid::unit(1.5, "lines"),
                            size = NULL, fontface = NULL, vjust = 1) {
  nRows <- max(ggplot2::ggplot_build(p)$layout$layout$ROW)
  if (length(tags) < nRows)
    stop("Need at least ", nRows, " tags, got ", length(tags))

  th <- ggplot2::complete_theme(p$theme)
  # Match the vertical gap between panels so the rows line up with `p`.
  spacingY <- ggplot2::calc_element("panel.spacing.y", th)
  # Style the letters like the theme's plot tags (what patchwork's
  # plot_annotation(tag_levels = "A") uses), unless overridden.
  tagEl <- ggplot2::calc_element("plot.tag", th)
  if (is.null(size))     size     <- tagEl$size
  if (is.null(fontface)) fontface <- tagEl$face

  tagDat <- data.frame(row = factor(seq_len(nRows)), label = tags[seq_len(nRows)])

  tagPlot <- ggplot2::ggplot(tagDat) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 1, label = label),
                       hjust = 0, vjust = vjust, fontface = fontface,
                       family = tagEl$family, colour = tagEl$colour,
                       size = size / ggplot2::.pt) +
    ggplot2::facet_grid(rows = ggplot2::vars(row)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   panel.spacing.y = spacingY,
                   plot.margin = ggplot2::margin(0, 0, 0, 0))

  tagPlot + p +
    patchwork::plot_layout(widths = grid::unit.c(width, grid::unit(1, "null")))
}
