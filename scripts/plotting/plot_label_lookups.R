# Display name lookups. Keys are raw experiment IDs; values are display names.
# Shared by 5_results_report.qmd and the plotting functions built in
# 3_format_results.R -- sourced by both rather than defined twice.
artLabels <- c(
  perpPara_170725   = "Perp./Para.",
  thickOb_140823    = "Thick oblique",
  thinOb_150823     = "Oblique ±45°",
  brightDiff_250725 = "Transp./opaque"
)

natcanLabels <- c(
  natcan1_170923 = "Canopy E",
  natcan2_300725 = "Canopy F",
  natcan3_080825 = "Canopy G (pilot)",
  natcan3_190925 = "Canopy G",
  natcan4_180825 = "Canopy H",
  natcan5_290825 = "Canopy I",
  natcan6_070925 = "Canopy J"
)

# HTML image labels for ggtext strip rendering
# TODO: canopyD.png is missing from the new icon directory — add it or confirm canopyJ.png is its replacement
stimPngDir <- "../figures_and_schematics/legend_icons/stimulus_pair_icons"
imgW <- 65

natcanImgLabels <- c(
  "Canopy E"  = paste0("<img src='", stimPngDir, "/canopyE.png' width='", imgW, "'/><br/>**Canopy E**"),
  "Canopy F"  = paste0("<img src='", stimPngDir, "/canopyF.png' width='", imgW, "'/><br/>**Canopy F**"),
  "Canopy G (pilot)" = paste0("<img src='", stimPngDir, "/canopyG.png' width='", imgW, "'/><br/>**Canopy G (pilot)**"),
  "Canopy G" = paste0("<img src='", stimPngDir, "/canopyG.png' width='", imgW, "'/><br/>**Canopy G**"),
  "Canopy H"  = paste0("<img src='", stimPngDir, "/canopyH.png' width='", imgW, "'/><br/>**Canopy H**"),
  "Canopy I"  = paste0("<img src='", stimPngDir, "/canopyI.png' width='", imgW, "'/><br/>**Canopy I**"),
  "Canopy J"  = paste0("<img src='", stimPngDir, "/canopyJ.png' width='", imgW, "'/><br/>**Canopy J**")
)

artImgLabels <- setNames(
  c(
    paste0("<img src='", stimPngDir, "/perpPara.png' width='",    imgW, "'/><br/>**Perp./Para.**"),
    paste0("<img src='", stimPngDir, "/thickOb.png' width='",     imgW, "'/><br/>**Thick oblique**"),
    paste0("<img src='", stimPngDir, "/thinOb.png' width='",      imgW, "'/><br/>**Oblique ±45°**"),
    paste0("<img src='", stimPngDir, "/brightDiff.png' width='",  imgW, "'/><br/>**Transp./Opaque**")
  ),
  unname(artLabels)
)
