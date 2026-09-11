# Builds the supplementary model-coefficients table (tbl-model-coefs).
# Combines the former supp-model-table-prep (data prep, was include:false
# in the qmd, never used anywhere else) and tbl-model-coefs (rendering)
# chunks into one step.
# Objects created:
# tblModelCoefsStr (raw kableExtra output string, for knitr::asis_output()
# in the qmd)

suppArtModelData <- mAllCoefsTbl %>%
  filter(nat_or_art == "artificial") %>%
  mutate(experiment = forcats::fct_relevel(experiment, q2ArtTableOrder)) %>%
  arrange(experiment, orderCoefTerms(term))

suppNatModelData <- mAllCoefsTbl %>%
  filter(nat_or_art == "naturalistic") %>%
  mutate(
    experiment = recode(experiment, !!!natcanLabels),
    experiment = factor(experiment, levels = unname(natcanLabels))
  ) %>%
  arrange(experiment, orderCoefTerms(term))

suppModelData <- bind_rows(suppArtModelData, suppNatModelData)

# Group anchors for pack_rows — art block
suppArtModelGroups <- suppArtModelData %>%
  group_by(experiment) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    end   = cumsum(n),
    start = end - n + 1,
    label = unname(artLabels[as.character(experiment)])
  )

# Group anchors for pack_rows — nat block (row indices continue after art block)
artNRows <- nrow(suppArtModelData)
suppNatModelGroups <- suppNatModelData %>%
  group_by(experiment) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    end   = artNRows + cumsum(n),
    start = end - n + 1,
    label = as.character(experiment)
  )

suppModelTable <- suppModelData %>%
  select(term, logOdds95CI, pFmt)

k <- knitr::kable(
  suppModelTable,
  col.names = c("Term", "Log-odds [95% CI]", "P"),
  caption = paste0(
    "\\label{tbl-model-coefs}Model coefficients for all experiments. ",
    "Fixed effects: trial number, side alternation, reward side, and (for ",
    "experiments with a control phase) experiment phase (manipulation) and ",
    "its interaction with trial number. Random effect: individual ID."
  ),
  booktabs = TRUE, escape = FALSE
)
allGroups <- bind_rows(suppArtModelGroups, suppNatModelGroups)
for (i in seq_len(nrow(allGroups))) {
  k <- kableExtra::pack_rows(k,
    allGroups$label[i],
    allGroups$start[i],
    allGroups$end[i]
  )
}

# Match the 9pt sizing used by the other two supplementary tables. The
# original qmd chunk omitted this, but the table pasted into the manuscript
# had it -- without it this table renders full-size and spills onto an
# extra page.
k <- kableExtra::kable_styling(k, font_size = 9, latex_options = "hold_position")

# The × in the interaction labels passes through kable unescaped, but a raw
# UTF-8 × does not render under pdflatex/T1 (it comes out as "Ö"), so swap it
# for proper math mode here -- writing $\times$ in the label itself does not
# work, as kableExtra escapes it despite escape = FALSE.
tblModelCoefsStr <- gsub("×", "$\\\\times$",
                    gsub("<U\\+2014>", "---",
                    gsub("< ", "$<$ ",
                         paste(as.character(k), collapse = "\n"))))
