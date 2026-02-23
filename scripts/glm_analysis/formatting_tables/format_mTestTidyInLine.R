mTestTidyInLine <- mTestTidy %>%
  mutate(pFmt = pvalue(mTestTidy$p.value, accuracy = 0.001)) %>%
  fmtNumCols() %>%
  mutate(
    confint = paste(conf.low, conf.high, sep = "-"),
    term_label = recode(term, !!!term_labels)
  )
