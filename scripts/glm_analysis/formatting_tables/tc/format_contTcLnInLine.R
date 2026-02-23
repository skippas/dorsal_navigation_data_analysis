contTcLnInLine <- contTcLn %>%
  mutate(pFmt = pvalue(p.value)) %>%
  fmtNumCols() %>%
  mutate(confint = paste(asymp.LCL, asymp.UCL, sep = "-"))
