tf_func <- function(data){
  data %>%
    group_by(stimulus, decision) %>%
    summarise(
      wasp_n = max(wasp_n),
      total = sum(total),
      cases = sum(cases)
    ) %>%
    rowwise() %>%
    mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
    unnest(cols = c(tst))
}


res_list <- lapply(bin_list, tf_func)
bin_subsets <- bind_rows(res_list, .id = "bin_subset")
rm(res_list, tf_func, bin123, bin345, bin6789)

