# running pairwise prop.tests and extracting necessary stats
# should be possible by simply modifying pairwise.prop.test, but couldnt get that working.

pw_prop_test_df_out <- function(succfail_mat, p_adjust_method = "holm") {
  # Generate all row combinations for pairwise comparisons
  row_combinations <- combn(1:nrow(succfail_mat), 2)
  
  # Perform pairwise proportion tests
  results <- apply(row_combinations, 2, function(row_indices) {
    # Combine rows into a 2-row matrix for prop.test
    # index 2 before 1 so that the larger group is compared to the smaller
    succfail <- rbind(succfail_mat[row_indices[2], ], succfail_mat[row_indices[1], ]) 
    test <- prop.test(succfail)
    
    # Extract desired results
    list(
      groups = paste(rownames(succfail_mat)[row_indices], collapse = " vs "),
      p_value = test$p.value,
      estimate_diff = test$estimate[1] - test$estimate[2],  # Difference in proportions
      conf_int = test$conf.int  # Confidence interval
    )
  })
  
  # Convert results list into a data frame
  results_df <- do.call(rbind, lapply(results, function(res) {
    data.frame(
      groups = res$groups,
      p_value = res$p_value,
      estimate_diff = res$estimate_diff,
      conf_low = res$conf_int[1],
      conf_high = res$conf_int[2]
    )
  }))
  
  # Adjust p-values using the specified method
  results_df$p_value <- p.adjust(results_df$p_value, method = p_adjust_method)
  
  return(results_df)
}


