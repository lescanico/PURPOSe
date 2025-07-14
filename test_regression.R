# Test script to verify regression analysis
cat("=== TESTING REGRESSION ANALYSIS ===\n")

# Load the feature importance data
feature_importance <- readRDS("data/mod/features_90d_12m_feature_importance.rds")

cat("Feature importance structure:\n")
str(feature_importance)

cat("\n=== REGRESSION RESULTS ===\n")
if (!is.null(feature_importance$regression_results)) {
  for (outcome_name in names(feature_importance$regression_results)) {
    cat("\nOutcome:", outcome_name, "\n")
    results <- feature_importance$regression_results[[outcome_name]]
    cat("Features analyzed:", nrow(results), "\n")
    cat("Significant features:", sum(results$significant), "\n")
    
    if (nrow(results) > 0) {
      cat("Top 5 features by absolute coefficient:\n")
      for (i in 1:min(5, nrow(results))) {
        feat <- results[i, ]
        cat(sprintf("  %d. %s (β = %.3f, p = %.3f%s)\n", 
                    i, feat$feature, feat$coefficient, feat$p_value,
                    ifelse(feat$significant, " *", "")))
      }
    }
  }
} else {
  cat("No regression results found!\n")
}

cat("\n=== CORRELATION RESULTS ===\n")
if (!is.null(feature_importance$outcome_correlations)) {
  for (outcome_name in names(feature_importance$outcome_correlations)) {
    cat("\nTop correlations with", outcome_name, ":\n")
    correlations <- feature_importance$outcome_correlations[[outcome_name]]
    for (i in seq_along(correlations)) {
      cat(sprintf("  %d. %s: %.3f\n", i, names(correlations)[i], correlations[i]))
    }
  }
} else {
  cat("No correlation results found!\n")
} 