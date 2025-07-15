#' Helper function to extract top significant features from regression results
#' @param feature_analysis output from calculate_feature_analysis
#' @param outcome_name specific outcome to analyze (if NULL, uses first available)
#' @param top_n number of top features to return (default: 10)
#' @param significance_level p-value threshold (default: 0.05)
#' @return data.frame with top significant features
extract_top_features <- function(feature_analysis, outcome_name = NULL, top_n = 10, significance_level = 0.05) {
  if (is.null(feature_analysis$regression_results)) {
    message("No regression results available")
    return(NULL)
  }
  
  # Use first outcome if none specified
  if (is.null(outcome_name)) {
    outcome_name <- names(feature_analysis$regression_results)[1]
  }
  
  if (!outcome_name %in% names(feature_analysis$regression_results)) {
    message(paste0("Outcome '", outcome_name, "' not found in regression results"))
    return(NULL)
  }
  
  results <- feature_analysis$regression_results[[outcome_name]]
  
  # Filter by significance and sort by absolute coefficient
  significant_features <- results[results$p_value < significance_level, ]
  significant_features <- significant_features[order(significant_features$abs_coefficient, decreasing = TRUE), ]
  
  # Return top N features
  return(significant_features[1:min(top_n, nrow(significant_features)), ])
}

#' Helper function to print regression summary in readable format
#' @param feature_analysis output from calculate_feature_analysis
#' @param outcome_name specific outcome to analyze (if NULL, uses first available)
#' @param top_n number of top features to show (default: 10)
print_regression_summary <- function(feature_analysis, outcome_name = NULL, top_n = 10) {
  if (is.null(feature_analysis$regression_results)) {
    message("No regression results available")
    return(NULL)
  }
  
  # Use first outcome if none specified
  if (is.null(outcome_name)) {
    outcome_name <- names(feature_analysis$regression_results)[1]
  }
  
  if (!outcome_name %in% names(feature_analysis$regression_results)) {
    message(paste0("Outcome '", outcome_name, "' not found in regression results"))
    return(NULL)
  }
  
  results <- feature_analysis$regression_results[[outcome_name]]
  
  cat("=== LINEAR REGRESSION RESULTS FOR", outcome_name, "===\n")
  cat("Total features analyzed:", nrow(results), "\n")
  cat("Significant features (p < 0.05):", sum(results$significant), "\n")
  cat("Significant features (p < 0.01):", sum(results$p_value < 0.01), "\n")
  cat("Significant features (p < 0.001):", sum(results$p_value < 0.001), "\n\n")
  
  # Show top features by absolute coefficient
  top_features <- results[order(results$abs_coefficient, decreasing = TRUE), ][1:top_n, ]
  
  cat("TOP", top_n, "FEATURES BY ABSOLUTE COEFFICIENT:\n")
  cat("Feature", "Coefficient", "Std.Error", "t-value", "p-value", "Significant\n", sep = "\t")
  cat("-------", "----------", "---------", "-------", "-------", "-----------\n", sep = "\t")
  
  for (i in 1:nrow(top_features)) {
    feat <- top_features[i, ]
    sig_mark <- ifelse(feat$significant, "*", "")
    cat(sprintf("%-30s %8.3f %9.3f %7.2f %7.3f %s\n", 
                feat$feature, feat$coefficient, feat$std_error, 
                feat$t_value, feat$p_value, sig_mark))
  }
  
  cat("\n* = significant at p < 0.05\n")
}

# Load required packages with error handling
required_packages <- c("dplyr", "lubridate", "stringr", "timeDate", "data.table")

# Function to safely load packages
load_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tryCatch({
      install.packages(pkg, repos = "https://cran.rstudio.com/")
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      message(paste0("Could not install package '", pkg, "'. This may be due to permissions or a locked-down environment. Please install manually if needed."))
      stop(paste0("Failed to install/load package '", pkg, "': ", e$message))
    })
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Load all required packages
for (pkg in required_packages) {
  load_package(pkg)
}

#' Create calendar features for visit data
#' @param visits_data data.table of visits
#' @param medications_var optional medications variable name
#' @return data.table with calendar features added
create_calendar_features <- function(visits_data, medications_var = NULL) {
  message("Creating calendar features...")
  
  # Validate date column exists and is properly formatted
  if (!"date" %in% names(visits_data)) {
    stop("Required column 'date' not found in visits_data")
  }
  
  # Ensure date is properly formatted
  if (!inherits(visits_data$date, "Date")) {
    tryCatch({
      visits_data$date <- as.Date(visits_data$date)
    }, error = function(e) {
      stop("Failed to convert 'date' column to Date format: ", e$message)
    })
  }
  
  # Define US holidays with error handling
  tryCatch({
    years <- unique(year(visits_data$date))
    us_holidays <- do.call(c, lapply(years, holidayNYSE)) |> as.Date()
  }, error = function(e) {
    warning(paste0("Failed to calculate US holidays: ", e$message, ". Holiday features will be set to FALSE."))
    us_holidays <- as.Date(character(0))
  })
  
  # Add calendar features with error handling
  tryCatch({
    visits_data[, `:=`(
      weekday = wday(date),
      is_weekend = wday(date) %in% c(1, 7),
      on_holiday = date %in% us_holidays,
      day_before_holiday = (date + 1) %in% us_holidays,
      day_after_holiday = (date - 1) %in% us_holidays,
      day_of_year = yday(date),
      week_of_month = ceiling(day(date) / 7),
      week_of_year = isoweek(date),
      month = factor(month.abb[month(date)], levels = month.abb),
      quarter = quarter(date),
      season = factor(case_when(
        month(date) %in% c(12,1,2) ~ "Winter",
        month(date) %in% c(3,4,5) ~ "Spring",
        month(date) %in% c(6,7,8) ~ "Summer",
        TRUE ~ "Fall"
      ), levels = c("Winter","Spring","Summer","Fall"))
    )]
  }, error = function(e) {
    stop("Failed to create calendar features: ", e$message)
  })
  
  # Add time bin if time column exists
  if ("time" %in% names(visits_data)) {
    tryCatch({
      visits_data[, time_bin := factor(case_when(
        is.na(time) ~ NA_character_,
        time < 12 ~ "Morning",
        time < 17 ~ "Afternoon",
        TRUE ~ "Evening"
      ), levels = c("Morning","Afternoon","Evening"))]
    }, error = function(e) {
      warning("Failed to create time bins: ", e$message)
    })
  }
  
  # Add medication count if medications variable specified
  if (!is.null(medications_var) && medications_var %in% names(visits_data)) {
    tryCatch({
      visits_data[, n_medications := ifelse(is.na(get(medications_var)) | get(medications_var) == "None", 
                                            0L, 
                                            str_count(get(medications_var), "\n") + 1L)]
    }, error = function(e) {
      warning("Failed to calculate medication counts: ", e$message)
      visits_data[, n_medications := 0L]
    })
  }
  
  return(visits_data)
}

#' Calculate visit patterns and timing features
#' @param joined_data data.table with joined visit and patient data
#' @param id_var patient identifier variable
#' @param static_vars vector of static (patient-level) variables
#' @param dynamic_vars vector of dynamic (visit-level) variables
#' @param observed_days number of observation days
#' @param diagnosis_var optional diagnosis variable
#' @param medications_var optional medications variable
#' @return data.table with engineered features
calculate_visit_patterns <- function(joined_data, id_var, static_vars, dynamic_vars, observed_days, 
                                   diagnosis_var = NULL, medications_var = NULL) {
  message("Engineering visit patterns...")
  # First, get all unique levels for categorical variables to ensure consistent columns
  categorical_vars <- c()
  all_levels <- list()
  for (var in dynamic_vars) {
    if (!is.numeric(joined_data[[var]])) {
      categorical_vars <- c(categorical_vars, var)
      all_levels[[var]] <- unique(joined_data[[var]])
    }
  }
  features <- joined_data[, {
    # Static variables (take first value)
    out <- list()
    for (var in static_vars) {
      out[[var]] <- .SD[[var]][1]
    }
    # Dynamic variable aggregations
    for (var in dynamic_vars) {
      if (is.numeric(.SD[[var]])) {
        out[[paste0(var, "_mean")]] <- mean(.SD[[var]], na.rm = TRUE)
        out[[paste0(var, "_sum")]] <- sum(.SD[[var]], na.rm = TRUE)
        out[[paste0(var, "_count")]] <- sum(!is.na(.SD[[var]]))
      } else {
        var_table <- table(.SD[[var]])
        var_levels <- all_levels[[var]]
        for (level in var_levels) {
          count_val <- ifelse(level %in% names(var_table), as.integer(var_table[level]), 0L)
          out[[paste0(var, "_", level, "_count")]] <- count_val
          out[[paste0(var, "_", level, "_pct")]] <- ifelse(nrow(.SD) > 0, as.numeric(count_val / nrow(.SD)), 0)
        }
      }
    }
    
    # Calendar features
    out$n_visits <- as.integer(nrow(.SD))
    out$n_days_active <- as.integer(n_distinct(date))
    out$active_weeks <- as.integer(n_distinct(format(date, "%Y-%U")))
    
    # Visit gaps
    if (nrow(.SD) >= 2) {
      gaps <- diff(sort(date))
      out$mean_gap <- mean(gaps, na.rm = TRUE)
      out$max_gap <- max(gaps, na.rm = TRUE)
      out$visit_std_gap <- sd(gaps, na.rm = TRUE)
    } else {
      out$mean_gap <- observed_days
      out$max_gap <- observed_days
      out$visit_std_gap <- 0  # No variation for single visits
    }
    
    # Handle NAs in visit_std_gap (shouldn't happen but just in case)
    if (is.na(out$visit_std_gap)) {
      out$visit_std_gap <- 0
    }
    
    # Calendar patterns
    out$pct_weekend_visits <- mean(is_weekend, na.rm = TRUE)
    out$pct_holiday_visits <- mean(on_holiday, na.rm = TRUE)
    out$pct_day_before_holiday <- mean(day_before_holiday, na.rm = TRUE)
    out$pct_day_after_holiday <- mean(day_after_holiday, na.rm = TRUE)
    
    # Weekday patterns (1=Sunday, 2=Monday, ..., 7=Saturday)
    out$pct_sunday_visits <- mean(weekday == 1, na.rm = TRUE)
    out$pct_monday_visits <- mean(weekday == 2, na.rm = TRUE)
    out$pct_tuesday_visits <- mean(weekday == 3, na.rm = TRUE)
    out$pct_wednesday_visits <- mean(weekday == 4, na.rm = TRUE)
    out$pct_thursday_visits <- mean(weekday == 5, na.rm = TRUE)
    out$pct_friday_visits <- mean(weekday == 6, na.rm = TRUE)
    out$pct_saturday_visits <- mean(weekday == 7, na.rm = TRUE)
    
    # Time of day patterns
    if ("time_bin" %in% names(.SD)) {
      out$pct_morning_visits <- mean(time_bin == "Morning", na.rm = TRUE)
      out$pct_afternoon_visits <- mean(time_bin == "Afternoon", na.rm = TRUE)
      out$pct_evening_visits <- mean(time_bin == "Evening", na.rm = TRUE)
    }
    
    # Seasonal patterns
    out$pct_winter_visits <- mean(season == "Winter", na.rm = TRUE)
    out$pct_spring_visits <- mean(season == "Spring", na.rm = TRUE)
    out$pct_summer_visits <- mean(season == "Summer", na.rm = TRUE)
    out$pct_fall_visits <- mean(season == "Fall", na.rm = TRUE)
    
    # Monthly patterns
    out$pct_jan_visits <- mean(month == "Jan", na.rm = TRUE)
    out$pct_feb_visits <- mean(month == "Feb", na.rm = TRUE)
    out$pct_mar_visits <- mean(month == "Mar", na.rm = TRUE)
    out$pct_apr_visits <- mean(month == "Apr", na.rm = TRUE)
    out$pct_may_visits <- mean(month == "May", na.rm = TRUE)
    out$pct_jun_visits <- mean(month == "Jun", na.rm = TRUE)
    out$pct_jul_visits <- mean(month == "Jul", na.rm = TRUE)
    out$pct_aug_visits <- mean(month == "Aug", na.rm = TRUE)
    out$pct_sep_visits <- mean(month == "Sep", na.rm = TRUE)
    out$pct_oct_visits <- mean(month == "Oct", na.rm = TRUE)
    out$pct_nov_visits <- mean(month == "Nov", na.rm = TRUE)
    out$pct_dec_visits <- mean(month == "Dec", na.rm = TRUE)
    
    # Quarterly patterns
    out$pct_q1_visits <- mean(quarter == 1, na.rm = TRUE)
    out$pct_q2_visits <- mean(quarter == 2, na.rm = TRUE)
    out$pct_q3_visits <- mean(quarter == 3, na.rm = TRUE)
    out$pct_q4_visits <- mean(quarter == 4, na.rm = TRUE)
    
    # Calendar timing features
    out$avg_day_of_year <- mean(day_of_year, na.rm = TRUE)
    out$std_day_of_year <- sd(day_of_year, na.rm = TRUE)
    out$min_day_of_year <- min(day_of_year, na.rm = TRUE)
    out$max_day_of_year <- max(day_of_year, na.rm = TRUE)
    
    # Weekly patterns
    out$avg_week_of_month <- mean(week_of_month, na.rm = TRUE)
    out$std_week_of_month <- sd(week_of_month, na.rm = TRUE)
    out$avg_week_of_year <- mean(week_of_year, na.rm = TRUE)
    out$std_week_of_year <- sd(week_of_year, na.rm = TRUE)
    
    # Handle NAs in timing variation features (for patients with same-day visits)
    if (is.na(out$std_day_of_year)) out$std_day_of_year <- 0
    if (is.na(out$std_week_of_month)) out$std_week_of_month <- 0
    if (is.na(out$std_week_of_year)) out$std_week_of_year <- 0
    
    # Handle NaN/Inf values
    numeric_cols <- sapply(out, is.numeric)
    for (col in names(out)[numeric_cols]) {
      if (is.nan(out[[col]]) || is.infinite(out[[col]])) {
        out[[col]] <- 0
      }
    }
    
    # Clinical features if diagnosis variable specified
    if (!is.null(diagnosis_var) && diagnosis_var %in% .SD) {
      out$n_unique_diagnoses <- as.integer(n_distinct(.SD[[diagnosis_var]], na.rm = TRUE))
    }
    
    # Medication features if medications variable specified
    if (!is.null(medications_var) && medications_var %in% .SD) {
      out$total_medications <- sum(n_medications, na.rm = TRUE)
      out$avg_medications_per_visit <- mean(n_medications, na.rm = TRUE)
    }
    
    out
  }, by = id_var]
  return(features)
}

#' Calculate outcomes and utilization tiers
#' @param visits_data data.table of visits
#' @param patients_index data.table with patient timeline information
#' @param id_var patient identifier variable
#' @param prediction_months number of prediction months
#' @param use_followup_weights whether to use follow-up weights
#' @return data.table with outcomes and utilization tiers
calculate_outcomes <- function(visits_data, patients_index, id_var, prediction_months, use_followup_weights) {
  message("Calculating outcomes...")
  
  # Debug: Check patients_index structure
  message("Patients index columns: ", paste(names(patients_index), collapse = ", "))
  message("Patients index rows: ", nrow(patients_index))
  
  # Merge with explicit column selection
  outcomes <- merge(visits_data, patients_index[, c(id_var, "feature_end_date", "prediction_end_date", "actual_followup_days"), with = FALSE], 
                   by = id_var, all.x = TRUE)
  
  # Ensure we have the required columns
  if (!all(c("feature_end_date", "prediction_end_date", "actual_followup_days") %in% names(outcomes))) {
    stop("Required columns (feature_end_date, prediction_end_date, actual_followup_days) not found in merged outcomes data. Available columns: ", 
         paste(names(outcomes), collapse = ", "))
  }
  
  # Calculate outcomes with continuous follow-up weights
  outcomes <- outcomes[, .(
    future_visits = sum(date > feature_end_date & date <= prediction_end_date, na.rm = TRUE),
    future_hours = sum(length * (date > feature_end_date & date <= prediction_end_date), na.rm = TRUE) / 60,
    actual_followup_days = first(actual_followup_days),
    expected_followup_days = as.integer(prediction_months * 30.44),
    followup_complete = first(actual_followup_days) >= as.integer(prediction_months * 30.44),
    followup_completeness_ratio = first(actual_followup_days) / as.integer(prediction_months * 30.44),
    prediction_weight = if(use_followup_weights) pmin(first(actual_followup_days) / as.integer(prediction_months * 30.44), 1.0) else 1.0
  ), by = id_var]
  
  # Calculate utilization tier percentiles for binary outcomes
  if (nrow(outcomes) > 0) {
    # Calculate percentiles for visits and hours
    outcomes[, `:=`(
      visits_percentile = percent_rank_safe(future_visits),
      hours_percentile = percent_rank_safe(future_hours)
    )]
    
    # Create binary utilization tier variables with "High" as first level
    outcomes[, `:=`(
      # Visit utilization tiers (vut = visit utilization tier)
      vut50 = factor(visits_percentile >= 0.50, levels = c(TRUE, FALSE), labels = c("High", "Low")),
      vut75 = factor(visits_percentile >= 0.75, levels = c(TRUE, FALSE), labels = c("High", "Low")),
      vut90 = factor(visits_percentile >= 0.90, levels = c(TRUE, FALSE), labels = c("High", "Low")),
      vut99 = factor(visits_percentile >= 0.99, levels = c(TRUE, FALSE), labels = c("High", "Low")),
      
      # Hour utilization tiers (hut = hour utilization tier)
      hut50 = factor(hours_percentile >= 0.50, levels = c(TRUE, FALSE), labels = c("High", "Low")),
      hut75 = factor(hours_percentile >= 0.75, levels = c(TRUE, FALSE), labels = c("High", "Low")),
      hut90 = factor(hours_percentile >= 0.90, levels = c(TRUE, FALSE), labels = c("High", "Low")),
      hut99 = factor(hours_percentile >= 0.99, levels = c(TRUE, FALSE), labels = c("High", "Low"))
    )]
  }
  
  return(outcomes)
}

#' Create utilization summary with percentile rankings
#' @param visits_data data.table of visits
#' @param id_var patient identifier variable
#' @return data.table with utilization summary
create_utilization_summary <- function(visits_data, id_var) {
  message("Creating utilization summary...")
  
  # Create utilization_summary (utilization summary with percentile rankings)
  utilization_summary <- visits_data[, .(
    n_visits = .N,
    n_hours = sum(length, na.rm = TRUE) / 60,
    n_noshow = sum(status == "No Show", na.rm = TRUE),
    n_noshow_hours = sum(length * (status == "No Show"), na.rm = TRUE) / 60,
    schedule_span = pmax(as.numeric(max(date) - min(date)), 1)
  ), by = id_var]
  
  # Only calculate percentiles if we have data
  if (nrow(utilization_summary) > 0) {
    # First, calculate the basic metrics
    utilization_summary[, `:=`(
      noshow_rate = ifelse(n_visits > 0, n_noshow / n_visits, 0),
      pct_visits_share = 100 * n_visits / sum(n_visits),
      pct_hours_share = 100 * n_hours / sum(n_hours),
      hours_percentile = percent_rank_safe(n_hours)
    )]
    
    # Then, calculate the utilization tier based on the percentile
    utilization_summary[, utilization_tier := factor(case_when(
      hours_percentile >= 0.99 ~ "Top 1%",
      hours_percentile >= 0.95 ~ "1–5%",
      hours_percentile >= 0.90 ~ "5–10%",
      hours_percentile >= 0.75 ~ "10–25%",
      hours_percentile >= 0.50 ~ "25–50%",
      TRUE ~ "Bottom 50%"
    ), levels = c("Top 1%", "1–5%", "5–10%", "10–25%", "25–50%", "Bottom 50%"), ordered = TRUE)]
  } else {
    # Create empty columns if no data
    utilization_summary[, `:=`(
      noshow_rate = numeric(0),
      pct_visits_share = numeric(0),
      pct_hours_share = numeric(0),
      hours_percentile = numeric(0),
      utilization_tier = factor(character(0), levels = c("Top 1%", "1–5%", "5–10%", "10–25%", "25–50%", "Bottom 50%"), ordered = TRUE)
    )]
  }
  
  # Handle NaN/Inf values in utilization_summary
  numeric_cols <- sapply(utilization_summary, is.numeric)
  for (col in names(utilization_summary)[numeric_cols]) {
    utilization_summary[is.nan(get(col)) | is.infinite(get(col)), (col) := 0]
  }
  
  return(utilization_summary)
}

#' Calculate comprehensive feature analysis
#' @param patients_feat data.table with patient features
#' @param verbose logical, if TRUE print diagnostic output
#' @return list with multiple feature analysis metrics
calculate_feature_analysis <- function(patients_feat, verbose = FALSE) {
  if (nrow(patients_feat) < 10) {
    message("Insufficient data for feature importance preview (need at least 10 patients)")
    return(NULL)
  }
  
  # Identify outcome and feature columns (more comprehensive)
  outcome_cols <- grep("future_", names(patients_feat), value = TRUE)
  tracking_vars <- c("actual_followup_days", "expected_followup_days", "followup_complete", 
                     "followup_completeness_ratio", "prediction_weight", "single_visit_flag", "same_day_visits_flag")
  
  # Ensure outcome columns are numeric
  for (col in outcome_cols) {
    if (!is.numeric(patients_feat[[col]])) {
      patients_feat[[col]] <- as.numeric(patients_feat[[col]])
    }
  }
  
  # Include ALL features, not just specific patterns
  feature_cols <- setdiff(names(patients_feat), c("patient_id", outcome_cols, tracking_vars))
  
  message("Outcome columns found: ", paste(outcome_cols, collapse = ", "))
  message("Feature columns found: ", length(feature_cols), " features")
  
  message("Feature importance analysis:")
  message("  - Outcomes found: ", paste(outcome_cols, collapse = ", "))
  message("  - Features found: ", length(feature_cols))
  message("  - Total patients: ", nrow(patients_feat))
  
  if (length(outcome_cols) == 0 || length(feature_cols) == 0) {
    message("No outcome or feature columns found for analysis")
    return(NULL)
  }
  
  # Select numeric columns for analysis
  analysis_data <- patients_feat[, c(feature_cols, outcome_cols), with = FALSE]
  numeric_cols <- sapply(analysis_data, is.numeric)
  analysis_data <- analysis_data[, names(analysis_data)[numeric_cols], with = FALSE]

  # PATCH: Check for missing or non-numeric outcome columns
  missing_outcomes <- setdiff(outcome_cols, names(analysis_data))
  if (length(missing_outcomes) > 0) {
    warning(paste0("The following outcome columns are missing or not numeric in analysis_data: ", paste(missing_outcomes, collapse = ", ")))
    message("Names in analysis_data: ", paste(names(analysis_data), collapse = ", "))
    message("Outcome columns expected: ", paste(outcome_cols, collapse = ", "))
    return(NULL)
  }

  # PATCH: Diagnostic printout for outcome columns (only if verbose)
  if (verbose) {
    for (col in outcome_cols) {
      if (col %in% names(analysis_data)) {
        message(paste0("Summary for outcome column '", col, "':"))
        print(summary(analysis_data[[col]]))
        message(paste0("NA count for '", col, "': ", sum(is.na(analysis_data[[col]]))))
      }
    }
    # Print number of complete cases
    n_complete_all <- sum(complete.cases(analysis_data))
    n_complete_outcomes <- sum(complete.cases(analysis_data[, outcome_cols, with = FALSE]))
    message(paste0("Number of complete cases (all columns): ", n_complete_all))
    message(paste0("Number of complete cases (outcome columns only): ", n_complete_outcomes))
  }
  
  if (ncol(analysis_data) < 2) {
    message("Insufficient numeric columns for analysis")
    return(NULL)
  }
  
  # Check which outcomes made it into the analysis data
  analysis_outcomes <- intersect(outcome_cols, names(analysis_data))
  
  # 1. Enhanced Correlation Analysis
  correlation_matrix <- cor(analysis_data, use = "complete.obs")
  outcome_correlations <- list()

  # FIX: Use colnames for outcome presence check
  outcomes_in_matrix <- intersect(analysis_outcomes, colnames(correlation_matrix))
  if (length(outcomes_in_matrix) == 0) {
    warning("No outcomes found in correlation matrix. Check data structure.")
  }

  # Ensure correlation matrix has proper names
  if (is.null(colnames(correlation_matrix)) || is.null(rownames(correlation_matrix))) {
    warning("Correlation matrix missing row or column names. This may cause issues with feature identification.")
  }
  
  for (outcome in analysis_outcomes) {
    if (outcome %in% names(correlation_matrix)) {
      # Get correlations with this outcome
      correlations <- correlation_matrix[outcome, ]
      
      # Remove self-correlation, NA values, and other outcomes
      correlations <- correlations[!is.na(correlations) & correlations != 1]
      # Only keep correlations with feature variables (not other outcomes)
      feature_correlations <- correlations[names(correlations) %in% feature_vars]
      
      # Sort by absolute value and get top 10
      correlations_abs <- abs(feature_correlations)
      top_indices <- order(correlations_abs, decreasing = TRUE)[1:min(10, length(correlations_abs))]
      top_correlations <- feature_correlations[top_indices]
      
      # Ensure we preserve the feature names and have valid data
      if (length(top_correlations) > 0 && !is.null(names(top_correlations))) {
        outcome_correlations[[outcome]] <- top_correlations
      }
    }
  }
  
  # 2. Variance Analysis
  feature_vars <- setdiff(names(analysis_data), analysis_outcomes)
  variances <- sapply(analysis_data[, feature_vars, with = FALSE], var, na.rm = TRUE)
  top_variance_features <- sort(variances, decreasing = TRUE)[1:10]
  
  # 3. Simple Linear Regression Analysis (NEW)
  regression_results <- list()
  
  # Helper to sanitize feature names for formulas
  sanitize_feature_name <- function(x) {
    if (grepl("[^a-zA-Z0-9_]", x)) paste0("`", x, "`") else x
  }
  
  for (outcome in analysis_outcomes) {
    if (outcome %in% names(analysis_data)) {
      # Prepare data for regression (remove rows with missing outcome)
      reg_data <- analysis_data[!is.na(analysis_data[[outcome]]), ]

      if (nrow(reg_data) > 10) {
        # Get top features by correlation for regression (use more features)
        outcome_correlations <- abs(correlation_matrix[outcome, feature_vars])
        # Use top 20 features or all features if fewer than 20
        n_features_to_use <- min(20, length(feature_vars))
        top_features <- names(sort(outcome_correlations, decreasing = TRUE)[1:n_features_to_use])

        # Sanitize feature names for formula
        top_features_sanitized <- sapply(top_features, sanitize_feature_name)
        feature_formula <- paste(top_features_sanitized, collapse = " + ")
        formula_str <- paste(outcome, "~", feature_formula)

        message("Fitting regression for ", outcome, " with ", length(top_features), " features")

        tryCatch({
          # Fit simple linear regression
          model <- lm(as.formula(formula_str), data = reg_data, na.action = na.omit)

          # Extract coefficients and p-values
          coef_summary <- summary(model)$coefficients

          # Create results data frame
          results_df <- data.frame(
            feature = rownames(coef_summary),
            coefficient = coef_summary[, "Estimate"],
            std_error = coef_summary[, "Std. Error"],
            t_value = coef_summary[, "t value"],
            p_value = coef_summary[, "Pr(>|t|)"],
            significant = coef_summary[, "Pr(>|t|)"] < 0.05
          )

          # Sort by absolute coefficient value (excluding intercept)
          results_df <- results_df[results_df$feature != "(Intercept)", ]
          results_df$abs_coefficient <- abs(results_df$coefficient)
          results_df <- results_df[order(results_df$abs_coefficient, decreasing = TRUE), ]

          # Keep all features (up to 10)
          regression_results[[outcome]] <- results_df

          message("Regression successful for ", outcome, ": ", nrow(results_df), " features analyzed")

        }, error = function(e) {
          message("Regression analysis failed for ", outcome, ": ", e$message)
          # Try with just top 10 features if it fails
          if (length(top_features) > 10) {
            message("Trying with top 10 features only...")
            top_features_reduced <- top_features[1:10]
            top_features_reduced_sanitized <- sapply(top_features_reduced, sanitize_feature_name)
            feature_formula_reduced <- paste(top_features_reduced_sanitized, collapse = " + ")
            formula_str_reduced <- paste(outcome, "~", feature_formula_reduced)

            tryCatch({
              model_reduced <- lm(as.formula(formula_str_reduced), data = reg_data, na.action = na.omit)
              coef_summary_reduced <- summary(model_reduced)$coefficients

              results_df_reduced <- data.frame(
                feature = rownames(coef_summary_reduced),
                coefficient = coef_summary_reduced[, "Estimate"],
                std_error = coef_summary_reduced[, "Std. Error"],
                t_value = coef_summary_reduced[, "t value"],
                p_value = coef_summary_reduced[, "Pr(>|t|)"],
                significant = coef_summary_reduced[, "Pr(>|t|)"] < 0.05
              )

              results_df_reduced <- results_df_reduced[results_df_reduced$feature != "(Intercept)", ]
              results_df_reduced$abs_coefficient <- abs(results_df_reduced$coefficient)
              results_df_reduced <- results_df_reduced[order(results_df_reduced$abs_coefficient, decreasing = TRUE), ]

              regression_results[[outcome]] <- results_df_reduced
              message("Reduced regression successful for ", outcome)

            }, error = function(e2) {
              message("Reduced regression also failed for ", outcome, ": ", e2$message)
            })
          }
        })
      } else {
        message("Insufficient data for regression analysis of ", outcome, " (need > 10 rows)")
      }
    }
  }
  
  # 4. Feature Categories Analysis
  feature_categories <- list(
    visit_patterns = grep("n_visits|n_days_active|mean_gap|max_gap", feature_vars, value = TRUE),
    timing_patterns = grep("pct_|weekday|time_bin", feature_vars, value = TRUE),
    clinical_patterns = grep("diagnosis|medication|provider", feature_vars, value = TRUE),
    financial_patterns = grep("copay|prepay|payer", feature_vars, value = TRUE),
    demographic_patterns = grep("age|sex|race|marital", feature_vars, value = TRUE)
  )
  
  # Calculate average importance by category
  category_importance <- list()
  for (outcome in analysis_outcomes) {
    if (outcome %in% names(correlation_matrix)) {
      category_importance[[outcome]] <- list()
      for (cat_name in names(feature_categories)) {
        cat_features <- feature_categories[[cat_name]]
        if (length(cat_features) > 0) {
          cat_correlations <- abs(correlation_matrix[outcome, cat_features])
          category_importance[[outcome]][[cat_name]] <- mean(cat_correlations, na.rm = TRUE)
        }
      }
    }
  }
  
  # 5. Feature Ranking by Multiple Metrics
  feature_rankings <- list()
  for (outcome in analysis_outcomes) {
    if (outcome %in% names(analysis_data)) {
      # Combine correlation and variance metrics
      cor_scores <- abs(correlation_matrix[outcome, feature_vars])
      var_scores <- variances[feature_vars] / max(variances, na.rm = TRUE)
      
      # Create composite score (normalized correlation + variance)
      composite_scores <- (cor_scores + var_scores) / 2
      feature_rankings[[outcome]] <- sort(composite_scores, decreasing = TRUE)[1:10]
    }
  }
  
  return(list(
    correlation_matrix = correlation_matrix,
    outcome_correlations = outcome_correlations,
    top_variance_features = top_variance_features,
    regression_results = regression_results,  # NEW: Actual statistical modeling results
    feature_rankings = feature_rankings,
    feature_categories = feature_categories,
    category_importance = category_importance,
    analysis_summary = list(
      total_features = length(feature_vars),
      total_outcomes = length(analysis_outcomes),
      features_analyzed = ncol(analysis_data),
      regression_models_fitted = length(regression_results)
    )
  ))
}

#' Automatically exclude features with missing data or low variance
#' @param data data.table with features
#' @param id_var patient identifier variable
#' @param static_vars vector of static (patient-level) variables to always keep
#' @param missing_threshold maximum proportion of missing values allowed (default: 0.1)
#' @param variance_threshold minimum variance for numeric features (default: 0.01)
#' @param near_zero_var_threshold threshold for near-zero variance (default: 0.95)
#' @param correlation_threshold maximum correlation allowed between features (default: 0.95)
#' @return list with cleaned data and exclusion summary
clean_features <- function(data, id_var, static_vars, 
                         missing_threshold = 0.1, 
                         variance_threshold = 0.01,
                         near_zero_var_threshold = 0.95,
                         correlation_threshold = 0.95,
                         exclude_vars = character(0)) {
  
  message("Cleaning features...")
  
  # Variables to always keep
  keep_vars <- c(id_var, static_vars)
  
  # Validate that required variables exist
  missing_keep_vars <- setdiff(keep_vars, names(data))
  if (length(missing_keep_vars) > 0) {
    stop("Required variables not found in data: ", paste(missing_keep_vars, collapse = ", "))
  }
  
  # Get feature variables (exclude ID, demographics, outcomes, tracking variables, and any extra exclude_vars)
  outcome_vars <- grep("future_", names(data), value = TRUE)
  tracking_vars <- c("actual_followup_days", "expected_followup_days", "followup_complete", 
                     "followup_completeness_ratio", "prediction_weight")
  flag_vars <- c("single_visit_flag", "same_day_visits_flag")
  
  # Validate that expected variables exist (warn if missing)
  expected_vars <- c(tracking_vars, flag_vars)
  missing_expected <- setdiff(expected_vars, names(data))
  if (length(missing_expected) > 0) {
    warning("Some expected variables not found in data: ", paste(missing_expected, collapse = ", "))
  }
  
  feature_vars <- setdiff(names(data), c(keep_vars, outcome_vars, tracking_vars, flag_vars, exclude_vars))
  
  # Initialize exclusion tracking
  excluded_missing <- c()
  excluded_low_variance <- c()
  excluded_near_zero <- c()
  excluded_collinear <- c()
  
  # 1. Check for missing values
  missing_counts <- sapply(data[, feature_vars, with = FALSE], function(x) sum(is.na(x)))
  missing_proportions <- missing_counts / nrow(data)
  
  excluded_missing <- names(missing_proportions[missing_proportions > missing_threshold])
  
  if (length(excluded_missing) > 0) {
    message("Excluding ", length(excluded_missing), " features with >", 
            round(missing_threshold * 100, 1), "% missing values")
    message("  - Excluded features: ", paste(excluded_missing, collapse = ", "))
  }
  
  # 2. Check for low variance in numeric features
  numeric_features <- feature_vars[sapply(data[, feature_vars, with = FALSE], is.numeric)]
  numeric_features <- setdiff(numeric_features, excluded_missing)
  
  if (length(numeric_features) > 0) {
    variances <- sapply(data[, numeric_features, with = FALSE], var, na.rm = TRUE)
    excluded_low_variance <- names(variances[variances < variance_threshold])
    
    if (length(excluded_low_variance) > 0) {
      message("Excluding ", length(excluded_low_variance), " numeric features with variance < ", variance_threshold)
      message("  - Excluded features: ", paste(excluded_low_variance, collapse = ", "))
    }
  }
  
  # 3. Check for near-zero variance (categorical features with very few unique values)
  categorical_features <- feature_vars[!feature_vars %in% numeric_features]
  categorical_features <- setdiff(categorical_features, excluded_missing)
  
  if (length(categorical_features) > 0) {
    for (var in categorical_features) {
      unique_vals <- length(unique(data[[var]]))
      most_common_freq <- max(table(data[[var]]))
      freq_ratio <- most_common_freq / nrow(data)
      
      if (freq_ratio > near_zero_var_threshold) {
        excluded_near_zero <- c(excluded_near_zero, var)
      }
    }
    
    if (length(excluded_near_zero) > 0) {
      message("Excluding ", length(excluded_near_zero), " categorical features with near-zero variance")
      message("  - Excluded features: ", paste(excluded_near_zero, collapse = ", "))
    }
  }
  
  # 4. Check for collinearity (high correlation between features)
  remaining_features <- setdiff(numeric_features, c(excluded_low_variance, excluded_missing))
  
  if (length(remaining_features) > 1) {
    # Calculate correlation matrix for remaining numeric features
    correlation_data <- data[, remaining_features, with = FALSE]
    
    # Remove any features with too many missing values for correlation calculation
    missing_counts_corr <- sapply(correlation_data, function(x) sum(is.na(x)))
    valid_features <- names(missing_counts_corr[missing_counts_corr <= nrow(data) * 0.5])
    
    if (length(valid_features) > 1) {
      # Limit correlation calculation to prevent memory issues
      if (length(valid_features) > 100) {
        message("Large number of features (", length(valid_features), ") detected. Limiting correlation analysis to first 100 features to prevent memory issues.")
        valid_features <- valid_features[1:100]
      }
      
      tryCatch({
        correlation_matrix <- cor(correlation_data[, valid_features, with = FALSE], use = "complete.obs")
      }, error = function(e) {
        warning("Failed to calculate correlation matrix: ", e$message, ". Skipping collinearity detection.")
        correlation_matrix <- matrix(0, nrow = length(valid_features), ncol = length(valid_features))
        dimnames(correlation_matrix) <- list(valid_features, valid_features)
      })
      
      # Find highly correlated feature pairs
      high_corr_pairs <- which(abs(correlation_matrix) > correlation_threshold & correlation_matrix != 1, arr.ind = TRUE)
      
      if (nrow(high_corr_pairs) > 0) {
        # Remove duplicate pairs and self-correlations
        high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] < high_corr_pairs[, 2], , drop = FALSE]
        
        if (nrow(high_corr_pairs) > 0) {
          # For each pair, keep the feature with higher variance (more informative)
          for (i in 1:nrow(high_corr_pairs)) {
            feat1 <- valid_features[high_corr_pairs[i, 1]]
            feat2 <- valid_features[high_corr_pairs[i, 2]]
            
            var1 <- var(data[[feat1]], na.rm = TRUE)
            var2 <- var(data[[feat2]], na.rm = TRUE)
            
            # Remove the feature with lower variance
            if (var1 < var2) {
              excluded_collinear <- c(excluded_collinear, feat1)
            } else {
              excluded_collinear <- c(excluded_collinear, feat2)
            }
          }
          
          excluded_collinear <- unique(excluded_collinear)
          
          if (length(excluded_collinear) > 0) {
            message("Excluding ", length(excluded_collinear), " features due to high correlation (>", correlation_threshold, ")")
            message("  - Excluded features: ", paste(excluded_collinear, collapse = ", "))
          }
        }
      }
    }
  }
  
  # Combine all excluded features
  all_excluded <- unique(c(excluded_missing, excluded_low_variance, excluded_near_zero, excluded_collinear))
  
  # Create cleaned dataset
  if (length(all_excluded) > 0) {
    cleaned_data <- data[, !names(data) %in% all_excluded, with = FALSE]
    message("Removed ", length(all_excluded), " features total")
  } else {
    cleaned_data <- data
    message("No features excluded - all features meet quality thresholds")
  }
  
  # Summary
  original_features <- length(feature_vars)
  final_features <- length(setdiff(names(cleaned_data), c(keep_vars, outcome_vars, tracking_vars, flag_vars)))
  
  message("Feature cleaning summary:")
  message("  - Original features: ", original_features)
  message("  - Final features: ", final_features)
  message("  - Features removed: ", original_features - final_features)
  message("  - Retention rate: ", round(100 * final_features / original_features, 1), "%")
  
  # Detailed breakdown
  message("  - Removed due to missing data: ", length(excluded_missing))
  message("  - Removed due to low variance: ", length(excluded_low_variance))
  message("  - Removed due to near-zero variance: ", length(excluded_near_zero))
  message("  - Removed due to collinearity: ", length(excluded_collinear))
  
  return(list(
    cleaned_data = cleaned_data,
    excluded_features = all_excluded,
    exclusion_summary = list(
      missing = excluded_missing,
      low_variance = excluded_low_variance,
      near_zero = excluded_near_zero,
      collinear = excluded_collinear,
      original_count = original_features,
      final_count = final_features
    )
  ))
}

#' Add Roxygen comments for all new utility functions (fill_missing, safe_setnames, add_suffix, log_msg, create_output_dir, percent_rank_fallback, percent_rank_safe)
#' @param names_vec character vector of names
#' @param suffix string to append
#' @return character vector with suffix
add_suffix <- function(names_vec, suffix) {
  paste0(names_vec, suffix)
}

#' Fill missing, NaN, or Inf values in specified columns
#' @param dt data.table to modify
#' @param vars character vector of column names
#' @param value value to use for numeric/logical columns
#' @param factor_level optional: if set, use this level for factor columns; otherwise set to NA
#' @return Modifies dt in place
fill_missing <- function(dt, vars, value, factor_level = NULL) {
  for (var in vars) {
    if (var %in% names(dt)) {
      if (is.numeric(dt[[var]]) || is.logical(dt[[var]])) {
        dt[is.na(get(var)) | is.nan(get(var)) | is.infinite(get(var)), (var) := value]
      } else if (is.factor(dt[[var]])) {
        if (!is.null(factor_level) && factor_level %in% levels(dt[[var]])) {
          dt[is.na(get(var)), (var) := factor_level]
        } else {
          dt[is.na(get(var)), (var) := NA]
        }
      }
    }
  }
}

#' Safely set column names in a data.table
#' @param dt data.table to modify
#' @param old character vector of old column names
#' @param new character vector of new column names (must be same length as old)
#' @return Modifies dt in place. Only renames columns present in dt and where lengths match.
safe_setnames <- function(dt, old, new) {
  idx <- which(old %in% names(dt))
  if (length(idx) > 0 && length(idx) == length(new)) {
    setnames(dt, old[idx], new[idx])
  } else if (length(idx) > 0 && length(idx) != length(new)) {
    warning("safe_setnames: Length of old and new names do not match. No renaming performed.")
  }
}

#' Log a message or warning, optionally to a file
#' @param msg message string
#' @param log_file optional file path
#' @param warning logical, if TRUE logs as warning
#' @return None
log_msg <- function(msg, log_file = NULL, warning = FALSE) {
  if (warning) {
    warning(msg, call. = FALSE)
  } else {
    message(msg)
  }
  if (!is.null(log_file)) {
    prefix <- ifelse(warning, "WARNING: ", "")
    cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", prefix, msg, "\n"), file = log_file, append = TRUE)
  }
}

#' Create output directory if it does not exist
#' @param path file path
#' @return None
create_output_dir <- function(path) {
  output_dir <- dirname(path)
  if (output_dir != "." && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
}

#' Fallback for percent_rank if dplyr is not available
#' @param x numeric vector
#' @return numeric vector of percent ranks
percent_rank_fallback <- function(x) {
  if (length(x) <= 1) return(rep(0, length(x)))
  rx <- rank(x, ties.method = "min", na.last = "keep")
  (rx - 1) / (sum(!is.na(x)) - 1)
}

#' Use dplyr::percent_rank if available, else fallback
#' @param x numeric vector
#' @return numeric vector of percent ranks
percent_rank_safe <- function(x) {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::percent_rank(x)
  } else {
    percent_rank_fallback(x)
  }
}

#' Sort patient dataframe columns according to specified order
#' @param patients_feat data.table with patient features
#' @param id_var patient identifier variable
#' @param static_vars vector of static variables
#' @param dynamic_vars vector of dynamic variables
#' @param use_followup_weights logical, whether followup weights were used
#' @return data.table with columns sorted according to specifications
sort_patient_dataframe <- function(patients_feat, id_var, static_vars, dynamic_vars, use_followup_weights) {
  # 1. patient_id
  id_cols <- id_var
  
  # 2. outcomes (future_* variables and binary outcomes)
  outcome_cols <- grep("^future_", names(patients_feat), value = TRUE)
  binary_outcome_cols <- grep("^(hut|vut)", names(patients_feat), value = TRUE)
  outcome_cols <- c(outcome_cols, binary_outcome_cols)
  
  # 3. predictor features (static and dynamic with suffixes)
  # Find all feature columns with _d suffix (these are the engineered features)
  feature_cols <- grep("_.*_d$", names(patients_feat), value = TRUE)
  
  # Separate static and dynamic features based on original variable names
  static_feature_cols <- character(0)
  dynamic_feature_cols <- character(0)
  
  for (var in static_vars) {
    # Look for features that start with the static variable name and end with _d
    static_pattern <- paste0("^", var, "_.*_d$")
    static_feature_cols <- c(static_feature_cols, grep(static_pattern, names(patients_feat), value = TRUE))
  }
  
  for (var in dynamic_vars) {
    # Look for features that start with the dynamic variable name and end with _d
    dynamic_pattern <- paste0("^", var, "_.*_d$")
    dynamic_feature_cols <- c(dynamic_feature_cols, grep(dynamic_pattern, names(patients_feat), value = TRUE))
  }
  
  # Other engineered features (not from static or dynamic vars)
  other_feature_cols <- setdiff(feature_cols, c(static_feature_cols, dynamic_feature_cols))
  
  # Combine all predictor features in order: static, dynamic, other
  predictor_cols <- c(static_feature_cols, dynamic_feature_cols, other_feature_cols)
  
  # 4. followup_weight if applies
  weight_cols <- character(0)
  if (use_followup_weights && "prediction_weight" %in% names(patients_feat)) {
    weight_cols <- "prediction_weight"
  }
  
  # 5. everything else (tracking variables, flags, etc.)
  all_cols <- names(patients_feat)
  remaining_cols <- setdiff(all_cols, c(id_cols, outcome_cols, predictor_cols, weight_cols))
  
  # Combine in specified order
  ordered_cols <- c(id_cols, outcome_cols, predictor_cols, weight_cols, remaining_cols)
  
  # Only include columns that actually exist in the dataframe
  existing_cols <- intersect(ordered_cols, names(patients_feat))
  
  # Reorder the dataframe
  setcolorder(patients_feat, existing_cols)
  
  return(patients_feat)
}

# --- Sub-functions for engineer_utilization ---

# 1. Input Validation & Logging
validate_engineer_utilization_inputs <- function(visits_data, id_var, static_vars, dynamic_vars, observed_days, prediction_months, diagnosis_var, medications_var, log_file) {
  log_message <- function(msg) log_msg(msg, log_file)
  if (is.null(visits_data) || nrow(visits_data) == 0) {
    log_message("No data provided. Check that visits_data is not empty.")
    stop("No data provided. Check that visits_data is not empty.")
  }
  if (is.null(id_var) || length(id_var) != 1) {
    log_message("id_var must be a single character string.")
    stop("id_var must be a single character string.")
  }
  if (!id_var %in% names(visits_data)) {
    log_message(paste0("id_var '", id_var, "' not found in visits_data"))
    stop("id_var '", id_var, "' not found in visits_data")
  }
  if (length(unique(visits_data[[id_var]])) == 0) {
    log_message("No unique patients found. Check id_var specification.")
    stop("No unique patients found. Check id_var specification.")
  }
  missing_demographic <- setdiff(static_vars, names(visits_data))
  if (length(missing_demographic) > 0) {
    log_message(paste("Demographic variables not found:", paste(missing_demographic, collapse = ", ")))
    warning("Demographic variables not found: ", paste(missing_demographic, collapse = ", "))
    static_vars <- setdiff(static_vars, missing_demographic)
  }
  missing_visit <- setdiff(dynamic_vars, names(visits_data))
  if (length(missing_visit) > 0) {
    log_message(paste("Visit variables not found:", paste(missing_visit, collapse = ", ")))
    warning("Visit variables not found: ", paste(missing_visit, collapse = ", "))
    dynamic_vars <- setdiff(dynamic_vars, missing_visit)
  }
  required_cols <- c("date")
  if (!is.null(diagnosis_var)) required_cols <- c(required_cols, diagnosis_var)
  if (!is.null(medications_var)) required_cols <- c(required_cols, medications_var)
  missing_required <- setdiff(required_cols, names(visits_data))
  if (length(missing_required) > 0) {
    log_message(paste("Required columns not found:", paste(missing_required, collapse = ", ")))
    stop("Required columns not found: ", paste(missing_required, collapse = ", "))
  }
  if (!"length" %in% names(visits_data)) {
    log_message("Column 'length' is required in visits_data for hour-based calculations. Please add this column to your data.")
    stop("Column 'length' is required in visits_data for hour-based calculations. Please add this column to your data.")
  }
  if (!"status" %in% names(visits_data)) {
    log_message("Column 'status' is required in visits_data for no-show and status-based calculations. Please add this column to your data.")
    stop("Column 'status' is required in visits_data for no-show and status-based calculations. Please add this column to your data.")
  }
  if (!is.numeric(observed_days) || observed_days <= 0) {
    log_message("observed_days must be a positive number")
    stop("observed_days must be a positive number")
  }
  if (!is.numeric(prediction_months) || prediction_months <= 0) {
    log_message("prediction_months must be a positive number")
    stop("prediction_months must be a positive number")
  }
  list(static_vars = static_vars, dynamic_vars = dynamic_vars)
}

# 2. Data Quality Assessment
assess_data_quality <- function(visits_data, id_var, log_file) {
  data_quality_summary <- list(
    missing_dates = sum(is.na(visits_data$date)),
    future_dates = sum(visits_data$date > Sys.Date(), na.rm = TRUE),
    duplicate_visits = nrow(visits_data) - nrow(unique(visits_data)),
    total_patients = length(unique(visits_data[[id_var]])),
    date_range = range(visits_data$date, na.rm = TRUE)
  )
  log_msg("Data quality summary:", log_file)
  log_msg(paste0("  - Total visits: ", nrow(visits_data)), log_file)
  log_msg(paste0("  - Unique patients: ", data_quality_summary$total_patients), log_file)
  log_msg(paste0("  - Date range: ", data_quality_summary$date_range[1], " to ", data_quality_summary$date_range[2]), log_file)
  data_quality_summary
}

# 3. Apply Filtering
filter_visits_data <- function(visits_data, filter_var, filter_levels, id_var, log_file) {
  if (!is.null(filter_var) && !is.null(filter_levels)) {
    if (!filter_var %in% names(visits_data)) {
      log_msg(paste0("filter_var '", filter_var, "' not found in visits_data"), log_file)
      stop("filter_var '", filter_var, "' not found in visits_data")
    }
    visits_data <- visits_data[get(filter_var) %in% filter_levels]
    log_msg(paste("Filtered data to", nrow(visits_data), "rows"), log_file)
    if (nrow(visits_data) == 0) {
      log_msg("No data remaining after filtering. Check filter_var and filter_levels.", log_file)
      stop("No data remaining after filtering. Check filter_var and filter_levels.")
    }
    if (length(unique(visits_data[[id_var]])) == 0) {
      log_msg("No patients remaining after filtering. Check filter_var and filter_levels.", log_file)
      stop("No patients remaining after filtering. Check filter_var and filter_levels.")
    }
  }
  visits_data
}

# 4. Create Patient Index
create_patient_index_modular <- function(visits_data, id_var, observed_days, prediction_months, log_file) {
  patients_index <- visits_data[, .(index_date = min(date)), by = id_var]
  dataset_end_date <- max(visits_data$date, na.rm = TRUE)
  total_timeline_days <- observed_days + (prediction_months * 30.44)
  patients_index[, available_days := as.numeric(dataset_end_date - index_date)]
  exclusion_threshold_days <- observed_days * 2
  flagging_threshold_days <- total_timeline_days
  patients_index[, prediction_end_date := pmin(index_date + days(observed_days) + days(as.integer(prediction_months * 30.44)), dataset_end_date)]
  patients_index[, `:=`(
    feature_end_date = index_date + days(observed_days),
    actual_followup_days = as.numeric(prediction_end_date - (index_date + days(observed_days)))
  )]
  patients_index <- patients_index[available_days >= exclusion_threshold_days]
  patients_index
}

# 5. Feature Engineering (already modular: calculate_visit_patterns)
# 6. Calculate Outcomes (already modular: calculate_outcomes)
# 7. Merge Features and Outcomes (inline)
# 8. Post-processing & Suffixing (inline)
# 9. Feature Cleaning (already modular: clean_features)
# 10. Feature Analysis (already modular: calculate_feature_analysis)
# 11. Output Saving (already modular: create_output_dir, saveRDS)
# 12. Summary Statistics (inline)

# --- Refactored main function ---
engineer_utilization <- function(
  visits_data,
  id_var,
  static_vars = character(0),
  dynamic_vars = character(0),
  observed_days,
  prediction_months,
  filter_var = NULL,
  filter_levels = NULL,
  diagnosis_var = NULL,
  medications_var = NULL,
  require_complete_followup = FALSE,
  use_followup_weights = TRUE,
  output_path = NULL,
  missing_threshold = 0.1,
  variance_threshold = 0.01,
  near_zero_var_threshold = 0.95,
  correlation_threshold = 0.95,
  log_file = NULL,
  verbose = FALSE
) {
  # 1. Input Validation
  validated <- validate_engineer_utilization_inputs(visits_data, id_var, static_vars, dynamic_vars, observed_days, prediction_months, diagnosis_var, medications_var, log_file)
  static_vars <- validated$static_vars
  dynamic_vars <- validated$dynamic_vars

  # 2. Data Quality Assessment
  data_quality_summary <- assess_data_quality(visits_data, id_var, log_file)

  # 3. Convert to data.table
  tryCatch({ setDT(visits_data) }, error = function(e) { stop("Failed to convert visits_data to data.table: ", e$message) })

  # 4. Apply Filtering
  visits_data <- filter_visits_data(visits_data, filter_var, filter_levels, id_var, log_file)

  # 5. Create calendar features
  visits_data <- create_calendar_features(visits_data, medications_var)

  # 6. Create patient index
  patients_index <- create_patient_index_modular(visits_data, id_var, observed_days, prediction_months, log_file)
  if (nrow(patients_index) == 0) stop("No patients remaining after filtering for sufficient timeline data.")

  # 7. Join data for feature window
  joined <- merge(visits_data, patients_index, by = id_var, all.x = TRUE)
  joined <- joined[date <= feature_end_date]
  if (nrow(joined) == 0) stop("No data remaining after joining visits with patient timeline.")

  # 8. Feature engineering
  features <- calculate_visit_patterns(joined, id_var, static_vars, dynamic_vars, observed_days, diagnosis_var, medications_var)

  # 9. Calculate outcomes
  outcomes <- calculate_outcomes(visits_data, patients_index, id_var, prediction_months, use_followup_weights)

  # 10. Merge features and outcomes
  model_data <- merge(features, outcomes, by = id_var, all.x = TRUE)

  # 11. Post-processing & Suffixing
  outcome_vars <- c("future_visits", "future_hours", "actual_followup_days", "expected_followup_days", "followup_complete", "followup_completeness_ratio", "prediction_weight")
  fill_missing(model_data, intersect(outcome_vars, names(model_data)), 0)
  if ("followup_complete" %in% names(model_data)) model_data[is.na(followup_complete), followup_complete := FALSE]
  model_data[, single_visit_flag := n_visits == 1]
  model_data[, same_day_visits_flag := n_days_active == 1]
  binary_vars <- c("hut50", "hut75", "hut90", "hut99", "vut50", "vut75", "vut90", "vut99")
  for (var in binary_vars) {
    if (var %in% names(model_data)) {
      model_data[is.na(get(var)), (var) := factor("Low", levels = c("High", "Low"))]
    }
  }
  if (require_complete_followup) {
    model_data <- model_data[followup_complete == TRUE]
  }
  outcome_cols <- c("future_visits", "future_hours")
  missing_outcomes <- setdiff(outcome_cols, names(model_data))
  outcome_cols <- setdiff(outcome_cols, missing_outcomes)
  if (length(outcome_cols) > 0) {
    outcome_new_names <- add_suffix(outcome_cols, paste0("_", prediction_months, "m"))
    safe_setnames(model_data, outcome_cols, outcome_new_names)
  }
  binary_outcome_cols <- c("hut50", "hut75", "hut90", "hut99", "vut50", "vut75", "vut90", "vut99")
  binary_outcome_new_names <- add_suffix(binary_outcome_cols, paste0("_", prediction_months, "m"))
  safe_setnames(model_data, binary_outcome_cols, binary_outcome_new_names)
  static_vars_full <- c(id_var, static_vars)
  outcome_vars2 <- c("actual_followup_days", "expected_followup_days", "followup_complete", "followup_completeness_ratio", "prediction_weight")
  flag_vars <- c("single_visit_flag", "same_day_visits_flag")
  feature_cols <- setdiff(names(model_data), c(static_vars_full, outcome_vars2, flag_vars, outcome_new_names, binary_outcome_new_names))
  new_names <- add_suffix(feature_cols, paste0("_", observed_days, "d"))
  safe_setnames(model_data, feature_cols, new_names)

  # 12. Create visits_feat (unaggregated with calendar features)
  visits_feat <- visits_data

  # 13. Create patients_feat (aggregated features for modeling)
  patients_feat <- model_data

  # 14. Create utilization_summary (utilization summary with percentile rankings)
  utilization_summary <- create_utilization_summary(visits_data, id_var)

  # 15. Clean features to remove low-quality ones
  feature_cleaning_result <- clean_features(
    patients_feat, id_var, static_vars_full, 
    missing_threshold, variance_threshold, near_zero_var_threshold, correlation_threshold,
    exclude_vars = c(outcome_new_names, binary_outcome_new_names, outcome_vars2, flag_vars)
  )
  patients_feat <- feature_cleaning_result$cleaned_data

  # 16. Sort patient dataframe according to specifications
  patients_feat <- sort_patient_dataframe(patients_feat, id_var, static_vars, dynamic_vars, use_followup_weights)

  # 17. Calculate feature analysis after cleaning
  feature_analysis <- calculate_feature_analysis(patients_feat, verbose = verbose)

  # 18. Save if output path specified
  if (!is.null(output_path)) {
    create_output_dir(output_path)
    
    # Extract directory and base name
    output_dir <- dirname(output_path)
    if (output_dir == ".") {
      output_dir <- getwd()
    }
    base_name <- tools::file_path_sans_ext(basename(output_path))
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Create all file paths in the same directory
    visits_file <- file.path(output_dir, paste0(base_name, "_visits_", timestamp, ".rds"))
    patients_file <- file.path(output_dir, paste0(base_name, "_patients_", timestamp, ".rds"))
    util_file <- file.path(output_dir, paste0(base_name, "_utilization_summary_", timestamp, ".rds"))
    analysis_file <- file.path(output_dir, paste0(base_name, "_feature_analysis_", timestamp, ".rds"))
    
    saveRDS(visits_feat, visits_file)
    saveRDS(patients_feat, patients_file)
    saveRDS(utilization_summary, util_file)
    if (!is.null(feature_analysis)) {
      saveRDS(feature_analysis, analysis_file)
    }
    
    message("Files saved to directory: ", output_dir)
    message("  - Visits data: ", basename(visits_file))
    message("  - Patients data: ", basename(patients_file))
    message("  - Utilization summary: ", basename(util_file))
    if (!is.null(feature_analysis)) {
      message("  - Feature analysis: ", basename(analysis_file))
    }
  }

  # 19. Summary statistics (unchanged, can be modularized further if desired)
  if (nrow(patients_feat) > 0) {
    n_complete <- if ("followup_complete" %in% names(patients_feat)) sum(patients_feat$followup_complete, na.rm = TRUE) else NA
    n_truncated <- if ("followup_complete" %in% names(patients_feat)) sum(!patients_feat$followup_complete, na.rm = TRUE) else NA
    completeness_rate <- if ("followup_complete" %in% names(patients_feat)) round(100 * mean(patients_feat$followup_complete, na.rm = TRUE), 1) else NA
    avg_weight <- if ("prediction_weight" %in% names(patients_feat)) round(mean(patients_feat$prediction_weight, na.rm = TRUE), 3) else NA
    n_high_conf <- if ("prediction_weight" %in% names(patients_feat)) sum(patients_feat$prediction_weight >= 0.8, na.rm = TRUE) else NA
    n_med_conf <- if ("prediction_weight" %in% names(patients_feat)) sum(patients_feat$prediction_weight >= 0.5 & patients_feat$prediction_weight < 0.8, na.rm = TRUE) else NA
    n_low_conf <- if ("prediction_weight" %in% names(patients_feat)) sum(patients_feat$prediction_weight < 0.5, na.rm = TRUE) else NA
    message("  - Patients with complete follow-up: ", n_complete)
    message("  - Patients with truncated follow-up: ", n_truncated)
    message("  - Follow-up completeness rate: ", completeness_rate, "%")
    message("  - Average prediction weight: ", avg_weight)
    message("  - Patients with high confidence (weight >= 0.8): ", n_high_conf)
    message("  - Patients with medium confidence (0.5 <= weight < 0.8): ", n_med_conf)
    message("  - Patients with low confidence (weight < 0.5): ", n_low_conf)
  }

  return(list(
    visits_feat = visits_feat,
    patients_feat = patients_feat,
    utilization_summary = utilization_summary,
    feature_analysis = feature_analysis,
    data_quality_summary = data_quality_summary,
    feature_cleaning_summary = feature_cleaning_result$exclusion_summary
  ))
} 