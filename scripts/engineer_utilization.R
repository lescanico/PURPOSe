# Load required packages with error handling
required_packages <- c("dplyr", "lubridate", "stringr", "timeDate", "data.table")

# Function to safely load packages
load_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tryCatch({
      install.packages(pkg, repos = "https://cran.rstudio.com/")
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      message("Could not install package '", pkg, "'. This may be due to permissions or a locked-down environment. Please install manually if needed.")
      stop("Failed to install/load package '", pkg, "': ", e$message)
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
    warning("Failed to calculate US holidays: ", e$message, ". Holiday features will be set to FALSE.")
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
      month = factor(month(date)),
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
#' @param id_var patient identifier variable name
#' @param demographic_vars vector of demographic variables
#' @param visit_vars vector of visit variables
#' @param observed_days number of observation days
#' @param diagnosis_var optional diagnosis variable
#' @param medications_var optional medications variable
#' @return data.table with engineered features
calculate_visit_patterns <- function(joined_data, id_var, demographic_vars, visit_vars, observed_days, 
                                   diagnosis_var = NULL, medications_var = NULL) {
  message("Engineering visit patterns...")
  
  # First, get all unique levels for categorical variables to ensure consistent columns
  categorical_vars <- c()
  all_levels <- list()
  
  for (var in visit_vars) {
    if (!is.numeric(joined_data[[var]])) {
      categorical_vars <- c(categorical_vars, var)
      all_levels[[var]] <- unique(joined_data[[var]])
    }
  }
  
  features <- joined_data[, {
    # Demographic variables (take first value) - more efficient approach
    out <- list()
    for (var in demographic_vars) {
      out[[var]] <- .SD[[var]][1]
    }
    
    # Visit variable aggregations
    for (var in visit_vars) {
      if (is.numeric(.SD[[var]])) {
        # Numeric variables - calculate mean, sum, count
        out[[paste0(var, "_mean")]] <- mean(.SD[[var]], na.rm = TRUE)
        out[[paste0(var, "_sum")]] <- sum(.SD[[var]], na.rm = TRUE)
        out[[paste0(var, "_count")]] <- sum(!is.na(.SD[[var]]))
      } else {
        # Categorical variables - calculate counts and proportions for ALL levels
        var_table <- table(.SD[[var]])
        var_levels <- all_levels[[var]]
        
        for (level in var_levels) {
          count_val <- ifelse(level %in% names(var_table), as.integer(var_table[level]), 0L)
          out[[paste0(var, "_", level, "_count")]] <- count_val
          # Avoid division by zero
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
    if (!is.null(diagnosis_var) && diagnosis_var %in% names(.SD)) {
      out$n_unique_diagnoses <- as.integer(n_distinct(.SD[[diagnosis_var]], na.rm = TRUE))
    }
    
    # Medication features if medications variable specified
    if (!is.null(medications_var) && medications_var %in% names(.SD)) {
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
      visits_percentile = percent_rank(future_visits),
      hours_percentile = percent_rank(future_hours)
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
      hours_percentile = percent_rank(n_hours)
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

#' Calculate basic feature importance preview
#' @param patients_feat data.table with patient features
#' @return list with correlation matrix and feature importance summary
calculate_feature_importance_preview <- function(patients_feat) {
  if (nrow(patients_feat) < 10) {
    message("Insufficient data for feature importance preview (need at least 10 patients)")
    return(NULL)
  }
  
  # Identify key outcome and feature columns
  outcome_cols <- grep("future_", names(patients_feat), value = TRUE)
  feature_cols <- grep("_mean$|_sum$|_count$|n_visits|n_days_active", names(patients_feat), value = TRUE)
  
  if (length(outcome_cols) == 0 || length(feature_cols) == 0) {
    message("No outcome or feature columns found for correlation analysis")
    return(NULL)
  }
  
  # Select columns for correlation analysis
  analysis_cols <- c(outcome_cols, feature_cols)
  analysis_data <- patients_feat[, analysis_cols, with = FALSE]
  
  # Remove non-numeric columns
  numeric_cols <- sapply(analysis_data, is.numeric)
  analysis_data <- analysis_data[, names(analysis_data)[numeric_cols], with = FALSE]
  
  if (ncol(analysis_data) < 2) {
    message("Insufficient numeric columns for correlation analysis")
    return(NULL)
  }
  
  # Calculate correlation matrix
  correlation_matrix <- cor(analysis_data, use = "complete.obs")
  
  # Find top correlations with outcomes
  outcome_correlations <- list()
  for (outcome in outcome_cols) {
    if (outcome %in% names(correlation_matrix)) {
      correlations <- correlation_matrix[outcome, ]
      correlations <- correlations[!is.na(correlations) & correlations != 1]
      top_correlations <- sort(abs(correlations), decreasing = TRUE)[1:5]
      outcome_correlations[[outcome]] <- top_correlations
    }
  }
  
  return(list(
    correlation_matrix = correlation_matrix,
    outcome_correlations = outcome_correlations
  ))
}

#' Automatically exclude features with missing data or low variance
#' @param data data.table with features
#' @param id_var patient identifier variable
#' @param demographic_vars vector of demographic variables to always keep
#' @param missing_threshold maximum proportion of missing values allowed (default: 0.1)
#' @param variance_threshold minimum variance for numeric features (default: 0.01)
#' @param near_zero_var_threshold threshold for near-zero variance (default: 0.95)
#' @param correlation_threshold maximum correlation allowed between features (default: 0.95)
#' @return list with cleaned data and exclusion summary
clean_features <- function(data, id_var, demographic_vars, 
                         missing_threshold = 0.1, 
                         variance_threshold = 0.01,
                         near_zero_var_threshold = 0.95,
                         correlation_threshold = 0.95) {
  
  message("Cleaning features...")
  
  # Variables to always keep
  keep_vars <- c(id_var, demographic_vars)
  
  # Validate that required variables exist
  missing_keep_vars <- setdiff(keep_vars, names(data))
  if (length(missing_keep_vars) > 0) {
    stop("Required variables not found in data: ", paste(missing_keep_vars, collapse = ", "))
  }
  
  # Get feature variables (exclude ID, demographics, outcomes, and tracking variables)
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
  
  feature_vars <- setdiff(names(data), c(keep_vars, outcome_vars, tracking_vars, flag_vars))
  
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

#' Engineer Utilization Features for Healthcare Prediction
#'
#' Creates features for predictive modeling with configurable static and dynamic variables.
#'
#' @param visits_data Data frame of patient visits
#' @param id_var Character or factor variable identifying patients
#' @param demographic_vars Vector of demographic variables to include (e.g., age, sex, race)
#' @param visit_vars Vector of visit-level variables to aggregate (e.g., length, status, type)
#' @param observed_days Integer, number of days for the feature window (typically 30-365)
#' @param prediction_months Integer, number of months for the prediction window (typically 3-24)
#' @param filter_var Optional variable to filter data (e.g., "status")
#' @param filter_levels Optional levels to keep from filter_var (e.g., c("Completed", "No Show"))
#' @param diagnosis_var Optional diagnosis variable for clinical features
#' @param medications_var Optional medications variable for medication features
#' @param require_complete_followup Logical, whether to exclude patients with truncated outcome windows (default: FALSE)
#' @param use_followup_weights Logical, whether to calculate continuous follow-up weights (default: TRUE). 
#'   Note: When require_complete_followup = TRUE, all patients will have prediction_weight = 1.0, 
#'   making use_followup_weights = TRUE redundant. Consider setting use_followup_weights = FALSE 
#'   when require_complete_followup = TRUE for cleaner output.
#' @param output_path Optional file path to save results
#' @param missing_threshold Maximum proportion of missing values allowed for features (default: 0.1)
#' @param variance_threshold Minimum variance for numeric features (default: 0.01)
#' @param near_zero_var_threshold Threshold for near-zero variance in categorical features (default: 0.95)
#' @param correlation_threshold Maximum correlation allowed between features (default: 0.95)
#' @param log_file Optional file path to log messages and warnings (default: NULL)
#' @return List containing:
#'   - visits_feat: Enhanced visits data with calendar features (unaggregated)
#'   - patients_feat: Patient-level aggregated features for modeling (cleaned)
#'   - utilization_summary: Patient-level utilization summary with percentile rankings
#'   - feature_importance_preview: Correlation analysis and feature importance
#'   - data_quality_summary: Data quality metrics and issues
#'   - feature_cleaning_summary: Summary of feature cleaning process
#' @details
#' The returned list has the following structure:
#'   - visits_feat: data.table of all visits with engineered calendar features
#'   - patients_feat: data.table of patient-level features (cleaned)
#'   - utilization_summary: data.table of patient-level utilization summaries
#'   - feature_importance_preview: list with correlation matrix and top correlations
#'   - data_quality_summary: list of data quality metrics
#'   - feature_cleaning_summary: list of excluded features and summary stats
#' @examples
#' # Basic usage
#' features <- engineer_utilization(
#'   visits_data = visits_clean,
#'   id_var = "patient_id",
#'   demographic_vars = c("age", "sex", "race"),
#'   visit_vars = c("length", "status", "type"),
#'   observed_days = 90,
#'   prediction_months = 12
#' )
#' 
#' # Access the outputs
#' visits_feat <- features$visits_feat
#' patients_feat <- features$patients_feat
#' utilization_summary <- features$utilization_summary
#'
#' # With filtering and clinical variables
#' features <- engineer_utilization(
#'   visits_data = visits_clean,
#'   id_var = "patient_id",
#'   demographic_vars = c("age", "sex"),
#'   visit_vars = c("length", "status"),
#'   observed_days = 90,
#'   prediction_months = 12,
#'   filter_var = "status",
#'   filter_levels = c("Completed", "No Show"),
#'   diagnosis_var = "diagnosis",
#'   medications_var = "medications",
#'   output_path = "data/model_data_90d.rds"
#' )
#' 
#' # High-quality data (complete follow-up only, no weights needed)
#' features <- engineer_utilization(
#'   visits_data = visits_clean,
#'   id_var = "patient_id",
#'   demographic_vars = c("age", "sex", "race"),
#'   visit_vars = c("length", "status", "type"),
#'   observed_days = 90,
#'   prediction_months = 12,
#'   require_complete_followup = TRUE,
#'   use_followup_weights = FALSE  # No weights needed when all patients have complete follow-up
#' )
#' 
#' # Inclusive data (include incomplete follow-up with weights)
#' features <- engineer_utilization(
#'   visits_data = visits_clean,
#'   id_var = "patient_id",
#'   demographic_vars = c("age", "sex", "race"),
#'   visit_vars = c("length", "status", "type"),
#'   observed_days = 90,
#'   prediction_months = 12,
#'   require_complete_followup = FALSE,
#'   use_followup_weights = TRUE  # Weights reflect varying follow-up completeness
#' )
#' 
#' # With custom feature cleaning thresholds
#' features <- engineer_utilization(
#'   visits_data = visits_clean,
#'   id_var = "patient_id",
#'   demographic_vars = c("age", "sex", "race"),
#'   visit_vars = c("length", "status", "type"),
#'   observed_days = 90,
#'   prediction_months = 12,
#'   missing_threshold = 0.05,  # More strict missing data threshold
#'   variance_threshold = 0.005,  # More strict variance threshold
#'   near_zero_var_threshold = 0.99,  # More strict near-zero variance threshold
#'   correlation_threshold = 0.9  # More strict collinearity threshold
#' )
#' @export
engineer_utilization <- function(
  visits_data,
  id_var,
  demographic_vars,
  visit_vars,
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
  log_file = NULL
) {
  # Optionally log messages and warnings to a file
  log_message <- function(msg) {
    message(msg)
    if (!is.null(log_file)) {
      cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", msg, "\n"), file = log_file, append = TRUE)
    }
  }
  log_warning <- function(msg) {
    warning(msg, call. = FALSE)
    if (!is.null(log_file)) {
      cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - WARNING: ", msg, "\n"), file = log_file, append = TRUE)
    }
  }

  # Enhanced input validation
  if (is.null(visits_data) || nrow(visits_data) == 0) {
    stop("No data provided. Check that visits_data is not empty.")
  }
  
  if (is.null(id_var) || length(id_var) != 1) {
    stop("id_var must be a single character string.")
  }
  
  if (!id_var %in% names(visits_data)) {
    stop("id_var '", id_var, "' not found in visits_data")
  }
  
  if (length(unique(visits_data[[id_var]])) == 0) {
    stop("No unique patients found. Check id_var specification.")
  }
  
  missing_demographic <- setdiff(demographic_vars, names(visits_data))
  if (length(missing_demographic) > 0) {
    stop("Demographic variables not found: ", paste(missing_demographic, collapse = ", "))
  }
  
  missing_visit <- setdiff(visit_vars, names(visits_data))
  if (length(missing_visit) > 0) {
    stop("Visit variables not found: ", paste(missing_visit, collapse = ", "))
  }
  
  # Ensure required columns exist
  required_cols <- c("date")
  if (!is.null(diagnosis_var)) required_cols <- c(required_cols, diagnosis_var)
  if (!is.null(medications_var)) required_cols <- c(required_cols, medications_var)
  
  missing_required <- setdiff(required_cols, names(visits_data))
  if (length(missing_required) > 0) {
    stop("Required columns not found: ", paste(missing_required, collapse = ", "))
  }
  
  # Check for length column (needed for hours calculations)
  if (!"length" %in% names(visits_data)) {
    warning("Column 'length' not found. Hours calculations will be set to 0.")
    visits_data$length <- 0
  }
  
  # Check for status column (needed for no-show calculations)
  if (!"status" %in% names(visits_data)) {
    warning("Column 'status' not found. No-show calculations will be set to 0.")
    visits_data$status <- "Unknown"
  }
  
  # Enhanced parameter validation
  if (!is.numeric(observed_days) || observed_days <= 0) {
    stop("observed_days must be a positive number")
  }
  if (!is.numeric(prediction_months) || prediction_months <= 0) {
    stop("prediction_months must be a positive number")
  }
  
  # Configuration validation with reasonable bounds
  if (observed_days > 365) {
    warning("Observation period > 1 year may not be typical for healthcare utilization prediction.")
  }
  
  if (prediction_months > 24) {
    warning("Prediction period > 2 years may be beyond typical healthcare planning horizons.")
  }
  
  # Validate that observation period is not longer than prediction period
  observed_months <- observed_days / 30.44  # Convert days to months
  if (observed_months > prediction_months) {
    stop("Observation period (", round(observed_months, 1), " months) cannot be longer than prediction period (", 
         prediction_months, " months). This would mean predicting shorter-term outcomes from longer-term patterns, ",
         "which is not logical for healthcare utilization prediction.")
  }
  
  # Validate that total timeline doesn't exceed reasonable bounds
  total_timeline_months <- observed_months + prediction_months
  
  if (total_timeline_months > 60) {  # 5 years maximum
    warning("Total timeline is ", round(total_timeline_months, 1), " months (", 
            round(observed_months, 1), " months observation + ", prediction_months, " months prediction). ",
            "This may be longer than typical healthcare datasets.")
  }
  
  # Validate logical consistency of follow-up parameters
  if (require_complete_followup && use_followup_weights) {
    warning("use_followup_weights = TRUE with require_complete_followup = TRUE is redundant. ",
            "All patients will have prediction_weight = 1.0. ",
            "Consider setting use_followup_weights = FALSE for cleaner output.")
  }
  
  # Memory usage check for large datasets
  if (nrow(visits_data) > 1e6) {
    message("Large dataset detected (", nrow(visits_data), " rows). Consider processing in chunks if memory issues occur.")
  }
  
  # Data quality assessment
  data_quality_summary <- list(
    missing_dates = sum(is.na(visits_data$date)),
    future_dates = sum(visits_data$date > Sys.Date(), na.rm = TRUE),
    duplicate_visits = nrow(visits_data) - nrow(unique(visits_data)),
    total_patients = length(unique(visits_data[[id_var]])),
    date_range = range(visits_data$date, na.rm = TRUE)
  )
  
  if (data_quality_summary$missing_dates > 0) {
    warning("Found ", data_quality_summary$missing_dates, " missing dates.")
  }
  
  if (data_quality_summary$future_dates > 0) {
    warning("Found ", data_quality_summary$future_dates, " future dates.")
  }
  
  message("Data quality summary:")
  message("  - Total visits: ", nrow(visits_data))
  message("  - Unique patients: ", data_quality_summary$total_patients)
  message("  - Date range: ", data_quality_summary$date_range[1], " to ", data_quality_summary$date_range[2])
  
  # Convert to data.table for efficiency
  tryCatch({
    setDT(visits_data)
  }, error = function(e) {
    stop("Failed to convert visits_data to data.table: ", e$message)
  })
  
  # Apply filtering if specified
  if (!is.null(filter_var) && !is.null(filter_levels)) {
    if (!filter_var %in% names(visits_data)) {
      stop("filter_var '", filter_var, "' not found in visits_data")
    }
    visits_data <- visits_data[get(filter_var) %in% filter_levels]
    message("Filtered data to ", nrow(visits_data), " rows")
    
    # Check if filtering removed all data
    if (nrow(visits_data) == 0) {
      stop("No data remaining after filtering. Check filter_var and filter_levels.")
    }
    
    # Check if filtering removed all patients
    if (length(unique(visits_data[[id_var]])) == 0) {
      stop("No patients remaining after filtering. Check filter_var and filter_levels.")
    }
  }
  
  # Create calendar features
  visits_data <- create_calendar_features(visits_data, medications_var)
  
  # Create patient index
  message("Creating patient index...")
  patients_index <- visits_data[, .(
    index_date = min(date)
  ), by = id_var]
  
  # Check for patients with insufficient data for the entire timeline
  dataset_end_date <- max(visits_data$date, na.rm = TRUE)
  total_timeline_days <- observed_days + (prediction_months * 30.44)
  patients_index[, available_days := as.numeric(dataset_end_date - index_date)]
  
  # Calculate timeline filtering thresholds
  exclusion_threshold_days <- observed_days * 2  # Patients need enough time for both feature window AND outcome window
  flagging_threshold_days <- total_timeline_days  # Patients need enough time for complete timeline
  
  # Identify patients to exclude (insufficient time for both features and outcomes)
  excluded_patients <- patients_index[available_days < exclusion_threshold_days]
  if (nrow(excluded_patients) > 0) {
    message("Excluding ", nrow(excluded_patients), " patients (", 
            round(100 * nrow(excluded_patients) / nrow(patients_index), 1), 
            "%) with insufficient data for both feature window AND outcome window (< ", exclusion_threshold_days, " days needed).")
    message("  - Exclusion threshold: ", observed_days, " days × 2 = ", exclusion_threshold_days, " days")
    message("  - Minimum available days: ", min(excluded_patients$available_days))
    message("  - Maximum available days: ", max(excluded_patients$available_days))
  }
  
  # Identify patients to flag (sufficient time for features but may have truncated outcomes)
  flagged_patients <- patients_index[available_days >= exclusion_threshold_days & available_days < flagging_threshold_days]
  if (nrow(flagged_patients) > 0) {
    message("Flagging ", nrow(flagged_patients), " patients (", 
            round(100 * nrow(flagged_patients) / nrow(patients_index), 1), 
            "%) with sufficient data for features but potentially truncated outcomes.")
    message("  - Flagging range: ", exclusion_threshold_days, " to ", flagging_threshold_days, " days")
    message("  - These patients will be included with follow-up weights < 1.0")
  }
  
  # Exclude patients with insufficient timeline data
  patients_index <- patients_index[available_days >= exclusion_threshold_days]
  
  # Check if we have any patients remaining after filtering
  if (nrow(patients_index) == 0) {
    stop("No patients remaining after filtering for sufficient timeline data. ",
         "Consider reducing observed_days (currently ", observed_days, ") or prediction_months (currently ", prediction_months, ") or ",
         "increasing the dataset time range.")
  }
  
  # Calculate prediction end date first using consistent day-based calculations
  patients_index[, prediction_end_date := pmin(index_date + days(observed_days) + days(as.integer(prediction_months * 30.44)), 
                                             max(visits_data$date, na.rm = TRUE))]
  
  # Then calculate other dates
  patients_index[, `:=`(
    feature_end_date = index_date + days(observed_days),
    actual_followup_days = as.numeric(prediction_end_date - (index_date + days(observed_days)))
  )]
  
  # Check for patients with truncated follow-up (use integer days for consistency)
  truncated_patients <- patients_index[actual_followup_days < as.integer(prediction_months * 30.44)]
  if (nrow(truncated_patients) > 0) {
    warning("Found ", nrow(truncated_patients), " patients (", 
            round(100 * nrow(truncated_patients) / nrow(patients_index), 1), 
            "%) with truncated prediction windows due to end of dataset. ",
            "Their prediction windows are shorter than the requested ", prediction_months, " months.")
    
    # Debug: Check the data timeline
    message("Data timeline debug:")
    message("  - Dataset date range: ", min(visits_data$date), " to ", max(visits_data$date))
    message("  - Total dataset span: ", as.numeric(max(visits_data$date) - min(visits_data$date)), " days")
    message("  - Expected prediction period: ", as.integer(prediction_months * 30.44), " days")
    message("  - Sample truncated patient actual_followup_days: ", 
            paste(head(truncated_patients$actual_followup_days, 5), collapse = ", "))
  }
  
  # Join data for feature window
  message("Joining data for feature window...")
  joined <- merge(visits_data, patients_index, by = id_var, all.x = TRUE)
  joined <- joined[date <= feature_end_date]
  
  # Check if we have any data after joining
  if (nrow(joined) == 0) {
    stop("No data remaining after joining visits with patient timeline. ",
         "This may indicate a mismatch between visit dates and patient index dates.")
  }
  
  # Feature engineering
  features <- calculate_visit_patterns(joined, id_var, demographic_vars, visit_vars, observed_days, diagnosis_var, medications_var)
  
  # Calculate outcomes
  outcomes <- calculate_outcomes(visits_data, patients_index, id_var, prediction_months, use_followup_weights)
  
  # Merge features and outcomes
  model_data <- merge(features, outcomes, by = id_var, all.x = TRUE)
  
  # Fill missing outcomes with 0 (with validation)
  outcome_vars <- c("future_visits", "future_hours", "actual_followup_days", "expected_followup_days", 
                    "followup_complete", "followup_completeness_ratio", "prediction_weight")
  
  for (var in outcome_vars) {
    if (var %in% names(model_data)) {
      if (var %in% c("future_visits", "future_hours", "actual_followup_days", "expected_followup_days", "followup_completeness_ratio", "prediction_weight")) {
        model_data[is.na(get(var)), (var) := 0]
      } else if (var == "followup_complete") {
        model_data[is.na(get(var)), (var) := FALSE]
      }
    }
  }
  
  # Create flags for special visit patterns (for modeling insights)
  message("Creating visit pattern flags...")
  
  # Flag for single visit patients (can't calculate visit gaps)
  model_data[, single_visit_flag := n_visits == 1]
  
  # Flag for same-day visits (can't calculate timing variation)
  model_data[, same_day_visits_flag := n_days_active == 1]
  
  # Fill missing binary utilization tier variables with "Low"
  binary_vars <- c("hut50", "hut75", "hut90", "hut99", "vut50", "vut75", "vut90", "vut99")
  for (var in binary_vars) {
    if (var %in% names(model_data)) {
      model_data[is.na(get(var)), (var) := factor("Low", levels = c("High", "Low"))]
    }
  }
  
  # Optionally exclude patients with truncated follow-up
  if (require_complete_followup) {
    original_n <- nrow(model_data)
    model_data <- model_data[followup_complete == TRUE]
    excluded_n <- original_n - nrow(model_data)
    if (excluded_n > 0) {
      message("Excluded ", excluded_n, " patients (", 
              round(100 * excluded_n / original_n, 1), 
              "%) with truncated follow-up periods.")
    }
  }
  
  # Add prediction window suffix to outcome variables FIRST (before feature suffixing)
  outcome_cols <- c("future_visits", "future_hours")
  
  # Validate that outcome columns exist
  missing_outcomes <- setdiff(outcome_cols, names(model_data))
  if (length(missing_outcomes) > 0) {
    warning("Some outcome columns not found: ", paste(missing_outcomes, collapse = ", "))
    outcome_cols <- setdiff(outcome_cols, missing_outcomes)
  }
  
  if (length(outcome_cols) > 0) {
    outcome_new_names <- paste0(outcome_cols, "_", prediction_months, "m")
    setnames(model_data, outcome_cols, outcome_new_names)
  }
  
  # Add prediction window suffix to binary utilization tier variables
  binary_outcome_cols <- c("hut50", "hut75", "hut90", "hut99", "vut50", "vut75", "vut90", "vut99")
  binary_outcome_new_names <- paste0(binary_outcome_cols, "_", prediction_months, "m")
  setnames(model_data, binary_outcome_cols, binary_outcome_new_names)
  
  # Suffix feature columns with window duration (excluding ID and demographic variables)
  static_vars <- c(id_var, demographic_vars)
  outcome_vars <- c("actual_followup_days", "expected_followup_days", "followup_complete", "followup_completeness_ratio", "prediction_weight")
  
  # Only add suffix to time-windowed features (not static variables, flags, or already-suffixed outcomes)
  flag_vars <- c("single_visit_flag", "same_day_visits_flag")
  feature_cols <- setdiff(names(model_data), c(static_vars, outcome_vars, flag_vars, outcome_new_names, binary_outcome_new_names))
  new_names <- paste0(feature_cols, "_", observed_days, "d")
  setnames(model_data, feature_cols, new_names)
  
  # Create visits_feat (unaggregated with calendar features)
  visits_feat <- visits_data
  
  # Create patients_feat (aggregated features for modeling)
  patients_feat <- model_data
  
  # Create utilization_summary (utilization summary with percentile rankings)
  utilization_summary <- create_utilization_summary(visits_data, id_var)
  
  # Clean features to remove low-quality ones
  feature_cleaning_result <- clean_features(patients_feat, id_var, demographic_vars, missing_threshold, variance_threshold, near_zero_var_threshold, correlation_threshold)
  patients_feat <- feature_cleaning_result$cleaned_data
  
  # Calculate feature importance preview after cleaning
  feature_importance_preview <- calculate_feature_importance_preview(patients_feat)
  
  # Save if output path specified
  if (!is.null(output_path)) {
    # Create output directory if it doesn't exist
    output_dir <- dirname(output_path)
    if (output_dir != "." && !dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      message("Created output directory: ", output_dir)
    }
    
    # Save all datasets with robust file path handling
    base_name <- tools::file_path_sans_ext(output_path)
    visits_file <- paste0(base_name, "_visits.rds")
    patients_file <- paste0(base_name, "_patients.rds")
    util_file <- paste0(base_name, "_util.rds")
    
    message("Saving results...")
    saveRDS(visits_feat, visits_file)
    saveRDS(patients_feat, patients_file)
    saveRDS(utilization_summary, util_file)
    
    message("Saved visits_feat to: ", visits_file)
    message("Saved patients_feat to: ", patients_file)
    message("Saved utilization_summary to: ", util_file)
    
    # Save feature importance preview if available
    if (!is.null(feature_importance_preview)) {
      importance_file <- paste0(base_name, "_feature_importance.rds")
      saveRDS(feature_importance_preview, importance_file)
      message("Saved feature importance preview to: ", importance_file)
    }
  }
  
  message("Feature engineering complete.")
  message("  - visits_feat: ", nrow(visits_feat), " visit records with calendar features")
  message("  - patients_feat: ", nrow(patients_feat), " patient records with ", ncol(patients_feat), " features")
  message("  - utilization_summary: ", nrow(utilization_summary), " patient utilization summaries")
  
  # Enhanced summary statistics
  if (nrow(patients_feat) > 0) {
    message("\n=== SUMMARY STATISTICS ===")
    message("  - Patients with visits: ", nrow(patients_feat))
    n_visits_col <- paste0("n_visits_", observed_days, "d")
    if (n_visits_col %in% names(patients_feat)) {
      message("  - Average visits per patient: ", round(mean(patients_feat[[n_visits_col]], 1)))
      message("  - Median visits per patient: ", round(median(patients_feat[[n_visits_col]], 1)))
      message("  - Patients with 1 visit: ", sum(patients_feat[[n_visits_col]] == 1))
      message("  - Patients with 2+ visits: ", sum(patients_feat[[n_visits_col]] >= 2))
    } else {
      message("  - Average visits per patient: [n_visits column not found]")
    }
    
    # Future utilization summary
    future_visits_col <- paste0("future_visits_", prediction_months, "m")
    future_hours_col <- paste0("future_hours_", prediction_months, "m")
    
    if (future_visits_col %in% names(patients_feat)) {
      message("  - Patients with future visits: ", sum(patients_feat[[future_visits_col]] > 0))
      message("  - Average future visits: ", round(mean(patients_feat[[future_visits_col]], 2)))
      message("  - Median future visits: ", round(median(patients_feat[[future_visits_col]], 2)))
    }
    
    if (future_hours_col %in% names(patients_feat)) {
      message("  - Patients with future hours: ", sum(patients_feat[[future_hours_col]] > 0))
      message("  - Average future hours: ", round(mean(patients_feat[[future_hours_col]], 2)))
      message("  - Median future hours: ", round(median(patients_feat[[future_hours_col]], 2)))
    }
    
    # Follow-up completeness summary
    if ("followup_complete" %in% names(patients_feat)) {
      message("  - Patients with complete follow-up: ", sum(patients_feat$followup_complete))
      message("  - Patients with truncated follow-up: ", sum(!patients_feat$followup_complete))
      message("  - Follow-up completeness rate: ", round(100 * mean(patients_feat$followup_complete), 1), "%")
    }
    
    if ("prediction_weight" %in% names(patients_feat)) {
      message("  - Average prediction weight: ", round(mean(patients_feat$prediction_weight, na.rm = TRUE), 3))
      message("  - Patients with high confidence (weight >= 0.8): ", sum(patients_feat$prediction_weight >= 0.8, na.rm = TRUE))
      message("  - Patients with medium confidence (0.5 <= weight < 0.8): ", sum(patients_feat$prediction_weight >= 0.5 & patients_feat$prediction_weight < 0.8, na.rm = TRUE))
      message("  - Patients with low confidence (weight < 0.5): ", sum(patients_feat$prediction_weight < 0.5, na.rm = TRUE))
    }
    
    # Feature count summary
    feature_count <- ncol(patients_feat) - length(c(id_var, demographic_vars, "actual_followup_days", "expected_followup_days", "followup_complete", "followup_completeness_ratio", "prediction_weight"))
    message("  - Total engineered features: ", feature_count)
    
    # Feature importance preview
    if (!is.null(feature_importance_preview)) {
      message("\n=== FEATURE IMPORTANCE PREVIEW ===")
      for (outcome_name in names(feature_importance_preview$outcome_correlations)) {
        message("Top correlations with ", outcome_name, ":")
        correlations <- feature_importance_preview$outcome_correlations[[outcome_name]]
        for (i in seq_along(correlations)) {
          feature_name <- names(correlations)[i]
          correlation_value <- correlations[i]
          message("  ", i, ". ", feature_name, ": ", round(correlation_value, 3))
        }
        message("")
      }
    }
    
    # Data quality summary
    message("\n=== DATA QUALITY SUMMARY ===")
    message("  - Missing dates: ", data_quality_summary$missing_dates)
    message("  - Future dates: ", data_quality_summary$future_dates)
    message("  - Duplicate visits: ", data_quality_summary$duplicate_visits)
    message("  - Date range: ", data_quality_summary$date_range[1], " to ", data_quality_summary$date_range[2])
    
  } else {
    message("\nWarning: No patient features created. Check your data and parameters.")
  }
  
  return(list(
    visits_feat = visits_feat,
    patients_feat = patients_feat,
    utilization_summary = utilization_summary,
    feature_importance_preview = feature_importance_preview,
    data_quality_summary = data_quality_summary,
    feature_cleaning_summary = feature_cleaning_result$exclusion_summary
  ))
} 