

v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
x1 <- id.dc(v1)
x2 <- hd(x1, series = 2)
plot(x2)



uk_var_model <- vars::VAR(df_wide[, c("US", "Germany", "UK", "Japan")], lag.max = 10, ic = "AIC")
structural_model <- svars::id.dc(uk_var_model)  # Using distance covariances method
uk_hd_result <- svars::hd(structural_model, series = 3)  # UK is the 3rd series
uk_hd_raw <- uk_hd_result$hidec  # Extract the historical decomposition matrix


# Let's examine exactly what the hd() function produces
print("Raw HD data structure:")
print(head(uk_hd_raw, 10))
print("\nColumn names:")
print(colnames(uk_hd_raw))

# The key insight: HD shows CUMULATIVE deviations from the UNCONDITIONAL MEAN
# Let's check what the unconditional mean is for the UK series
uk_var_model <- vars::VAR(df_wide[, c("US", "Germany", "UK", "Japan")], lag.max = 10, ic = "AIC")
uk_unconditional_mean <- mean(df_wide$UK, na.rm = TRUE)

# The "Demeaned series" in HD output should match UK - unconditional_mean
uk_actual_minus_unconditional <- uk_var_period |>
  mutate(deviation_from_unconditional = UK - uk_unconditional_mean)

# Create the correct comparison
plot_correct <- ggplot() +
  geom_line(
    data = uk_actual_minus_unconditional,
    aes(x = date, y = deviation_from_unconditional),
    color = "black",
    size = 1.2
  ) +
  geom_line(
    data = data.frame(
      date = decomp_dates,
      demeaned = uk_hd_raw[,2]  # Column 2 is "Demeaned series"
    ),
    aes(x = date, y = demeaned),
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "UK HD Verification: Actual vs HD Demeaned Series",
    subtitle = "Black = UK - unconditional mean, Red = HD 'Demeaned series'",
    y = "Deviation from unconditional mean"
  ) 

print(plot_correct)

# Print the means for comparison
cat("UK sample mean (VAR period):", round(uk_sample_mean, 4), "\n")
cat("UK unconditional mean (full series):", round(uk_unconditional_mean, 4), "\n")
cat("First HD demeaned value:", round(uk_hd_raw[1,2], 4), "\n")
cat("First actual - unconditional mean:", round(uk_actual_minus_unconditional$deviation_from_unconditional[1], 4), "\n")

====



# Check what the rownames actually look like
print("First few rownames:")
print(head(rownames(hd_results_all[[1]]$hidec)))

# Instead of trying to convert rownames to dates, let's use the dates we already have
# or create a proper date sequence based on the data dimensions

# Check the number of rows in HD data
n_rows <- nrow(hd_results_all[[1]]$hidec)
cat("Number of HD rows:", n_rows, "\n")

# Create a proper date sequence that matches the HD data length
# Based on your VAR model, let's use monthly dates starting from your VAR period
var_start <- as.Date("2015-03-01")  # Adjust based on your actual VAR start date
hd_dates_corrected <- seq.Date(var_start, by = "month", length.out = n_rows)

# Calculate unconditional means for each country
unconditional_means <- sapply(df_wide[, country_names], function(x) mean(x, na.rm = TRUE))

# Create the comprehensive plots with corrected dates
hd_plots_all <- list()

for (i in 1:4) {
  # Extract raw HD data
  hd_raw <- hd_results_all[[i]]$hidec
  n_rows <- nrow(hd_raw)
  
  # Use the corrected dates
  dates_for_hd <- hd_dates_corrected[1:n_rows]
  
  # Create data frame with contributions from each shock
  hd_data <- data.frame(
    date = rep(dates_for_hd, 4),
    shock = rep(country_names, each = n_rows),
    contribution = c(hd_raw[,4], hd_raw[,5], hd_raw[,6], hd_raw[,7])  # Columns 4-7 are shock contributions
  )
  
  # Get actual series deviation from unconditional mean for the matching period
  actual_data <- df_wide[df_wide$date >= min(dates_for_hd) & df_wide$date <= max(dates_for_hd), ]
  
  # Handle case where actual_data might have different length
  if(nrow(actual_data) != n_rows) {
    # Interpolate or subset actual data to match HD data length
    actual_data <- actual_data[1:min(n_rows, nrow(actual_data)), ]
    if(nrow(actual_data) < n_rows) {
      # Extend with the last available values if needed
      last_row <- actual_data[nrow(actual_data), ]
      while(nrow(actual_data) < n_rows) {
        actual_data <- rbind(actual_data, last_row)
      }
    }
    actual_data$date <- dates_for_hd[1:nrow(actual_data)]
  }
  
  actual_deviation <- data.frame(
    date = actual_data$date,
    actual = actual_data[[country_names[i]]] - unconditional_means[i]
  )
  
  # Create the plot
  hd_plots_all[[i]] <- ggplot() +
    # Stacked area chart for shock contributions
    geom_area(
      data = hd_data,
      aes(x = date, y = contribution, fill = shock),
      position = "stack",
      alpha = 0.7
    ) +
    # Line for actual deviations from unconditional mean
    geom_line(
      data = actual_deviation,
      aes(x = date, y = actual),
      color = "black",
      size = 1.2,
      linetype = "solid"
    ) +
    # Reference line at zero (unconditional mean)
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = "gray50") +
    # Formatting
    scale_fill_manual(
      name = "Shock Origin",
      values = c("US" = "#E31A1C", "Germany" = "#1F78B4", "UK" = "#33A02C", "Japan" = "#FF7F00"),
      labels = c("US", "Germany", "UK", "Japan")
    ) +
    labs(
      title = plot_titles[i],
      subtitle = "Black line = Actual deviation from unconditional mean",
      x = "Date",
      y = "Deviation from Unconditional Mean (percentage points)",
      caption = paste("Unconditional mean:", round(unconditional_means[i], 3), "%")
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.title = element_blank()
    )
  
  # Verification: check how well the decomposition matches actual data
  total_contribution <- hd_data |>
    group_by(date) |>
    summarise(total = sum(contribution), .groups = "drop")
  
  verification <- actual_deviation |>
    left_join(total_contribution, by = "date") |>
    mutate(difference = abs(actual - total))
  
  cat(paste("\n=== Verification for", country_names[i], "===\n"))
  cat("Mean absolute difference:", round(mean(verification$difference, na.rm = TRUE), 6), "\n")
  cat("Max absolute difference:", round(max(verification$difference, na.rm = TRUE), 6), "\n")
  cat("Unconditional mean:", round(unconditional_means[i], 3), "%\n")
}

# Display all plots
for (i in 1:4) {
  print(hd_plots_all[[i]])
}


# Check what the rownames actually look like
print("First few rownames:")
print(head(rownames(hd_results_all[[1]]$hidec)))

# Instead of trying to convert rownames to dates, let's use the dates we already have
# or create a proper date sequence based on the data dimensions

# Check the number of rows in HD data
n_rows <- nrow(hd_results_all[[1]]$hidec)
cat("Number of HD rows:", n_rows, "\n")

# Create a proper date sequence that matches the HD data length
# Based on your VAR model, let's use monthly dates starting from your VAR period
var_start <- as.Date("2015-03-01")  # Adjust based on your actual VAR start date
hd_dates_corrected <- seq.Date(var_start, by = "month", length.out = n_rows)

# Calculate unconditional means for each country
unconditional_means <- sapply(df_wide[, country_names], function(x) mean(x, na.rm = TRUE))

# Create the comprehensive plots with corrected dates
hd_plots_all <- list()

for (i in 1:4) {
  # Extract raw HD data
  hd_raw <- hd_results_all[[i]]$hidec
  n_rows <- nrow(hd_raw)
  
  # Use the corrected dates
  dates_for_hd <- hd_dates_corrected[1:n_rows]
  
  # Create data frame with contributions from each shock
  hd_data <- data.frame(
    date = rep(dates_for_hd, 4),
    shock = rep(country_names, each = n_rows),
    contribution = c(hd_raw[,4], hd_raw[,5], hd_raw[,6], hd_raw[,7])  # Columns 4-7 are shock contributions
  )
  
  # Get actual series deviation from unconditional mean for the matching period
  actual_data <- df_wide[df_wide$date >= min(dates_for_hd) & df_wide$date <= max(dates_for_hd), ]
  
  # Handle case where actual_data might have different length
  if(nrow(actual_data) != n_rows) {
    # Interpolate or subset actual data to match HD data length
    actual_data <- actual_data[1:min(n_rows, nrow(actual_data)), ]
    if(nrow(actual_data) < n_rows) {
      # Extend with the last available values if needed
      last_row <- actual_data[nrow(actual_data), ]
      while(nrow(actual_data) < n_rows) {
        actual_data <- rbind(actual_data, last_row)
      }
    }
    actual_data$date <- dates_for_hd[1:nrow(actual_data)]
  }
  
  actual_deviation <- data.frame(
    date = actual_data$date,
    actual = actual_data[[country_names[i]]] - unconditional_means[i]
  )
  
  # Create the plot
  hd_plots_all[[i]] <- ggplot() +
    # Stacked area chart for shock contributions
    geom_area(
      data = hd_data,
      aes(x = date, y = contribution, fill = shock),
      position = "stack",
      alpha = 0.7
    ) +
    # Line for actual deviations from unconditional mean
    geom_line(
      data = actual_deviation,
      aes(x = date, y = actual),
      color = "black",
      size = 1.2,
      linetype = "solid"
    ) +
    # Reference line at zero (unconditional mean)
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = "gray50") +
    # Formatting
    scale_fill_manual(
      name = "Shock Origin",
      values = c("US" = "#E31A1C", "Germany" = "#1F78B4", "UK" = "#33A02C", "Japan" = "#FF7F00"),
      labels = c("US", "Germany", "UK", "Japan")
    ) +
    labs(
      title = plot_titles[i],
      subtitle = "Black line = Actual deviation from unconditional mean",
      x = "Date",
      y = "Deviation from Unconditional Mean (percentage points)",
      caption = paste("Unconditional mean:", round(unconditional_means[i], 3), "%")
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.title = element_blank()
    )
  
  # Verification: check how well the decomposition matches actual data
  total_contribution <- hd_data |>
    group_by(date) |>
    summarise(total = sum(contribution), .groups = "drop")
  
  verification <- actual_deviation |>
    left_join(total_contribution, by = "date") |>
    mutate(difference = abs(actual - total))
  
  cat(paste("\n=== Verification for", country_names[i], "===\n"))
  cat("Mean absolute difference:", round(mean(verification$difference, na.rm = TRUE), 6), "\n")
  cat("Max absolute difference:", round(max(verification$difference, na.rm = TRUE), 6), "\n")
  cat("Unconditional mean:", round(unconditional_means[i], 3), "%\n")
}

# Display all plots
for (i in 1:4) {
  print(hd_plots_all[[i]])
}



# Complete Historical Decomposition Analysis for 4-Country Bond Market VAR
library(vars)
library(svars) 
library(tidyverse)
library(ggplot2)





# Step 1: Prepare the data and estimate VAR model
country_names <- c("US", "Germany", "UK", "Japan")

# Remove any rows with missing values - FIXED
clean_data <- df_wide |>
  filter(complete.cases(df_wide)) |>  # Use df_wide directly instead of .
  arrange(date)

# Step 2: Estimate VAR model
cat("Estimating VAR model...\n")
var_data <- clean_data[, country_names]
var_model <- vars::VAR(var_data, lag.max = 10, ic = "AIC")
cat("VAR lag order selected:", var_model$p, "\n")

# Step 3: Apply structural identification
cat("Applying structural identification...\n")
structural_model <- svars::id.dc(var_model)

# Step 4: Generate historical decomposition for each country
cat("Generating historical decompositions...\n")
hd_results_all <- list()
for (i in 1:4) {
  cat("Processing", country_names[i], "...\n")
  hd_results_all[[i]] <- svars::hd(structural_model, series = i)
}

# Step 5: Calculate unconditional means
unconditional_means <- sapply(clean_data[, country_names], function(x) mean(x, na.rm = TRUE))
names(unconditional_means) <- country_names

# Step 6: Create dates for historical decomposition period
n_obs <- nrow(hd_results_all[[1]]$hidec)
hd_dates <- seq.Date(as.Date("2015-03-01"), by = "month", length.out = n_obs)

# Step 7: Plot setup
plot_titles <- c(
  "US 10-Year Treasury Yields: Historical Decomposition",
  "German 10-Year Bund Yields: Historical Decomposition", 
  "UK 10-Year Gilt Yields: Historical Decomposition",
  "Japanese 10-Year Bond Yields: Historical Decomposition"
)

# Step 8: Create comprehensive plots for all countries
hd_plots_all <- list()

for (i in 1:4) {
  cat("Creating plot for", country_names[i], "...\n")
  
  # Extract HD data
  hd_raw <- hd_results_all[[i]]$hidec
  n_rows <- nrow(hd_raw)
  dates_for_hd <- hd_dates[1:n_rows]
  
  # Create shock contributions data
  hd_contributions <- data.frame(
    date = rep(dates_for_hd, 4),
    shock_origin = rep(country_names, each = n_rows),
    contribution = c(hd_raw[,4], hd_raw[,5], hd_raw[,6], hd_raw[,7])
  )
  
  # HD constructed series (what the decomposition reconstructs)
  hd_constructed <- data.frame(
    date = dates_for_hd,
    constructed = hd_raw[,3]  # Column 3: "Constructed series"
  )
  
  # Get actual yields for the same period (deviations from unconditional mean)
  actual_yields <- clean_data |>
    filter(date >= min(dates_for_hd), date <= max(dates_for_hd)) |>
    slice_head(n = n_rows) |>
    mutate(
      date = dates_for_hd,
      actual_deviation = .data[[country_names[i]]] - unconditional_means[i]
    ) |>
    dplyr::select(date, actual_deviation)
  
  # Create the comprehensive plot
  hd_plots_all[[i]] <- ggplot() +
    # Stacked areas showing shock contributions
    geom_area(
      data = hd_contributions,
      aes(x = date, y = contribution, fill = shock_origin),
      position = "stack",
      alpha = 0.7
    ) +
    # Black line: HD reconstructed series
    geom_line(
      data = hd_constructed,
      aes(x = date, y = constructed),
      color = "black",
      size = 1.2,
      linetype = "solid"
    ) +
    # Red dashed line: Actual yield deviations
    geom_line(
      data = actual_yields,
      aes(x = date, y = actual_deviation),
      color = "red",
      size = 1,
      linetype = "dashed",
      alpha = 0.8
    ) +
    # Reference line at zero (unconditional mean)
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.6, color = "gray50") +
    # Formatting
    scale_fill_manual(
      name = "Shock Origin",
      values = c("US" = "#E31A1C", "Germany" = "#1F78B4", 
                "UK" = "#33A02C", "Japan" = "#FF7F00"),
      labels = country_names
    ) +
    labs(
      title = plot_titles[i],
      subtitle = "Black = HD reconstruction, Red = Actual deviations, Areas = Shock contributions",
      x = "Date",
      y = "Deviation from Unconditional Mean (percentage points)",
      caption = paste0("Unconditional mean: ", round(unconditional_means[i], 3), "%")
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Print verification statistics
  verification <- hd_constructed |>
    left_join(actual_yields, by = "date") |>
    mutate(difference = abs(constructed - actual_deviation))
  
  cat("\n=== Verification for", country_names[i], "===\n")
  cat("Mean absolute difference (HD vs Actual):", 
      round(mean(verification$difference, na.rm = TRUE), 6), "\n")
  cat("Correlation (HD vs Actual):", 
      round(cor(verification$constructed, verification$actual_deviation, use = "complete.obs"), 4), "\n")
  cat("Unconditional mean:", round(unconditional_means[i], 3), "%\n")
}

# Step 9: Display all plots
for (i in 1:4) {
  print(hd_plots_all[[i]])
}

# Step 10: Create summary statistics
cat("\n=== Model Summary ===\n")
cat("VAR lag order:", var_model$p, "\n")
cat("Sample period:", min(hd_dates), "to", max(hd_dates), "\n")
cat("Number of observations:", n_obs, "\n")
cat("Countries analyzed:", paste(country_names, collapse = ", "), "\n")




# Check Stationarity


# Step 1: Test for stationarity and determine proper transformation
library(urca)

# Test each series for unit roots
stationarity_tests <- list()
for (country in country_names) {
  # ADF test
  adf_test <- ur.df(clean_data[[country]], type = "trend", lags = 4)
  stationarity_tests[[paste0(country, "_adf")]] <- adf_test
  
  cat("\n=== Unit Root Test for", country, "===\n")
  cat("ADF test statistic:", adf_test@teststat[1], "\n")
  cat("Critical values:", adf_test@cval[1,], "\n")
  cat("Likely non-stationary:", abs(adf_test@teststat[1]) < abs(adf_test@cval[1,2]), "\n")
}

# Step 2: Use first differences for VAR if series are non-stationary
# Create differenced data
clean_data_diff <- clean_data |>
  arrange(date) |>
  mutate(
    across(all_of(country_names), ~ c(NA, diff(.x)), .names = "d_{.col}")
  ) |>
  dplyr::select(date, starts_with("d_")) |>
  na.omit()

# Rename columns to remove d_ prefix for consistency
names(clean_data_diff)[-1] <- country_names

# Step 3: Re-estimate VAR on differenced data
cat("Re-estimating VAR on first differences...\n")
var_data_diff <- clean_data_diff[, country_names]
var_model_diff <- vars::VAR(var_data_diff, lag.max = 10, ic = "AIC")
cat("VAR lag order (differences):", var_model_diff$p, "\n")

# Step 4: Apply structural identification to differenced VAR
structural_model_diff <- svars::id.dc(var_model_diff)

# Step 5: Generate HD for differenced model
hd_results_diff <- list()
for (i in 1:4) {
  cat("Processing", country_names[i], "differences...\n")
  hd_results_diff[[i]] <- svars::hd(structural_model_diff, series = i)
}

# Step 6: Create plots showing change decomposition
n_obs_diff <- nrow(hd_results_diff[[1]]$hidec)
hd_dates_diff <- clean_data_diff$date[1:n_obs_diff]

# Calculate unconditional means for differences (should be close to zero)
unconditional_means_diff <- sapply(clean_data_diff[, country_names], function(x) mean(x, na.rm = TRUE))

hd_plots_changes <- list()

for (i in 1:4) {
  cat("Creating change decomposition plot for", country_names[i], "...\n")
  
  # Extract HD data for changes
  hd_raw_diff <- hd_results_diff[[i]]$hidec
  n_rows_diff <- nrow(hd_raw_diff)
  dates_for_hd_diff <- hd_dates_diff[1:n_rows_diff]
  
  # Shock contributions to changes
  hd_contributions_diff <- data.frame(
    date = rep(dates_for_hd_diff, 4),
    shock_origin = rep(country_names, each = n_rows_diff),
    contribution = c(hd_raw_diff[,4], hd_raw_diff[,5], hd_raw_diff[,6], hd_raw_diff[,7])
  )
  
  # HD constructed series for changes
  hd_constructed_diff <- data.frame(
    date = dates_for_hd_diff,
    constructed = hd_raw_diff[,3]  # Column 3: "Constructed series"
  )
  
  # Actual yield changes (first differences)
  actual_changes <- clean_data_diff |>
    filter(date >= min(dates_for_hd_diff), date <= max(dates_for_hd_diff)) |>
    slice_head(n = n_rows_diff) |>
    mutate(
      date = dates_for_hd_diff,
      actual_change = .data[[country_names[i]]]
    ) |>
    dplyr::select(date, actual_change)
  
  # Create the change decomposition plot
  hd_plots_changes[[i]] <- ggplot() +
    # Stacked areas showing shock contributions to changes
    geom_area(
      data = hd_contributions_diff,
      aes(x = date, y = contribution, fill = shock_origin),
      position = "stack",
      alpha = 0.7
    ) +
    # Black line: HD reconstructed changes
    geom_line(
      data = hd_constructed_diff,
      aes(x = date, y = constructed),
      color = "black",
      size = 1.2,
      linetype = "solid"
    ) +
    # Red line: Actual yield changes
    geom_line(
      data = actual_changes,
      aes(x = date, y = actual_change),
      color = "red",
      size = 1,
      linetype = "dashed",
      alpha = 0.8
    ) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.6, color = "gray50") +
    scale_fill_manual(
      name = "Shock Origin",
      values = c("US" = "#E31A1C", "Germany" = "#1F78B4", 
                "UK" = "#33A02C", "Japan" = "#FF7F00"),
      labels = country_names
    ) +
    labs(
      title = paste(country_names[i], "Bond Yield Changes: Historical Decomposition"),
      subtitle = "Black = HD reconstruction, Red = Actual monthly changes, Areas = Shock contributions",
      x = "Date",
      y = "Monthly Change in Yield (percentage points)",
      caption = "Decomposition of first differences (stationary transformation)"
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.title = element_blank()
    )
  
  # Verification
  verification_diff <- hd_constructed_diff |>
    left_join(actual_changes, by = "date") |>
    mutate(difference = abs(constructed - actual_change))
  
  cat("\n=== Verification for", country_names[i], "Changes ===\n")
  cat("Mean absolute difference:", round(mean(verification_diff$difference, na.rm = TRUE), 6), "\n")
  cat("Correlation:", round(cor(verification_diff$constructed, verification_diff$actual_change, use = "complete.obs"), 4), "\n")
}

# Display change decomposition plots
for (i in 1:4) {
  print(hd_plots_changes[[i]])
}

cat("\n=== Model Comparison ===\n")
cat("Original VAR (levels) - Lag order:", var_model$p, "\n")
cat("Differenced VAR - Lag order:", var_model_diff$p, "\n")
cat("Historical decomposition now shows contributions to CHANGES in bond yields\n")
cat("This should eliminate the gap since HD works properly on stationary data\n")


# Step 7: Add cumulative analysis showing cumulative contributions
hd_plots_cumulative <- list()

for (i in 1:4) {
  cat("Creating cumulative plot for", country_names[i], "...\n")
  
  # Extract HD data for changes
  hd_raw_diff <- hd_results_diff[[i]]$hidec
  n_rows_diff <- nrow(hd_raw_diff)
  dates_for_hd_diff <- hd_dates_diff[1:n_rows_diff]
  
  # Shock contributions to changes (period-by-period)
  hd_contributions_changes <- data.frame(
    date = rep(dates_for_hd_diff, 4),
    shock_origin = rep(country_names, each = n_rows_diff),
    period_contribution = c(hd_raw_diff[,4], hd_raw_diff[,5], hd_raw_diff[,6], hd_raw_diff[,7])
  )
  
  # Calculate cumulative contributions from each shock
  hd_contributions_cumulative <- hd_contributions_changes |>
    group_by(shock_origin) |>
    arrange(date) |>
    mutate(cumulative_contribution = cumsum(period_contribution)) |>
    ungroup()
  
  # HD constructed series for changes (period-by-period)
  hd_constructed_changes <- data.frame(
    date = dates_for_hd_diff,
    period_change = hd_raw_diff[,3]  # Column 3: "Constructed series" (changes)
  )
  
  # Calculate cumulative HD reconstruction
  hd_constructed_cumulative <- hd_constructed_changes |>
    arrange(date) |>
    mutate(cumulative_constructed = cumsum(period_change))
  
  # Actual yield changes (first differences)
  actual_changes <- clean_data_diff |>
    filter(date >= min(dates_for_hd_diff), date <= max(dates_for_hd_diff)) |>
    slice_head(n = n_rows_diff) |>
    mutate(
      date = dates_for_hd_diff,
      period_change = .data[[country_names[i]]]
    ) |>
    dplyr::select(date, period_change)
  
  # Calculate cumulative actual changes
  actual_changes_cumulative <- actual_changes |>
    arrange(date) |>
    mutate(cumulative_actual = cumsum(period_change))
  
  # Create the cumulative plot
  hd_plots_cumulative[[i]] <- ggplot() +
    # Stacked areas showing cumulative shock contributions
    geom_area(
      data = hd_contributions_cumulative,
      aes(x = date, y = cumulative_contribution, fill = shock_origin),
      position = "stack",
      alpha = 0.7
    ) +
    # Black line: Cumulative HD reconstruction
    geom_line(
      data = hd_constructed_cumulative,
      aes(x = date, y = cumulative_constructed),
      color = "black",
      size = 1.2,
      linetype = "solid"
    ) +
    # Red line: Cumulative actual yield changes
    geom_line(
      data = actual_changes_cumulative,
      aes(x = date, y = cumulative_actual),
      color = "red",
      size = 1,
      linetype = "dashed",
      alpha = 0.8
    ) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.6, color = "gray50") +
    scale_fill_manual(
      name = "Shock Origin",
      values = c("US" = "#E31A1C", "Germany" = "#1F78B4", 
                "UK" = "#33A02C", "Japan" = "#FF7F00"),
      labels = country_names
    ) +
    labs(
      title = paste(country_names[i], "Bond Yields: Cumulative Change Decomposition"),
      subtitle = "Black = HD cumulative reconstruction, Red = Actual cumulative changes, Areas = Cumulative shock contributions",
      x = "Date",
      y = "Cumulative Change from Initial Level (percentage points)",
      caption = "Shows how period-by-period shock contributions accumulate over time"
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.title = element_blank()
    )
  
  # Verification for cumulative analysis
  verification_cumulative <- hd_constructed_cumulative |>
    left_join(actual_changes_cumulative, by = "date") |>
    mutate(
      cumulative_difference = abs(cumulative_constructed - cumulative_actual)
    )
  
  cat("\n=== Cumulative Verification for", country_names[i], "===\n")
  cat("Mean absolute difference (cumulative):", 
      round(mean(verification_cumulative$cumulative_difference, na.rm = TRUE), 6), "\n")
  cat("Correlation (cumulative):", 
      round(cor(verification_cumulative$cumulative_constructed, 
                verification_cumulative$cumulative_actual, use = "complete.obs"), 4), "\n")
  cat("Final cumulative change - HD:", 
      round(tail(verification_cumulative$cumulative_constructed, 1), 3), "pp\n")
  cat("Final cumulative change - Actual:", 
      round(tail(verification_cumulative$cumulative_actual, 1), 3), "pp\n")
}

# Display both sets of plots
cat("\n=== Displaying Period-by-Period Change Plots ===\n")
for (i in 1:4) {
  print(hd_plots_changes[[i]])
}

cat("\n=== Displaying Cumulative Change Plots ===\n")
for (i in 1:4) {
  print(hd_plots_cumulative[[i]])
}

# Summary comparison
cat("\n=== Final Summary ===\n")
cat("Period-by-Period Analysis: Shows monthly contributions from each market's shocks\n")
cat("Cumulative Analysis: Shows how these contributions accumulate to explain total yield changes\n")
cat("Both analyses use stationary (first-differenced) data for proper HD decomposition\n")






# The key fix: Align cumulative changes to actual starting levels
for (i in 1:4) {
  cat("Creating properly aligned cumulative plot for", country_names[i], "...\n")
  
  # Get historical decomposition for changes (as before)
  hd_raw_diff <- hd_results_diff[[i]]$hidec
  n_rows_diff <- nrow(hd_raw_diff)
  dates_for_hd_diff <- hd_dates_diff[1:n_rows_diff]
  
  # Shock contributions to changes (period-by-period)
  hd_contributions_changes <- data.frame(
    date = rep(dates_for_hd_diff, 4),
    shock_origin = rep(country_names, each = n_rows_diff),
    period_contribution = c(hd_raw_diff[,4], hd_raw_diff[,5], hd_raw_diff[,6], hd_raw_diff[,7])
  )
  
  # Get the ACTUAL starting level from original data
  start_date <- min(dates_for_hd_diff)
  initial_level <- clean_data |>
    filter(date <= start_date) |>
    tail(1) |>
    pull(!!country_names[i])
  
  cat("Initial level for", country_names[i], ":", initial_level, "\n")
  
  # Calculate cumulative contributions from each shock (starting from zero)
  hd_contributions_cumulative <- hd_contributions_changes |>
    group_by(shock_origin) |>
    arrange(date) |>
    mutate(cumulative_contribution = cumsum(period_contribution)) |>
    ungroup()
  
  # HD period changes and cumulative reconstruction
  hd_constructed_changes <- data.frame(
    date = dates_for_hd_diff,
    period_change = hd_raw_diff[,3]
  )
  
  # CORRECTED: Cumulate HD changes and add to initial level
  hd_constructed_cumulative <- hd_constructed_changes |>
    arrange(date) |>
    mutate(
      cumulative_change = cumsum(period_change),
      cumulative_level = initial_level + cumulative_change  # Add to initial level
    )
  
  # Get actual data for the same period
  actual_data_period <- clean_data |>
    filter(date >= min(dates_for_hd_diff), date <= max(dates_for_hd_diff)) |>
    slice_head(n = n_rows_diff) |>
    mutate(date = dates_for_hd_diff) |>
    dplyr::select(date, actual_level = !!country_names[i])
  
  # Create the LEVEL comparison plot (not changes)
  hd_plot_levels <- ggplot() +
    # Stacked areas showing cumulative shock contributions (relative to initial level)
    geom_area(
      data = hd_contributions_cumulative |>
        mutate(cumulative_level_contribution = initial_level + cumulative_contribution),
      aes(x = date, y = cumulative_level_contribution - initial_level, fill = shock_origin),
      position = "stack",
      alpha = 0.7
    ) +
    # Black line: HD cumulative reconstruction (levels)
    geom_line(
      data = hd_constructed_cumulative,
      aes(x = date, y = cumulative_level),
      color = "black",
      size = 1.2,
      linetype = "solid"
    ) +
    # Red line: Actual bond yield levels
    geom_line(
      data = actual_data_period,
      aes(x = date, y = actual_level),
      color = "red",
      size = 1,
      linetype = "dashed",
      alpha = 0.8
    ) +
    # Reference line at initial level
    geom_hline(yintercept = initial_level, linetype = "dotted", alpha = 0.6, color = "gray50") +
    scale_fill_manual(
      name = "Shock Origin",
      values = c("US" = "#E31A1C", "Germany" = "#1F78B4", 
                "UK" = "#33A02C", "Japan" = "#FF7F00"),
      labels = country_names
    ) +
    labs(
      title = paste(country_names[i], "Bond Yields: Level Decomposition (Corrected)"),
      subtitle = "Black = HD level reconstruction, Red = Actual levels, Areas = Cumulative shock contributions",
      x = "Date",
      y = "Bond Yield Level (percentage)",
      caption = paste("Starting level:", round(initial_level, 3), "%")
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.title = element_blank()
    )
  
  print(hd_plot_levels)
  
  # Verification using LEVELS
  verification_levels <- hd_constructed_cumulative |>
    left_join(actual_data_period, by = "date") |>
    mutate(level_difference = abs(cumulative_level - actual_level))
  
  cat("\n=== CORRECTED Level Verification for", country_names[i], "===\n")
  cat("Mean absolute difference (levels):", 
      round(mean(verification_levels$level_difference, na.rm = TRUE), 6), "\n")
  cat("Correlation (levels):", 
      round(cor(verification_levels$cumulative_level, 
                verification_levels$actual_level, use = "complete.obs"), 4), "\n")
  cat("Final HD level:", round(tail(verification_levels$cumulative_level, 1), 3), "%\n")
  cat("Final actual level:", round(tail(verification_levels$actual_level, 1), 3), "%\n")
}

# Final Section here

# Complete demonstration showing how HD contributions add up to the demeaned series
# and how this relates to actual UK bond yields

# Using your existing uk_hd_raw data
print("uk_hd_raw structure:")
print(colnames(uk_hd_raw))
print(paste("Rows:", nrow(uk_hd_raw)))

# Extract the key components from uk_hd_raw
n_periods <- nrow(uk_hd_raw)
hd_dates <- seq.Date(as.Date("2015-03-01"), by = "month", length.out = n_periods)

# Create comprehensive data frame with all HD components
hd_comprehensive <- data.frame(
  date = hd_dates,
  # Column 2: Demeaned series (UK yields - unconditional mean)
  demeaned_series = uk_hd_raw[,2],
  # Column 3: Constructed series (sum of all shock contributions)
  constructed_series = uk_hd_raw[,3],
  # Individual shock contributions (cumulative effects)
  us_shock = uk_hd_raw[,4],
  germany_shock = uk_hd_raw[,5],
  uk_shock = uk_hd_raw[,6],
  japan_shock = uk_hd_raw[,7]
)

# Verify that constructed series equals sum of individual shocks
hd_comprehensive$shock_sum_check <- with(hd_comprehensive, 
  us_shock + germany_shock + uk_shock + japan_shock)

# Get actual UK bond yields for the same period
actual_uk_yields <- clean_data |>
  filter(date >= min(hd_dates), date <= max(hd_dates)) |>
  slice_head(n = n_periods) |>
  mutate(date = hd_dates) |>
  dplyr::select(date, uk_actual = UK)

# Calculate the unconditional mean and add actual yield deviations
uk_unconditional_mean <- mean(clean_data$UK, na.rm = TRUE)

# Combine all data
final_data <- hd_comprehensive |>
  left_join(actual_uk_yields, by = "date") |>
  mutate(
    # Calculate actual deviations from unconditional mean
    actual_deviation = uk_actual - uk_unconditional_mean,
    # Calculate actual UK yield levels from HD demeaned + unconditional mean
    reconstructed_uk_level = demeaned_series + uk_unconditional_mean
  )

# Create shock contributions in long format for stacking
shock_contributions <- data.frame(
  date = rep(hd_dates, 4),
  shock_origin = rep(c("US", "Germany", "UK", "Japan"), each = n_periods),
  cumulative_contribution = c(hd_comprehensive$us_shock,
                             hd_comprehensive$germany_shock, 
                             hd_comprehensive$uk_shock,
                             hd_comprehensive$japan_shock)
)

# Plot 1: HD Decomposition showing how shocks add up to demeaned series
plot_hd_decomposition <- ggplot() +
  # Stacked areas showing cumulative shock contributions
  geom_area(
    data = shock_contributions,
    aes(x = date, y = cumulative_contribution, fill = shock_origin),
    position = "stack",
    alpha = 0.7
  ) +
  # Line 1: HD Demeaned Series (what we're trying to explain)
  geom_line(
    data = final_data,
    aes(x = date, y = demeaned_series),
    color = "black",
    size = 1.5,
    linetype = "solid"
  ) +
  # Line 2: HD Constructed Series (verification line - should match demeaned)
  geom_line(
    data = final_data,
    aes(x = date, y = constructed_series),
    color = "red",
    size = 1,
    linetype = "dashed",
    alpha = 0.8
  ) +
  # Reference line at zero
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.6, color = "gray50") +
  scale_fill_manual(
    name = "Shock Origin",
    values = c("US" = "#E31A1C", "Germany" = "#1F78B4", 
              "UK" = "#33A02C", "Japan" = "#FF7F00")
  ) +
  labs(
    title = "UK Bond Yields: Historical Decomposition of Deviations from Unconditional Mean",
    subtitle = "Black = HD Demeaned Series, Red = HD Constructed (verification), Areas = Cumulative shock contributions",
    x = "Date",
    y = "Deviation from Unconditional Mean (percentage points)",
    caption = paste0("Unconditional mean: ", round(uk_unconditional_mean, 3), "%")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# Plot 2: Relationship to actual UK bond yield levels
plot_yield_levels <- ggplot() +
  # Line 1: Actual UK bond yield levels
  geom_line(
    data = final_data,
    aes(x = date, y = uk_actual),
    color = "blue",
    size = 1.5,
    linetype = "solid"
  ) +
  # Line 2: Reconstructed UK levels (HD demeaned + unconditional mean)
  geom_line(
    data = final_data,
    aes(x = date, y = reconstructed_uk_level),
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  # Reference line at unconditional mean
  geom_hline(yintercept = uk_unconditional_mean, 
             linetype = "dotted", alpha = 0.6, color = "gray50") +
  labs(
    title = "UK Bond Yield Levels: Actual vs HD Reconstruction",
    subtitle = "Blue = Actual UK yields, Red = HD reconstruction (demeaned + unconditional mean)",
    x = "Date",
    y = "Bond Yield Level (%)",
    caption = paste0("Unconditional mean: ", round(uk_unconditional_mean, 3), "%")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# Show first few rows of verification data - FIXED
cat("\n=== First 10 rows of verification data ===\n")
verification_display <- final_data[1:10, c("date", "demeaned_series", "constructed_series", 
                                          "shock_sum_check", "uk_actual", "actual_deviation")]

# Round only the numeric columns, keep date as-is
verification_display_rounded <- verification_display |>
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )

print(verification_display_rounded)