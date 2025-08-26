# 04_Rigobon.R

# Estimate VAR for Asset Prices (ie, 10y rates)
# Construct Historical Decomposition of Shocks

# Packages----
lapply(
  c(
    'tidyverse',
    'fredr',
    'lubridate',
    'xts',
    'vars',
    'svars',
    'patchwork',
    'here'
  ),
  require,
  character.only = TRUE
)

# SETTINGS
startDate <- as.Date("2015-01-01", format = "%Y-%m-%d")
fredr_set_key("14447b2e57e05e5bde5dfc65dd0f5fd3")

# FRED series codes for 10-year government bond yields:
bond_series <- c(
  "US" = "GS10", # US 10-Year Treasury
  "Germany" = "IRLTLT01DEM156N", # German 10-Year Bund
  "UK" = "IRLTLT01GBM156N", # UK 10-Year Gilt
  "Japan" = "IRLTLT01JPM156N" # Japan 10-Year Bond
)

# Function to get bond data
get_bond_yields <- function() {
  map_dfr(
    names(bond_series),
    ~ {
      fredr(
        bond_series[.x],
        frequency = "m", # Monthly frequency [highest for these codes]
        observation_start = as.Date(startDate)
      ) %>%
        mutate(country = .x) %>%
        dplyr::select(date, country, yield = value)
    }
  )
}
# Get all bond yields
bond_yields <- get_bond_yields()

# Inspect data
bond_yields %>%
  group_by(country) %>%
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    latest_yield = last(yield),
    .groups = "drop"
  )

# Plot
ggplot(bond_yields, aes(x = date, y = yield, color = country)) +
  geom_line() +
  geom_point() +
  scale_color_jco() + # Add JCO color palette
  labs(
    title = "Government Bond Yields (10-year)",
    #    subtitle = "Monthly data from FRED",
    x = "Date",
    y = "Yield (%)",
    color = NULL
  )

df <- bond_yields |> dplyr::select(-contains('date'))
df_wide <- bond_yields |>
  pivot_wider(id_cols = date, names_from = country, values_from = yield) |>
  na.omit()

# wide and clean [dat]
dat <- df_wide |>
  dplyr::select(-date) |>
  na.omit()

# Estimate VAR, HistDecomps
v1 <- vars::VAR(dat, lag.max = 4, type = "both", ic = "AIC")
summary(v1)

# sbDate
sbDate <- lubridate::ymd("2020-03-01")
nSB <- nrow(df_wide[df_wide$date <= lubridate::ymd(sbDate), ])
EA.cv <- id.cv(v1, SB = nSB) # changes in volatility;
summary(EA.cv)

# Put HistDecomp into x1-x4 (Lists)
k <- ncol(dat)
for (i in 1:k) {
  assign(paste0("x", i, sep = ""), hd(EA.cv, series = i))
}

# Create DFs with HistDecomps, Clean
hdlist <- list(x1[['hidec']], x2[['hidec']], x3[['hidec']], x4[['hidec']])
for (i in 1:length(hdlist)) {
  assign(paste("temp", i, sep = ''), as.data.frame(hdlist[i]))
}

# Clean function, then apply
cleanDF <- function(df) {
  # Get the original date column (should be row names from df_wide)
  date_col <- df_wide$date[(nrow(df_wide) - nrow(df) + 1):nrow(df_wide)]

  df <- df %>%
    rename('US' = 4, 'Germany' = 5, 'UK' = 6, 'Japan' = 7) %>% # Fixed country names
    dplyr::select(-1, -2, -3) %>%
    mutate(date = date_col) %>% # Use actual dates from df_wide
    dplyr::select(date, everything()) # Move date to front

  return(df)
}

# Estimate VAR, HistDecomps
v1 <- vars::VAR(dat, lag.max = 4, type = "both", ic = "AIC")
summary(v1)

# Structural break date
sbDate <- lubridate::ymd("2020-03-01")
nSB <- nrow(df_wide[df_wide$date <= sbDate, ])
EA.cv <- id.cv(v1, SB = nSB) # changes in volatility identification
summary(EA.cv)

# Generate historical decompositions for each series
k <- ncol(dat)
hd_results <- list()
for (i in 1:k) {
  hd_results[[i]] <- hd(EA.cv, series = i)
}

# Extract historical decomposition matrices
hdlist <- lapply(hd_results, function(x) x[['hidec']])
names(hdlist) <- colnames(dat)

# Create clean dataframes for each country's decomposition
country_names <- c("US", "Germany", "UK", "Japan")
temp_dfs <- list()

for (i in 1:length(hdlist)) {
  temp_df <- as.data.frame(hdlist[[i]]) %>%
    # Add proper date sequence
    mutate(date = df_wide$date[(nrow(df_wide) - nrow(.) + 1):nrow(df_wide)]) %>%
    # Rename shock columns to country names
    rename_with(~country_names, .cols = 4:7) %>%
    # Remove the first 3 columns (they contain technical info)
    dplyr::select(date, all_of(country_names))

  temp_dfs[[i]] <- temp_df
}

# Convert to long format for plotting
df_long_list <- list()
for (i in 1:length(temp_dfs)) {
  df_long_list[[i]] <- temp_dfs[[i]] %>%
    pivot_longer(
      cols = all_of(country_names),
      names_to = 'shock',
      values_to = 'value'
    )
}

# Create improved plots with better styling
plot_titles <- c(
  "US 10y Treasury Yields",
  "German 10y Bund Yields",
  "UK 10y Gilt Yields",
  "Japan 10y Bond Yields"
)

# Generate plots
plots_list <- list()
for (i in 1:k) {
  plots_list[[i]] <- ggplot(
    df_long_list[[i]],
    aes(fill = shock, y = value, x = date)
  ) +
    geom_bar(position = "stack", stat = "identity", alpha = 0.8) +
    scale_fill_viridis_d(name = "Shock Origin") +
    ggtitle(plot_titles[i]) +
    labs(
      x = "Date",
      y = "Contribution (percentage points)",
      subtitle = "Historical decomposition"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Display plots
plots_list[[1]] # US decomposition
plots_list[[2]] # German decomposition
plots_list[[3]] # UK decomposition
plots_list[[4]] # Japanese decomposition

# Summary of structural break results
cat("=== Rigobon Structural VAR Results ===\n")
cat("Structural break date:", as.character(sbDate), "\n")
cat("Pre-break observations:", nSB, "\n")
cat("Post-break observations:", nrow(dat) - nSB, "\n")
cat("Number of variables:", k, "\n")
cat("VAR lag order selected:", v1$p, "\n")

# Print the structural matrix (Lambda matrix)
cat("\nStructural coefficients (Lambda matrix):\n")
print(round(EA.cv$Lambda, 4))

# Calculate variance decomposition at different horizons
fevd_results <- fevd(EA.cv, n.ahead = 12)
cat("\nForecast Error Variance Decomposition at 12-month horizon:\n")
print(round(fevd_results[["US"]][12, ], 3))


# Alternative: Show decomposition as deviations from initial level
plots_list_levels <- list()
for (i in 1:k) {
  country_name <- colnames(dat)[i]

  # Get the starting yield level
  initial_yield <- df_wide %>%
    slice_tail(n = nrow(df_long_list[[i]]) / 4) %>%
    slice_head(n = 1) %>%
    pull(!!country_name)

  # Calculate cumulative decomposition + initial level
  decomp_with_level <- df_long_list[[i]] %>%
    group_by(date) %>%
    summarise(
      total_change = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(implied_yield = initial_yield + total_change)

  # Get actual yields for comparison
  actual_data <- df_wide %>%
    slice_tail(n = nrow(decomp_with_level)) %>%
    dplyr::select(date, actual_yield = !!country_name)

  plots_list_levels[[i]] <- ggplot(df_long_list[[i]], aes(x = date)) +
    # Stacked decomposition
    geom_bar(
      aes(fill = shock, y = value),
      position = "stack",
      stat = "identity",
      alpha = 0.7
    ) +
    # Horizontal line at zero (baseline)
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    # Actual yield level (right axis)
    geom_line(
      data = actual_data,
      aes(y = (actual_yield - initial_yield) * 100), # Convert to basis points
      color = "red",
      size = 1.2
    ) +
    scale_fill_viridis_d(name = "Shock Origin") +
    ggtitle(paste(plot_titles[i], "- Decomposition vs Actual")) +
    labs(
      x = "Date",
      y = "Contribution to yield change (basis points)",
      subtitle = paste(
        "Decomposition (bars) vs actual change (red line) from",
        format(min(actual_data$date), "%Y-%m")
      )
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Display level-adjusted plots
plots_list_levels[[1]]
plots_list_levels[[2]]
plots_list_levels[[3]]
plots_list_levels[[4]]
