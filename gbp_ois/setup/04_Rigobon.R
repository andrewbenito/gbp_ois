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

# Plot----
plot.yields <- ggplot(bond_yields, aes(x = date, y = yield, color = country)) +
  geom_line() +
  geom_hline(yintercept = 0.0, lty = 4) +
  scale_color_jco() + # Add JCO color palette
  labs(
    title = "Government Bond Yields (10-year)",
    #    subtitle = "Monthly data from FRED",
    x = "Date",
    y = "Yield (%)",
    color = NULL
  ) +
  theme(legend.position = "bottom")
plot.yields

df <- bond_yields |> dplyr::select(-contains('date'))
df_wide <- bond_yields |>
  pivot_wider(id_cols = date, names_from = country, values_from = yield) |>
  na.omit()

# wide and clean [dat], drop date
dat <- df_wide |>
  dplyr::select(-date) |>
  na.omit()

#============================
# Estimate VAR, HistDecomps
#============================
v1 <- vars::VAR(dat, lag.max = 4, type = "both", ic = "AIC")
summary(v1)

# sbDate
sbDate <- lubridate::ymd("2022-03-01")
nSB <- nrow(df_wide[df_wide$date <= lubridate::ymd(sbDate), ])
EA.cv <- id.cv(v1, SB = nSB) # changes in volatility;
summary(EA.cv)

# Generate historical decompositions for each series
k <- ncol(dat)
hd_results <- list()
for (i in 1:k) {
  hd_results[[i]] <- hd(EA.cv, series = i)
}

# Extract historical decomposition matrices
hdlist <- lapply(hd_results, function(x) x[['hidec']])
names(hdlist) <- colnames(df_wide)[-1]

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

# Styling
plot_titles <- c(
  "Spillovers: US 10y Treasury Yields",
  "Spillovers: German 10y Bund Yields",
  "Spillovers: UK 10y Gilt Yields",
  "Spillovers: Japan 10y Bond Yields"
)

# Generate plots
plots_list <- list()
for (i in 1:k) {
  plots_list[[i]] <- ggplot(
    df_long_list[[i]],
    aes(fill = shock, y = value, x = date)
  ) +
    geom_bar(position = "stack", stat = "identity", alpha = 0.8) +
    scale_fill_jco(name = "Shock Origin") +
    ggtitle(plot_titles[i]) +
    labs(
      x = "Date",
      y = "Contribution (percentage points)",
      subtitle = "Historical decomposition: VAR results"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(size = 12, face = "bold")
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
cat("Post-break observations:", (nrow(dat) / k) - nSB, "\n")
cat("Number of variables:", k, "\n")
cat("VAR lag order selected:", v1$p, "\n")

# Print the structural matrix (Lambda matrix)
cat("\nStructural coefficients (Lambda matrix):\n")
print(round(EA.cv$Lambda, 4))


# Add the actual yield changes AND sum of contributions to the historical decomposition plots
plots_list_with_actual <- list()
for (i in 1:k) {
  country_name <- colnames(dat)[i]

  # Calculate the actual month-to-month changes in bond yields
  actual_yield_changes <- df_wide %>%
    arrange(date) %>%
    slice_tail(n = nrow(df_long_list[[i]]) / 4) %>% # Match the decomposition period
    mutate(
      yield_change = !!sym(country_name) - lag(!!sym(country_name))
    ) %>%
    dplyr::select(date, yield_change) %>%
    filter(!is.na(yield_change))

  # Calculate sum of contributions per date
  decomp_totals <- df_long_list[[i]] %>%
    group_by(date) %>%
    summarise(total_contribution = sum(value, na.rm = TRUE), .groups = "drop")

  verification <- actual_yield_changes %>%
    left_join(decomp_totals, by = "date") %>%
    mutate(difference = abs(yield_change - total_contribution))

  cat("\n=== Verification for", country_name, "Change Decomposition ===\n")
  cat(
    "Mean absolute difference:",
    round(mean(verification$difference, na.rm = TRUE), 6),
    "\n"
  )
  cat(
    "Max absolute difference:",
    round(max(verification$difference, na.rm = TRUE), 6),
    "\n"
  )

  plots_list_with_actual[[i]] <- ggplot(df_long_list[[i]], aes(x = date)) +
    # Historical decomposition as stacked bars
    geom_bar(
      aes(fill = shock, y = value),
      position = "stack",
      stat = "identity",
      alpha = 0.8
    ) +
    # Add sum of contributions as a blue line
    geom_line(
      data = decomp_totals,
      aes(x = date, y = total_contribution),
      color = "blue",
      size = 1.2,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    # Add actual yield changes as a black line
    geom_line(
      data = actual_yield_changes,
      aes(x = date, y = yield_change),
      color = "black",
      size = 1.5,
      linetype = "solid",
      inherit.aes = FALSE
    ) +
    # Add points for actual yield changes
    geom_point(
      data = actual_yield_changes,
      aes(x = date, y = yield_change),
      color = "red",
      size = 2,
      inherit.aes = FALSE
    ) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_fill_jco(name = "Shock Origin") +
    ggtitle(plot_titles[i]) +
    labs(
      x = "Date",
      y = "Yield Change (percentage points)",
      subtitle = "Decomposition (bars), sum of contributions (blue dashed), actual changes (black line)"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(size = 12, face = "bold")
    )
}

# Display the corrected plots
plots_list_with_actual[[3]] # UK decomposition with actual changes and sum of contributions
