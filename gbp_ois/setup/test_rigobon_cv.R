# 04_Rigobon.R TEST

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
startDate <- as.Date("2020-01-01", format = "%Y-%m-%d")
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
  scale_color_jco() +
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

# Add this section after your existing VAR estimation and structural identification

#============================
# Historical Decomposition Analysis
#============================
country_names <- c('US', 'Germany', 'UK', 'Japan')

# Generate historical decomposition for all 4 countries
hd_results <- list()
for (i in 1:4) {
  cat("Processing historical decomposition for", country_names[i], "...\n")
  hd_results[[i]] <- svars::hd(EA.cv, series = i)
}

# Set country names and plot titles
country_names <- c("US", "Germany", "UK", "Japan")
plot_titles <- c(
  "US 10-Year Treasury Yields: Historical Decomposition",
  "German 10-Year Bund Yields: Historical Decomposition",
  "UK 10-Year Gilt Yields: Historical Decomposition",
  "Japanese 10-Year Bond Yields: Historical Decomposition"
)

# Calculate unconditional means for each country
unconditional_means <- sapply(df_wide[, country_names], function(x) {
  mean(x, na.rm = TRUE)
})
names(unconditional_means) <- country_names

# Create dates for historical decomposition period
n_obs_hd <- nrow(hd_results[[1]]$hidec)
hd_dates <- seq.Date(as.Date("2020-02-01"), by = "month", length.out = n_obs_hd)

# Create historical decomposition plots
hd_plots <- list()

for (i in 1:4) {
  cat("Creating plot for", country_names[i], "...\n")

  # Extract HD data
  hd_raw <- hd_results[[i]]$hidec
  n_rows <- nrow(hd_raw)
  dates_for_hd <- hd_dates[1:n_rows]

  # Create shock contributions data
  hd_contributions <- data.frame(
    date = rep(dates_for_hd, 4),
    shock_origin = rep(country_names, each = n_rows),
    contribution = c(hd_raw[, 4], hd_raw[, 5], hd_raw[, 6], hd_raw[, 7])
  )

  # HD constructed series (what the decomposition reconstructs)
  hd_constructed <- data.frame(
    date = dates_for_hd,
    constructed = hd_raw[, 3] # Column 3: "Constructed series"
  )

  # Get actual yields for the same period (deviations from unconditional mean)
  actual_yields <- df_wide |>
    filter(date >= min(dates_for_hd), date <= max(dates_for_hd)) |>
    slice_head(n = n_rows) |>
    mutate(
      date = dates_for_hd,
      actual_deviation = .data[[country_names[i]]] - unconditional_means[i]
    ) |>
    dplyr::select(date, actual_deviation)

  # Create the comprehensive plot
  hd_plots[[i]] <- ggplot() +
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
    # Add structural break line for March 2022
    geom_vline(
      xintercept = sbDate,
      linetype = "dotted",
      color = "blue",
      size = 1,
      alpha = 0.8
    ) +
    # Reference line at zero (unconditional mean)
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      alpha = 0.6,
      color = "gray50"
    ) +
    # Formatting
    scale_fill_manual(
      name = "Shock Origin",
      values = c(
        "US" = "#E31A1C",
        "Germany" = "#1F78B4",
        "UK" = "#33A02C",
        "Japan" = "#FF7F00"
      ),
      labels = country_names
    ) +
    labs(
      title = plot_titles[i],
      subtitle = "Black = HD reconstruction, Red = Actual deviations, Blue line = Structural break",
      x = "Date",
      y = "Deviation from Unconditional Mean (percentage points)",
      caption = paste0(
        "Unconditional mean: ",
        round(unconditional_means[i], 3),
        "%"
      )
    ) +
    theme_minimal() +
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
  cat(
    "Mean absolute difference (HD vs Actual):",
    round(mean(verification$difference, na.rm = TRUE), 6),
    "\n"
  )
  cat(
    "Correlation (HD vs Actual):",
    round(
      cor(
        verification$constructed,
        verification$actual_deviation,
        use = "complete.obs"
      ),
      4
    ),
    "\n"
  )
  cat("Unconditional mean:", round(unconditional_means[i], 3), "%\n")
}

# Display all plots
#for (i in 1:4) {
#  print(hd_plots[[i]])
#}

# Create combined plot using patchwork (if available)
if (require(patchwork, quietly = TRUE)) {
  combined_hd_plot <- (hd_plots[[1]] + hd_plots[[2]]) /
    (hd_plots[[3]] + hd_plots[[4]])
  #  print(combined_hd_plot)
} else {
  cat("Install 'patchwork' package to see combined plot\n")
}

# Summary statistics
cat("\n=== Historical Decomposition Summary ===\n")
cat("VAR lag order:", v1$p, "\n")
cat("Structural break date:", as.character(sbDate), "\n")
cat("Sample period for HD:", min(hd_dates), "to", max(hd_dates), "\n")
cat("Number of observations:", n_obs_hd, "\n")
cat("Countries analyzed:", paste(country_names, collapse = ", "), "\n")

# UK
#hd_plots[[3]]

#============================
# Structural Shocks Evolution Plots
#============================

# Extract structural shocks from the structural VAR model
structural_shocks <- EA.cv$u # These are the structural shocks

# Check if structural shocks were extracted successfully
if (is.null(structural_shocks)) {
  # Alternative extraction method if u component is not available
  var_residuals <- residuals(v1)
  B_matrix <- EA.cv$B
  structural_shocks <- var_residuals %*% solve(B_matrix)
}

# Create proper date sequence matching the shocks
shock_dates <- seq.Date(
  as.Date("2020-02-01"),
  by = "month",
  length.out = nrow(structural_shocks)
)

# Convert to data frame with proper dates and country labels
shocks_df <- data.frame(
  date = shock_dates,
  US_shock = structural_shocks[, 1],
  Germany_shock = structural_shocks[, 2],
  UK_shock = structural_shocks[, 3],
  Japan_shock = structural_shocks[, 4]
)

# Convert to long format for plotting
shocks_long <- shocks_df |>
  pivot_longer(
    cols = -date,
    names_to = "shock_origin",
    values_to = "shock_value"
  ) |>
  mutate(
    shock_origin = str_remove(shock_origin, "_shock"),
    shock_origin = factor(shock_origin, levels = country_names)
  )

# Create plot showing evolution of structural shocks
plot_structural_shocks <- ggplot(
  shocks_long,
  aes(x = date, y = shock_value, color = shock_origin)
) +
  geom_line(size = 0.8, alpha = 0.8) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    alpha = 0.5,
    color = "gray50"
  ) +
  # Add structural break line for March 2022
  geom_vline(
    xintercept = sbDate,
    linetype = "dotted",
    color = "blue",
    size = 1,
    alpha = 0.8
  ) +
  facet_wrap(~shock_origin, scales = "free_y", ncol = 2) +
  scale_color_manual(
    name = "Country",
    values = c(
      "US" = "#E31A1C",
      "Germany" = "#1F78B4",
      "UK" = "#33A02C",
      "Japan" = "#FF7F00"
    )
  ) +
  labs(
    title = "Estimated Structural Shocks in International Bond Markets",
    subtitle = "Identified structural shocks from 4-country VAR using change-in-volatility identification",
    x = "Date",
    y = "Structural Shock (standardized)"
  ) +
  theme(
    legend.position = "none", # Remove legend since facets show countries
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display the structural shocks plot
#print(plot_structural_shocks)
