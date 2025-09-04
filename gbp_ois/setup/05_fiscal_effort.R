# 06_fiscal_effort.R

# Estimate required fiscal, cross-country
# Calculate Debt Stabilising Primary Balance for different combinations of r, g

# PB = [(Rt – Gt) / (1 + Gt)] dt-1 + sfat

# Rt: effective nominal interest rate on government debt,
# Gt is the nominal growth rate, dt is the debt to GDP ratio, and sfat is stock-flow adjustments
#=================================================================================================
library(gt)
library(tidyverse)
library(here)

# Set initial values
sfa <- 0 # implicitly assumed, so ignored

# set up range for r and g
r <- seq(1.5, 4.5, 0.5)
g <- seq(2.0, 4.0, 0.5)

grid <- data.frame(expand.grid(
  r = r,
  g = g,
  d = 100
))

# debt stabilising primary balance
grid <- grid |>
  mutate(pb = (((r / 100) - (g / 100)) / (1 + (g / 100))) * d)

# Pivot to create table
dat <- grid |>
  dplyr::select(r, g, pb) |>
  pivot_wider(names_from = r, values_from = pb)

# format dat
gt.dat <- dat |>
  gt() |>
  fmt_number(
    decimals = 2
  ) |>
  fmt_number(
    columns = g,
    decimals = 1 # change g column to 1 digits
  ) |>
  tab_options(
    table.font.size = "small"
  ) |>
  # First apply column-wise coloring
  data_color(
    columns = -g,
    method = "numeric",
    palette = "inferno",
    direction = "column",
    reverse = TRUE
  ) |>
  cols_label(
    g = md("**g**")
  ) |>
  tab_header(
    title = "Required, Debt-stabilising, Primary Balance (% GDP)", #
    subtitle = md("**effective nominal interest rate, r**")
  ) |>
  tab_footnote(
    footnote = "Note: Calculations assume Debt/GDP at 100% and no stock-flow adjustments, for different combinations of 'r' and 'g'.",
    placement = "left"
  )
print(gt.dat)


#===========================================================
# Cross-country data: d, r, g for required pb and actual pb
#===========================================================
# Load up raw data----
# [1] IMF Fiscal monitor
fm <- read_csv(here('data', 'IMF_FM_WIDEF.csv')) |> # time-series data are wide
  t()
# [2] IMF WEO (by country): https://www.imf.org/en/Publications/WEO/weo-database/2025/april/download-entire-database
url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2025/april/WEOApr2025all.ashx"
weo <- read_tsv(url, locale = locale(encoding = "UTF-16LE"))

# Clean raw data----
fm <- clean_IMF_FM(fm) |>
  arrange(country, year)


# Check for 2024 data and primarynetlendingborrowing variable
dat_2024 <- dat |>
  filter(year == 2024) |>
  filter(!is.na(primarynetlendingborrowing))

# Get UK value for 2024
uk_value_2024 <- dat_2024 |>
  filter(country == "United Kingdom") |>
  pull(primarynetlendingborrowing)

cat("\nUK Primary Net Lending/Borrowing for 2024:", uk_value_2024, "\n")

# summary statistics
summary_stats <- dat_2024 |>
  summarise(
    n_countries = n(),
    mean_value = mean(primarynetlendingborrowing, na.rm = TRUE),
    median_value = median(primarynetlendingborrowing, na.rm = TRUE),
    sd_value = sd(primarynetlendingborrowing, na.rm = TRUE),
    min_value = min(primarynetlendingborrowing, na.rm = TRUE),
    max_value = max(primarynetlendingborrowing, na.rm = TRUE),
    q25 = quantile(primarynetlendingborrowing, 0.25, na.rm = TRUE),
    q75 = quantile(primarynetlendingborrowing, 0.75, na.rm = TRUE)
  )

# Calculate UK's position in the distribution
uk_percentile <- ecdf(dat_2024$primarynetlendingborrowing)(uk_value_2024) * 100

cat("\n=== Distribution Summary Statistics (2025) ===\n")
cat("Number of countries:", summary_stats$n_countries, "\n")
cat("Mean:", round(summary_stats$mean_value, 2), "%\n")
cat("Median:", round(summary_stats$median_value, 2), "%\n")
cat("Standard deviation:", round(summary_stats$sd_value, 2), "%\n")
cat(
  "Range:",
  round(summary_stats$min_value, 2),
  "% to",
  round(summary_stats$max_value, 2),
  "%\n"
)
cat("25th percentile:", round(summary_stats$q25, 2), "%\n")
cat("75th percentile:", round(summary_stats$q75, 2), "%\n")

cat("\n=== UK Position Analysis ===\n")
cat("UK value:", round(uk_value_2024, 2), "%\n")
cat("UK percentile position:", round(uk_percentile, 1), "%\n")
cat(
  "UK vs mean:",
  round(uk_value_2024 - summary_stats$mean_value, 2),
  "percentage points\n"
)
cat(
  "UK vs median:",
  round(uk_value_2024 - summary_stats$median_value, 2),
  "percentage points\n"
)

# Show countries with similar values to UK
similar_countries <- dat_2024 |>
  mutate(distance_from_uk = abs(primarynetlendingborrowing - uk_value_2024)) |>
  arrange(distance_from_uk) |>
  slice_head(n = 6) |>
  dplyr::select(country, primarynetlendingborrowing, distance_from_uk)

cat("\n=== Countries with Similar Values to UK ===\n")
print(similar_countries)

# Extract UK data and create lagged net debt
uk_time_series_amended <- dat |>
  filter(country == "United Kingdom") |>
  filter(!is.na(primarynetlendingborrowing), !is.na(netdebt)) |>
  arrange(year) |>
  mutate(
    # Create lagged net debt (previous year's value)
    netdebt_lag = lag(netdebt, 1),
    # Create forecast indicator for 2025 onwards
    is_forecast = year >= 2025,
    # Create period classification: pre-2008, post-2008, forecast
    period_type = case_when(
      year <= 2008 ~ "Pre-2008",
      year > 2008 & !is_forecast ~ "Post-2008",
      is_forecast ~ "Forecast (2025+)"
    )
  ) |>
  # Remove first observation since it won't have lagged debt
  filter(!is.na(netdebt_lag))

cat("UK time series data (amended):\n")
cat(
  "Years available:",
  min(uk_time_series_amended$year),
  "to",
  max(uk_time_series_amended$year),
  "\n"
)
cat(
  "Pre-2008 observations:",
  sum(uk_time_series_amended$period_type == "Pre-2008"),
  "\n"
)
cat(
  "Post-2008 observations:",
  sum(uk_time_series_amended$period_type == "Post-2008"),
  "\n"
)
cat(
  "Forecast observations:",
  sum(uk_time_series_amended$period_type == "Forecast (2025+)"),
  "\n"
)


# Create the amended time series plot
#------------------------------------
plot_uk_fiscal_amended <- ggplot(
  uk_time_series_amended,
  aes(x = netdebt_lag, y = primarynetlendingborrowing)
) +
  # Plot pre-2008 data (green circles with solid line)
  geom_point(
    data = uk_time_series_amended |> filter(period_type == "Pre-2008"),
    aes(color = period_type),
    size = 3,
    alpha = 0.8,
    shape = 19
  ) +
  geom_path(
    data = uk_time_series_amended |> filter(period_type == "Pre-2008"),
    aes(color = period_type),
    size = 1,
    alpha = 0.8
  ) +
  # Plot post-2008 data (blue squares with solid line)
  geom_point(
    data = uk_time_series_amended |> filter(period_type == "Post-2008"),
    aes(color = period_type),
    size = 3,
    alpha = 0.8,
    shape = 15
  ) +
  geom_path(
    data = uk_time_series_amended |> filter(period_type == "Post-2008"),
    aes(color = period_type),
    size = 1,
    alpha = 0.8
  ) +
  # Plot forecast data (red triangles with dashed line)
  geom_point(
    data = uk_time_series_amended |> filter(period_type == "Forecast (2025+)"),
    aes(color = period_type),
    size = 3,
    shape = 17,
    alpha = 0.8
  ) +
  geom_path(
    data = uk_time_series_amended |> filter(period_type == "Forecast (2025+)"),
    aes(color = period_type),
    size = 1,
    linetype = "dashed",
    alpha = 0.8
  ) +
  # Add year labels for key transition points
  geom_text(
    data = uk_time_series_amended |>
      filter(year %in% c(2000, 2008, 2009, 2020, 2024, 2029)),
    aes(label = year),
    hjust = -0.3,
    vjust = 0.5,
    size = 3,
    color = "gray30"
  ) +
  # Add reference lines
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    alpha = 0.6,
    color = "gray50"
  ) +
  geom_vline(
    xintercept = 50,
    linetype = "dotted",
    alpha = 0.6,
    color = "gray50"
  ) +
  # Add vertical line to mark 2008 financial crisis
  geom_vline(
    xintercept = uk_time_series_amended |>
      filter(year == 2008) |>
      pull(netdebt_lag),
    linetype = "dashed",
    alpha = 0.5,
    color = "orange",
    size = 1
  ) +
  # Color scheme distinguishing three periods
  scale_color_manual(
    name = "Period",
    values = c(
      "Pre-2008" = "#27AE60",
      "Post-2008" = "#3498DB",
      "Forecast (2025+)" = "#E74C3C"
    ),
    labels = c("Pre-2008", "Post-2008", "Forecast (2025+)")
  ) +
  # Formatting
  scale_x_continuous(
    name = "Previous Year's Net Debt (% of GDP)",
    labels = scales::percent_format(scale = 1),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_y_continuous(
    name = "Primary Net Lending/Borrowing (% of GDP)",
    labels = scales::percent_format(scale = 1),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title = "UK Fiscal Position: Primary Balance vs. Previous Year's Net Debt",
    subtitle = "Distinguishing pre-2008, post-2008, and forecast periods (2025+)",
    caption = "Source: IMF Fiscal Monitor. Orange line marks 2008 financial crisis transition. X-axis shows lagged net debt.",
    x = "Previous Year's Net Debt (% of GDP)",
    y = "Primary Net Lending/Borrowing (% of GDP)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(plot_uk_fiscal_amended)
