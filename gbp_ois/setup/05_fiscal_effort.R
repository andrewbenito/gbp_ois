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
# Load up raw data + Clean----
# [1] IMF Fiscal monitor
fm <- read_csv(here('data', 'IMF_FM_WIDEF.csv')) |> # time-series data are wide
  t()
fm <- clean_IMF_FM(fm) |>
  arrange(country, year)

# [2] IMF WEO (by country): https://www.imf.org/en/Publications/WEO/weo-database/2025/april/download-entire-database
url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2025/april/WEOApr2025all.ashx"
weo <- read_tsv(url, locale = locale(encoding = "UTF-16LE")) |>
  janitor::clean_names()
weo <- clean_IMF_WEO(weo) |>
  arrange(country, year)

# this analysis - get variables, pivot_wider
selection <- c("NGDP_RPCH", "NGDP_D", "PCPI", "NGAP_NPGDP")
weo <- weo |>
  dplyr::select(
    country,
    iso,
    weo_subject_code,
    weo_country_code,
    year,
    value
  ) |>
  rename(country_code = iso) |>
  filter(weo_subject_code %in% selection) |>
  pivot_wider(names_from = weo_subject_code, values_from = value) |>
  filter(year >= 1980)

# input IMF List of Advanced Economies (country codes)
imf_ae_list <- c(
  171,
  193,
  122,
  124,
  156,
  960,
  423,
  935,
  128,
  939,
  172,
  132,
  134,
  174,
  532,
  176,
  178,
  436,
  136,
  158,
  542,
  941,
  946,
  137,
  546,
  181,
  138,
  196,
  142,
  182,
  359,
  135,
  576,
  936,
  961,
  184,
  144,
  146,
  528,
  112,
  111
)
weo <- weo |>
  mutate(ae = if_else(weo_country_code %in% imf_ae_list, 1, 0)) |>
  filter(ae == 1)

# MERGE FM and WEO----
imf <- weo |>
  left_join(fm, by = c("country_code", "year")) |>
  dplyr::select(-ae, -country.y)

# PLOTS
# 1. UK: cyc-adj PB versus Net Debt, t-1; pre- and post-crisis
# 2: by country: g, d(t-1), pb and dspb. [dspb - pb = fiscal effort]

# Extract UK data and create lagged net debt
uk <- imf |>
  filter(country.x == "United Kingdom") |>
  filter(!is.na(primarynetlendingborrowing) & (!is.na(netdebt))) |>
  arrange(year) |>
  mutate(
    # Create lagged net debt (previous year's value)
    netdebt_L1 = lag(netdebt, 1),
    # Create forecast indicator for 2025 onwards
    is_forecast = year >= 2025,
    # Create period classification: pre-2008, post-2008, forecast
    period_gfc = case_when(
      year <= 2008 ~ "Pre-2008",
      year > 2008 & !is_forecast ~ "Post-2008",
      is_forecast ~ "Forecast (2025+)"
    )
  ) |>
  # Remove first observation since it won't have lagged debt
  filter(!is.na(netdebt_L1))

# UK plot for pb v netdebt_L1
#------------------------------------
plot_uk <- ggplot(
  uk,
  aes(x = netdebt_L1, y = primarynetlendingborrowing)
) +
  # Plot pre-2008 data (green circles with solid line)
  geom_point(
    data = uk |> filter(period_gfc == "Pre-2008"),
    aes(color = period_gfc),
    size = 3,
    alpha = 0.8,
    shape = 19
  ) +
  geom_path(
    data = uk |> filter(period_gfc == "Pre-2008"),
    aes(color = period_gfc),
    size = 1,
    alpha = 0.8
  ) +
  # Plot post-2008 data (blue squares with solid line)
  geom_point(
    data = uk |> filter(period_gfc == "Post-2008"),
    aes(color = period_gfc),
    size = 3,
    alpha = 0.8,
    shape = 15
  ) +
  geom_path(
    data = uk |> filter(period_gfc == "Post-2008"),
    aes(color = period_gfc),
    size = 1,
    alpha = 0.8
  ) +
  # Plot forecast data (red triangles with dashed line)
  geom_point(
    data = uk |> filter(period_gfc == "Forecast (2025+)"),
    aes(color = period_gfc),
    size = 3,
    shape = 17,
    alpha = 0.8
  ) +
  geom_path(
    data = uk |> filter(period_gfc == "Forecast (2025+)"),
    aes(color = period_gfc),
    size = 1,
    linetype = "dashed",
    alpha = 0.8
  ) +
  # Add year labels for key transition points
  geom_text(
    data = uk |>
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
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = 1.0,
    linetype = "dotted",
    color = "red"
  ) +
  # Add vertical line to mark 2008 financial crisis
  geom_vline(
    xintercept = uk |>
      filter(year == 2008) |>
      pull(netdebt_L1),
    linetype = "dashed",
    alpha = 0.5,
    color = "orange",
    size = 1
  ) +
  # Color scheme distinguishing three periods
  scale_color_jco(
    #    labels = c("Pre-2008", "Post-2008", "Forecast (2025+)")
  ) +
  # Formatting
  scale_x_continuous(
    name = "Net Debt, t-1 (% of GDP)",
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_y_continuous(
    name = "Primary Balance (% of GDP)",
    labels = scales::percent_format(scale = 1),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title = "The fiscal reaction",
    subtitle = "A positive slope implies debt-stabilisation",
    caption = "Source: IMF Fiscal Monitor",
    x = "Net Debt, t-1 (% of GDP)",
    y = "Primary Balance (% of GDP)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(plot_uk)
