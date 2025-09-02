# plots for OIS and Gilts data

#=======================================
# Fig: OIS, GBPUSD Cumul change, 90d----
#=======================================
dat <- delta.gbp.cumul.long
scale_factor <- 10

# EVENTS
events1 <- as.Date(c(
  '2025-05-07',
  '2025-06-18',
  '2025-07-30',
  '2025-08-22',
  '2025-09-17',
  '2025-10-29',
  '2025-12-10'
))
events1.label <- 'FOMC'

events2 <- as.Date(c(
  '2025-05-08',
  '2025-06-19',
  '2025-08-07',
  '2025-09-18',
  '2025-09-18',
  '2025-11-06',
  '2025-12-18'
))
events2.label <- 'BoE'

# Payrolls
events3 <- get_first_fridays()
events3.label <- 'US Payrolls'

# limit dates to chosen data window
date_range <- range(dat$date, na.rm = TRUE)
events_list <- list(events1, events2, events3)
for (i in seq_along(events_list)) {
  events_list[[i]] <- events_list[[i]][
    events_list[[i]] >= date_range[1] & events_list[[i]] <= date_range[2]
  ]
}

# Plot
plot.ois.gbp <- ggplot(dat, aes(x = date)) +
  # Primary Y-axis (Interest Rate Changes) - filter for OIS variables (x24, x60)
  geom_line(
    data = dat %>% filter(variable %in% c("x24", "x60")),
    aes(y = cumulative_change, color = factor(variable)),
    linewidth = 0.9
  ) +
  geom_point(
    data = dat %>% filter(variable %in% c("x24", "x60")),
    aes(y = cumulative_change, color = factor(variable)),
    size = 1.1
  ) +
  # Secondary Y-axis (GBPUSD scaled)
  geom_col(
    data = dat %>% filter(variable == "gbpusd"),
    aes(y = cumulative_change * scale_factor, fill = variable),
    color = 'deeppink4',
    alpha = 0.6
  ) +
  # ADD EVENT DATES - FOMC dates with labels
  geom_vline(
    xintercept = events_list[[1]],
    linetype = "dashed",
    color = "red",
    alpha = 0.7
  ) +
  # ADD EVENT DATES - BoE dates with labels
  geom_vline(
    xintercept = events_list[[2]],
    linetype = "dashed",
    color = "blue",
    alpha = 0.7
  ) +
  # ADD EVENT DATES - US Payrolls
  geom_vline(
    xintercept = events_list[[3]],
    linetype = "dashed",
    color = "gray70",
    alpha = 0.7
  ) +
  # Add text labels for the event lines
  annotate(
    "text",
    x = events_list[[1]],
    y = 10,
    label = events1.label,
    hjust = 0.5,
    vjust = 1.2,
    color = "red",
    size = 3,
    angle = 90
  ) +
  annotate(
    "text",
    x = events_list[[2]],
    y = 10,
    label = events2.label,
    hjust = 0.5,
    vjust = 1.2,
    color = "blue",
    size = 3,
    angle = 90
  ) +
  annotate(
    "text",
    x = events_list[[3]],
    y = 10,
    label = events3.label,
    hjust = 0.5,
    vjust = 1.2,
    color = "gray70",
    size = 3,
    angle = 90
  ) +
  # Reference lines
  geom_hline(yintercept = 0.0, linetype = 4) +
  # Color scales
  scale_color_jco(
    labels = c("x24" = "2y OIS", "x60" = "5y OIS")
  ) +
  # Fill scale for GBPUSD bars
  scale_fill_manual(
    values = c("gbpusd" = "deeppink4"),
    labels = c("gbpusd" = "GBPUSD")
  ) +
  # Primary and Secondary Y-Axis
  scale_y_continuous(
    name = "bp, cumulative change",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "GBPUSD % change"
    )
  ) +
  # Labels & Legends
  labs(
    title = "GBP OIS and GBPUSD",
    subtitle = "cumulative change",
    color = "",
    fill = ""
  ) +
  theme(legend.position = "bottom")
plot.ois.gbp

#===========================================
# Fig: Gilts, Equities Cumul change, 90d----
#===========================================
scale_factor <- 10

plot.gilts.eq <- ggplot(dat, aes(x = date)) +
  # Primary Y-axis (Interest Rate Changes) - filter for Gilt variables (col_4, col_20)
  geom_line(
    data = dat %>% filter(variable %in% c("col_4", "col_20", "col_50")),
    aes(y = cumulative_change, color = factor(variable)),
    linewidth = 0.9
  ) +
  geom_point(
    data = dat %>% filter(variable %in% c("col_4", "col_20", "col_50")),
    aes(y = cumulative_change, color = factor(variable)),
    size = 1.1
  ) +
  # Secondary Y-axis (FTSE scaled)
  geom_col(
    data = dat %>% filter(variable == "ftse_all"),
    aes(y = cumulative_change * scale_factor, fill = variable),
    color = 'gray70',
    alpha = 0.6
  ) +
  # ADD EVENT DATES - FOMC dates with labels
  geom_vline(
    xintercept = as.numeric(events_list[[1]]),
    linetype = "dashed",
    color = "red",
    alpha = 0.7
  ) +
  # ADD EVENT DATES - BoE dates with labels
  geom_vline(
    xintercept = as.numeric(events_list[[2]]),
    linetype = "dashed",
    color = "blue",
    alpha = 0.7
  ) +
  # ADD EVENT DATES - US Payrolls
  geom_vline(
    xintercept = events_list[[3]],
    linetype = "dashed",
    color = "gray70",
    alpha = 0.7
  ) +
  # Add text labels for the event lines
  annotate(
    "text",
    x = events_list[[1]],
    y = 10,
    label = events1.label,
    hjust = 0.5,
    vjust = 1.2,
    color = "red",
    size = 3,
    angle = 90
  ) +
  annotate(
    "text",
    x = events_list[[2]],
    y = 10,
    label = events2.label,
    hjust = 0.5,
    vjust = 1.2,
    color = "blue",
    size = 3,
    angle = 90
  ) +
  annotate(
    "text",
    x = events_list[[3]],
    y = 10,
    label = events3.label,
    hjust = 0.5,
    vjust = 1.2,
    color = "gray70",
    size = 3,
    angle = 90
  ) +
  # Reference lines
  geom_hline(yintercept = 0.0, linetype = 4) +
  # Color scales - control order with breaks parameter
  scale_color_jco(
    labels = c(
      "col_4" = "2y Gilt",
      "col_20" = "10y Gilt",
      "col_50" = "25y Gilt"
    ),
    breaks = c("col_4", "col_20", "col_50")
  ) +
  scale_fill_jco(
    labels = c("ftse_all" = "FTSE All Share")
  ) +
  # Primary and Secondary Y-Axis
  scale_y_continuous(
    name = "bp, cumulative change",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Equity index, % change"
    )
  ) +
  # Labels & Legends
  labs(
    title = "Gilt yields and UK Equity prices",
    subtitle = "cumulative change",
    color = "",
    fill = ""
  ) +
  theme(legend.position = "bottom")
plot.gilts.eq

#================================
# Figure 1: Evolving Forwards----
#================================
ois1 <- ggplot(fwcv, aes(x = date2, y = yield, group = date)) +
  geom_line(aes(colour = as.factor(date))) +
  geom_line(
    data = latest,
    aes(x = date2, y = yield),
    color = "black",
    lty = 2,
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = bankrate)) +
  geom_hline(yintercept = 0.0, lty = 4) +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "GBP OIS Curves and Bank Rate",
    subtitle = "monthly averages of end-of-day daily data",
    x = "date",
    y = "rate %",
    caption = "Source: Bank of England data"
  )
ois1
# save
ggsave(
  here("plots", "1.GBP_OIS.png"),
  plot = ois1,
  width = 10,
  height = 6,
  dpi = 300
)
#========================================
# Figure 2: Recent data, 12m lookback----
#========================================
last_12m <- fwcv |>
  distinct(date) |>
  arrange(desc(date)) |>
  slice_head(n = 12) |>
  pull(date)

ois2 <- ggplot(
  subset(fwcv, date %in% last_12m),
  aes(x = date2, y = yield, group = date)
) +
  geom_line(color = "gray70", linewidth = 1.25) +
  # ADD: Highlight the latest OIS curve with a dashed black line
  geom_line(
    data = fwcv %>%
      filter(!is.na(yield)) %>%
      filter(date == max(date)),
    aes(x = date2, y = yield),
    color = "black",
    lty = 2,
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  # OPTIONAL: Keep the red points if you want them too
  geom_point(
    data = fwcv %>%
      filter(!is.na(yield)) %>%
      filter(date == max(date)),
    aes(x = date2, y = yield),
    color = "red",
    size = 2,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = subset(fwcv, date %in% last_12m),
    aes(x = date2, y = bankrate),
    color = "black",
    linewidth = 1.25,
    inherit.aes = FALSE # avoid grouping by date
  ) +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "GBP OIS Curves: The past 12 months",
    subtitle = "monthly averages of end-of-day daily data",
    x = "date",
    y = "rate %",
    caption = "Source: Bank of England data"
  )
ois2
# save
ggsave(
  here("plots", "2.GBP_OIS_12m.png"),
  plot = ois2,
  width = 10,
  height = 6,
  dpi = 300
)

#=================#=================
# Plot Spreads
#=================#=================
start_date <- as.Date('2020-01-01')
#start_date <- today() - days(opt.h)

plot_spread <- function(dataf, spread_col) {
  # Extract the maturity parts and format with hyphen
  maturity_part <- gsub("spread", "", spread_col) # Remove "spread" prefix
  formatted_title <- gsub("(\\d+)s(\\d+)s", "\\1-\\2", maturity_part) # Convert "2s5s" to "2-5"

  dataf |>
    filter(date >= start_date) |>
    ggplot(aes(x = date, y = .data[[spread_col]])) +
    geom_line() +
    geom_hline(yintercept = 0, lty = 4) +
    labs(
      title = paste0(formatted_title, "y spread"),
      #      subtitle = "percentage points",
      x = "Date",
      y = "Spread (pp)"
    )
}

# Plot 2y-5y spread
plot2s5s <- plot_spread(glcspreads, "spread2s5s")

# Plot 2y-10y spread
plot2s10s <- plot_spread(glcspreads, "spread2s10s")

# Plot 5y-10y spread
plot5s10s <- plot_spread(glcspreads, "spread5s10s")

# Plot 10y-25y spread and 10y-30y
plot10s25s <- plot_spread(glcspreads, "spread10s25s")
plot10s30s <- plot_spread(glcspreads, "spread10s30s")

# combined plot
plot2s5s +
  plot5s10s +
  plot10s25s +
  plot_layout(
    axis_titles = "collect",
    ncol = 3,
    heights = 1,
    widths = 1,
    guides = "collect"
  ) &
  theme(plot.margin = margin(2, 2, 2, 2)) &
  ylim(
    range(
      c(
        layer_data(plot2s5s)$y,
        layer_data(plot5s10s)$y,
        layer_data(plot10s25s)$y
      ),
      na.rm = TRUE
    )
  )

#=================
# plot 2y v 10y,
#=================
# plot 2y v 10y with color coding for 2-year periods
plot2y_v_10y <- glc |>
  filter(date >= max(date) - years(10)) |>
  mutate(
    # Create 2-year period groupings
    period = case_when(
      date >= max(date) - years(2) ~ "2023-2025",
      date >= max(date) - years(4) ~ "2021-2023",
      date >= max(date) - years(6) ~ "2019-2021",
      date >= max(date) - years(8) ~ "2017-2019",
      date >= max(date) - years(10) ~ "2015-2017"
    ),
    # Ensure proper factor ordering (oldest to newest)
    period = factor(
      period,
      levels = c(
        "2015-2017",
        "2017-2019",
        "2019-2021",
        "2021-2023",
        "2023-2025"
      )
    )
  ) |>
  ggplot(aes(x = col_4, y = col_20, color = period)) +
  geom_point(alpha = 0.7) +
  geom_point(
    data = glc |>
      filter(date >= max(date) - years(10)) |>
      slice_tail(n = 10),
    aes(x = col_4, y = col_20),
    shape = 4,
    color = "black", # Highlight last 10 observations
    size = 3,
    inherit.aes = FALSE # Don't inherit the color aesthetic
  ) +
  geom_hline(yintercept = 0, lty = 4) +
  geom_vline(xintercept = 0, lty = 4) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_jco() + # Use ggsci color palette
  labs(
    title = "2y vs 10y Gilt yields",
    subtitle = "sample: last 10 years, daily data",
    x = "2y Gilt yield (%)",
    y = "10y Gilt yield (%)"
  ) +
  theme(legend.position = "bottom")
# save
ggsave(
  here("plots", "4.Gilt_2y_v_10y.png"),
  plot = plot2y_v_10y,
  width = 10,
  height = 6,
  dpi = 300
)

#=================
# plot 5y v 25y,
#=================
# plot 2y v 10y with color coding for 2-year periods
plot5y_v_25y <- glc |>
  filter(date >= max(date) - years(10)) |>
  mutate(
    # Create 2-year period groupings
    period = case_when(
      date >= max(date) - years(2) ~ "2023-2025",
      date >= max(date) - years(4) ~ "2021-2023",
      date >= max(date) - years(6) ~ "2019-2021",
      date >= max(date) - years(8) ~ "2017-2019",
      date >= max(date) - years(10) ~ "2015-2017"
    ),
    # Ensure proper factor ordering (oldest to newest)
    period = factor(
      period,
      levels = c(
        "2015-2017",
        "2017-2019",
        "2019-2021",
        "2021-2023",
        "2023-2025"
      )
    )
  ) |>
  ggplot(aes(x = col_10, y = col_50, color = period)) +
  geom_point(alpha = 0.7) +
  geom_point(
    data = glc |>
      filter(date >= max(date) - years(10)) |>
      slice_tail(n = 10),
    aes(x = col_10, y = col_50),
    shape = 4,
    color = "black", # Highlight last 10 observations
    size = 3,
    inherit.aes = FALSE # Don't inherit the color aesthetic
  ) +
  geom_hline(yintercept = 0, lty = 4) +
  geom_vline(xintercept = 0, lty = 4) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_jco() + # Use ggsci color palette
  labs(
    title = "5y vs 25y Gilt yields",
    subtitle = "sample: last 10 years, daily data",
    x = "5y Gilt yield (%)",
    y = "25y Gilt yield (%)"
  ) +
  theme(legend.position = "bottom")
# save
ggsave(
  here("plots", "4b.Gilt_5y_v_25y.png"),
  plot = plot5y_v_25y,
  width = 10,
  height = 6,
  dpi = 300
)

#==============
# Swap Spreads
#==============
plot.swsp5y <- swsp |>
  ggplot() +
  geom_line(aes(x = date, y = swsp5y)) +
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(x = "date", y = "5-year swap spread")


# find 3-year-ago values
# use a tolerance of ±7 days to find the closest match
calculate_3y_lag <- function(data, target_date, value_col, tolerance_days = 7) {
  target_date_3y_ago <- target_date - years(3)

  # Find observations within tolerance
  candidate_data <- data |>
    filter(abs(as.numeric(date - target_date_3y_ago)) <= tolerance_days) |>
    arrange(abs(as.numeric(date - target_date_3y_ago)))

  if (nrow(candidate_data) > 0) {
    return(candidate_data[[value_col]][1])
  } else {
    return(NA_real_)
  }
}

# Calculate 3-year lagged values for each observation
swsp_final <- swsp |>
  arrange(date) |>
  rowwise() |>
  mutate(
    swsp5y_3y_lag = calculate_3y_lag(swsp, date, "swsp5y"),
    x60_3y_lag = calculate_3y_lag(swsp, date, "x60"),
    col_20_3y_lag = calculate_3y_lag(swsp, date, "col_20")
  ) |>
  ungroup()

# Calculate the indexed changes and contributions
swsp_final <- swsp_final |>
  mutate(
    # Calculate 3-year changes
    dswsp5y_3y_change = swsp5y - swsp5y_3y_lag,
    dx60_3y_change = x60 - x60_3y_lag,
    dcol_20_3y_change = col_20 - col_20_3y_lag,

    # Index the changes (base = 100 at the 3-year-ago level)
    dswsp5y_3y = ifelse(
      !is.na(swsp5y_3y_lag) & swsp5y_3y_lag != 0,
      100 * (swsp5y / swsp5y_3y_lag),
      NA_real_
    ),

    # Calculate contributions (how much each component contributed to the total change)
    # Contribution = (change in component / initial swsp5y level) * 100
    contrib_x60_3y = ifelse(
      !is.na(swsp5y_3y_lag) & swsp5y_3y_lag != 0,
      100 * (dx60_3y_change / abs(swsp5y_3y_lag)),
      NA_real_
    ),

    contrib_col_20_3y = ifelse(
      !is.na(swsp5y_3y_lag) & swsp5y_3y_lag != 0,
      100 * (-dcol_20_3y_change / abs(swsp5y_3y_lag)), # Negative because swsp5y = x60 - col_20
      NA_real_
    ),

    # Alternative: Contributions as absolute basis points
    contrib_x60_3y_bp = dx60_3y_change * 100, # Convert to basis points
    contrib_col_20_3y_bp = -dcol_20_3y_change * 100, # Negative because of subtraction

    # Verification: contributions should sum to total change
    total_contrib_check = contrib_x60_3y_bp + contrib_col_20_3y_bp,
    total_change_bp = dswsp5y_3y_change * 100
  ) |>
  filter(!is.na(dswsp5y_3y)) # Remove observations without 3-year history


# Create contribution decomposition plot
# Calculate the date that represents "3 years ago" from the latest data
latest_date <- max(swsp_final$date, na.rm = TRUE)
three_years_ago_date <- latest_date - years(3)

contrib_data <- swsp_final |>
  filter(date >= as.Date("2015-01-01")) |>
  dplyr::select(
    date,
    contrib_x60_3y_bp,
    contrib_col_20_3y_bp,
    total_change_bp
  ) |>
  pivot_longer(
    cols = c(contrib_x60_3y_bp, contrib_col_20_3y_bp),
    names_to = "component",
    values_to = "contribution"
  )

plot_5yswsp <- ggplot() +

  plot_contributions_5yswsp <- ggplot() +
  # Stacked bar chart showing contributions
  geom_col(
    data = contrib_data,
    aes(x = date, y = contribution, fill = component),
    position = "stack",
    alpha = 0.8,
    width = 30 # Adjust width for daily data
  ) +
  # Line showing total change for verification
  geom_line(
    data = swsp_final |> filter(date >= as.Date("2015-01-01")),
    aes(x = date, y = total_change_bp),
    color = "black",
    size = 1,
    alpha = 0.7
  ) +
  # Add vertical line for 3 years ago
  geom_vline(
    xintercept = three_years_ago_date,
    linetype = "dashed",
    linewidth = 1.0,
    alpha = 0.8
  ) +
  annotate(
    "text",
    x = three_years_ago_date,
    y = max(contrib_data$contribution, na.rm = TRUE) * 0.9,
    label = paste("base period:\n", format(three_years_ago_date, "%b %Y")),
    size = 3.5,
    hjust = -0.1
  ) +
  scale_fill_jco(
    name = "Contribution:",
    labels = c("contrib_col_20_3y_bp" = "Gilt", "contrib_x60_3y_bp" = "OIS")
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    alpha = 0.6,
    color = "gray50"
  ) +
  labs(
    title = "Decomposition of 5Y Swap Spread",
    subtitle = "Contributions to change in 5y swap from 3years ago",
    x = "Date",
    y = "Contribution to Change (bp)",
    caption = ""
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

print(plot_contributions_5yswsp)
