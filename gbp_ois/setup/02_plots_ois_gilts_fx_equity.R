# plots for OIS and Gilts data

#=======================================
# Fig: OIS, GBPUSD Cumul change, 90d----
#=======================================
dat <- delta.gbp.cumul.long
scale_factor <- 20

plot.ois.gbp <- ggplot(dat, aes(x = date)) +
  # Primary Y-axis (Interest Rate Changes) - filter for OIS variables (x24, x60)
  geom_line(
    data = dat %>% filter(variable %in% c("x24", "x60")),
    aes(y = cumulative_change, color = factor(variable)),
    linewidth = 1.5
  ) +
  geom_point(
    data = dat %>% filter(variable %in% c("x24", "x60")),
    aes(y = cumulative_change, color = factor(variable)),
    size = 1.5
  ) +
  # Secondary Y-axis (GBPUSD scaled)
  geom_col(
    data = dat %>% filter(variable == "gbpusd"),
    aes(y = cumulative_change * scale_factor),
    fill = 'deeppink4',
    color = 'deeppink4',
    alpha = 0.6
  ) +
  # Reference lines
  geom_hline(yintercept = 0.0, linetype = 4) +
  # Color scales
  scale_color_manual(
    values = c("x24" = "blue", "x60" = "red"),
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
scale_factor <- 5

plot.gilts.eq <- ggplot(dat, aes(x = date)) +
  # Primary Y-axis (Interest Rate Changes) - filter for OIS variables (x24, x60)
  geom_line(
    data = dat %>% filter(variable %in% c("col_4", "col_20")),
    aes(y = cumulative_change, color = factor(variable)),
    linewidth = 1.5
  ) +
  geom_point(
    data = dat %>% filter(variable %in% c("col_4", "col_20")),
    aes(y = cumulative_change, color = factor(variable)),
    size = 1.5
  ) +
  # Secondary Y-axis (GBPUSD scaled)
  geom_col(
    data = dat %>% filter(variable == "ftse_all"),
    aes(y = cumulative_change * scale_factor),
    fill = 'gray70',
    color = 'gray70',
    alpha = 0.6
  ) +
  # Reference lines
  geom_hline(yintercept = 0.0, linetype = 4) +
  # Color scales
  scale_color_manual(
    values = c("col_4" = "blue", "col_20" = "red"),
    labels = c("col_4" = "2y Gilt", "col_20" = "10y Gilt")
  ) +
  # Fill scale for GBPUSD bars
  scale_fill_manual(
    values = c("ftse_all" = "gray70"),
    labels = c("ftse_all" = "FTSE All Share")
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
    title = "Gilt yields and FTSE-All share",
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
    linewidth = 1.2
  ) +
  geom_line(aes(y = bankrate)) +
  geom_hline(yintercept = 0.0, lty = 4) +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Bank Rate and GBP OIS Curves",
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
  geom_point(
    data = subset(fwcv, date == max(date)),
    aes(x = date2, y = yield),
    color = "red",
    size = 2,
    inherit.aes = FALSE # Add this to avoid group conflicts
  ) +
  geom_line(
    data = subset(fwcv, date %in% last_12m),
    aes(x = date2, y = bankrate),
    color = "black",
    linewidth = 1.25,
    inherit.aes = FALSE # Add this to avoid grouping by date
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
plot_spread <- function(dataf, spread_col) {
  # Extract the maturity parts and format with hyphen
  maturity_part <- gsub("spread", "", spread_col) # Remove "spread" prefix
  formatted_title <- gsub("(\\d+)s(\\d+)s", "\\1-\\2", maturity_part) # Convert "2s5s" to "2-5"

  dataf |>
    filter(date >= max(date) - days(opt.h)) |>
    ggplot(aes(x = date, y = .data[[spread_col]])) +
    geom_point() +
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
  ylim(range(
    c(
      layer_data(plot2s5s)$y,
      layer_data(plot5s10s)$y,
      layer_data(plot10s25s)$y
    ),
    na.rm = TRUE
  ))

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
  scale_color_aaas() + # Use ggsci color palette
  labs(
    title = "2y vs 10y Gilt yields",
    subtitle = "sample: last 10 years, daily data",
    x = "2y Gilt yield (%)",
    y = "10y Gilt yield (%)"
  ) +
  theme(legend.position = "right")
# save
ggsave(
  here("plots", "4.Gilt_2y_v_10y.png"),
  plot = plot2y_v_10y,
  width = 10,
  height = 6,
  dpi = 300
)
