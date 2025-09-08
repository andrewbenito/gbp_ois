# Plot GBP Asset Prices - Bouts of Risk Premium

#blpConnect()
# SETTINGS
start_date <- as.Date('2020-01-01')


# Plot GBPUSD Correlations, weekly and monthly
dat.correl <- df |>
  dplyr::select(c(contains("corr"), date)) |>
  pivot_longer(!date, names_to = "variable", values_to = "value")

# average monthly correlation
stat.avg1 <- dat.correl |>
  dplyr::filter(variable == "corr_monthly") |>
  summarise(average = mean(value, na.rm = TRUE)) |>
  pull(average)

# Plot
correl.plot <- ggplot(
  subset(dat.correl, date >= "2022-06-01"),
  aes(x = date, y = value, color = factor(variable))
) +
  geom_line(linewidth = 1.75) +
  geom_vline(xintercept = as.Date("2022-09-23"), lty = 4, size = 1.75) + # mini_Budget
  geom_vline(xintercept = as.Date("2024-10-30"), lty = 4, size = 1.75) + # Reeves Budget
  geom_vline(xintercept = as.Date("2025-01-07"), lty = 4, size = 1.75) + # January volatility
  geom_vline(xintercept = as.Date("2025-04-02"), lty = 4, size = 1.75) + # US Tariffs announced
  annotate(
    "text",
    x = as.Date("2022-09-23"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "mini-Budget",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2024-10-30"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Reeves\nBudget",
    hjust = 1.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2025-01-07"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Jan.\nvol.",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed",
    fontface = "bold",
    color = "red"
  ) +
  annotate(
    "text",
    x = as.Date("2025-04-02"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "US Tariffs",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed",
    color = "brown"
  ) +
  geom_hline(
    aes(yintercept = stat.avg1, linetype = "sample mean"),
    size = 1.75,
    color = "darkred"
  ) +
  scale_linetype_manual(name = "", values = c("sample mean" = 4)) +
  scale_color_manual(
    values = c("red", "dodgerblue"), # Custom colors for each series
    labels = c("1-month (rolling)", "2-week (rolling)") # Custom legend labels
  ) +
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(
    title = "Correlation of GB/US 10y Yield Gap and GBPUSD",
    y = "correlation",
    caption = "Source: Bloomberg",
    color = NULL
  ) +
  theme(legend.position = "right") +
  scale_x_date(limits = c(as.Date("2022-06-01"), as.Date("2025-06-30")))
correl.plot

# Monthly Plot, only
dat.correl <- dat.correl[dat.correl$variable != "corr_weekly", ]
correl.plot.m <- ggplot(
  subset(dat.correl, date >= "2022-06-01"),
  aes(x = date, y = value, color = factor(variable))
) +
  geom_line(linewidth = 1.75) +
  geom_vline(xintercept = as.Date("2022-09-23"), lty = 4, size = 1.75) + # mini_Budget
  geom_vline(xintercept = as.Date("2024-10-30"), lty = 4, size = 1.75) + # Reeves Budget
  geom_vline(xintercept = as.Date("2025-01-07"), lty = 4, size = 1.75) + # January volatility
  geom_vline(xintercept = as.Date("2025-04-02"), lty = 4, size = 1.75) + # US Tariffs announced
  annotate(
    "text",
    x = as.Date("2022-09-23"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "mini-Budget",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2024-10-30"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Reeves\nBudget",
    hjust = 1.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2025-01-07"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Jan.\nvol.",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed",
    fontface = "bold",
    color = "red"
  ) +
  annotate(
    "text",
    x = as.Date("2025-04-02"),
    y = min(subset(dat.correl, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "US Tariffs",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 13,
    family = "Roboto Condensed"
  ) +
  geom_hline(
    aes(yintercept = stat.avg1, linetype = "sample mean"),
    size = 1.75,
    color = "darkred"
  ) +
  scale_linetype_manual(name = "", values = c("sample mean" = 4)) +
  scale_color_manual(
    values = "blue", # Custom colors for each series
    labels = "1-month (rolling)"
  ) + # Custom legend labels
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(
    title = "Correlation of GB/US 10y Yield Gap and GBPUSD",
    y = "correlation",
    caption = "Source: Bloomberg",
    color = NULL
  ) +
  theme(legend.position = "right") +
  scale_x_date(limits = c(as.Date("2022-06-01"), as.Date("2025-06-30")))
correl.plot.m


df1 <- df |>
  dplyr::select(-c(contains("corr"), yieldgap10, gtgbp10yr_corp)) |>
  pivot_longer(!date, names_to = "variable", values_to = "value")

# plots yield gap against gbpusd
df2 <- df |>
  dplyr::select(date, yieldgap10, gbpusd_curncy) |>
  pivot_longer(!date, names_to = "variable", values_to = "value")

# Plot
#[1]
assets.plot1 <- ggplot(
  subset(df1, date >= '2022-06-01'),
  aes(x = date, y = value, color = factor(variable))
) +
  geom_line() +
  theme(legend.position = "none") +
  facet_wrap(~variable, scales = "free_y")
#[2]
assets.plot2 <- ggplot(
  subset(df2, date >= '2022-06-01'),
  aes(x = date, y = value, color = factor(variable))
) +
  geom_line() +
  theme(legend.position = "none") +
  facet_wrap(~variable, scales = "free_y")
