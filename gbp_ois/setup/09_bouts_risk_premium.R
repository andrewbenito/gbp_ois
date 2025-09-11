# Plot GBP Asset Prices - Bouts of Risk Premium

#blpConnect()
library(tidyquant)

# SETTINGS
start_date <- as.Date('2020-01-01')

# Plot GBPUSD Correlations, weekly and monthly
dat_correls <- dat_gbp |>
  filter(date >= start_date)
# Get daily yields for DGS10 from FRED
TY10y <- tq_get(
  "DGS10",
  from = start_date,
  to = Sys.Date(),
  get = "economic.data"
)

# Calculate 10y yield gap
dat_correls <- dat_correls |>
  left_join(TY10y, by = c("date" = "date")) |>
  rename(ty10y = price) |>
  mutate(gbus10y = col_20 - ty10y)

# first diff yield gap; % change in gbpusd and ftse_all
dat_correls <- dat_correls |>
  mutate(
    gbus10y_d = (gbus10y - lag(gbus10y)) * 100,
    gbpusd_ret = (gbpusd / lag(gbpusd) - 1) * 100,
    ftse_all_ret = (ftse_all / lag(ftse_all) - 1) * 100
  )

# add rolling correlations yeld gap and gbpusd; gb10y and equities (ftse_all)
# yield gap and gbpusd----
corr_list <- list(
  gbus10y_gbpusd_r20 = list(x = "gbus10y_d", y = "gbpusd_ret", width = 20),
  gbus10y_gbpusd_r40 = list(x = "gbus10y_d", y = "gbpusd_ret", width = 40),
  gb10y_ftse_r20 = list(x = "col_20", y = "ftse_all_ret", width = 20),
  gb10y_ftse_r40 = list(x = "col_20", y = "ftse_all_ret", width = 40)
)

for (nm in names(corr_list)) {
  spec <- corr_list[[nm]]
  dat_correls[[nm]] <- zoo::rollapply(
    data = dat_correls[, c(spec$x, spec$y)],
    width = spec$width,
    FUN = function(z) cor(z[, 1], z[, 2], use = "complete.obs"),
    by.column = FALSE,
    align = "right",
    fill = NA
  )
}

# Long format for plotting
dat_correls_long <- dat_correls |>
  dplyr::select(date, matches("_r20|_r40")) |>
  pivot_longer(!date, names_to = "variable", values_to = "value")

# Plot
correl.plot.10y.ftse <- dat_correls_long |>
  filter(
    date >= as.Date('2022-06-01'),
    variable %in% c("gb10y_ftse_r20", "gb10y_ftse_r40")
  ) |>
  ggplot(
    aes(x = date, y = value, color = factor(variable))
  ) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = as.Date("2022-09-23"), lty = 4, size = 1.5) + # mini_Budget
  geom_vline(xintercept = as.Date("2024-10-30"), lty = 4, size = 1.5) + # Reeves Budget
  geom_vline(xintercept = as.Date("2025-01-07"), lty = 4, size = 1.5) + # January volatility
  geom_vline(xintercept = as.Date("2025-04-02"), lty = 4, size = 1.5) + # US Tariffs announced
  geom_vline(xintercept = as.Date("2025-07-02"), lty = 4, size = 1.5) + # PMQs 07.02.2025
  annotate(
    "text",
    x = as.Date("2022-09-23"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "mini-Budget",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2024-10-30"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Reeves\nBudget",
    hjust = 1.0,
    vjust = 0,
    angle = 0,
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2025-01-07"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Jan.\nvol.",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    fontface = "bold",
    color = "red"
  ) +
  annotate(
    "text",
    x = as.Date("2025-04-02"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "US \nTariffs",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    color = "brown"
  ) +
  annotate(
    "text",
    x = as.Date("2025-07-02"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "PMQs",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    color = "red"
  ) +
  scale_color_manual(
    values = c("red", "dodgerblue"), # Custom colors for each series
    labels = c("20d (rolling)", "40d (rolling)") # Custom legend labels
  ) +
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(
    title = "Correlation of GB 10y Yield and Equities",
    y = "correlation",
    #    caption = "Source: Bloomberg",
    color = NULL
  ) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = c(as.Date("2022-06-01"), as.Date("2025-12-30")))
correl.plot.10y.ftse

# Plot
correl.plot.10y.gbp <- dat_correls_long |>
  filter(
    date >= as.Date('2022-06-01'),
    variable %in% c("gbus10y_gbpusd_r20", "gbus10y_gbpusd_r40")
  ) |>
  ggplot(
    aes(x = date, y = value, color = factor(variable))
  ) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = as.Date("2022-09-23"), lty = 4, size = 1.5) + # mini_Budget
  geom_vline(xintercept = as.Date("2024-10-30"), lty = 4, size = 1.5) + # Reeves Budget
  geom_vline(xintercept = as.Date("2025-01-07"), lty = 4, size = 1.5) + # January volatility
  geom_vline(xintercept = as.Date("2025-04-02"), lty = 4, size = 1.5) + # US Tariffs announced
  geom_vline(xintercept = as.Date("2025-07-02"), lty = 4, size = 1.5) + # PMQs 07.02.2025
  annotate(
    "text",
    x = as.Date("2022-09-23"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "mini-Budget",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2024-10-30"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Reeves\nBudget",
    hjust = 1.0,
    vjust = 0,
    angle = 0,
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.Date("2025-01-07"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "Jan.\nvol.",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    fontface = "bold",
    color = "red"
  ) +
  annotate(
    "text",
    x = as.Date("2025-04-02"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "US \nTariffs",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    color = "brown"
  ) +
  annotate(
    "text",
    x = as.Date("2025-07-02"),
    y = min(subset(dat_correls_long, date >= "2022-06-01")$value, na.rm = TRUE),
    label = "PMQs",
    hjust = 0.0,
    vjust = 0,
    angle = 0,
    size = 4,
    color = "red"
  ) +
  scale_color_manual(
    values = c("red", "dodgerblue"), # Custom colors for each series
    labels = c("20d (rolling)", "40d (rolling)") # Custom legend labels
  ) +
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(
    title = "Correlation of GB/US 10y Yield Gap and GBPUSD",
    y = "correlation",
    #    caption = "Source: Bloomberg",
    color = NULL
  ) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = c(as.Date("2022-06-01"), as.Date("2025-12-30")))
correl.plot.10y.gbp
