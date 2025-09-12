library(ggbeeswarm)


mpc_dates <- as.Date(c(
  '2025-02-06',
  '2025-03-20',
  '2025-05-08',
  '2025-06-19',
  '2025-08-07',
  '2025-09-18',
  '2025-11-06',
  '2025-12-18',
  '2026-02-05',
  '2026-03-19',
  '2026-04-30',
  '2026-06-18',
  '2026-07-30',
  '2026-09-17',
  '2026-11-05',
  '2026-12-17'
))

# Input Data (manual)----
sellside <- data.frame(
  date = mpc_dates,
  realized = c(4.5, 4.5, 4.25, 4.25, 4.0, rep(NA, 11)),
  barc = c(rep(NA, 5), 4.0, 3.75, 4.0, 3.75, 3.75, 3.5, 3.5, rep(3.5, 4)),
  citi = c(rep(NA, 5), 4.0, 4.0, 4.0, 3.75, 3.5, 3.25, 3.25, rep(3.25, 4)),
  db = c(rep(NA, 5), 4.0, 4.0, 4.0, 3.75, 3.75, 3.5, 3.5, rep(3.25, 4)),
  gs = c(rep(NA, 5), 4.0, 3.75, 4.0, 3.75, 3.75, 3.5, 3.5, rep(3.5, 4)),
  jpm = c(rep(NA, 5), 4.0, 4.0, 4.0, 3.75, 3.5, 3.5, 3.25, rep(3.25, 4)),
  ms = c(rep(NA, 5), 4.0, 4.0, 3.75, 3.5, 3.25, 3.25, 3.0, rep(3.0, 4))
)

# Long----
sellside.long <- sellside |>
  pivot_longer((!date) & (!realized), names_to = "bank", values_to = "rate") |>
  group_by(date) |>
  mutate(
    medianR = ifelse(is.na(realized), median(rate, na.rm = TRUE), realized)
  ) |>
  ungroup() |>
  arrange(date, bank)


# Plot: Sell-side Views----
plot.sellside <- ggplot(sellside.long, aes(date, rate, color = bank)) +
  geom_line(aes(x = date, y = medianR, color = ".median sell-side")) +
  geom_line(aes(x = date, y = realized), lwd = 1, color = 'black') +
  geom_beeswarm(size = 4, corral = "wrap", corral.width = 32) +
  geom_vline(xintercept = today(), lty = 4) +
  theme(legend.title = element_blank(), legend.position = "right") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  scale_y_continuous(
    breaks = seq(
      min(sellside.long[, c('realized', 'rate')], na.rm = T),
      max(sellside.long[, c('realized', 'rate')], na.rm = T),
      by = 0.25
    )
  ) +
  labs(
    x = "Date",
    y = "BoE Bank Rate, %",
    title = "Sell-side 'Dot-plot' -- views of BoE Bank Rate",
    caption = "Sources: Various"
  )
plot.sellside

# Violin Plot----
# Prepare median data with matching date_factor
plot.data <- sellside.long |>
  mutate(date_factor = factor(date, levels = sort(unique(date))))

medians <- plot.data |>
  group_by(date) |>
  summarise(median_rate = median(rate, na.rm = TRUE), .groups = "drop") |>
  mutate(date_factor = factor(date, levels = sort(unique(plot.data$date))))

plot.sellside.violin <- ggplot(plot.data, aes(x = date_factor, y = rate)) +
  geom_violin(aes(fill = date_factor), alpha = 0.7, trim = FALSE) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text_repel(
    aes(label = bank),
    size = 3.5,
    max.overlaps = 20
  ) +
  geom_point(
    data = medians,
    aes(x = date_factor, y = median_rate),
    color = "red",
    size = 3
  ) +
  geom_point(
    data = plot.data,
    aes(y = realized),
    color = "black",
    size = 4,
    shape = 18 # Diamond shape for realized rates
  ) +
  scale_fill_viridis_d(alpha = 0.8) +
  scale_y_continuous(limits = c(2.75, 4.75), breaks = seq(2.75, 4.75, 0.25)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Meeting Date",
    y = "BoE Bank Rate, %",
    title = "Distribution of Sell-side Bank Rate Forecasts",
    subtitle = "Red dots = median forecast, Black diamonds = realized rates",
    caption = "Sources: Various"
  )
plot.sellside.violin
