# Mon Pol Shocks using Braun et al data

lapply(
  c(
    'here',
    'tidyverse',
    'readxl',
    'ggtext',
    'ggsci',
    'patchwork',
    'ggbeeswarm',
    'sysfonts',
    'ggExtra',
    'showtext',
    'gt',
    'gtExtras',
    'janitor',
    'hablar'
  ),
  require,
  character.only = TRUE
)

# Mon pol factors
# DATA: Braun et al 2024
event.factors <- read_excel(
  here::here(
    'data',
    'measuring-monetary-policy-in-the-uk-the-ukmpesd_may_2025.xlsx'
  ),
  sheet = "factors"
) |>
  rename(date = Datetime) |>
  pivot_longer(!date, names_to = "monetary_factor", values_to = "effect")

event.factors <- event.factors |>
  filter(!monetary_factor == "isMPC")

event.factors.wide <- read_excel(
  here::here(
    'data',
    'measuring-monetary-policy-in-the-uk-the-ukmpesd_may_2025.xlsx'
  ),
  sheet = "factors"
) |>
  rename(date = Datetime)

MPC.factors <- read_excel(
  here::here(
    'data',
    'measuring-monetary-policy-in-the-uk-the-ukmpesd_may_2025.xlsx'
  ),
  sheet = "factors"
) |>
  rename(date = Datetime) |>
  pivot_longer(!date, names_to = "monetary_factor", values_to = "effect")

event.surprises <- read_excel(
  here::here(
    'data',
    'measuring-monetary-policy-in-the-uk-the-ukmpesd_may_2025.xlsx'
  ),
  sheet = "surprises"
)

# Plot Monetary Factors
ordered <- c("Target", "Path", "QE")
event.factors$monetary_factor <- factor(
  event.factors$monetary_factor,
  levels = ordered
)


p1 <- ggplot(event.factors) +
  geom_col(aes(x = date, y = effect, color = monetary_factor)) +
  facet_wrap(~monetary_factor) +
  labs(
    y = "effect, pp",
    title = "Evolving BoE Monetary Policy Factors",
    subtitle = "Monetary policy factors, at each MPC announcement event",
    caption = "Source: UK Monetary Policy Event Study Database (UKMPD)"
  ) +
  theme(legend.position = "none")

# Zooming in - recent past
datecusp <- as.Date('2020-01-01')
datecuspY <- year(datecusp)

p2 <- ggplot(filter(event.factors, date >= datecusp)) +
  geom_col(aes(x = date, y = effect, color = monetary_factor)) +
  facet_wrap(~monetary_factor) +
  labs(
    y = "effect, pp",
    title = glue::glue("Monetary policy factors since {datecuspY}"),
    caption = "Source: UK Monetary Policy Event Study Database (UKMPD)"
  ) +
  theme(legend.position = "none")

# take past 8 MPC meetings and summarise the market reactions
#=============================================================
n.events <- 12

dat <- event.surprises |>
  tail(n.events) |>
  dplyr::select(
    "date" = Datetime,
    starts_with("SON") |
      starts_with("GB") |
      starts_with(".FT") |
      starts_with("EUR")
  ) |>
  mutate(date = as.Date(date)) |>
  janitor::clean_names() |>
  rename(gbpusd = gbp)
names(dat) <- str_remove_all(names(dat), "_rr")


dat.long <- dat |>
  dplyr::select(
    date,
    gbp1yois,
    gbp2yois,
    gb2yt,
    gb5yt,
    gb10yt,
    gbpusd,
    eurgbp,
    ftse
  ) |>
  pivot_longer(!date, names_to = "variable", values_to = "value")

dat.long$variable <- factor(
  dat.long$variable,
  levels = c(
    "gbp1yois",
    "gbp2yois",
    "gb2yt",
    "gb5yt",
    "gb10yt",
    "gbpusd",
    "eurgbp",
    "ftse"
  )
)
# Plot
#-------
plot.reactions <- ggplot(dat.long, aes(x = value, y = date)) +
  # Add horizontal segment from zero to value
  geom_segment(
    aes(x = 0, xend = value, y = date, yend = date, color = value > 0),
    linewidth = 1
  ) +
  # Add dots
  geom_point(aes(color = value > 0), size = 3) +
  # Vertical reference line at x = 0
  geom_vline(xintercept = 0.0, lty = 4) +
  # Facet by variable
  facet_wrap(~variable) +
  scale_color_jco() +
  theme(legend.position = "none") +
  labs(
    title = "Asset price reactions at MPC meetings",
    x = "% or pp",
    caption = "Sources: Braun et al (2025) and own calculations"
  )
plot.reactions

# Market reactions
#===================
