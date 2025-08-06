# GBP OIS Forward Curves and How they have Evolved
# Download BoE data from url, Tidy and Plot incl Bank Rate
# Produce (i) 'Evolving Forward Curves' (ii) 'Recent OIS Curves'
# (iii) Daily OIS

# Packages ----
lapply(
  c(
    'tidyverse',
    'readxl',
    'lubridate',
    'xts',
    'ggsci',
    'rvest',
    'stringi',
    'showtext',
    'here'
  ),
  require,
  character.only = TRUE
)

# Inputs and functions----
source(here("functions", "functions.R"))

# Settings: ggplot2 ----
font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(
  theme_bw(base_size = 13, base_family = "Roboto Condensed") +
    theme(
      text = element_text(family = "Roboto Condensed", size = 13),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(linewidth = .5),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "dodgerblue")
    )
)
showtext_auto()
#==============================
# Data: Download and Tidy  ----
#==============================
url <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/oisddata.zip"
td <- tempdir()
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url, tf, mode = "wb") # Added binary mode for Excel files

fname1 <- unzip(tf, list = TRUE)$Name[1] # 2009-2015
fname2 <- unzip(tf, list = TRUE)$Name[2] # 2016-2024
fname3 <- unzip(tf, list = TRUE)$Name[3] # 2025
df1 <- read_xlsx(unzip(tf, files = fname1, exdir = td), sheet = "1. fwd curve")
df2 <- read_xlsx(
  unzip(tf, files = fname2, exdir = td),
  sheet = "1. fwds, short end"
)
df3 <- read_xlsx(
  unzip(tf, files = fname3, exdir = td),
  sheet = "1. fwds, short end"
)

# Add Latest OIS data ----
url4 <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip?la=en&hash=89B8A093FA97EF7DD79382044E15867840E45204"
tf4 <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url4, tf4, mode = "wb")
fname4 <- unzip(tf4, list = TRUE)$Name[4]
df4 <- read_xlsx(
  unzip(tf4, files = fname4, exdir = td),
  sheet = "1. fwds, short end"
)
#======================================
# Tidy Historic Forward Curve data ----
#======================================
# Clean the 3 downloaded dataframes
for (dfn in c("df1", "df2", "df3", "df4")) {
  assign(dfn, cleanOIS(get(dfn)))
}

df <- bind_rows(df1, df2, df3, df4) # Daily OIS data for inst fwds 1-60m

# Convert Daily Data to Monthly; pivot----
dfxts <- as.xts(df)
df_m <- as.data.frame(apply.monthly(dfxts, mean))
df_m$date = as_date(rownames(df_m))
# Dates from maturities
# dates2 is eom
fwcv <- df_m %>%
  pivot_longer(
    !date,
    names_to = "tau",
    values_to = "yield",
    names_prefix = "x"
  ) %>%
  mutate(
    month = month(date),
    tau = as.numeric(tau),
    date2 = ceiling_date(as.Date(date) %m+% months(tau), unit = "month") -
      days(1)
  )

# Bank Rate from Bank of England ----
url <- 'https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy/baserate.xls'
temp <- tempfile()
download.file(url, temp, mode = "wb") # Added binary mode for Excel files
# read the data
bankrate <- read_excel(temp, sheet = "HISTORICAL SINCE 1694", skip = 995) %>%
  rename(year = 1, day = 2, month = 3, bankrate = 4) |>
  dplyr::filter(!is.na(bankrate)) |>
  fill(year, .direction = "down") |>
  fill(year, .direction = "up")

# Create date variable
bankrate <- bankrate %>%
  mutate(
    date_str = paste(day, month, year, sep = " "),
    date = as.Date(date_str, format = "%d %b %Y"),
    date2 = ceiling_date(date, unit = 'month') - days(1)
  ) %>%
  dplyr::filter(!is.na(date)) %>% # Remove rows with invalid dates
  dplyr::select(date2, bankrate) %>%
  dplyr::filter(date2 >= as.Date('2007-01-01')) %>%
  arrange(date2) # Ensure proper ordering

# monthly data frame
start_date <- floor_date(min(bankrate$date2, na.rm = TRUE), unit = "month")
end_date <- ceiling_date(Sys.Date(), unit = "month") - days(1) # end of this month
eom_dates <- seq.Date(from = start_date, to = end_date, by = "month") %>%
  ceiling_date(unit = "month") -
  days(1)
eom_df <- tibble(date2 = eom_dates)

# make monthly Bank rate df [dat] from dates of rate changes [bankrate]
dat <- eom_df %>%
  left_join(bankrate, by = "date2") %>% # Explicit join column
  arrange(date2) %>% # Ensure proper ordering for fill
  fill(bankrate, .direction = "down")

# Join with forward curve data
fwcv <- left_join(fwcv, dat, by = 'date2', relationship = "many-to-many")

#================================
# Figure 1: Evolving Forwards----
#================================
latest <- fwcv |> dplyr::filter(date == max(date))

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
  geom_line(color = "gray70", linewidth = 1.4) +
  geom_point(
    data = subset(fwcv, date == max(date)),
    aes(x = date2, y = yield),
    color = "red",
    size = 2
  ) +
  geom_line(aes(y = bankrate), linewidth = 1.25) +
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
#========================================
# Figure 3: Daily data   ----
#========================================
opt.M <- 24 # 2y rate
opt.M2 <- 60 # 5y rate
opt.M3 <- 120 # 10y rate
opt.h <- 60 # past 60d
opt.start.cumul <- 30 # cumulative changes from start of prior month

# scraped MPC and Fed announcement days [in functions.R]
url_boe <- "https://www.bankofengland.co.uk/monetary-policy/upcoming-mpc-dates"
url_fed <- "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"

recent_mpc_dates <- get_mpc_dates(url_boe)
recent_fomc_dates <- get_fomc_dates(url_fed)

# select dates within 60d of max date
recent_mpc_dates <- recent_mpc_dates[
  recent_mpc_dates >= max(df$date) - days(opt.h) &
    recent_mpc_dates <= max(df$date) + 7
]
recent_fomc_dates <- recent_fomc_dates[
  recent_fomc_dates >= max(df$date) - days(opt.h) &
    recent_fomc_dates <= max(df$date) + 7
]

# DAILY OIS DATA
#================
# 1: delta.d
# 2: delta.cumul.d
# 3: spreads.d

df <- df |> tibble::rownames_to_column("date")
df$date <- as.Date(df$date)

# 1: delta.d, create dataframes for daily changes in OIS rates
delta.d <- df |> #
  janitor::clean_names() |>
  mutate(across(-date, ~ (. - lag(.)) * 100)) |> # daily changes in bp
  filter(!is.na(date))

# 2: delta.cumul.d, cumulative changes from start of prior month
delta.cumul.d <- df |>
  janitor::clean_names() |>
  mutate(
    # Create a month-year grouping variable
    month_group = floor_date(date - months(1), "month")
  ) |>
  group_by(month_group) |>
  mutate(
    # Calculate cumulative changes since start of prior month
    across(where(is.numeric), ~ cumsum(c(0, diff(.))) * 100)
  ) |>
  ungroup() |>
  select(-month_group) |>
  filter(!is.na(date))

# 3: spreads.d - need file with LT rates
spreads.d <- df |>
  janitor::clean_names() |>
  mutate(
    # Calculate spreads between different maturities
    x2y5y = x60 - x24
  ) |>
  select(date, x2y5y) |>
  filter(!is.na(date))

plot(spreads.d)

# PLOT - past 60d
#=================
plot.daily.60d <- delta.d |>
  filter(date >= max(date) - days(opt.h)) |> # filter final opt.h observations
  ggplot(aes(x = date, y = .data[[paste0("x", opt.M)]])) +
  geom_col() +
  labs(
    title = paste0("GBP 2y OIS"),
    subtitle = paste0(opt.h, " days, daily change (bp)"),
    x = "Date",
    y = paste0("daily change ", opt.h, "days (bps)")
  ) +
  geom_vline(
    xintercept = recent_mpc_dates,
    linetype = "dashed",
    color = "darkblue",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = recent_mpc_dates,
    y = 7,
    label = "MPC",
    color = "darkblue",
    size = 5,
    angle = 0,
    vjust = 0.5,
    hjust = 0
  ) +
  geom_vline(
    xintercept = recent_fomc_dates,
    linetype = "dashed",
    color = "darkgreen",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = recent_fomc_dates,
    y = 7,
    label = "FOMC",
    color = "darkgreen",
    size = 5,
    angle = 0,
    vjust = 0.5,
    hjust = 0
  )
plot.daily.60d
ggsave(
  here("plots", "3.OIS_2y_daily.png"),
  plot = plot.daily.60d,
  width = 10,
  height = 6,
  dpi = 300
)

# PLOT - cumulative changes
plot.cumul.60d <- delta.cumul.d |>
  filter(date >= max(date) - days(opt.h)) |> # filter final opt.h observations
  ggplot(aes(x = date, y = .data[[paste0("x", opt.M)]])) +
  geom_col() +
  labs(
    title = paste0("GBP 2y OIS"),
    subtitle = paste0(opt.h, " days, cumulative change (bp)"),
    x = "Date",
    y = paste0("cumulative change ", opt.h, "days (bps)")
  ) +
  geom_vline(
    xintercept = recent_mpc_dates,
    linetype = "dashed",
    color = "darkblue",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = recent_mpc_dates,
    y = 7,
    label = "MPC",
    color = "darkblue",
    size = 5,
    angle = 0,
    vjust = 0.5,
    hjust = 0
  ) +
  geom_vline(
    xintercept = recent_fomc_dates,
    linetype = "dashed",
    color = "darkgreen",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = recent_fomc_dates,
    y = 7,
    label = "FOMC",
    color = "darkgreen",
    size = 5,
    angle = 0,
    vjust = 0.5,
    hjust = 0
  )
