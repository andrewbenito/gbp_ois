# GBP OIS Forward Curves and How they have Evolved
# Download BoE data from url, Tidy and Plot incl Bank Rate

# Packages ----
lapply(
  c(
    'tidyverse',
    'readxl',
    'lubridate',
    'xts',
    'showtext',
    "data.table",
    'here'
  ),
  require,
  character.only = TRUE
)

# Settings: ggplot2 ----
font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(
  theme_bw(base_size = 12, base_family = "Roboto Condensed") +
    theme(
      text = element_text(family = "Roboto Condensed", size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(linewidth = .5),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "dodgerblue")
    )
)
showtext_auto()

# Data: Download and Tidy  ----
#==============================
url <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/oisddata.zip"
td <- tempdir()
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url, tf, mode = "wb") # Added binary mode for Excel files

fname1 <- unzip(tf, list = TRUE)$Name[1]
fname2 <- unzip(tf, list = TRUE)$Name[2]
df1 <- read_xlsx(unzip(tf, files = fname1, exdir = td), sheet = "1. fwd curve")
df2 <- read_xlsx(
  unzip(tf, files = fname2, exdir = td),
  sheet = "1. fwds, short end"
)

# Add Latest OIS data ----
url3 <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip?la=en&hash=89B8A093FA97EF7DD79382044E15867840E45204"
tf3 <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url3, tf3, mode = "wb")
fname3 <- unzip(tf3, list = TRUE)$Name[4]
df3 <- read_xlsx(
  unzip(tf3, files = fname3, exdir = td),
  sheet = "1. fwds, short end"
)

# Tidy Historic Forward Curve data ----
#======================================
cleanOIS <- function(df) {
  # Round the months row (excl date column)
  df[2, -1] <- round(df[2, -1], digits = 0)

  df <- df %>%
    set_names(slice(., 2)) %>%
    tail(-5) %>%
    type.convert(as.is = TRUE) %>%
    rename(date = 1) %>%
    mutate(date = as.Date(date, origin = "1899-12-30")) %>% # Convert Excel dates
    drop_na() %>%
    column_to_rownames(var = "date")

  return(df)
}

# Clean the 3 downloaded dataframes
for (dfn in c("df1", "df2", "df3")) {
  assign(dfn, cleanOIS(get(dfn)))
}
df <- bind_rows(df1, df2, df3) # Daily OIS data for inst fwds 1-60m

# Convert Daily Data to Monthly; pivot----
dfxts <- as.xts(df)
df_m <- as.data.frame(apply.monthly(dfxts, mean))
df_m$date = as_date(rownames(df_m))
# Dates from maturities
# dates2 is eom
fwcv <- pivot_longer(df_m, !date, names_to = "tau", values_to = "yield") %>%
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
  geom_line(aes(colour = as.factor(date)), linewidth = 1.4) +
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
