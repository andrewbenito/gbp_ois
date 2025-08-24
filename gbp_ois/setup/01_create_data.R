# GBP OIS Forward Curves and How they have Evolved
# Download BoE data from url, Tidy and Plot incl Bank Rate
# Produce (i) 'Evolving Forward Curves' (ii) 'Recent OIS Curves', both monthly data
# (iii) Daily OIS data

# Packages ----
lapply(
  c(
    'tidyverse',
    'readxl',
    'lubridate',
    'zoo',
    'ggrepel',
    'gt',
    'xts',
    'ggsci',
    'rvest',
    'stringi',
    'showtext',
    'patchwork',
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

latest <- fwcv |> dplyr::filter(date == max(date))


#===================================
# GLC data (Gilt yields)
#===================================
# historical Gilt yields from Bank of England
# https://www.bankofengland.co.uk/statistics/yield-curves
url <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/glcnominalddata.zip"
td <- tempdir()
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url, tf, mode = "wb") # Added binary mode for Excel files

# count files in zip
N <- length(unzip(tf, list = TRUE)$Name)
fname1 <- unzip(tf, list = TRUE)$Name[N - 2] # 2005-15; 6m-25y
fname2 <- unzip(tf, list = TRUE)$Name[N - 1] # 2016-24; 6m-40y
fname3 <- unzip(tf, list = TRUE)$Name[N] # 2025-      ; 6m-40y

# Read all files using a loop or map function
sheet_name <- "2. fwd curve"
file_names <- c(fname1, fname2, fname3)

glc_list <- list()
for (i in seq_along(file_names)) {
  glc_list[[i]] <- read_xlsx(
    unzip(tf, files = file_names[i], exdir = td),
    sheet = sheet_name
  )
}
# Assign to individual variables if needed
glc1 <- glc_list[[1]]
glc2 <- glc_list[[2]]
glc3 <- glc_list[[3]]

# Add Latest GLC data ----
url_latest <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip"
td <- tempdir()
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url_latest, tf, mode = "wb")
fname1 <- unzip(tf, list = TRUE)$Name[2] # GLC Nominal daily data current month.xlsx
glc_latest <- read_xlsx(
  unzip(tf, files = fname1, exdir = td),
  sheet = "2. fwd curve"
)

# Clean GLC downloaded dataframes
for (dfn in c("glc1", "glc2", "glc3", "glc_latest")) {
  assign(dfn, cleanGLC(get(dfn)))
}
glc <- bind_rows(glc1, glc2, glc3, glc_latest) # Daily GLC data
# add date column from rownames
glc <- glc |>
  tibble::rownames_to_column("date") |>
  mutate(date = as.Date(date)) |>
  select(date, everything()) # Ensure date is the first column

# spreads: 2y5y, 2y10y, 5y10, 10y30y
glcspreads <- glc |>
  mutate(
    spread2s5s = col_10 - col_4,
    spread2s10s = col_20 - col_4,
    spread5s10s = col_20 - col_10,
    spread10s25s = col_50 - col_20,
    spread10s30s = col_60 - col_20
  )

#========================================
# Figure 3: Daily data   ----
#========================================
opt.M <- 24 # 2y rate
opt.M2 <- 60 # 5y rate
opt.M3 <- 120 # 10y rate
opt.h <- 60 # past 60d
opt.start.cumul <- 30

# scraped MPC and Fed announcement days [in functions.R]
url_boe <- "https://www.bankofengland.co.uk/monetary-policy/upcoming-mpc-dates"
url_fed <- "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"

recent_mpc_dates <- get_mpc_dates(url_boe)
recent_fomc_dates <- get_fomc_dates(url_fed)

# DAILY OIS DATA
#================
# 1: delta.d
# 2: delta.cumul.d
# 3: spreads -> glc and glcspreads

df <- df |> tibble::rownames_to_column("date")
df$date <- as.Date(df$date)

# 1: delta.d, create dataframes for daily changes in OIS rates
delta.d <- df |> #
  janitor::clean_names() |>
  mutate(across(-date, ~ (. - lag(.)) * 100)) |> # daily changes in bp
  filter(!is.na(date))

# 2: delta.cumul.d, cumulative changes over past 60 days using cumsum
opt.h <- 60

delta.cumul.d <- df |>
  janitor::clean_names() |>
  arrange(date) |>
  # Take only the last opt.h rows
  slice_tail(n = opt.h) |>
  mutate(
    # Calculate daily changes and cumulative sum in one step
    across(where(is.numeric), ~ cumsum((. - lag(., default = first(.))) * 100))
  ) |>
  filter(!is.na(date))


# Long format for cumulative changes
delta.cumul.long <- delta.cumul.d |>
  pivot_longer(
    cols = starts_with("x"),
    names_to = "maturity",
    values_to = "cumulative_change"
  ) |>
  mutate(maturity = as.numeric(str_remove(maturity, "x")))
