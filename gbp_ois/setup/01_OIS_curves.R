# GBP OIS Forward Curves and How they have Evolved
# Download BoE data from url, Tidy and Plot

# Packages ----
lapply(
  c('tidyverse', 'readxl', 'lubridate', 'xts', 'showtext', 'dbnomics'),
  require,
  character.only = TRUE
)

# Settings: ggplot2 ----
font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(
  theme_bw(base_size = 15, base_family = "Roboto Condensed") +
    theme(
      text = element_text(family = "Roboto Condensed", size = 15),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(linewidth = .5),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "dodgerblue")
    )
)
showtext_auto()

# Data: Download and Tidy  ----
url <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/oisddata.zip"
td <- tempdir()
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url, tf)
fname1 <- unzip(tf, list = TRUE)$Name[1]
fname2 <- unzip(tf, list = TRUE)$Name[2]
df1 <- read_xlsx(unzip(tf, files = fname1, exdir = td), sheet = "1. fwd curve")
df2 <- read_xlsx(
  unzip(tf, files = fname2, exdir = td),
  sheet = "1. fwds, short end"
)

# Latest OIS data ----
url3 <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip?la=en&hash=89B8A093FA97EF7DD79382044E15867840E45204"
tf3 <- tempfile(tmpdir = td, fileext = ".zip")
download.file(url3, tf3, mode = "wb")
fname3 <- unzip(tf3, list = TRUE)$Name[4]
df3 <- read_xlsx(
  unzip(tf3, files = fname3, exdir = td),
  sheet = "1. fwds, short end"
)

# Tidy Historic Forward Curve data ----
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
fwcv <- pivot_longer(df_m, !date, names_to = "tau", values_to = "yield") %>%
  mutate(
    month = month(date),
    tau = as.numeric(tau),
    date2 = ymd(as.Date(date) %m+% months(tau) - 1)
  )


# Figure: Evolving Forwards----
p1 <- ggplot(fwcv, aes(x = date2, y = yield, group = date)) +
  geom_line(aes(colour = as.factor(date))) +
  theme(legend.position = "none") +
  labs(
    title = "GBP OIS Curves",
    subtitle = "monthly averages of end-of-day daily data",
    x = "date",
    y = "rate %",
    caption = "Source: Bank of England data"
  )
# save plot
ggsave(
  filename = file.path("gbp_ois", "plots", "1.GBP-OIS.png"),
  plot = p1,
  width = 9,
  height = 5,
  dpi = 90,
  units = "in",
  device = 'png'
)

# Recent data
comp.date <- last(fwcv$date) %m-% months(12)

p2 <- ggplot(
  subset(fwcv, date >= as.Date(comp.date)),
  aes(x = date2, y = yield, group = date)
) +
  geom_line(aes(colour = as.factor(date))) +
  theme(legend.position = "none") +
  labs(
    title = "GBP OIS Curves: Past 12 months",
    subtitle = "monthly averages of end-of-day daily data",
    x = "date",
    y = "rate %",
    caption = "Source: Bank of England data"
  )
ggsave(
  filename = file.path("gbp_ois", "plots", "2.GBP-OIS_12m.png"),
  plot = p2,
  width = 9,
  height = 5,
  dpi = 90,
  units = "in",
  device = 'png'
)

# save data
save(fwcv, file = file.path("gbp_ois", "data", "data.Rda"))
