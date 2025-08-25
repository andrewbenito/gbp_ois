# 04_Rigobon.R

# Estimate VAR for Asset Prices (eg 10y rates)
# Construct Historical Decomposition of Shocks

library(fredr)
library(tidyverse)
library(lubridate)
library(xts)
library(vars)
library(svars)

# SETTINGS
startDate <- as.Date("2015-01-01", format = "%Y-%m-%d")
fredr_set_key("14447b2e57e05e5bde5dfc65dd0f5fd3")

# FRED series codes for 10-year government bond yields:
bond_series <- c(
  "US" = "GS10", # US 10-Year Treasury
  "UK" = "IRLTLT01GBM156N", # UK 10-Year Gilt
  "Germany" = "IRLTLT01DEM156N", # German 10-Year Bund
  "Japan" = "IRLTLT01JPM156N" # Japan 10-Year Bond
)

# Function to get bond data
get_bond_yields <- function() {
  map_dfr(
    names(bond_series),
    ~ {
      fredr(
        bond_series[.x],
        frequency = "m", # Monthly frequency [highest for these codes]
        observation_start = as.Date(startDate)
      ) %>%
        mutate(country = .x) %>%
        select(date, country, yield = value)
    }
  )
}

# Get all bond yields
bond_yields <- get_bond_yields()

# View the data
bond_yields %>%
  group_by(country) %>%
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    latest_yield = last(yield),
    .groups = "drop"
  )

# Plot
ggplot(bond_yields, aes(x = date, y = yield, color = country)) +
  geom_line() +
  geom_point() +
  scale_color_jco() + # Add JCO color palette
  labs(
    title = "Government Bond Yields (10-year)",
    #    subtitle = "Monthly data from FRED",
    x = "Date",
    y = "Yield (%)",
    color = NULL
  )

df <- bond_yields |> select(-contains('date'))
df_wide <- bond_yields |>
  pivot_wider(!date, names_from = country, values_from = yield)

# Plot Raw Data
#================
df_long <- df_withDate %>%
  pivot_longer(
    cols = starts_with('US'):starts_with('GTJPY'),
    names_to = "Bond",
    values_to = "value"
  )

ggplot(subset(df_long, date >= startDate), aes(date, value, color = Bond)) +
  geom_point() +
  geom_line() +
  ggtitle("10y Bond Yields") +
  theme(legend.title = element_blank(), legend.position = "bottom")

# Estimate VAR, HistDecomps
v1 <- vars::VAR(df, lag.max = 4, type = "both", ic = "AIC")
summary(v1)

# sbDate
sbDate <- lubridate::ymd("2020-03-01")
nSB <- nrow(df_withDate[df_withDate$date <= lubridate::ymd(sbDate), ])
EA.cv <- id.cv(v1, SB = nSB) # changes in volatility;
summary(EA.cv)

# Put HistDecomp into x1-x4 (Lists)
for (i in 1:k) {
  assign(paste0("x", i, sep = ""), hd(EA.cv, series = i))
}

# Create DFs with HistDecomps, Clean
hdlist <- list(x1[['hidec']], x2[['hidec']], x3[['hidec']], x4[['hidec']])
for (i in 1:length(hdlist)) {
  assign(paste("temp", i, sep = ''), as.data.frame(hdlist[i]))
}

#=====

# Clean function, then apply
cleanDF <- function(df) {
  df <- df %>%
    rename('US' = 4, 'GER' = 5, 'UK' = 6, 'JP' = 7) %>%
    dplyr::select(-1, -2, -3) %>%
    tibble::rownames_to_column("date") %>%
    mutate(date = lubridate::ymd(date))
  return(df)
}

templist <- list(temp1, temp2, temp3, temp4) # These are HDs of contributions for each 10y rate
# Add date
for (i in 1:length(templist)) {
  assign(paste("temp", i, sep = ''), cleanDF(templist[[i]]))
}

df_long1 <- temp1 %>%
  pivot_longer(cols = 'US':'JP', names_to = 'shock', values_to = 'value')
df_long2 <- temp2 %>%
  pivot_longer(cols = 'US':'JP', names_to = 'shock', values_to = 'value')
df_long3 <- temp3 %>%
  pivot_longer(cols = 'US':'JP', names_to = 'shock', values_to = 'value')
df_long4 <- temp4 %>%
  pivot_longer(cols = 'US':'JP', names_to = 'shock', values_to = 'value')


# plots
ggplot(df_long1, aes(fill = shock, y = value, x = date)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("US 10y TY Decomposition") +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggplot(df_long2, aes(fill = shock, y = value, x = date)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("GER 10y Bund Yields Decomposition") +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggplot(df_long3, aes(fill = shock, y = value, x = date)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("10Y Gilt Yield Decomposition") +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggplot(df_long4, aes(fill = shock, y = value, x = date)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("JP 10y Bond Decomposition") +
  theme(legend.title = element_blank(), legend.position = "bottom")
