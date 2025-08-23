# 04_Rigobon.R

# Estimate VAR for Asset Prices (eg 10y rates)
# Construct Historical Decomposition of Shocks

library(tidyverse)
library(lubridate)
library(xts)
library(vars)
library(svars)
#library(Rblpapi)

# This Project;
here::here()

# SETTINGS
startDate <- as.Date("2019-01-01", format = "%Y-%m-%d")

# Download BBG Data:
dataSec <- c(
  "USGG10YR Index", # US
  "GTDEM10Y Govt", # GER
  "GTGBP10Y Govt", # UK
  "GTJPY10Y Govt" # JP
)
k = length(dataSec)

dfSec <- bdh(dataSec, "PX_LAST", startDate) # List
xtsSec <- lapply(dfSec, function(d) xts(d[, -1], order.by = as.Date(d[, 1])))
E1 <- do.call(merge.zoo, xtsSec)
df_withDate <- data.frame(date = index(E1), coredata(E1)) %>%
  drop_na()
rownames(df_withDate) <- df_withDate$date
df <- df_withDate %>% dplyr::select(-contains('date'))

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


#===== Scratch

# 10y rate Labels
# Store DFs in List
dfList <- lapply(1:k, function(x) eval(parse(text = paste0("df_long", x)))) # Data
names(dfList) <- lapply(1:k, function(x) paste0("df_long", x)) # DF names
dfLabel <- c(
  "df_long1" = "US 10Y TY",
  "df_long2" = "GER 10Y Bund Yield",
  "df_long3" = "10Y Gilt Yield",
  "df_long4" = "JPY 10Y Yield"
) # Chart Titles

# plots
for (i in seq(1, length(dfList))) {
  df_i <- dfList[[i]]
  g <- ggplot(df_i, aes(fill = shock, y = value, x = date)) +
    geom_bar(position = "stack", stat = "identity") +
    theme(legend.title = element_blank(), legend.position = "bottom")
  ggsave(g, file = paste0(names(dfLabel)[i], "_g.png"))
}
