# Download IMF WEO and FM data

remotes::install_github("opensdmx/rsdmx")
library(rsdmx)

sdmx <- readSDMX("WEO_PUB_APR2022.xml", isURL = FALSE)
dataset <- readSDMX("WEO_PUB_APR2025.xml")
dat <- as.data.frame(sdmx, labels = TRUE)


providers <- getSDMXServiceProviders()


imf <- findSDMXServiceProvider("IMF")


flowref <- 'IMF.STA,CPI'


# filter identifies the subset of the dataset you want.

filter <- 'USA...IX.M'


dataset <- as.data.frame(readSDMX(
  providerId = 'IMF_DATA',

  resource = 'data',

  flowRef = flowref,

  key = filter
))
