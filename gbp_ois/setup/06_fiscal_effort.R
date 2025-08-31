# 06_fiscal_effort.R

# Estimate required fiscal, cross-country
# Calculate Debt Stabilising Primary Balance for different combinations of r, g

# PB = [(Rt – Gt) / (1 + Gt)] dt-1 + sfat

# Rt: effective nominal interest rate on government debt,
# Gt is the nominal growth rate, dt is the debt to GDP ratio, and sfat is stock-flow adjustments
#=================================================================================================
library(gt)
library(tidyverse)
library(here)

# Set initial values
sfa <- 0 # implicitly assumed, so ignored

# set up range for r and g
r <- seq(1.5, 4.5, 0.5)
g <- seq(2.0, 4.0, 0.5)

grid <- data.frame(expand.grid(
  r = r,
  g = g,
  d = 100
))

# debt stabilising primary balance
grid <- grid |>
  mutate(pb = (((r / 100) - (g / 100)) / (1 + (g / 100))) * d)

# Pivot to create table
dat <- grid |>
  dplyr::select(r, g, pb) |>
  pivot_wider(names_from = r, values_from = pb)

# format dat
gt.dat <- dat |>
  gt() |>
  fmt_number(
    decimals = 2
  ) |>
  fmt_number(
    columns = g,
    decimals = 1 # change g column to 1 digits
  ) |>
  tab_options(
    table.font.size = "small"
  ) |>
  # First apply column-wise coloring
  data_color(
    columns = -g,
    method = "numeric",
    palette = "inferno",
    direction = "column",
    reverse = TRUE
  ) |>
  cols_label(
    g = md("**g**")
  ) |>
  tab_header(
    title = "Required, Debt-stabilising, Primary Balance (% GDP)", #
    subtitle = md("**effective nominal interest rate, r**")
  ) |>
  tab_footnote(
    footnote = "Note: Calculations assume Debt/GDP at 100% and no stock-flow adjustments, for different combinations of 'r' and 'g'.",
    placement = "left"
  )
print(gt.dat)


#===========================================================
# Cross-country data: d, r, g for required pb and actual pb
#===========================================================

dat <- read_csv(here('data', 'IMF_FM_WIDEF.csv')) |> # time-series data are wide
  t()

dat <- clean_IMF_FM(dat) |>
  arrange(country, year)
