# 01_create_data.R
rm(list = ls())
lapply(
  c(
    'here',
    'tidyverse',
    'janitor',
    'data.table',
    'openxlsx',
    'MacrobondAPI',
    'scales',
    'patchwork',
    'zoo',
    'gt',
    'gtExtras',
    'stargazer',
    'ggsci'
  ),
  require,
  character.only = TRUE
)

# Settings ----
here::here()
source(here('functions', 'functions.R'))
macro.start <- '19960101/'

#===============
# 1. IMPORT DATA
#===============
# A. create Quarterly macro data
map.var <- read.xlsx(here('data', 'inputcodes.xlsx')) %>% data.table(.)
vars.idx <- map.var[['Ticker']]
labels <- map.var[['Label']]
macro.data <- build.mb.dataset(vars.idx, labels, freq = "Quarterly") %>%
  .[macro.start] %>%
  as.data.frame()
macro.data <- macro.data |>
  mutate(
    date = as.Date(row.names(macro.data)),
    D.bankrate = c(NA, diff(bankr)),
    ratecut = (D.bankrate < 0),
    ratehike = (D.bankrate > 0),
    corecpi.infl = c(rep(NA, 4), 100 * diff(log(core_cpi), 4)),
    cpi.infl = c(rep(NA, 4), 100 * diff(log(cpi), 4)),
    vr = 100 * (v / E), # quarterly from monthly
    v_u = 3 * vr / ur
  )
# A2: create Monthly Macro data
map.var <- read.xlsx(here('data', 'inputcodes.xlsx')) %>% data.table(.)
vars.idx <- map.var[['Ticker']]
labels <- map.var[['Label']]
macro.data.mon <- build.mb.dataset(vars.idx, labels, freq = "Monthly") %>%
  .[macro.start] %>%
  as.data.frame()
macro.data.mon <- macro.data.mon |>
  mutate(
    date = as.Date(row.names(macro.data.mon)),
    mpr = (month(date) %in% c(2, 5, 8, 11)),
    D.bankrate = c(0.25, diff(bankr)), # hike at first meeting
    ratecut = (D.bankrate < 0),
    ratehike = (D.bankrate > 0),
    mpcdecision = case_when(
      D.bankrate > 0 ~ "Hike",
      D.bankrate == 0 ~ "Hold",
      D.bankrate < 0 ~ "Cut"
    ),
    corecpi.infl = c(rep(NA, 12), 100 * diff(log(core_cpi), 12)),
    cpi.infl = c(rep(NA, 12), 100 * diff(log(cpi), 12)),
    vr = 100 * (v / E),
    v_u = vr / ur
  )

# B: MPC Voting Data [BoE spreadsheet: mpcvoting.xlsx]
raw.data <- readxl::read_xlsx(here('data', 'mpcvoting.xlsx'), skip = 2)
# Clean data
data = list(
  rates = data.frame(raw.data),
  dissents = data.frame(),
  votes = data.frame()
)

# Clean MPC Voting data
# data$rates
data[['rates']] <- data[['rates']] |>
  remove_empty(c("rows", "cols")) |>
  clean_names() |>
  select(-1) |>
  tail(-7) |>
  rename(date = "x2", bankrate = "current_members") |> # ad hoc
  mutate(across(
    where(is.character),
    ~ case_when(
      . == "Increase" ~ "0.0025",
      . == "Decrease" ~ "-0.0025",
      TRUE ~ as.character(.)
    )
  )) |>
  mutate(across(where(is.character), ~ 100 * parse_number(.))) |>
  mutate(date = excel_numeric_to_date(date / 100), date_system = "modern") |>
  mutate(across(
    where(is.numeric),
    ~ if_else(.x < 0.5 & date <= as.Date('1999-01-01'), .x + bankrate, .x)
  )) |>
  mutate(
    de_anne_julius = if_else(date == as.Date('1998-06-04'), 7.0, de_anne_julius)
  ) |> # DEJ votes to cut when MPC hikes
  dplyr::select(-date_system)

# data$dissents
data[['dissents']] <- data[['rates']] |>
  mutate(across(
    where(is.numeric),
    ~ case_when(
      . == bankrate ~ "agree",
      . > bankrate ~ "dissent-higher",
      . < bankrate ~ "dissent-lower"
    )
  ))
data[['dissents']]$bankrate <- data[['rates']]$bankrate

# first dissent [MPC member]
data[['dissents.first']] <- data[['dissents']] |>
  mutate(
    across(contains("_"), ~ factor(.)),
    across(
      contains("_"),
      ~ case_when(
        .x == "dissent-higher" & lag(.x) != "dissent-higher" ~
          "first-dissent.H",
        .x == "dissent-lower" & lag(.x) != "dissent-lower" ~ "first-dissent.L",
        .x == "agree" | !is.na(.x) ~ "not.first.dissent"
      )
    )
  )


# data$Nvotes [meetings]
data[['Nvotes']] <- data[['dissents']] |>
  rowwise() |>
  mutate(
    n.Agree = sum(c_across(contains("_")) == "agree", na.rm = TRUE),
    n.Dissent = sum(c_across(contains("_")) != "agree", na.rm = TRUE),
    n.Higher = sum(c_across(contains("_")) == "dissent-higher", na.rm = TRUE),
    n.Lower = -1 * sum(c_across(contains("_")) == "dissent-lower", na.rm = TRUE)
  ) |>
  dplyr::select(date, bankrate, starts_with("n."))

#data$votechange [meetings]
data[['votechange']] <- data[['rates']] |>
  mutate(across(contains("_"), ~ . - lag(.))) |>
  rowwise() |>
  mutate(
    n.Hikes = sum(c_across(contains("_")) > 0.0, na.rm = TRUE),
    n.Cuts = sum(c_across(contains("_")) < 0.0, na.rm = TRUE),
    n.Unch = sum(c_across(contains("_")) == 0.0, na.rm = TRUE)
  )

# data$votehike [mpc member-level]
data[['votehike']] <- data[['rates']] |>
  mutate(across(
    contains("_"),
    ~ case_when(
      . == lag(., 1) ~ "unch",
      . > lag(., 1) ~ "hike",
      . < lag(., 1) ~ "cut"
    )
  ))
data[['votechange']]$bankrate <- data[['rates']]$bankrate

#======================

# BoE RATE CYCLES

#======================
# cutting and hiking cycles
data[['cutting']] <- data[['rates']] |>
  dplyr::select(c(date, bankrate)) |>
  mutate(
    cycle = bankrate < (lag(bankrate, 1)) |
      (bankrate < lag(bankrate, 2)) |
      (bankrate < lag(bankrate, 3)),
    cycle = replace_na(cycle, FALSE)
  ) |>
  mutate(
    group = as.factor(cumsum(cycle != lag(cycle, default = first(cycle))))
  ) |>
  group_by(group) |>
  mutate(run.count = row_number()) |>
  ungroup() |>
  dplyr::filter(cycle == TRUE) |>
  dplyr::select(-cycle) |>
  # drop last 2 obs each cycle
  group_by(group) |>
  slice(-tail(row_number(), 2)) |>
  group_by(group) |>
  mutate(start.date = as.factor(date[1]), end.date = max(date))

data[['hiking']] <- data[['rates']] |>
  dplyr::select(c(date, bankrate)) |>
  mutate(
    cycle = bankrate > (lag(bankrate, 1)) |
      (bankrate > lag(bankrate, 2)) |
      (bankrate > lag(bankrate, 3)),
    cycle = replace_na(cycle, TRUE)
  ) |>
  mutate(
    group = as.factor(cumsum(cycle != lag(cycle, default = first(cycle))))
  ) |>
  group_by(group) |>
  mutate(run.count = row_number()) |>
  ungroup() |>
  dplyr::filter(cycle == TRUE) |>
  dplyr::select(-cycle) |>
  # drop last 2 obs each cycle
  group_by(group) |>
  slice(-tail(row_number(), 2)) |>
  group_by(group) |>
  mutate(start.date = as.factor(date[1]), end.date = max(date))

# more ad hoc cases
data[['cutting']] <- data[['cutting']] |>
  group_by(group) |>
  mutate(
    D.bankrate = c(-0.25, diff(bankrate)),
    D.bankrate = if_else(date == '2020-03-11', -0.50, D.bankrate),
    Cy.start = bankrate[1],
    Cy.bankrate = cumsum(D.bankrate)
  )
data[['hiking']] <- data[['hiking']] |>
  group_by(group) |>
  mutate(
    D.bankrate = c(0.25, diff(bankrate)),
    D.bankrate = if_else(date == '2021-12-16', 0.15, D.bankrate),
    Cy.start = bankrate[1],
    Cy.bankrate = cumsum(D.bankrate)
  )

# select on minimum length of '2'
data[['cutting']] <- data[['cutting']] |>
  group_by(group) |>
  mutate(max.length = max(run.count)) |>
  filter(max.length >= 2)
data[['hiking']] <- data[['hiking']] |>
  group_by(group) |>
  mutate(max.length = max(run.count)) |>
  filter(max.length >= 2)

# Cutting Cycles
ggplot(
  data = data[['cutting']],
  aes(x = run.count, y = Cy.bankrate, colour = start.date)
) +
  geom_line() +
  geom_point() +
  scale_color_discrete() +
  labs(
    title = "BoE: Bank Rate cutting cycles",
    x = "nth meeting from start date",
    y = "change from peak, pp",
    color = "start date:",
    caption = "Sources: BoE, Eisler Capital"
  ) +
  theme(legend.position = "bottom")

# Hiking cycles
ggplot(
  data = data[['hiking']],
  aes(x = run.count, y = Cy.bankrate, colour = start.date)
) +
  geom_line() +
  geom_point() +
  scale_color_discrete() +
  labs(
    title = "BoE: Bank Rate hiking cycles",
    x = "nth meeting from start date",
    y = "change from peak, pp",
    color = "start date:",
    caption = "Sources: BoE, Eisler Capital"
  ) +
  theme(legend.position = "bottom")


# Chart
# [1] Ch Bank Rate and PMIs
# [2] Ch Bank Rate amd Core Infl
p1 <- ggplot(data = macro.data, aes(x = date)) +
  geom_line(aes(y = D.bankrate))
p2 <- ggplot(data = macro.data, aes(x = date)) +
  geom_line(aes(y = pmic))
p1 + p2

p3 <- ggplot(data = macro.data, aes(x = date)) +
  geom_line(aes(y = D.bankrate))
p4 <- ggplot(data = macro.data, aes(x = date)) +
  geom_line(aes(y = corecpi.infl))
p3 + p4


# tab Meetings by Dissent
tabyl(data[['Nvotes']][['n.Dissent']])
tabyl(data[['Nvotes']][['n.Higher']])
tabyl(data[['Nvotes']][['n.Lower']])


# Policy Decisions by MPR/IR
mpr.proportion <- macro.data.mon |>
  filter(!is.na(mpcdecision)) |>
  group_by(mpr, mpcdecision) |>
  summarise(count = n()) |>
  mutate(percent = count / sum(count) * 100)

ggplot(mpr.proportion, aes(x = mpcdecision, y = percent, fill = mpr)) +
  geom_bar(stat = "identity", width = 0.7, position = "dodge") +
  scale_x_discrete(limits = c("Cut", "Hold", "Hike")) +
  labs(
    title = "MPC Decision by whether an MPR/IR mtg or not",
    subtitle = "Proportion of MPC Decisions",
    fill = "MPR / IR meeting:",
    x = "Decision",
    y = "per cent"
  ) +
  theme(legend.position = "bottom")
