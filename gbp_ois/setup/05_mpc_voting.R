# 05_mpc_voting.R
# Imports MPC voting data to ilustrate:
# (i) extent of recent disagreement vs historical average
# (ii) voting patterns of current MPC members

# Import MPC voting Data ----
url_voting <- "https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-summary-and-minutes/mpcvoting.xlsx"
td <- tempdir()
tf <- tempfile(tmpdir = td, fileext = ".xlsx")
download.file(url_voting, tf, mode = "wb")
voting <- read_xlsx(tf, sheet = "Bank Rate Decisions")

# Clean raw voting data
mpc <- clean_mpc_voting(voting)

# MPC votes - pivot to long format
mpc.long <- mpc |>
  pivot_longer(
    cols = -c(date, bank_rate),
    names_to = "member",
    values_to = "vote"
  ) |>
  mutate(
    dissent = if_else(vote != bank_rate, 1, 0)
  )
