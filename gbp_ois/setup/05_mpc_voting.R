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

# Clean MPC voting data ----
clean_mpc_voting <- function(df) {
  df_clean <- df |>
    janitor::clean_names() |>
    mutate(
      date = as.Date(date),
      decision = as.numeric(decision),
      vote_for = as.numeric(vote_for),
      vote_against = as.numeric(vote_against),
      total_votes = as.numeric(total_votes),
      dissenters = as.numeric(dissenters)
    ) |>
    filter(!is.na(date)) |>
    arrange(date)
  return(df_clean)
}
