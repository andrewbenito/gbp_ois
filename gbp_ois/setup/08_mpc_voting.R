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

# Clean raw voting data on Bank Rate
mpc <- clean_mpc_voting(voting)
current_members <- names(mpc)[3:11]

# Calculate member column range dynamically
member_cols <- 3:ncol(mpc) # All member columns (everything after date and bank_rate)
mpcSum <- mpc[, c("date", "bank_rate")]

mpcSum$votes_for <- rowSums(mpc[, member_cols] == mpc$bank_rate, na.rm = TRUE)
mpcSum$votes_above <- rowSums(mpc[, member_cols] > mpc$bank_rate, na.rm = TRUE)
mpcSum$votes_below <- rowSums(mpc[, member_cols] < mpc$bank_rate, na.rm = TRUE)
mpcSum$votes_against <- mpc$votes_above + mpc$votes_below
mpcSum$since_2020 <- if_else(mpc$date >= as.Date("2020-01-01"), 1, 0)

# summarise proportions for votes_for for all meetings and since 2020
# Calculate proportions for all periods
voting_proportions <- mpcSum |>
  # Create a combined dataset with "All meetings" category
  bind_rows(
    mpc |> mutate(period = "All Meetings"),
    mpc |>
      filter(date >= as.Date("2020-01-01")) |>
      mutate(period = "Since 2020")
  ) |>
  group_by(period) |>
  count(votes_for) |>
  mutate(proportion = round(n / sum(n), 3)) |>
  select(period, votes_for, proportion) |>
  pivot_wider(names_from = period, values_from = proportion, values_fill = 0) |>
  # Reorder columns
  select(votes_for, `All Meetings`, `Since 2020`)

table.voting.sum <- voting_proportions |>
  arrange(desc(votes_for)) |>
  gt() |>
  tab_header(
    title = "MPC Voting Summary",
    subtitle = "Proportion of meetings by number of members voting for Bank Rate decision"
  ) |>
  cols_label(
    votes_for = "Members Voting For",
    `All Meetings` = "All Meetings",
    `Since 2020` = "Since 2020"
  ) |>
  fmt_number(
    columns = c(`All Meetings`, `Since 2020`),
    decimals = 3
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) |>
  tab_footnote(
    footnote = "In 1998, two MPC votes were split 4-4, with an 8 member MPC.",
    locations = cells_column_labels(columns = votes_for)
  )

# Create the data for the histogram with period split
mpc_histogram_data <- mpcSum |>
  # Create a period indicator
  mutate(
    period = if_else(
      date >= as.Date("2020-01-01"),
      "Since 2020",
      "All meetings"
    )
  ) |>
  # Count votes_for by period
  count(votes_for, period) |>
  # Calculate proportions within each period
  group_by(period) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# histogram---
hist.votes <- ggplot(
  mpc_histogram_data,
  aes(x = votes_for, y = proportion, fill = period)
) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.8) +
  scale_fill_manual(
    values = c("All meetings" = "#4575b4", "Since 2020" = "#d73027")
  ) +
  scale_x_continuous(breaks = 4:9) +
  labs(
    title = "MPC Voting: A weaker Consensus",
    subtitle = "Distribution of number of members voting for Bank Rate decision",
    x = "Number of MPC Members Voting For Bank Rate Decision",
    y = "Proportion of Meetings",
    fill = " "
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  )
hist.votes

#==================================
# MPC voter-level data [pivot long]
#==================================
mpc.long <- mpc |>
  pivot_longer(
    cols = -c(date, bank_rate),
    names_to = "member",
    values_to = "vote"
  ) |>
  mutate(
    member = str_remove_all(member, "\\r\\n"), # Clean the member names
    current_member = case_when(
      member %in% current_members ~ "Current MPC",
      TRUE ~ "Past Member"
    ),
    dissent = if_else(vote != bank_rate, 1, 0),
    dissent_hawk = if_else(vote > bank_rate, 1, 0),
    dissent_dove = if_else(vote < bank_rate, 1, 0)
  ) |>
  filter(!is.na(vote)) # MPC member present at the vote
print(paste0("number of MPC meetings: ", nrow(mpc)))
print(paste0("number of MPC members: ", n_distinct(mpc.long$member)))

#==============================
# Some Disagreement Metrics ----
#==============================
# Calculate disagreement metrics per meeting
meeting_disagreement <- mpc.long |>
  summarise(
    total_voters = n(),
    total_dissents = sum(dissent),
    dissent_rate = mean(dissent),
    # Herfindahl index for vote concentration
    vote_concentration = sum((table(vote) / n())^2),
    # Vote spread (range of votes)
    vote_range = max(vote) - min(vote),
    .groups = "drop"
  )
meeting_disagreement

# Individual member voting behavior: dissent rates, hawkish/dovish tendencies
#================================#================================
member_patterns <- mpc.long |>
  mutate(
    current_member = if_else(
      member %in% current_members,
      "Current MPC",
      "Past Member"
    )
  ) |>
  group_by(member) |>
  summarise(
    total_votes = n(),
    dissent_rate = mean(dissent),
    avg_vote_deviation = mean(abs(vote - bank_rate)),
    hawkish_bias = mean(vote > bank_rate), # Tendency to vote higher
    dovish_bias = mean(vote < bank_rate), # Tendency to vote lower
    net_bias = hawkish_bias - dovish_bias,
    current_member = first(current_member),
    .groups = "drop"
  ) |>
  arrange(desc(dissent_rate))


# Rolling disagreement over time (e.g., 8-mtg windows)
disagreement_trends <- mpc.long |>
  arrange(date) |>
  group_by(date) |>
  summarise(meeting_dissent_rate = mean(dissent), .groups = "drop") |>
  mutate(
    rolling_12m_dissent = zoo::rollmean(
      meeting_dissent_rate,
      k = 8,
      fill = NA,
      align = "right"
    ),
    year = year(date)
  )
disagreement_trends

# Identify consistent voting pairs/groups
voting_correlations <- mpc |>
  select(-date, -bank_rate) |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("member1") |>
  pivot_longer(-member1, names_to = "member2", values_to = "correlation") |>
  filter(member1 != member2, !is.na(correlation)) |>
  arrange(desc(correlation))
voting_correlations

# indicate if member1 and member2 are current members
voting_correlations <- voting_correlations |>
  mutate(
    member1_current = if_else(
      member1 %in% current_members,
      "Current MPC",
      "Past Member"
    ),
    member2_current = if_else(
      member2 %in% current_members,
      "Current MPC",
      "Past Member"
    ),
    both_current_mpc = if_else(
      member1_current == "Current MPC" & member2_current == "Current MPC",
      "Both Current MPC",
      "At least one Past Member"
    )
  )

# Plot histogram of voting correlations, separately for both_current_mpc and at least one past member

library(ggplot2)
ggplot(
  voting_correlations,
  aes(x = both_current_mpc, y = correlation, fill = both_current_mpc)
) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white") +
  labs(
    title = "Distribution of Pairwise Voting Correlations",
    x = "Committee Group",
    y = "Correlation"
  )


# Plot disagreement trends
plot.disagreement.t <- ggplot(
  disagreement_trends,
  aes(x = date, y = meeting_dissent_rate)
) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.3) +
  labs(
    title = "MPC Disagreement",
    y = "Dissent Rate per Meeting",
    x = "Date"
  )
plot.disagreement.t

# plot hist of correlation

#===============================================
# Scatter plot of hawkish vs dissent tendencies
#===============================================

library(ggrepel)

# Bar-plot summary by MPC member
ggplot(
  member_patterns,
  aes(x = reorder(member, net_bias), y = net_bias, fill = bias_direction)
) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Hawkish" = "#d73027",
      "Dovish" = "#4575b4",
      "Neutral" = "#756bb1"
    )
  ) +
  labs(
    title = "MPC Member Net Bias (Hawkish - Dovish)",
    x = "Member",
    y = "Net Bias Rate",
    fill = "Overall Tendency"
  )

# Scatter plot of net bias vs dissent rate
plot.member_patterns <- ggplot(
  member_patterns,
  aes(x = net_bias, y = dissent_rate, color = current_member)
) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = member), max.overlaps = 25) +
  scale_color_manual(
    values = c("Current MPC" = "#d73027", "Past Member" = "#756bb1")
  ) +
  geom_vline(xintercept = 0, lty = 4) +
  labs(
    title = "MPC Member Bank Rate Voting",
    x = "Net Hawkish Tendency (Hawkish - Dovish)",
    y = "Dissent Rate",
    color = ""
  ) +
  theme(legend.position = "bottom")
plot.member_patterns
