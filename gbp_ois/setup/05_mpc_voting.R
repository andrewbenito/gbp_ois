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

current_members <- names(mpc)[3:11] # Current MPC members (9 columns)

# MPC votes - pivot to long format
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

# Individual member voting behavior
member_patterns <- mpc.long |>
  group_by(member) |>
  summarise(
    total_votes = n(),
    dissent_rate = mean(dissent),
    avg_vote_deviation = mean(abs(vote - bank_rate)),
    hawkish_bias = mean(vote > bank_rate), # Tendency to vote higher
    dovish_bias = mean(vote < bank_rate), # Tendency to vote lower
    .groups = "drop"
  ) |>
  arrange(desc(dissent_rate))
member_patterns

# Rolling disagreement over time (e.g., 12-month windows)
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

# Standard deviation of votes per meeting (higher = more disagreement)
policy_uncertainty <- mpc.long |>
  group_by(date, bank_rate) |>
  summarise(
    vote_std = sd(vote),
    policy_uncertainty = vote_std / mean(vote), # Coefficient of variation
    .groups = "drop"
  )
policy_uncertainty

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

# Plot disagreement trends
ggplot(disagreement_trends, aes(x = date, y = meeting_dissent_rate)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.3) +
  labs(
    title = "MPC Disagreement",
    y = "Dissent Rate per Meeting",
    x = "Date"
  )

# Scatter plot of hawkish vs dissent tendencies
library(ggrepel)
ggplot(member_patterns, aes(x = hawkish_bias, y = dissent_rate)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = member)) +
  labs(
    title = "MPC Member Voting Patterns",
    x = "Hawkish Tendency (% votes above bank rate)",
    y = "Dissent Rate"
  )

member_patterns_net <- member_patterns |>
  mutate(
    net_bias = hawkish_bias - dovish_bias,
    bias_direction = case_when(
      net_bias > 0.1 ~ "Hawkish",
      net_bias < -0.1 ~ "Dovish",
      TRUE ~ "Neutral"
    )
  )

ggplot(
  member_patterns_net,
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
  ) +
  theme_minimal()

ggplot(member_patterns_net, aes(x = net_bias, y = dissent_rate)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = member)) +
  scale_color_manual(
    values = c("Current MPC" = "#d73027", "Past Member" = "#756bb1")
  ) +
  geom_vline(xintercept = 0, lty = 4) +
  labs(
    title = "MPC Member Voting Patterns",
    x = "Hawkish Tendency (% votes above bank rate)",
    y = "Dissent Rate"
  )

ggplot(
  member_patterns_net,
  aes(x = net_bias, y = dissent_rate, color = current_member)
) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = member), max.overlaps = 20) +
  scale_color_manual(
    values = c("Current MPC" = "#d73027", "Past Member" = "#756bb1")
  ) +
  geom_vline(xintercept = 0, lty = 4) +
  labs(
    title = "MPC Member Voting Patterns",
    x = "Net Bias (Hawkish - Dovish Tendency)",
    y = "Dissent Rate",
    color = "Member Status"
  )

# need to fix current_member in mpc.long
