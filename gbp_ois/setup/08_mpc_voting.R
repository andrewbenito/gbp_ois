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

# Create mpcSum summary dataframe
mpcSum <- mpc |>
  rowwise() |>
  mutate(
    # Get all voting columns (exclude date and bank_rate)
    votes = list(c_across(-c(date, bank_rate))),
    # Remove NA values from votes
    valid_votes = list(votes[!is.na(votes)]),
    # Count votes for different outcomes
    votes_for = sum(valid_votes == bank_rate, na.rm = TRUE),
    votes_for_higher = sum(valid_votes > bank_rate, na.rm = TRUE),
    votes_for_lower = sum(valid_votes < bank_rate, na.rm = TRUE),
    total_votes = length(valid_votes)
  ) |>
  dplyr::select(
    date,
    bank_rate,
    votes_for,
    votes_for_higher,
    votes_for_lower,
    total_votes
  ) |>
  ungroup()

# summarise proportions for votes_for for all meetings and since 2020
all_meetings <- mpcSum %>%
  count(votes_for, name = "n_meetings") %>%
  mutate(proportion_all = n_meetings / sum(n_meetings)) %>%
  dplyr::select(votes_for, proportion_all)

# Calculate proportions since 2020
since_2020 <- mpcSum %>%
  filter(date >= as.Date("2020-01-01")) %>%
  count(votes_for, name = "n_meetings") %>%
  mutate(proportion_since_2020 = n_meetings / sum(n_meetings)) %>%
  dplyr::select(votes_for, proportion_since_2020)

# Combine and create the table
voting_proportions_fixed <- tibble(votes_for = 4:9) %>%
  left_join(all_meetings, by = "votes_for") %>%
  left_join(since_2020, by = "votes_for") %>%
  mutate(
    proportion_all = replace_na(proportion_all, 0),
    proportion_since_2020 = replace_na(proportion_since_2020, 0)
  ) %>%
  rename(
    `All Meetings` = proportion_all,
    `Since 2020` = proportion_since_2020
  )

# the gt table
table.voting.sum <- voting_proportions_fixed %>%
  gt() %>%
  cols_label(
    votes_for = "Members voting for Bank Rate decision"
  ) %>%
  fmt_percent(
    columns = c(`All Meetings`, `Since 2020`),
    decimals = 1
  ) %>%
  tab_header(
    title = "Proportion of meetings by number of members voting for Bank Rate decision"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = c(`All Meetings`, `Since 2020`))
  )
table.voting.sum

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
    hawkish_tilt = mean(vote > bank_rate), # Tendency to vote higher
    dovish_tilt = mean(vote < bank_rate), # Tendency to vote lower
    net_tilt = hawkish_tilt - dovish_tilt,
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

# Plot disagreement trends
plot.disagreement.t <- ggplot(
  disagreement_trends,
  aes(x = date, y = meeting_dissent_rate)
) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.3) +
  labs(
    title = "MPC Disagreement",
    subtitle = "Dissent rate per meeting (loess smoothed, $\alpha$=0.3)",
    y = "Dissent Rate per Meeting",
    x = "Date"
  )
plot.disagreement.t

# Fix the function and calculate historical comparison more carefully
calculate_period_agreement <- function(
  data,
  period_name,
  start_date = NULL,
  end_date = NULL
) {
  # Filter by date range
  period_data <- data
  if (!is.null(start_date)) {
    period_data <- period_data %>% filter(date >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    period_data <- period_data %>% filter(date <= as.Date(end_date))
  }

  print(paste(
    "Analyzing period:",
    period_name,
    "with",
    nrow(period_data),
    "meetings"
  ))

  # Get member columns
  member_cols <- names(period_data)[
    !names(period_data) %in% c("date", "bank_rate")
  ]

  # Find active members (participated in at least 10 meetings)
  active_members <- c()
  for (member in member_cols) {
    meetings_participated <- sum(!is.na(period_data[[member]]))
    if (meetings_participated >= 10) {
      active_members <- c(active_members, member)
    }
  }

  print(paste("Active members:", length(active_members)))

  if (length(active_members) < 2) {
    return(data.frame(
      Period = period_name,
      agreement_pct = NA,
      n_pairs = 0,
      n_members = length(active_members)
    ))
  }

  # Calculate all pairwise agreements
  all_agreements <- c()
  pair_count <- 0

  for (i in 1:(length(active_members) - 1)) {
    for (j in (i + 1):length(active_members)) {
      member1 <- active_members[i]
      member2 <- active_members[j]

      votes1 <- period_data[[member1]]
      votes2 <- period_data[[member2]]

      # Find meetings where both voted
      both_voted <- !is.na(votes1) & !is.na(votes2)
      common_meetings <- sum(both_voted)

      if (common_meetings >= 5) {
        identical_votes <- sum(votes1[both_voted] == votes2[both_voted])
        agreement_rate <- identical_votes / common_meetings
        all_agreements <- c(all_agreements, agreement_rate)
        pair_count <- pair_count + 1
      }
    }
  }
  if (length(all_agreements) > 0) {
    mean_agreement <- mean(all_agreements) * 100
  } else {
    mean_agreement <- NA
  }

  return(data.frame(
    Period = period_name,
    agreement_pct = round(mean_agreement, 1),
    n_pairs = pair_count,
    n_members = length(active_members)
  ))
}

# Calculate for different periods
print("=== CALCULATING HISTORICAL PERIODS ===")

current <- calculate_period_agreement(
  mpc,
  "Current (2024-2025)",
  "2024-01-01",
  "2025-08-31"
)
recent <- calculate_period_agreement(
  mpc,
  "Recent (2020-2023)",
  "2020-01-01",
  "2023-12-31"
)
post_crisis <- calculate_period_agreement(
  mpc,
  "Post-Crisis (2010-2019)",
  "2010-01-01",
  "2019-12-31"
)
crisis <- calculate_period_agreement(
  mpc,
  "Crisis (2007-2009)",
  "2007-01-01",
  "2009-12-31"
)
early <- calculate_period_agreement(
  mpc,
  "Early MPC (1997-2006)",
  "1997-06-01",
  "2006-12-31"
)
all_time <- calculate_period_agreement(
  mpc,
  "All Time (1997-2025)",
  "1997-06-01",
  "2025-08-31"
)

# Combine results
historical_comparison <- rbind(
  current,
  recent,
  post_crisis,
  crisis,
  early,
  all_time
)

# Create a bar chart comparing periods
mpc_agree_plot <- historical_comparison %>%
  filter(Period != "All Time (1997-2025)") %>% # Exclude all-time average for clarity
  mutate(
    Period = factor(
      Period,
      levels = c(
        "Early MPC (1997-2006)",
        "Crisis (2007-2009)",
        "Post-Crisis (2010-2019)",
        "Recent (2020-2023)",
        "Current (2024-2025)"
      )
    ),
    is_current = ifelse(
      Period == "Current (2024-2025)",
      "Current",
      "Historical"
    )
  ) %>%
  ggplot(aes(x = Period, y = agreement_pct, fill = is_current)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(agreement_pct, "%")),
    vjust = -0.3,
    size = 3,
    fontface = "bold"
  ) +
  geom_hline(
    yintercept = 77.5,
    linetype = "dashed",
    color = "red",
    alpha = 0.7
  ) +
  scale_fill_manual(
    values = c("Current" = "#e31a1c", "Historical" = "#1f78b4")
  ) +
  scale_y_continuous(limits = c(0, 95), breaks = seq(0, 90, 10)) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  ) +
  labs(
    title = "MPC Voting Agreement",
    subtitle = "Percent of pairwise equal votes",
    x = "",
    y = "Agreement Rate (%)"
  )

print(mpc_agree_plot)

#===============================================
# Scatter plot of hawkish vs dissent tendencies
#===============================================

library(ggrepel)

# Bar-plot summary by MPC member
member_patterns <- member_patterns %>%
  mutate(
    tilt_category = case_when(
      net_tilt > 0.1 ~ "Hawkish",
      net_tilt < -0.1 ~ "Dovish",
      TRUE ~ "Neutral"
    )
  )

# Bar chart with categorical fill
ggplot(
  member_patterns,
  aes(x = reorder(member, net_tilt), y = net_tilt, fill = tilt_category)
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
    title = "MPC Member Net Tilt (Hawkish - Dovish)",
    x = "Member",
    y = "Net Hawkish Tilt",
    fill = "Overall Tendency"
  )

# Scatter net tilt vs dissent rate
plot.member_patterns <- ggplot(
  member_patterns,
  aes(x = net_tilt, y = dissent_rate, color = current_member)
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
