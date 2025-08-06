# functions used in this project

get_mpc_dates <- function(url = url_boe) {
  page <- read_html(url) |>
    html_nodes("table")

  mpc_dates <- page[[2]] |>
    html_table() |>
    rename(date_text = 1) |>
    mutate(
      # Clean up the date text to extract just the date part
      date_clean = str_extract(date_text, "\\d{1,2} [A-Za-z]+"),
      # Parse dates assuming 2025
      date = dmy(paste0(date_clean, " 2025"))
    ) |>
    filter(!is.na(date)) |>
    select(date_text, date_clean, date)

  return(mpc_dates)
}

cleanOIS <- function(df) {
  # Convert all but first column to numeric
  df <- df %>% mutate(across(-1, as.numeric))

  # Round the second row (months row, excl. date column)
  months_rounded <- round(as.numeric(df[2, -1]), digits = 0)
  colnames(df)[-1] <- as.character(months_rounded) # set as column names

  # Clean up - remove rows, set column names, etc.
  df <- df %>%
    tail(-5) %>%
    type.convert(as.is = TRUE) %>%
    rename(date = 1) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    janitor::clean_names() %>%
    drop_na(date) %>%
    column_to_rownames(var = "date")

  return(df)
}
