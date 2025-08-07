# functions used in this project

# webscrape MPC mtg dates from BoE website
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

  return(mpc_dates$date)
}

get_fomc_dates <- function(url = url_fed) {
  # Define the date pattern inside the function
  date_pattern <- "\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2}([-–]\\d{1,2})?\\s*,?\\s*2025"

  fed_page <- read_html(url_fed)

  fed_text <- fed_page |>
    html_text() |>
    str_split("\n") |>
    unlist() |>
    str_trim()

  # Look for lines containing 2025 dates
  fed_date_lines <- fed_text[str_detect(fed_text, "2025")] |>
    str_subset(
      "January|February|March|April|May|June|July|August|September|October|November|December"
    )

  fed_dates_extracted <- fed_date_lines |>
    str_extract_all(date_pattern) |>
    unlist() |>
    unique()

  recent_fed_dates <- fed_dates_extracted |>
    mdy()

  return(recent_fed_dates)
}


# Clean Downloaded OIS data from BoE website
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

# Clean Downloaded GLC data from BoE website
cleanGLC <- function(df) {
  # First, handle any NA or empty column names by giving them temporary names
  col_names <- names(df)
  col_names[is.na(col_names) | col_names == ""] <- paste0(
    "temp_col_",
    seq_along(col_names[is.na(col_names) | col_names == ""])
  )
  names(df) <- col_names

  # Get the original column names from the first row of data
  # Skip the first column (which will be dates) and take the rest as column names
  first_row_names <- as.character(unlist(df[1, -1]))
  # Handle any NA values in the first row
  first_row_names[is.na(first_row_names)] <- paste0(
    "col_",
    seq_along(first_row_names[is.na(first_row_names)])
  )

  new_col_names <- c("date", first_row_names)

  # Remove rows with all NA values and the header row
  df <- df %>%
    # Set proper column names
    setNames(new_col_names) %>%
    # Remove the header row (now that we've used it for names)
    slice(-1) %>%
    # Remove any completely empty rows
    filter(!if_all(-date, is.na)) %>%
    # Convert date column to proper Date format (Excel dates)
    mutate(
      date = as.Date(as.numeric(date), origin = "1899-12-30")
    ) %>%
    # Convert all other columns to numeric
    mutate(across(-date, as.numeric)) %>%
    # Clean up column names
    janitor::clean_names() %>%
    # Remove rows with invalid dates
    drop_na(date) %>%
    # Remove any existing row names and convert date to row names
    `rownames<-`(NULL) %>%
    column_to_rownames(var = "date")

  return(df)
}
