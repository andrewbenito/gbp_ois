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
    dplyr::select(date_text, date_clean, date)

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

# for Payrolls dates
get_first_fridays <- function(start_year = 2025, end_year = 2025) {
  # Create all first days of months in the range
  dates <- seq(
    as.Date(paste0(start_year, "-01-01")),
    as.Date(paste0(end_year, "-12-01")),
    by = "month"
  )

  # Calculate first Friday for each month
  first_fridays <- dates + (6 - wday(dates)) %% 7

  # Create results dataframe
  results <- data.frame(
    date = first_fridays,
    year = year(first_fridays),
    month = month(first_fridays),
    month_name = month.name[month(first_fridays)],
    formatted = format(first_fridays, "%A, %B %d, %Y")
  )

  return(results$date)
}

# Clean Downloaded OIS data
cleanOIS <- function(df) {
  # Convert all but first column to numeric
  df <- df %>% mutate(across(-1, as.numeric))

  # Check if row 2 has valid month information
  months_raw <- as.numeric(df[2, -1])

  if (all(is.na(months_raw))) {
    # If row 2 is all NA, use sequential numbering for columns
    colnames(df)[-1] <- paste0("x", 1:(ncol(df) - 1))
  } else {
    # Round the second row (months row, excl. date column)
    months_rounded <- round(months_raw, digits = 0)
    colnames(df)[-1] <- as.character(months_rounded)
  }

  # Remove first 4 rows (header rows) - keeps data starting from row 5
  df <- df %>%
    tail(-4) %>%
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


# Function to scrape major macroeconomic headlines with keyword filtering
scrape_macro_headlines <- function() {
  # Calculate date 6 months ago
  six_months_ago <- Sys.Date() - months(6)

  # Define keywords to filter headlines
  keywords <- c("Fed", "ECB", "BoE", "inflation")
  keyword_pattern <- paste(keywords, collapse = "|")

  headlines_list <- list()

  # 1. Reuters Economics News
  # 2. Financial Times Economics
  tryCatch(
    {
      ft_url <- "https://www.ft.com/global-economy"
      ft_page <- read_html(ft_url)

      ft_headlines <- ft_page %>%
        html_elements("a[data-trackable='heading-link']") %>%
        html_text(trim = TRUE)

      if (length(ft_headlines) > 0) {
        ft_data <- tibble(
          source = "Financial Times",
          headline = ft_headlines[1:min(length(ft_headlines), 15)],
          date = Sys.Date(),
          url = ft_url
        ) %>%
          filter(str_detect(
            headline,
            regex(keyword_pattern, ignore_case = TRUE)
          ))

        if (nrow(ft_data) > 0) {
          headlines_list[["ft"]] <- ft_data
        }
      }
    },
    error = function(e) message("FT scraping failed: ", e$message)
  )

  # 3. BBC Economics News
  tryCatch(
    {
      bbc_url <- "https://www.bbc.co.uk/news/business/economy"
      bbc_page <- read_html(bbc_url)

      bbc_headlines <- bbc_page %>%
        html_elements(
          "h3[data-testid='card-headline'], .gs-c-promo-heading__title"
        ) %>%
        html_text(trim = TRUE)

      if (length(bbc_headlines) > 0) {
        bbc_data <- tibble(
          source = "BBC",
          headline = bbc_headlines[1:min(length(bbc_headlines), 15)],
          date = Sys.Date(),
          url = bbc_url
        ) %>%
          filter(str_detect(
            headline,
            regex(keyword_pattern, ignore_case = TRUE)
          ))

        if (nrow(bbc_data) > 0) {
          headlines_list[["bbc"]] <- bbc_data
        }
      }
    },
    error = function(e) message("BBC scraping failed: ", e$message)
  )

  # 4. Bank of England News & Publications
  tryCatch(
    {
      boe_url <- "https://www.bankofengland.co.uk/news"
      boe_page <- read_html(boe_url)

      boe_headlines <- boe_page %>%
        html_elements("h3 a, .listing-item__title a") %>%
        html_text(trim = TRUE)

      boe_dates <- boe_page %>%
        html_elements(".listing-item__date, time") %>%
        html_text(trim = TRUE) %>%
        dmy() %>%
        na.omit()

      if (length(boe_headlines) > 0) {
        boe_data <- tibble(
          source = "Bank of England",
          headline = boe_headlines[1:min(length(boe_headlines), 15)],
          date = if (length(boe_dates) > 0) {
            boe_dates[1:min(length(boe_headlines), 15)]
          } else {
            Sys.Date()
          },
          url = boe_url
        ) %>%
          filter(date >= six_months_ago) %>%
          filter(str_detect(
            headline,
            regex(keyword_pattern, ignore_case = TRUE)
          ))

        if (nrow(boe_data) > 0) {
          headlines_list[["boe"]] <- boe_data
        }
      }
    },
    error = function(e) message("BoE scraping failed: ", e$message)
  )

  # 5. Federal Reserve News
  tryCatch(
    {
      fed_url <- "https://www.federalreserve.gov/newsevents/pressreleases.htm"
      fed_page <- read_html(fed_url)

      fed_headlines <- fed_page %>%
        html_elements("h3 a, .row__col-1 a") %>%
        html_text(trim = TRUE)

      fed_dates <- fed_page %>%
        html_elements(".time, .eventlist__meta-item") %>%
        html_text(trim = TRUE) %>%
        mdy() %>%
        na.omit()

      if (length(fed_headlines) > 0) {
        fed_data <- tibble(
          source = "Federal Reserve",
          headline = fed_headlines[1:min(length(fed_headlines), 10)],
          date = if (length(fed_dates) > 0) {
            fed_dates[1:min(length(fed_headlines), 10)]
          } else {
            Sys.Date()
          },
          url = fed_url
        ) %>%
          filter(date >= six_months_ago) %>%
          filter(str_detect(
            headline,
            regex(keyword_pattern, ignore_case = TRUE)
          ))

        if (nrow(fed_data) > 0) {
          headlines_list[["fed"]] <- fed_data
        }
      }
    },
    error = function(e) message("Fed scraping failed: ", e$message)
  )

  # Combine all headlines
  if (length(headlines_list) > 0) {
    all_headlines <- bind_rows(headlines_list) %>%
      arrange(desc(date)) %>%
      distinct(headline, .keep_all = TRUE) %>%
      slice_head(n = 50)

    return(all_headlines)
  } else {
    # Fallback data with keyword-relevant headlines
    message("All scraping attempts failed. Returning sample data.")
    return(tibble(
      source = c("Bank of England", "Federal Reserve", "Reuters"),
      headline = c(
        "BoE MPC maintains Bank Rate at 4.75%",
        "Fed FOMC holds federal funds rate steady",
        "UK inflation rises to 3.8% in July"
      ),
      date = c(
        Sys.Date() - days(14),
        Sys.Date() - days(21),
        Sys.Date() - days(7)
      ),
      url = c(
        "https://www.bankofengland.co.uk",
        "https://www.federalreserve.gov",
        "https://www.reuters.com"
      )
    ))
  }
}

headlines <- scrape_macro_headlines()

# Clean MPC voting data ----
clean_mpc_voting <- function(df) {
  # Find where the actual meeting data starts (look for "Meetings" in column 2)
  meetings_row <- which(df[[2]] == "Meetings")[1]

  if (is.na(meetings_row)) {
    stop("Could not find 'Meetings' row in the data")
  }

  # Extract the voting data starting from the row after "Meetings"
  df_votes <- df |>
    slice((meetings_row + 2):n()) |>
    dplyr::select(-1)

  # colnames in row 2, MPC member names excl first column
  mpcnames <- df[2, -1]
  colnames(df_votes) <- mpcnames

  # Convert Excel date serial numbers to proper dates
  df_votes <- df_votes |>
    rename(date = 1, bank_rate = 2) |>
    # First convert bank_rate to numeric for the recoding logic
    mutate(bank_rate = as.numeric(bank_rate) * 100) |>
    # Recode "Increase" to bank_rate +25bp; "Decrease" to bank_rate -25bp
    mutate(
      across(
        -c(date, bank_rate),
        ~ case_when(
          . == "Increase" ~ bank_rate + 0.25,
          . == "Decrease" ~ bank_rate - 0.25,
          TRUE ~ as.numeric(.) * 100 # Convert other numeric values and multiply by 100
        )
      )
    ) |>
    mutate(
      date = as.Date(as.numeric(date), origin = "1899-12-30")
    ) |>
    dplyr::select(-contains("Past")) |> # remove past members column
    filter(!is.na(date))

  return(df_votes)
}

# Function to Classify Market Reaction based on co-movement of Equities and Bonds----
classify_market_reactions <- function(
  data,
  date_col = "date",
  short_yield_col = "gb2yt",
  long_yield_col = "gb10yt",
  equity_col = "ftse",
  value_col = "value",
  variable_col = "variable",
  data_format = c("long", "wide"),
  aggregation_method = "mean",
  return_details = TRUE
) {
  # Match arguments and load libraries
  data_format <- match.arg(data_format)
  library(dplyr)
  library(tidyr)

  # Convert data to wide format if needed
  if (data_format == "long") {
    # Handle aggregation of duplicates
    agg_fun <- switch(
      aggregation_method,
      "mean" = mean,
      "median" = median,
      "first" = function(x) first(x),
      "last" = function(x) last(x),
      mean
    ) # default to mean

    data_wide <- data %>%
      group_by(!!sym(date_col), !!sym(variable_col)) %>%
      summarise(
        !!sym(value_col) := agg_fun(!!sym(value_col), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = !!sym(variable_col),
        values_from = !!sym(value_col)
      )
  } else {
    data_wide <- data
  }

  # Validate required columns
  required_cols <- c(date_col, short_yield_col, long_yield_col, equity_col)
  missing_cols <- setdiff(required_cols, names(data_wide))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Apply classification
  classified_data <- data_wide %>%
    mutate(
      # Calculate absolute changes
      abs_short_yield = abs(!!sym(short_yield_col)),
      abs_long_yield = abs(!!sym(long_yield_col)),

      # Calculate correlation
      yield_equity_corr = cor(
        !!sym(short_yield_col),
        !!sym(equity_col),
        use = "complete.obs"
      )
    ) %>%
    mutate(
      # Classification conditions
      condition_long_lt_short = abs_long_yield < abs_short_yield,
      condition_long_gt_short = abs_long_yield > abs_short_yield,
      condition_positive_corr = yield_equity_corr > 0,
      condition_negative_corr = yield_equity_corr < 0,

      # Apply classification rules
      market_reaction_type = case_when(
        # Growth: |long_yield| < |short_yield| AND correlation > 0
        condition_long_lt_short & condition_positive_corr ~ "Growth",

        # Risk premium: |short_yield| < |long_yield| AND correlation > 0
        condition_long_gt_short & condition_positive_corr ~ "Risk premium",

        # Conventional monetary policy: |long_yield| < |short_yield| AND correlation < 0
        condition_long_lt_short & condition_negative_corr ~
          "Conventional monetary policy",

        # Unconventional monetary policy: |long_yield| > |short_yield| AND correlation < 0
        condition_long_gt_short & condition_negative_corr ~
          "Unconventional monetary policy",

        TRUE ~ "Unclassified"
      )
    )

  # Create summary statistics
  summary_stats <- classified_data %>%
    count(market_reaction_type, name = "frequency") %>%
    mutate(percentage = round(frequency / sum(frequency) * 100, 1))

  # Prepare results
  if (return_details) {
    results <- list(
      classified_data = classified_data,
      summary = summary_stats,
      correlation = unique(classified_data$yield_equity_corr),
      n_observations = nrow(classified_data),
      date_range = range(classified_data[[date_col]], na.rm = TRUE),
      classification_conditions = classified_data %>%
        dplyr::select(
          !!sym(date_col),
          condition_long_lt_short,
          condition_long_gt_short,
          condition_positive_corr,
          condition_negative_corr,
          market_reaction_type
        )
    )
  } else {
    results <- classified_data %>%
      dplyr::select(
        !!sym(date_col),
        !!sym(short_yield_col),
        !!sym(long_yield_col),
        !!sym(equity_col),
        market_reaction_type
      )
  }

  return(results)
}


# function to clean the IMF Fiscal Monitor dat matrix and convert to long format

clean_IMF_FM <- function(
  raw_data,
  metadata_rows = 16,
  remove_na = TRUE,
  convert_numeric = TRUE,
  clean_column_names = TRUE,
  custom_column_names = NULL
) {
  # Step 1: Handle input data format
  if (is.matrix(raw_data)) {
    raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE)
  }

  # Step 2: Extract metadata and year data from the raw matrix structure
  metadata_df <- raw_data[1:metadata_rows, , drop = FALSE]
  year_rows_start <- metadata_rows + 1
  year_df <- raw_data[year_rows_start:nrow(raw_data), , drop = FALSE]
  years <- as.numeric(rownames(year_df))

  # Step 3: Create metadata lookup table
  metadata_t <- as.data.frame(t(metadata_df), stringsAsFactors = FALSE)
  colnames(metadata_t) <- rownames(metadata_df)

  # Step 4: Process data year by year and combine into long format
  all_data <- list()

  for (i in seq_along(years)) {
    year_val <- years[i]

    # Get values for this year and convert to numeric
    values <- if (convert_numeric) {
      as.numeric(year_df[i, ])
    } else {
      as.character(year_df[i, ])
    }

    # Create data frame for this year
    year_data <- data.frame(
      year = year_val,
      country_code = metadata_t$REF_AREA,
      country = metadata_t$REF_AREA_LABEL,
      indicator_label = metadata_t$INDICATOR_LABEL,
      value = values,
      stringsAsFactors = FALSE
    )

    # Remove NA values if requested
    if (remove_na) {
      year_data <- year_data[!is.na(year_data$value), ]
    }

    all_data[[i]] <- year_data
  }

  # Step 5: Combine all years into a single dataframe
  combined_data <- do.call(rbind, all_data)

  # Step 6: Pivot to wide format with indicators as columns
  result <- combined_data %>%
    pivot_wider(
      names_from = indicator_label,
      values_from = value,
      id_cols = c(year, country_code, country)
    )

  # Step 7: Clean column names if requested
  if (clean_column_names) {
    if (!is.null(custom_column_names)) {
      # Use custom column names if provided and they match the number of columns
      if (length(custom_column_names) == ncol(result)) {
        colnames(result) <- custom_column_names
      } else {
        warning(
          "Custom column names length doesn't match number of columns. Using automatic cleaning."
        )
        clean_column_names <- TRUE # Fall back to automatic cleaning
      }
    }

    # Automatic column name cleaning (if custom names not used or don't match)
    if (
      is.null(custom_column_names) ||
        length(custom_column_names) != ncol(result)
    ) {
      old_names <- colnames(result)
      new_names <- old_names

      # Clean indicator column names (skip year, country_code, country)
      indicator_cols <- setdiff(old_names, c("year", "country_code", "country"))

      for (col in indicator_cols) {
        clean_name <- col %>%
          gsub("\\(.*?\\)", "", .) %>% # Remove parentheses and content
          gsub("[^A-Za-z0-9\\s]", "", .) %>% # Remove special chars
          gsub("also referred as.*", "", .) %>% # Remove long explanations
          trimws() %>% # Trim whitespace
          gsub("\\s+", "_", .) %>% # Replace spaces with underscores
          tolower() # Convert to lowercase

        new_names[old_names == col] <- clean_name
      }

      colnames(result) <- new_names
    }
  }

  # Step 8: Arrange by year and country for better organization
  result <- result %>%
    arrange(year, country_code)

  return(result)
}

# clean raw WEO data
clean_IMF_WEO <- function(dataf) {
  result <- dataf |>
    pivot_longer(
      cols = starts_with("x"),
      names_prefix = "x",
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      year = as.numeric(ifelse(
        trimws(tolower(year)) %in% c("n/a", "na", ""),
        NA,
        year
      )),
      value = as.numeric(ifelse(
        trimws(tolower(value)) %in% c("n/a", "na", ""),
        NA,
        value
      ))
    )

  return(result)
}
