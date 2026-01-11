pacman::p_load(
  httr,        # For API requests
  jsonlite,    # For parsing JSON responses
  tidyverse,   # For data wrangling
  lubridate    # For date handling
)

source("config.R") #Load in API

# Function to search Guardian articles
get_guardian_articles <- function(query, from_date, to_date, page = 1) {
  
# Base URL for Guardian API
base_url <- "https://content.guardianapis.com/search"
  
# Set up parameters
params <- list(
    `q` = query,
    `from-date` = from_date,
    `to-date` = to_date,
    `page` = page,
    `page-size` = 50,  # Max 50 results per page
    `show-fields` = "headline,bodyText,byline,wordcount",
    `api-key` = guardian_api_key
  )
  
# Make the API request
 response <- GET(base_url, query = params)
  
# Check if request was successful
  if (status_code(response) == 200) {
    # Parse JSON response
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content, flatten = TRUE)
    return(data)
  } else {
    stop(paste("API request failed with status:", status_code(response)))
  }
}

# Test the function
test_data <- get_guardian_articles(
  query = "climate change",
  from_date = "2024-01-01",
  to_date = "2024-12-31",
  page = 1
)

# View the structure
str(test_data)

# Extract just the articles from the nested structure
extract_articles <- function(api_response) {
  
  # Get the results (articles)
  articles <- api_response$response$results
  
  # Select and rename important columns
  clean_data <- articles %>%
    select(
      article_id = id,
      section = sectionName,
      publication_date = webPublicationDate,
      web_title = webTitle,
      web_url = webUrl,
      headline = fields.headline,
      body_text = fields.bodyText,
      byline = fields.byline,
      word_count = fields.wordcount
    ) %>%
    mutate(
      publication_date = ymd_hms(publication_date),  # Convert to datetime
      word_count = as.numeric(word_count)
    )
  
  return(clean_data)
}

# Test
test_df <- extract_articles(test_data)
View(test_df)

# Function to collect all pages for a query
collect_all_articles <- function(query, from_date, to_date, max_pages = 10) {
  
  all_articles <- list()
  
  # Get first page to see total pages
  first_page <- get_guardian_articles(query, from_date, to_date, page = 1)
  total_pages <- min(first_page$response$pages, max_pages)
  
  cat("Collecting", total_pages, "pages...\n")
  
  # Loop through all pages
  for (page in 1:total_pages) {
    cat("Page", page, "of", total_pages, "\n")
    
    # Get data
    page_data <- get_guardian_articles(query, from_date, to_date, page = page)
    
    # Extract articles
    articles <- extract_articles(page_data)
    
    # Add to list
    all_articles[[page]] <- articles
    
    # Be polite to the API - add delay
    Sys.sleep(0.5)
  }
  
  # Combine all pages into one data frame
  final_data <- bind_rows(all_articles)
  
  return(final_data)
}

# Collect data on your chosen migration
  my_data <- collect_all_articles(
         query = "migrant",
         from_date = "2025-01-01",
         to_date = "2025-12-31",
         max_pages = 50)
  dim(my_data)
  summary(my_data)
  View(my_data)
  
#Data wrangling
# Check for issues in your data
# Look at missing values
  colSums(is.na(my_data))
  
  # Remove duplicates if any
  my_data <- my_data %>%
    distinct(article_id, .keep_all = TRUE)
  
  # Clean the data
  # Remove duplicates if any
  my_data <- my_data %>%
    distinct(article_id, .keep_all = TRUE)
  
  # Clean the data
  my_data_clean <- my_data %>%
    # Remove articles with no body text
    filter(!is.na(body_text), body_text != "") %>%
    # Remove articles with no headline
    filter(!is.na(headline), headline != "") %>%
    # Clean section names
    mutate(section = str_to_title(section)) %>%
    # Extract date components for analysis
    mutate(
      year = year(publication_date),
      month = month(publication_date, label = TRUE),
      day_of_week = wday(publication_date, label = TRUE),
      date = as_date(publication_date)
    ) %>%
    arrange(date)
  
  # Check cleaned data
  dim(my_data_clean)
  summary(my_data_clean)
  View(my_data_clean)
  
  # Enhanced cleaning for sociological analysis
  my_data_clean <- my_data %>%
    distinct(article_id, .keep_all = TRUE) %>%
    filter(!is.na(body_text), body_text != "",
           !is.na(headline), headline != "") %>%
    mutate(
      section = str_to_title(section),
      year = year(publication_date),
      month = month(publication_date, label = TRUE),
      day_of_week = wday(publication_date, label = TRUE),
      date = as_date(publication_date),
      
      # Framing indicators
      mentions_refugee = str_detect(tolower(body_text), "refugee"),
      mentions_asylum = str_detect(tolower(body_text), "asylum"),
      mentions_border = str_detect(tolower(body_text), "border"),
      mentions_immigration = str_detect(tolower(body_text), "immigration"),
      mentions_crisis = str_detect(tolower(body_text), "crisis"),
      
      # Article length category
      article_length = case_when(
        word_count < 500 ~ "Short",
        word_count < 1000 ~ "Medium",
        word_count >= 1000 ~ "Long"
      )
    ) %>%
    arrange(date)

  # Save the data for use in Shiny app
  dir.create("data", showWarnings = FALSE)
  
  # Save cleaned data
  saveRDS(my_data_clean, "data/guardian_migration_clean.rds")
  write_csv(my_data_clean, "data/guardian_migration_clean.csv")
  
  # Also save raw data for reproducibility
  saveRDS(my_data, "data/guardian_migration_raw.rds")
  
  cat("Data collection complete!\n")
  cat("Cleaned data saved to: data/guardian_migration_clean.rds\n")
  cat("Total articles:", nrow(my_data_clean), "\n")