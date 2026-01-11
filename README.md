# Guardian Migration Coverage Analysis (2025)

## Project Overview

This project analyses media coverage of migration in The Guardian newspaper throughout 2025. Using automated data collection via The Guardian's API, this study examines temporal patterns, discursive framing, and institutional coverage of migration-related articles.

## Repository Structure
```
project/
├── config.R                          # API key configuration
├── data_collection.R                 # Main data collection and cleaning script
├── app.R                             # R Shiny interactive dashboard
├── data/
│   ├── guardian_migration_raw.rds   # Raw collected data
│   └── guardian_migration_clean.rds # Cleaned data for analysis
├── report.html                      # Final HTML report
└── README.md                        # This file
```

## Data Collection

### Source
Data collected from [The Guardian Open Platform API](https://open-platform.theguardian.com/)

### Time Period
January 1, 2025 - December 31, 2025

### Search Query
"migrant"

### Variables Collected
- `article_id`: Unique article identifier
- `section`: Guardian section (e.g., Politics, World, Opinion)
- `publication_date`: Date and time of publication
- `web_title`: Article title as appears on website
- `web_url`: URL to full article
- `headline`: Article headline
- `body_text`: Full article text
- `byline`: Author byline
- `word_count`: Article word count
- `date`: Publication date (date only)
- `year`, `month`, `day_of_week`: Temporal components
- `mentions_refugee`, `mentions_asylum`, `mentions_border`, `mentions_immigration`, `mentions_crisis`: Binary indicators for discourse framing
- `article_length`: Categorical variable (Short/Medium/Long)

## Setup Instructions

### Prerequisites

R version 4.0 or higher

### Required Packages

Install packages using:
```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, jsonlite, tidyverse, lubridate, shiny, plotly, shinythemes)
```

### API Key Setup

1. Register for a free API key at [The Guardian Open Platform](https://open-platform.theguardian.com/access/)
2. Create a file called `config.R` in the project root directory
3. Add the following line to `config.R`:
```r
   guardian_api_key <- "YOUR_API_KEY_HERE"
```

## Reproducibility

### Step 1: Data Collection

Run the data collection script:
```r
source("data_collection.R")
```

This will:
1. Load required packages
2. Connect to The Guardian API
3. Collect all migration-related articles from 2025
4. Clean and process the data
5. Save cleaned data to `data/guardian_migration_clean.rds`

### Step 2: Run Interactive Dashboard

Launch the Shiny dashboard:
```r
shiny::runApp("app.R")
```

Or open `app.R` in RStudio and click "Run App"

## Interactive Dashboard Features

The dashboard contains three sociologically-informed visualisations:

### 1. Coverage Over Time (Agenda-Setting Analysis)
- **Purpose**: Examines temporal patterns in media coverage
- **Filters**: Date range, time aggregation (daily/weekly/monthly), section selection
- **Sociological Relevance**: Reveals media agenda-setting, moral panics, and news cycles

### 2. Discourse Framing Analysis
- **Purpose**: Analyzes how migration is linguistically framed
- **Filters**: Date range, absolute count vs. percentage
- **Frames Examined**: Refugee, Asylum, Border, Immigration, Crisis
- **Sociological Relevance**: Tests framing theory and securitization of migration discourse

### 3. Institutional Coverage by Section
- **Purpose**: Examines which Guardian sections cover migration
- **Filters**: Number of sections displayed, average word count overlay
- **Sociological Relevance**: Reveals institutional gatekeeping and editorial priorities

## Data Cleaning Process

The following cleaning steps were applied:

1. Removed duplicate articles (by `article_id`)
2. Filtered out articles with missing body text or headlines
3. Standardized section names (title case)
4. Extracted temporal components (year, month, day of week)
5. Created binary indicators for discourse framing analysis
6. Categorized articles by length
7. Sorted by publication date
